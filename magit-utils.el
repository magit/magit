;;; magit-utils.el --- various utilities

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Contains code from GNU Emacs https://www.gnu.org/software/emacs,
;; released under the GNU General Public License version 3 or later.

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Code:

(require 'cl-lib)
(require 'dash)

(eval-when-compile (require 'ido))
(declare-function ido-completing-read 'ido)

;;; Options

(defcustom magit-completing-read-function 'magit-builtin-completing-read
  "Function to be called when requesting input from the user."
  :group 'magit
  :type '(radio (function-item magit-builtin-completing-read)
                (function-item magit-ido-completing-read)
                (function-item helm-completing-read-with-cands-in-buffer)
                (function :tag "Other")))

(defcustom magit-no-confirm nil
  "A list of symbols for actions Magit should not confirm, or t.
Actions are: `reverse', `discard', `rename', `resurrect',
`trash', `delete', `abort-merge', `merge-dirty', `drop-stashes'
`reset-bisect', `kill-process', `delete-unmerged-branch',
`stage-all-changes' and `unstage-all-changes'.  If t then don't
require confirmation for any of these actions."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type '(choice (const :tag "No confirmation needed" t)
                 (set (const reverse)           (const discard)
                      (const rename)            (const resurrect)
                      (const trash)             (const delete)
                      (const abort-merge)       (const merge-dirty)
                      (const drop-stashes)      (const resect-bisect)
                      (const kill-process)      (const delete-unmerged-branch)
                      (const stage-all-changes) (const unstage-all-changes))))

(defcustom magit-ellipsis ?…
  "Character used to abreviate text."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'character)

;;; Compatibility

(eval-and-compile

  ;; Added in Emacs 24.3
  (unless (fboundp 'user-error)
    (defalias 'user-error 'error))

  ;; Added in Emacs 24.3 (mirrors/emacs@b335efc3).
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  ;; Added in Emacs 24.3 (mirrors/emacs@b335efc3).
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var)))))

  ;; Added in Emacs 24.4
  (unless (fboundp 'with-current-buffer-window)
    (defmacro with-current-buffer-window (buffer-or-name action quit-function &rest body)
      "Evaluate BODY with a buffer BUFFER-OR-NAME current and show that buffer.
This construct is like `with-temp-buffer-window' but unlike that
makes the buffer specified by BUFFER-OR-NAME current for running
BODY."
      (declare (debug t))
      (let ((buffer (make-symbol "buffer"))
            (window (make-symbol "window"))
            (value (make-symbol "value")))
        `(let* ((,buffer (temp-buffer-window-setup ,buffer-or-name))
                (standard-output ,buffer)
                ,window ,value)
           (with-current-buffer ,buffer
             (setq ,value (progn ,@body))
             (setq ,window (temp-buffer-window-show ,buffer ,action)))

           (if (functionp ,quit-function)
               (funcall ,quit-function ,window ,value)
             ,value)))))

  ;; Added in Emacs 24.4
  (unless (fboundp 'string-suffix-p)
    (defun string-suffix-p (suffix string  &optional ignore-case)
      "Return non-nil if SUFFIX is a suffix of STRING.
If IGNORE-CASE is non-nil, the comparison is done without paying
attention to case differences."
      (let ((start-pos (- (length string) (length suffix))))
        (and (>= start-pos 0)
             (eq t (compare-strings suffix nil nil
                                    string start-pos nil ignore-case))))))
  )

;;; User Input

(defun magit-completing-read
  (prompt collection &optional predicate require-match initial-input hist def)
  "Magit wrapper around `completing-read' or an alternative function.

Option `magit-completing-read-function' can be used to wrap
around another `completing-read'-like function.  Unless it
doesn't have the exact same signature, an additional wrapper is
required.  Even if it has the same signature it might be a good
idea to wrap it, so that `magit-prompt-with-default' can be used.

See `completing-read' for the meanings of the arguments, but note
that this wrapper makes the following changes:

- If REQUIRE-MATCH is nil and the user exits without a choice,
  then return nil instead of an empty string.

- If REQUIRE-MATCH is non-nil and the users exits without a
  choice, then raise an user-error.

- \": \" is appended to PROMPT.

- If a `magit-completing-read-function' is used which in turn
  uses `magit-prompt-with-completion' and DEF is non-nil, then
  PROMPT is modified to end with \" (default DEF): \".

The use of another completing function and/or wrapper obviously
results in additional differences."
  (let ((reply (funcall magit-completing-read-function
                        (concat prompt ": ") collection predicate
                        require-match initial-input hist def)))
    (if (string= reply "")
        (if require-match
            (user-error "Nothing selected")
          nil)
      reply)))

(defun magit-builtin-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "Magit wrapper for standard `completing-read' function."
  (completing-read (magit-prompt-with-default prompt def)
                   choices predicate require-match
                   initial-input hist def))

(defun magit-ido-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "Ido-based completing-read almost-replacement."
  (require 'ido)
  (let ((reply (ido-completing-read
                prompt
                (if (consp (car choices))
                    (mapcar #'car choices)
                  choices)
                predicate require-match initial-input hist def)))
    (or (and (consp (car choices))
             (cdr (assoc reply choices)))
        reply)))

(defun magit-prompt-with-default (prompt def)
  (if (and def (> (length prompt) 2)
           (string-equal ": " (substring prompt -2)))
      (format "%s (default %s): " (substring prompt 0 -2) def)
    prompt))

(defun magit-read-string
    (prompt &optional initial-input history default-value)
  (let ((reply (read-string (magit-prompt-with-default
                             (concat prompt ": ") default-value)
                            initial-input history default-value)))
    (if (string= reply "")
        (user-error "Need non-empty input")
      reply)))

(defmacro magit-read-char-case (prompt verbose &rest clauses)
  (declare (indent 2)
           (debug (form form &rest (characterp form body))))
  `(pcase (read-char-choice
           (concat ,prompt
                   ,(concat (mapconcat 'cadr clauses ", ")
                            (and verbose ", or [C-g] to abort") " "))
           ',(mapcar 'car clauses))
     ,@(--map `(,(car it) ,@(cddr it)) clauses)))

(cl-defun magit-confirm (action &optional prompt prompt-n (items nil sitems))
  (declare (indent defun))
  (setq prompt-n (format (concat (or prompt-n prompt) "? ") (length items))
        prompt   (format (concat (or prompt (magit-confirm-make-prompt action))
                                 "? ")
                         (car items)))
  (cond ((and (not (eq action t))
              (or (eq magit-no-confirm t)
                  (memq action magit-no-confirm)))
         (or (not sitems) items))
        ((not sitems)
         (y-or-n-p prompt))
        ((= (length items) 1)
         (and (y-or-n-p prompt) items))
        ((> (length items) 1)
         (let ((buffer (get-buffer-create " *Magit Confirm*")))
           (with-current-buffer buffer
             (with-current-buffer-window
              buffer (cons 'display-buffer-below-selected
                           '((window-height . fit-window-to-buffer)))
              (lambda (window _value)
                (with-selected-window window
                  (unwind-protect (and (y-or-n-p prompt-n) items)
                    (when (window-live-p window)
                      (quit-restore-window window 'kill)))))
              (dolist (item items)
                (insert item "\n"))))))))

(defun magit-confirm-files (action files &optional prompt)
  (when files
    (unless prompt
      (setq prompt (magit-confirm-make-prompt action)))
    (magit-confirm action
      (concat prompt " %s")
      (concat prompt " %i files")
      files)))

(defun magit-confirm-make-prompt (action)
  (let ((prompt (symbol-name action)))
    (replace-regexp-in-string
     "-" " " (concat (upcase (substring prompt 0 1)) (substring prompt 1)))))

;;; Text Utilities

(defmacro magit-bind-match-strings (varlist string &rest body)
  (declare (indent 2) (debug (listp form body)))
  (let ((s (cl-gensym "string"))
        (i 0))
    `(let ((,s ,string))
       (let ,(save-match-data
               (--map (list it (list 'match-string (cl-incf i) s)) varlist))
         ,@body))))

(defun magit-string-pad (string width)
  (concat string (make-string (max 0 (- width (length string))) ?\s)))

(defun magit-delete-line ()
  "Delete the rest of the current line."
  (delete-region (point) (1+ (line-end-position))))

(defun magit-delete-match (&optional num)
  "Delete text matched by last search.
If optional NUM is specified only delete that subexpression."
  (delete-region (match-beginning (or num 0))
                 (match-end (or num 0))))

(defun magit-file-line (file)
  "Return the first line of FILE as a string."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min)
                                      (line-end-position)))))

(defun magit-file-lines (file &optional keep-empty-lines)
  "Return a list of strings containing one element per line in FILE.
Unless optional argument KEEP-EMPTY-LINES is t, trim all empty lines."
  (when (file-regular-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (split-string (buffer-string) "\n" (not keep-empty-lines)))))

(defun magit-face-remap-set-base (face &optional base)
  (make-local-variable 'face-remapping-alist)
  (--if-let (assq face  face-remapping-alist)
      (if base
          (setcar (last it) base)
        (if (cddr it)
            (setcar (last it) face)
          (setq face-remapping-alist (remq it face-remapping-alist))))
    (when base
      (push (list face base) face-remapping-alist))))

;;; magit-utils.el ends soon
(provide 'magit-utils)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; magit-utils.el ends here
