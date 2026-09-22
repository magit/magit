;;; magit-transient.el --- Support for transients  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements Magit-specific prefix and suffix classes,
;; and their methods.

;;; Code:

(require 'magit-git)
(require 'magit-mode)
(require 'magit-process)

(require 'transient)

;;; Classes

(defclass magit--git-variable (transient-variable)
  ((scope       :initarg :scope)
   (global      :initarg :global      :initform nil)
   (default     :initarg :default     :initform nil)
   (location                          :initform nil)
   (accessible-format                 :initform "%i%k %d is %v")))

(defclass magit--git-variable:choices (magit--git-variable)
  ((choices     :initarg :choices)
   (fallback    :initarg :fallback    :initform nil)))

(defclass magit--git-variable:boolean (magit--git-variable:choices)
  ((choices     :initarg :choices     :initform '("true" "false"))))

(defclass magit--git-variable:urls (magit--git-variable)
  ((seturl-arg  :initarg :seturl-arg  :initform nil)))

;;; Methods
;;;; Init

(cl-defmethod transient-init-scope ((obj magit--git-variable))
  (oset obj scope
        (cond (transient--prefix
               (oref transient--prefix scope))
              ((slot-boundp obj 'scope)
               (funcall (oref obj scope) obj)))))

(cl-defmethod transient-init-value ((obj magit--git-variable))
  (oset obj variable (format (oref obj variable) (oref obj scope)))
  (cond-let
    ((oref obj global)
     (oset obj location 'global)
     (oset obj value (magit--git-variable-get obj "--global")))
    ([_(magit-get-boolean "extensions.worktreeConfig")]
     [val (magit--git-variable-get obj "--worktree")]
     (oset obj location 'worktree)
     (oset obj value val))
    (t
     (oset obj location 'local)
     (oset obj value (magit--git-variable-get obj "--local")))))

(defun magit--git-variable-get (obj arg)
  (let ((var (oref obj variable)))
    (cond
      ((cl-typep obj 'magit--git-variable:boolean)
       (with-temp-buffer
         (and (zerop (magit-process-git t "config" "--bool" arg var))
              (buffer-substring (point-min) (1- (point-max))))))
      ((oref obj multi-value)
       (magit-get-all arg var))
      ((magit-get arg var)))))

;;;; Read

(cl-defmethod transient-infix-read :around ((obj magit--git-variable:urls))
  (transient--with-emergency-exit
    (transient--with-suspended-override
     (mapcar (lambda (url)
               (if (string-prefix-p "~" url)
                   (expand-file-name url)
                 url))
             (cl-call-next-method obj)))))

(cl-defmethod transient-infix-read :around ((obj magit--git-variable))
  (pcase (list (and (= (prefix-numeric-value current-prefix-arg) 16)
                    (magit-get-boolean "extensions.worktreeConfig"))
               (oref obj location))
    ('(t local)
     (oset obj location 'worktree)
     (oref obj value))
    ('(t worktree)
     (oset obj location 'local)
     (magit-set nil "--worktree" (oref obj variable))
     (magit--git-variable-get obj "--local"))
    (_ (cl-call-next-method obj))))

(cl-defmethod transient-infix-read ((obj magit--git-variable:choices))
  (let ((choices (oref obj choices)))
    (when (functionp choices)
      (setq choices (funcall choices)))
    (cond-let
      ((or transient-prefer-reading-value current-prefix-arg)
       (pcase-let*
           ((`(,unset . ,choices)
             (magit--git-variable-list-choices obj))
            (unset (or unset "(unset)"))
            (choice (magit-completing-read
                     (format "Set `%s' to" (oref obj variable))
                     (nconc (mapcar #'magit--delete-text-properties choices)
                            (list (propertize unset 'face
                                              'transient-inactive-value)))
                     nil t)))
         (if (equal choice unset) nil choice)))
      ([value (oref obj value)]
       (cadr (member value choices)))
      ((car choices)))))

;;;; Readers

(defun magit-transient-read-person (prompt initial-input history)
  (magit-completing-read
   prompt (magit-list-people) nil nil initial-input history))

(defun magit-transient-read-persons (prompt initial-input history)
  (magit-completing-read-multiple
   prompt (magit-list-people) nil nil initial-input history))

(defun magit-list-people ()
  (mapcar (##save-excursion
            (and (string-match "\\`[\s\t]+[0-9]+\t" %)
                 (list (substring % (match-end 0)))))
          (magit-git-lines "shortlog" "-n" "-s" "-e" "HEAD")))

(defun magit-transient-read-revision (prompt initial-input history)
  (magit-completing-read prompt (cons "HEAD" (magit-list-refnames))
                         nil 'any initial-input history
                         (or (magit-branch-or-commit-at-point)
                             (magit-get-current-branch))))

;;;; Set

(cl-defmethod transient-infix-set ((obj magit--git-variable) value)
  (let ((variable (oref obj variable))
        (arg (format "--%s" (oref obj location))))
    (oset obj value value)
    (if (oref obj multi-value)
        (magit-set-all value arg variable)
      (magit-set value arg variable))
    (magit-refresh)
    (unless (or value transient--prefix)
      (message "Unset %s" variable))))

(cl-defmethod transient-infix-set ((obj magit--git-variable:urls) values)
  (let ((previous (oref obj value))
        (seturl   (oref obj seturl-arg))
        (remote   (oref transient--prefix scope)))
    (oset obj value values)
    (dolist (v (cl-set-difference values previous :test #'equal))
      (magit-call-git "remote" "set-url" seturl "--add" remote v))
    (dolist (v (cl-set-difference previous values :test #'equal))
      (magit-call-git "remote" "set-url" seturl "--delete" remote
                      (concat "^" (regexp-quote v) "$")))
    (magit-refresh)))

;;;; Draw

(cl-defmethod transient-format-description ((obj magit--git-variable))
  (concat (and (eq (oref obj location) 'worktree)
               "worktree's ")
          (or (oref obj description)
              (oref obj variable))))

(cl-defmethod transient-format-value ((obj magit--git-variable))
  (cond-let*
    ([value (oref obj value)]
     (if (oref obj multi-value)
         (if (cdr value)
             (mapconcat (##concat "\n     "
                                  (propertize % 'face 'transient-value))
                        value "")
           (propertize (car value) 'face 'transient-value))
       (propertize (car (split-string value "\n"))
                   'face 'transient-value)))
    ([default (oref obj default)]
     [default (if (functionp default) (funcall default) default)]
     (if transient-prefer-reading-value
         (format "unset, using default, which is %s"
                 (propertize default 'face 'transient-value))
       (concat (propertize "default:" 'face 'transient-inactive-value)
               (propertize default 'face 'transient-value))))
    ((propertize "unset" 'face 'transient-inactive-value))))

(cl-defmethod transient-format-value ((obj magit--git-variable:choices))
  (if transient-prefer-reading-value
      (cl-call-next-method)
    (pcase-let ((`(,fallback . ,choices) (magit--git-variable-list-choices obj)))
      (concat
       (propertize "[" 'face 'transient-inactive-value)
       (string-join choices (propertize "|" 'face 'transient-inactive-value))
       (and fallback (propertize "|" 'face 'transient-inactive-value))
       fallback
       (propertize "]" 'face 'transient-inactive-value)))))

(defun magit--git-variable-list-choices (obj)
  (with-slots (choices value location) obj
    (when (functionp choices)
      (setq choices (funcall choices)))
    (cons
     (cond-let*
       ([_(eq location 'worktree)]
        [local (magit--git-variable-get obj "--local")]
        (propertize (concat "local:" local)
                    'face (cond (value
                                 'transient-inactive-value)
                                ((member local choices)
                                 'transient-value)
                                ('font-lock-warning-face))))
       ([_(not (eq location 'global))]
        [global (magit--git-variable-get obj "--global")]
        (propertize (concat "global:" global)
                    'face (cond (value
                                 'transient-inactive-value)
                                ((member global choices)
                                 'transient-value)
                                ('font-lock-warning-face))))
       ([fallback* (oref obj fallback)]
        [fallback  (magit-get fallback*)]
        (propertize (concat fallback* ":" fallback)
                    'face (if value
                              'transient-inactive-value
                            'transient-value)))
       ([default* (oref obj default)]
        [default  (if (functionp default*) (funcall default* obj) default*)]
        (propertize (if (functionp default*)
                        (concat "dwim:" default)
                      (concat "default:" default))
                    'face (if value
                              'transient-inactive-value
                            'transient-value))))
     (mapcar (lambda (choice)
               (propertize choice
                           'face (if (equal choice value)
                                     (if (member choice choices)
                                         'transient-value
                                       'font-lock-warning-face)
                                   'transient-inactive-value)))
             (if (and value (not (member value choices)))
                 (cons value choices)
               choices)))))

;;; _
(provide 'magit-transient)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("thread$"      . "cond-let--thread$")
;;   ("when$"        . "cond-let--when$")
;;   ("and-let*"     . "cond-let--and-let*")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let*"      . "cond-let--if-let*")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let*"    . "cond-let--when-let*")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let*"   . "cond-let--while-let*")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; magit-transient.el ends here
