;;; magit-utils.el --- various utilities  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

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

;;; Commentary:

;; This library defines several utility functions used by several
;; other libraries which cannot depend on one another (because
;; circular dependencies are not good).  Luckily most (all) of these
;; functions have very little (nothing) to do with Git, so we not only
;; have to do this, it even makes sense.

;; Unfortunately there are also some options which are used by several
;; libraries which cannot depend on one another, they are defined here
;; too.

;;; Code:

(require 'cl-lib)
(require 'dash)

(eval-when-compile (require 'ido))
(declare-function ido-completing-read+ 'ido-completing-read+)

(defvar magit-wip-before-change-mode)

;;; Options

(defcustom magit-completing-read-function 'magit-builtin-completing-read
  "Function to be called when requesting input from the user.

For Helm users, the simplest way to get Helm completion is to
turn on `helm-mode' and leave this option set to the default
value.  However, if you prefer to not use `helm-mode' but still
want Magit to use Helm for completion, you can set this option to
`helm--completing-read-default'."
  :group 'magit
  :type '(radio (function-item magit-builtin-completing-read)
                (function-item magit-ido-completing-read)
                (function-item helm--completing-read-default)
                (function :tag "Other")))

(defcustom magit-no-confirm nil
  "A list of symbols for actions Magit should not confirm, or t.

Many potentially dangerous commands by default ask the user for
confirmation.  Each of the below symbols stands for an action
which, when invoked unintentionally or without being fully aware
of the consequences, could lead to tears.  In many cases there
are several commands that perform variations of a certain action,
so we don't use the command names but more generic symbols.

Applying changes:

  `discard' Discarding one or more changes (i.e. hunks or the
  complete diff for a file) loses that change, obviously.

  `reverse' Reverting one or more changes can usually be undone
  by reverting the reversion.

  `stage-all-changes', `unstage-all-changes' When there are both
  staged and unstaged changes, then un-/staging everything would
  destroy that distinction.  Of course that also applies when
  un-/staging a single change, but then less is lost and one does
  that so often that having to confirm every time would be
  unacceptable.

Files:

  `delete' When a file that isn't yet tracked by Git is deleted
  then it is completely lost, not just the last changes.  Very
  dangerous.

  `trash' Instead of deleting a file it can also be move to the
  system trash.  Obviously much less dangerous than deleting it.

  Also see option `magit-delete-by-moving-to-trash'.

  `resurrect' A deleted file can easily be resurrected by
  \"deleting\" the deletion, which is done using the same command
  that was used to delete the same file in the first place.

  `rename' Renaming a file can easily be undone.

Sequences:

  `reset-bisect' Aborting (known to Git as \"resetting\") a
  bisect operation loses all information collected so far.

  `abort-merge' Aborting a merge throws away all conflict
  resolutions which has already been carried out by the user.

  `merge-dirty' Merging with a dirty worktree can make it hard to
  go back to the state before the merge was initiated.

References:

  `delete-unmerged-branch' Once a branch has been deleted it can
  only be restored using low-level recovery tools provided by
  Git.  And even then the reflog is gone.  The user always has
  to confirm the deletion of a branch by accepting the default
  choice (or selecting another branch), but when a branch has
  not been merged yet, also make sure the user is aware of that.

  `drop-stashes' Dropping a stash is dangerous because Git stores
  stashes in the reflog.  Once a stash is removed, there is no
  going back without using low-level recovery tools provided by
  Git.  When a single stash is dropped, then the user always has
  to confirm by accepting the default (or selecting another).
  This action only concerns the deletion of multiple stashes at
  once.

Various:

  `kill-process' There seldom is a reason to kill a process.

Global settings:

  Instead of adding all of the above symbols to the value of this
  option you can also set it to the atom `t', which has the same
  effect as adding all of the above symbols.  Doing that most
  certainly is a bad idea, especially because other symbols might
  be added in the future.  So even if you don't want to be asked
  for confirmation for any of these actions, you are still better
  of adding all of the respective symbols individually.

  When `magit-wip-before-change-mode' is enabled then these actions
  can fairly easily be undone: `discard', `reverse',
  `stage-all-changes', and `unstage-all-changes'.  If and only if
  this mode is enabled then `safe-with-wip' has the same effect
  as adding all of these symbols individually."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type '(choice (const :tag "No confirmation needed" t)
                 (set (const reverse)           (const discard)
                      (const rename)            (const resurrect)
                      (const trash)             (const delete)
                      (const abort-merge)       (const merge-dirty)
                      (const drop-stashes)      (const resect-bisect)
                      (const kill-process)      (const delete-unmerged-branch)
                      (const stage-all-changes) (const unstage-all-changes)
                      (const safe-with-wip))))

(defcustom magit-ellipsis ?…
  "Character used to abbreviate text."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'character)

(defcustom magit-update-other-window-delay 0.2
  "Delay before automatically updating the other window.

When moving around in certain buffers certain other buffers,
which are being displayed in another window, may optionally be
updated to display information about the section at point.

When holding down a key to move by more than just one section,
then that would update that buffer for each section on the way.
To prevent that, updating the revision buffer is delayed, and
this option controls for how long.  For optimal experience you
might have to adjust this delay and/or the keyboard repeat rate
and delay of your graphical environment or operating system."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'number)

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
  "Ido-based `completing-read' almost-replacement.

Unfortunately `ido-completing-read' is not suitable as a
drop-in replacement for `completing-read', instead we use
`ido-completing-read+' from the third-party package by the
same name."
  (if (require 'ido-completing-read+ nil t)
      (ido-completing-read+ prompt choices predicate require-match
                            initial-input hist def)
    (display-warning 'magit "ido-completing-read+ is not installed

To use Ido completion with Magit you need to install the
third-party `ido-completing-read+' packages.  Falling
back to built-in `completing-read' for now." :error)
    (magit-builtin-completing-read prompt choices predicate require-match
                                   initial-input hist def)))

(defun magit-prompt-with-default (prompt def)
  (if (and def (> (length prompt) 2)
           (string-equal ": " (substring prompt -2)))
      (format "%s (default %s): " (substring prompt 0 -2) def)
    prompt))

(defvar magit-minibuffer-local-ns-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\s" 'magit-whitespace-disallowed)
    (define-key map "\t" 'magit-whitespace-disallowed)
    map))

(defun magit-whitespace-disallowed ()
  "Beep to tell the user that whitespace is not allowed."
  (interactive)
  (ding)
  (message "Whitespace isn't allowed here")
  (setq defining-kbd-macro nil)
  (force-mode-line-update))

(defun magit-read-string (prompt &optional initial-input history default-value
                                 inherit-input-method no-whitespace)
  "Read a string from the minibuffer, prompting with string PROMPT.

This is similar to `read-string', but
* empty input is only allowed if DEFAULT-VALUE is non-nil in
  which case that is returned,
* whitespace is not allowed if NO-WHITESPACE is non-nil,
* \": \" is appended to PROMPT, and
* an invalid DEFAULT-VALUE is silently ignored."
  (when default-value
    (when (consp default-value)
      (setq default-value (car default-value)))
    (unless (stringp default-value)
      (setq default-value nil)))
  (let* ((minibuffer-completion-table nil)
         (val (read-from-minibuffer
               (magit-prompt-with-default (concat prompt ": ") default-value)
               initial-input (and no-whitespace magit-minibuffer-local-ns-map)
               nil history default-value inherit-input-method)))
    (when (and (string= val "") default-value)
      (setq val default-value))
    (cond ((string= val "")
           (user-error "Need non-empty input"))
          ((and no-whitespace (string-match-p "[\s\t\n]" val))
           (user-error "Input contains whitespace"))
          (t val))))

(defun magit-read-string-ns (prompt &optional initial-input history
                                    default-value inherit-input-method)
  "Call `magit-read-string' with non-nil NO-WHITESPACE."
  (magit-read-string prompt initial-input history default-value
                     inherit-input-method t))

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
                  (memq action
                        `(,@magit-no-confirm
                          ,@(and magit-wip-before-change-mode
                                 (memq 'safe-with-wip magit-no-confirm)
                                 `(discard reverse
                                   stage-all-changes
                                   unstage-all-changes))))))
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
  "Bind variables to submatches according to VARLIST then evaluate BODY.
Bind the symbols in VARLIST to submatches of the current match
data, starting with 1 and incrementing by 1 for each symbol.  If
the last match was against a string then that has to be provided
as STRING."
  (declare (indent 2) (debug (listp form body)))
  (let ((s (cl-gensym "string"))
        (i 0))
    `(let ((,s ,string))
       (let ,(save-match-data
               (--map (list it (list 'match-string (cl-incf i) s)) varlist))
         ,@body))))

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

;;; Kludges

(defun magit-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' but work around an Apple bug.
See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21573#17
and https://github.com/magit/magit/issues/2295."
  (and (file-directory-p filename)
       (file-accessible-directory-p filename)))

;;; magit-utils.el ends soon
(provide 'magit-utils)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; magit-utils.el ends here
