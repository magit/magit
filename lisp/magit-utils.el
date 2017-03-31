;;; magit-utils.el --- various utilities  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
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

(require 'crm)

(eval-when-compile (require 'ido))
(declare-function ido-completing-read+ 'ido-completing-read+)
(declare-function Info-get-token 'info)

(eval-when-compile (require 'vc-git))
(declare-function vc-git--run-command-string 'vc-git)

(defvar magit-wip-before-change-mode)

;;; Options

(defcustom magit-completing-read-function 'magit-builtin-completing-read
  "Function to be called when requesting input from the user.

For Helm users, the simplest way to get Helm completion is to
turn on `helm-mode' and leave this option set to the default
value.  However, if you prefer to not use `helm-mode' but still
want Magit to use Helm for completion, you can set this option to
`helm--completing-read-default'."
  :group 'magit-essentials
  :type '(radio (function-item magit-builtin-completing-read)
                (function-item magit-ido-completing-read)
                (function-item helm--completing-read-default)
                (function :tag "Other")))

(defcustom magit-no-confirm-default nil
  "A list of commands which should just use the default choice.

Many commands let the user choose the target they act on offering
a sensible default as default choice.  If you think that that
default is so sensible that it should always be used without even
offering other choices, then add that command here.

Commands have to explicitly support this option.  Currently only
these commands do:
  `magit-branch'
  `magit-branch-and-checkout'
  `magit-branch-orphan'
  `magit-worktree-branch'
    For these four commands `magit-branch-read-upstream-first'
    must be non-nil, or adding them here has no effect.
  `magit-branch-rename'
  `magit-tag'"
  :package-version '(magit . "2.9.0")
  :group 'magit-miscellaneous
  :type '(list :convert-widget custom-hook-convert-widget)
  :options '(magit-branch
             magit-branch-and-checkout
             magit-branch-orphan
             magit-branch-rename
             magit-worktree-branch
             magit-tag))

(defconst magit--confirm-actions
  '((const reverse)           (const discard)
    (const rename)            (const resurrect)
    (const trash)             (const delete)
    (const abort-rebase)
    (const abort-merge)       (const merge-dirty)
    (const drop-stashes)      (const resect-bisect)
    (const kill-process)      (const delete-unmerged-branch)
    (const stage-all-changes) (const unstage-all-changes)
    (const safe-with-wip)))

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

  `abort-rebase' Aborting a rebase throws away all already
  modified commits, but it's possible to restore those from the
  reflog.

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
  :group 'magit-essentials
  :group 'magit-commands
  :type `(choice (const :tag "Always require confirmation" nil)
                 (const :tag "Never require confirmation" t)
                 (set   :tag "Require confirmation only for"
                        ,@magit--confirm-actions)))

(defcustom magit-slow-confirm '(drop-stashes)
  "Whether to ask user \"y or n\" or \"yes or no\" questions.

When this is nil, then `y-or-n-p' is used when the user has to
confirm a potentially destructive action.  When this is t, then
`yes-or-no-p' is used instead.  If this is a list of symbols
identifying actions, then `yes-or-no-p' is used for those,
`y-or-no-p' for all others.  The list of actions is the same as
for `magit-no-confirm' (which see)."
  :package-version '(magit . "2.9.0")
  :group 'magit-miscellaneous
  :type `(choice (const :tag "Always ask \"yes or no\" questions" t)
                 (const :tag "Always ask \"y or n\" questions" nil)
                 (set   :tag "Ask \"yes or no\" questions only for"
                        ,@magit--confirm-actions)))

(defcustom magit-no-message nil
  "A list of messages Magit should not display.

Magit displays most echo area messages using `message', but a few
are displayed using `magit-message' instead, which takes the same
arguments as the former, FORMAT-STRING and ARGS.  `magit-message'
forgoes printing a message if any member of this list is a prefix
of the respective FORMAT-STRING.

If Magit prints a message which causes you grief, then please
first investigate whether there is another option which can be
used to suppress it.  If that is not the case, then ask the Magit
maintainers to start using `magit-message' instead of `message'
in that case.  We are not proactively replacing all uses of
`message' with `magit-message', just in case someone *might* find
some of these messages useless.

Messages which can currently be suppressed using this option are:
* \"Turning on magit-auto-revert-mode...\""
  :package-version '(magit . "2.8.0")
  :group 'magit-miscellaneous
  :type '(repeat string))

(defcustom magit-ellipsis ?…
  "Character used to abbreviate text.

Currently this is used to abbreviate author names in the margin
and in process buffers to elide `magit-git-global-arguments'."
  :package-version '(magit . "2.1.0")
  :group 'magit-miscellaneous
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
  :group 'magit-miscellaneous
  :type 'number)

(defcustom magit-view-git-manual-method 'info
  "How links to Git documentation are followed from Magit's Info manuals.

`info'  Follow the link to the node in the `gitman' Info manual
        as usual.  Unfortunately that manual is not installed by
        default on some platforms, and when it is then the nodes
        look worse than the actual manpages.

`man'   View the respective man-page using the `man' package.

`woman' View the respective man-page using the `woman' package."
  :package-version '(magit . "2.9.0")
  :group 'magit-miscellaneous
  :type '(choice (const :tag "view info manual" info)
                 (const :tag "view manpage using `man'" man)
                 (const :tag "view manpage using `woman'" woman)))

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

(defun magit--completion-table (collection)
  (lambda (string pred action)
    (if (eq action 'metadata)
        '(metadata (display-sort-function . identity))
      (complete-with-action action collection string pred))))

(defun magit-builtin-completing-read
  (prompt choices &optional predicate require-match initial-input hist def)
  "Magit wrapper for standard `completing-read' function."
  (cl-letf (((symbol-function 'completion-pcm--all-completions)
             #'magit-completion-pcm--all-completions))
    (completing-read (magit-prompt-with-default prompt def)
                     (magit--completion-table choices)
                     predicate require-match
                     initial-input hist def)))

(defvar helm-completion-in-region-default-sort-fn)

(defun magit-completing-read-multiple
  (prompt choices &optional sep default hist keymap)
  "Read multiple items from CHOICES, separated by SEP.

Set up the `crm' variables needed to read multiple values with
`read-from-minibuffer'.

SEP is a regexp matching characters that can separate choices.
When SEP is nil, it defaults to `crm-default-separator'.
DEFAULT, HIST, and KEYMAP are passed to `read-from-minibuffer'.
When KEYMAP is nil, it defaults to `crm-local-completion-map'.

Unlike `completing-read-multiple', the return value is not split
into a list."
  (let* ((crm-separator (or sep crm-default-separator))
         (crm-completion-table (magit--completion-table choices))
         (choose-completion-string-functions
          '(crm--choose-completion-string))
         (minibuffer-completion-table #'crm--collection-fn)
         (minibuffer-completion-confirm t)
         (helm-completion-in-region-default-sort-fn nil)
         (input
          (cl-letf (((symbol-function 'completion-pcm--all-completions)
                     #'magit-completion-pcm--all-completions))
            (read-from-minibuffer
             (concat prompt (and default (format " (%s)" default)) ": ")
             nil (or keymap crm-local-completion-map)
             nil hist default))))
    (when (string-equal input "")
      (or (setq input default)
          (user-error "Nothing selected")))
    input))

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

(defun magit-y-or-n-p (prompt &optional action)
  "Ask user a \"y or n\" or a \"yes or no\" question using PROMPT.
Which kind of question is used depends on whether
ACTION is a member of option `magit-slow-confirm'."
  (if (or (eq magit-slow-confirm t)
          (and action (member action magit-slow-confirm)))
      (yes-or-no-p prompt)
    (y-or-n-p prompt)))

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
         (magit-y-or-n-p prompt action))
        ((= (length items) 1)
         (and (magit-y-or-n-p prompt action) items))
        ((> (length items) 1)
         (let ((buffer (get-buffer-create " *Magit Confirm*")))
           (with-current-buffer buffer
             (with-current-buffer-window
              buffer (cons 'display-buffer-below-selected
                           '((window-height . fit-window-to-buffer)))
              (lambda (window _value)
                (with-selected-window window
                  (unwind-protect (and (magit-y-or-n-p prompt-n action) items)
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

;;; Debug Utilities

;;;###autoload
(defun magit-emacs-Q-command ()
  (interactive)
  (let ((cmd (mapconcat
              #'shell-quote-argument
              `(,(concat invocation-directory invocation-name)
                "-Q" "--eval" "(setq debug-on-error t)"
                ,@(cl-mapcan
                   (lambda (dir) (list "-L" dir))
                   (delete-dups
                    (mapcar (lambda (lib)
                              (file-name-directory (locate-library lib)))
                            '("magit" "magit-popup" "with-editor"
                              "git-commit" "dash"))))
                ;; Avoid Emacs bug#16406 by using full path.
                "-l" ,(file-name-sans-extension (locate-library "magit")))
              " ")))
    (message "Uncustomized Magit command saved to kill-ring, %s"
             "please run it in a terminal.")
    (kill-new cmd)))

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

;;; Missing from Emacs

(defun magit-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

;;; Kludges for Emacs Bugs

(defun magit-file-accessible-directory-p (filename)
  "Like `file-accessible-directory-p' but work around an Apple bug.
See http://debbugs.gnu.org/cgi/bugreport.cgi?bug=21573#17
and https://github.com/magit/magit/issues/2295."
  (and (file-directory-p filename)
       (file-accessible-directory-p filename)))

(when (version<= "25.1" emacs-version)
  (with-eval-after-load 'vc-git
    (defun vc-git-conflicted-files (directory)
      "Return the list of files with conflicts in DIRECTORY."
      (let* ((status
              (vc-git--run-command-string directory "diff-files"
                                          "--name-status"))
             (lines (when status (split-string status "\n" 'omit-nulls)))
             files)
        (dolist (line lines files)
          (when (string-match "\\([ MADRCU?!]\\)[ \t]+\\(.+\\)" line)
            (let ((state (match-string 1 line))
                  (file (match-string 2 line)))
              (when (equal state "U")
                (push (expand-file-name file directory) files)))))))))

;; `completion-pcm--all-completions' reverses the completion list.  To
;; preserve the order of our pre-sorted completions, we'll temporarily
;; override it with the function below.  bug#24676
(defun magit-completion-pcm--all-completions (prefix pattern table pred)
  (if (completion-pcm--pattern-trivial-p pattern)
      (all-completions (concat prefix (car pattern)) table pred)
    (let* ((regex (completion-pcm--pattern->regex pattern))
           (case-fold-search completion-ignore-case)
           (completion-regexp-list (cons regex completion-regexp-list))
           (compl (all-completions
                   (concat prefix
                           (if (stringp (car pattern)) (car pattern) ""))
                   table pred)))
      (if (not (functionp table))
          compl
        (let ((poss ()))
          (dolist (c compl)
            (when (string-match-p regex c) (push c poss)))
          ;; This `nreverse' call is the only code change made to the
          ;; `completion-pcm--all-completions' that shipped with Emacs 25.1.
          (nreverse poss))))))

;;; Kludges for Incompatible Modes

(defvar whitespace-mode)

(defun whitespace-dont-turn-on-in-magit-mode ()
  "Prevent `whitespace-mode' from being turned on in Magit buffers.
Because `whitespace-mode' uses font-lock and Magit does not,
they are not compatible.  See `magit-diff-paint-whitespace'
for an alternative."
  (when (derived-mode-p 'magit-mode)
    (setq whitespace-mode nil)
    (user-error
     "Whitespace-Mode isn't compatible with Magit.  %s"
     "See `magit-diff-paint-whitespace' for an alternative.")))

(advice-add 'whitespace-turn-on :before
            'whitespace-dont-turn-on-in-magit-mode)

;;; Kludges for Custom

(defun magit-custom-initialize-reset (symbol exp)
  "Initialize SYMBOL based on EXP.
Set the symbol, using `set-default' (unlike
`custom-initialize-reset' which uses the `:set' function if any.)
The value is either the symbol's current value
 (as obtained using the `:get' function), if any,
or the value in the symbol's `saved-value' property if any,
or (last of all) the value of EXP."
  (set-default-toplevel-value
   symbol
   (condition-case nil
       (let ((def (default-toplevel-value symbol))
             (getter (get symbol 'custom-get)))
         (if getter (funcall getter symbol) def))
     (error
      (eval (let ((sv (get symbol 'saved-value)))
              (if sv (car sv) exp)))))))

(defun magit-hook-custom-get (symbol)
  (if (symbol-file symbol 'defvar)
      (default-toplevel-value symbol)
    ;;
    ;; Called by `custom-initialize-reset' on behalf of `symbol's
    ;; `defcustom', which is being evaluated for the first time to
    ;; set the initial value, but there's already a default value,
    ;; which most likely was stablished by one or more `add-hook'
    ;; calls.
    ;;
    ;; We combine the `standard-value' and the current value, while
    ;; preserving the order established by `:options', and return
    ;; the result of that to be used as the "initial" default value.
    ;;
    (let ((standard (eval (car (get symbol 'standard-value))))
          (current (default-toplevel-value symbol))
          (value nil))
      (dolist (fn (get symbol 'custom-options))
        (when (or (memq fn standard)
                  (memq fn current))
          (push fn value)))
      (dolist (fn current)
        (unless (memq fn value)
          (push fn value)))
      (nreverse value))))

;;; Kludges for Info Manuals

;;;###autoload
(defun Info-follow-nearest-node--magit-gitman (fn &optional fork)
  (if magit-view-git-manual-method
      (let ((node (Info-get-token
                   (point) "\\*note[ \n\t]+"
                   "\\*note[ \n\t]+\\([^:]*\\):\\(:\\|[ \n\t]*(\\)?")))
        (if (and node (string-match "^(gitman)\\(.+\\)" node))
            (pcase magit-view-git-manual-method
              (`man   (require 'man)
                      (man (match-string 1 node)))
              (`woman (require 'woman)
                      (woman (match-string 1 node)))
              (_
               (user-error "Invalid value for `magit-view-git-documentation'")))
          (funcall fn fork)))
    (funcall fn fork)))

;;;###autoload
(advice-add 'Info-follow-nearest-node :around
            'Info-follow-nearest-node--magit-gitman)

;;;###autoload
(defun org-man-export--magit-gitman (fn link description format)
  (if (and (eq format 'texinfo)
           (string-match-p "\\`git" link))
      (replace-regexp-in-string "%s" link "
@ifinfo
@ref{%s,,,gitman,}.
@end ifinfo
@ifhtml
@html
the <a href=\"http://git-scm.com/docs/%s\">%s(1)</a> manpage.
@end html
@end ifhtml
@iftex
the %s(1) manpage.
@end iftex
")
    (funcall fn link description format)))

;;;###autoload
(advice-add 'org-man-export :around
            'org-man-export--magit-gitman)

;;; Miscellaneous

(defun magit-message (format-string &rest args)
  "Display a message at the bottom of the screen, or not.
Like `message', except that if the users configured option
`magit-no-message' to prevent the message corresponding to
FORMAT-STRING to be displayed, then don't."
  (unless (--first (string-prefix-p it format-string) magit-no-message)
    (apply #'message format-string args)))

(provide 'magit-utils)
;;; magit-utils.el ends here
