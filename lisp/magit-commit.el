;;; magit-commit.el --- create Git commits  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2019  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

;; This library implements commands for creating Git commits.  These
;; commands just initiate the commit, support for writing the commit
;; messages is implemented in `git-commit.el'.

;;; Code:

(require 'magit)
(require 'magit-sequence)

(eval-when-compile (require 'epa)) ; for `epa-protocol'
(eval-when-compile (require 'epg))
(eval-when-compile (require 'subr-x))

;;; Options

(defcustom magit-commit-ask-to-stage 'verbose
  "Whether to ask to stage all unstaged changes when committing and nothing is staged."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type '(choice (const :tag "Ask showing diff" verbose)
                 (const :tag "Ask" t)
                 (const :tag "Don't ask" nil)))

(defcustom magit-commit-show-diff t
  "Whether the relevant diff is automatically shown when committing."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-extend-override-date t
  "Whether using `magit-commit-extend' changes the committer date."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-reword-override-date t
  "Whether using `magit-commit-reword' changes the committer date."
  :package-version '(magit . "2.3.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-squash-confirm t
  "Whether the commit targeted by squash and fixup has to be confirmed.
When non-nil then the commit at point (if any) is used as default
choice, otherwise it has to be confirmed.  This option only
affects `magit-commit-squash' and `magit-commit-fixup'.  The
\"instant\" variants always require confirmation because making
an error while using those is harder to recover from."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-post-commit-hook nil
  "Hook run after creating a commit without the user editing a message.

This hook is run by `magit-refresh' if `this-command' is a member
of `magit-post-stage-hook-commands'.  This only includes commands
named `magit-commit-*' that do *not* require that the user edits
the commit message in a buffer and then finishes by pressing
\\<with-editor-mode-map>\\[with-editor-finish].

Also see `git-commit-post-finish-hook'."
  :package-version '(magit . "2.90.0")
  :group 'magit-commands
  :type 'hook)

(defvar magit-post-commit-hook-commands
  '(magit-commit-extend
    magit-commit-fixup
    magit-commit-augment
    magit-commit-instant-fixup
    magit-commit-instant-squash))

;;; Popup

;;;###autoload (autoload 'magit-commit "magit-commit" nil t)
(define-transient-command magit-commit ()
  "Create a new commit or replace an existing commit."
  :info-manual "(magit)Initiating a Commit"
  :man-page "git-commit"
  ["Arguments"
   ("-a" "Stage all modified and deleted files"   ("-a" "--all"))
   ("-e" "Allow empty commit"                     "--allow-empty")
   ("-v" "Show diff of changes to be committed"   ("-v" "--verbose"))
   ("-n" "Disable hooks"                          ("-n" "--no-verify"))
   ("-R" "Claim authorship and reset author date" "--reset-author")
   (magit:--author :description "Override the author")
   (7 "-D" "Override the author date" "--date=" transient-read-date)
   ("-s" "Add Signed-off-by line"                 ("-s" "--signoff"))
   (5 magit:--gpg-sign)
   (magit-commit:--reuse-message)]
  [["Create"
    ("c" "Commit"         magit-commit-create)]
   ["Edit HEAD"
    ("e" "Extend"         magit-commit-extend)
    ("w" "Reword"         magit-commit-reword)
    ("a" "Amend"          magit-commit-amend)
    (6 "n" "Reshelve"     magit-commit-reshelve)]
   ["Edit"
    ("f" "Fixup"          magit-commit-fixup)
    ("s" "Squash"         magit-commit-squash)
    ("A" "Augment"        magit-commit-augment)
    (6 "x" "Absorb changes" magit-commit-absorb)]
   [""
    ("F" "Instant fixup"  magit-commit-instant-fixup)
    ("S" "Instant squash" magit-commit-instant-squash)]]
  (interactive)
  (if-let ((buffer (magit-commit-message-buffer)))
      (switch-to-buffer buffer)
    (transient-setup 'magit-commit)))

(defun magit-commit-arguments nil
  (transient-args 'magit-commit))

(define-infix-argument magit:--gpg-sign ()
  :description "Sign using gpg"
  :class 'transient-option
  :shortarg "-S"
  :argument "--gpg-sign="
  :allow-empty t
  :reader 'magit-read-gpg-secret-key)

(defvar magit-gpg-secret-key-hist nil)

(defun magit-read-gpg-secret-key (prompt &optional _initial-input history)
  (require 'epa)
  (let ((keys (--map (concat (epg-sub-key-id (car (epg-key-sub-key-list it)))
                             " "
                             (when-let ((id-obj (car (epg-key-user-id-list it))))
                               (let ((id-str (epg-user-id-string id-obj)))
                                 (if (stringp id-str)
                                     id-str
                                   (epg-decode-dn id-obj)))))
                     (epg-list-keys (epg-make-context epa-protocol) nil t))))
    (car (split-string (magit-completing-read
                        prompt keys nil nil nil history
                        (car (or history keys)))
                       " "))))

(define-infix-argument magit-commit:--reuse-message ()
  :description "Reuse commit message"
  :class 'transient-option
  :shortarg "-C"
  :argument "--reuse-message="
  :reader 'magit-read-reuse-message
  :history-key 'magit-revision-history)

(defun magit-read-reuse-message (prompt &optional default history)
  (magit-completing-read prompt (magit-list-refnames)
                         nil nil nil history
                         (or default
                             (and (magit-rev-verify "ORIG_HEAD")
                                  "ORIG_HEAD"))))

;;; Commands

;;;###autoload
(defun magit-commit-create (&optional args)
  "Create a new commit on `HEAD'.
With a prefix argument, amend to the commit at `HEAD' instead.
\n(git commit [--amend] ARGS)"
  (interactive (if current-prefix-arg
                   (list (cons "--amend" (magit-commit-arguments)))
                 (list (magit-commit-arguments))))
  (when (member "--all" args)
    (setq this-command 'magit-commit-all))
  (when (setq args (magit-commit-assert args))
    (let ((default-directory (magit-toplevel)))
      (magit-run-git-with-editor "commit" args))))

;;;###autoload
(defun magit-commit-amend (&optional args)
  "Amend the last commit.
\n(git commit --amend ARGS)"
  (interactive (list (magit-commit-arguments)))
  (magit-commit-amend-assert)
  (magit-run-git-with-editor "commit" "--amend" args))

;;;###autoload
(defun magit-commit-extend (&optional args override-date)
  "Amend the last commit, without editing the message.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-extend-override-date' can be used
to inverse the meaning of the prefix argument.  \n(git commit
--amend --no-edit)"
  (interactive (list (magit-commit-arguments)
                     (if current-prefix-arg
                         (not magit-commit-extend-override-date)
                       magit-commit-extend-override-date)))
  (when (setq args (magit-commit-assert args (not override-date)))
    (magit-commit-amend-assert)
    (let ((process-environment process-environment))
      (unless override-date
        (push (magit-rev-format "GIT_COMMITTER_DATE=%cD") process-environment))
      (magit-run-git-with-editor "commit" "--amend" "--no-edit" args))))

;;;###autoload
(defun magit-commit-reword (&optional args override-date)
  "Reword the last commit, ignoring staged changes.

With a prefix argument keep the committer date, otherwise change
it.  The option `magit-commit-reword-override-date' can be used
to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.
\n(git commit --amend --only)"
  (interactive (list (magit-commit-arguments)
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (magit-commit-amend-assert)
  (let ((process-environment process-environment))
    (unless override-date
      (push (magit-rev-format "GIT_COMMITTER_DATE=%cD") process-environment))
    (cl-pushnew "--allow-empty" args :test #'equal)
    (magit-run-git-with-editor "commit" "--amend" "--only" args)))

;;;###autoload
(defun magit-commit-fixup (&optional commit args)
  "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--fixup" commit args))

;;;###autoload
(defun magit-commit-squash (&optional commit args)
  "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--squash" commit args))

;;;###autoload
(defun magit-commit-augment (&optional commit args)
  "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--squash" commit args nil t))

;;;###autoload
(defun magit-commit-instant-fixup (&optional commit args)
  "Create a fixup commit targeting COMMIT and instantly rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--fixup" commit args t))

;;;###autoload
(defun magit-commit-instant-squash (&optional commit args)
  "Create a squash commit targeting COMMIT and instantly rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal "--squash" commit args t))

(defun magit-commit-squash-internal
    (option commit &optional args rebase edit confirmed)
  (when-let ((args (magit-commit-assert args t)))
    (when commit
      (when (and rebase (not (magit-rev-ancestor-p commit "HEAD")))
        (magit-read-char-case
            (format "%s isn't an ancestor of HEAD.  " commit) nil
          (?c "[c]reate without rebasing" (setq rebase nil))
          (?s "[s]elect other"            (setq commit nil))
          (?a "[a]bort"                   (user-error "Quit")))))
    (when commit
      (setq commit (magit-rebase-interactive-assert commit t)))
    (if (and commit
             (or confirmed
                 (not (or rebase
                          current-prefix-arg
                          magit-commit-squash-confirm))))
        (let ((magit-commit-show-diff nil))
          (push (concat option "=" commit) args)
          (unless edit
            (push "--no-edit" args))
          (if rebase
              (magit-with-editor
                (magit-call-git
                 "commit" "--no-gpg-sign"
                 (-remove-first
                  (apply-partially #'string-match-p "\\`--gpg-sign=")
                  args)))
            (magit-run-git-with-editor "commit" args))
          t) ; The commit was created; used by below lambda.
      (magit-log-select
        (lambda (commit)
          (when (and (magit-commit-squash-internal option commit args
                                                   rebase edit t)
                     rebase)
            (magit-commit-amend-assert commit)
            (magit-rebase-interactive-1 commit
                (list "--autosquash" "--autostash" "--keep-empty")
              "" "true" nil t)))
        (format "Type %%p on a commit to %s into it,"
                (substring option 2))
        nil nil nil commit)
      (when magit-commit-show-diff
        (let ((magit-display-buffer-noselect t))
          (apply #'magit-diff-staged nil (magit-diff-arguments)))))))

(defun magit-commit-amend-assert (&optional commit)
  (--when-let (magit-list-publishing-branches commit)
    (let ((m1 "This commit has already been published to ")
          (m2 ".\nDo you really want to modify it"))
      (magit-confirm 'amend-published
        (concat m1 "%s" m2)
        (concat m1 "%i public branches" m2)
        nil it))))

(defun magit-commit-assert (args &optional strict)
  (cond
   ((or (magit-anything-staged-p)
        (and (magit-anything-unstaged-p)
             ;; ^ Everything of nothing is still nothing.
             (member "--all" args))
        (and (not strict)
             ;; ^ For amend variants that don't make sense otherwise.
             (or (member "--amend" args)
                 (member "--allow-empty" args))))
    (or args (list "--")))
   ((and (magit-rebase-in-progress-p)
         (not (magit-anything-unstaged-p))
         (y-or-n-p "Nothing staged.  Continue in-progress rebase? "))
    (setq this-command 'magit-rebase-continue)
    (magit-run-git-sequencer "rebase" "--continue")
    nil)
   ((and (file-exists-p (magit-git-dir "MERGE_MSG"))
         (not (magit-anything-unstaged-p)))
    (or args (list "--")))
   ((not (magit-anything-unstaged-p))
    (user-error "Nothing staged (or unstaged)"))
   (magit-commit-ask-to-stage
    (when (eq magit-commit-ask-to-stage 'verbose)
      (magit-diff-unstaged))
    (prog1 (when (y-or-n-p "Nothing staged.  Stage and commit all unstaged changes? ")
             (magit-run-git "add" "-u" ".")
             (or args (list "--")))
      (when (and (eq magit-commit-ask-to-stage 'verbose)
                 (derived-mode-p 'magit-diff-mode))
        (magit-mode-bury-buffer))))
   (t
    (user-error "Nothing staged"))))

(defvar magit--reshelve-history nil)

;;;###autoload
(defun magit-commit-reshelve (date)
  "Change the committer date and possibly the author date of `HEAD'.

If you are the author of `HEAD', then both dates are changed,
otherwise only the committer date.  The current time is used
as the initial minibuffer input and the original author (if
that is you) or committer date is available as the previous
history element."
  (interactive
   (let ((author-p (magit-rev-author-p "HEAD")))
     (push (magit-rev-format (if author-p "%ad" "%cd") "HEAD"
                             (concat "--date=format:%F %T %z"))
           magit--reshelve-history)
     (list (read-string (if author-p
                            "Change author and committer dates to: "
                          "Change committer date to: ")
                        (cons (format-time-string "%F %T %z") 17)
                        'magit--reshelve-history))))
  (let ((process-environment process-environment))
    (push (concat "GIT_COMMITTER_DATE=" date) process-environment)
    (magit-run-git "commit" "--amend" "--no-edit"
                   (and (magit-rev-author-p "HEAD")
                        (concat "--date=" date)))))

;;;###autoload (autoload 'magit-commit-absorb "magit-commit" nil t)
(define-transient-command magit-commit-absorb (phase commit args)
  "Spread unstaged changes across recent commits.
With a prefix argument use a transient command to select infix
arguments.  This command requires the git-autofixup script, which
is available from https://github.com/torbiak/git-autofixup."
  ["Arguments"
   (magit-autofixup:--context)
   (magit-autofixup:--strict)]
  ["Actions"
   ("x"  "Absorb" magit-commit-absorb)]
  (interactive (if current-prefix-arg
                   (list 'transient nil nil)
                 (list 'select
                       (magit-get-upstream-branch)
                       (transient-args 'magit-commit-absorb))))
  (if (eq phase 'transient)
      (transient-setup 'magit-commit-absorb)
    (unless (executable-find "git-autofixup")
      (user-error "This command requires the git-autofixup script, which %s"
                  "is available from https://github.com/torbiak/git-autofixup"))
    (when (magit-anything-staged-p)
      (user-error "Cannot absorb when there are staged changes"))
    (unless (magit-anything-unstaged-p)
      (user-error "There are no unstaged changes that could be absorbed"))
    (when commit
      (setq commit (magit-rebase-interactive-assert commit t)))
    (if (and commit (eq phase 'run))
        (progn (magit-run-git-async "autofixup" "-vv" args commit) t)
      (magit-log-select
        (lambda (commit)
          (with-no-warnings ; about non-interactive use
            (magit-commit-absorb 'run commit args)))
        nil nil nil nil commit))))

(define-infix-argument magit-autofixup:--context ()
  :description "Diff context lines"
  :class 'transient-option
  :shortarg "-c"
  :argument "--context="
  :reader 'transient-read-number-N0)

(define-infix-argument magit-autofixup:--strict ()
  :description "Strictness"
  :class 'transient-option
  :shortarg "-s"
  :argument "--strict="
  :reader 'transient-read-number-N0)

;;; Pending Diff

(defun magit-commit-diff ()
  (when (and git-commit-mode magit-commit-show-diff)
    (when-let ((diff-buffer (magit-get-mode-buffer 'magit-diff-mode)))
      ;; This window just started displaying the commit message
      ;; buffer.  Without this that buffer would immediately be
      ;; replaced with the diff buffer.  See #2632.
      (unrecord-window-buffer nil diff-buffer))
    (condition-case nil
        (let ((args (car (magit-diff-arguments)))
              (magit-inhibit-save-previous-winconf 'unset)
              (magit-display-buffer-noselect t)
              (inhibit-quit nil))
          (message "Diffing changes to be committed (C-g to abort diffing)")
          (if-let ((fn (cl-case last-command
                         (magit-commit
                          (apply-partially 'magit-diff-staged nil))
                         (magit-commit-all
                          (apply-partially 'magit-diff-working-tree nil))
                         ((magit-commit-amend
                           magit-commit-reword
                           magit-rebase-reword-commit)
                          'magit-diff-while-amending))))
              (funcall fn args)
            (if (magit-anything-staged-p)
                (magit-diff-staged nil args)
              (magit-diff-while-amending args))))
      (quit))))

;; Mention `magit-diff-while-committing' because that's
;; always what I search for when I try to find this line.
(add-hook 'server-switch-hook 'magit-commit-diff)

(add-to-list 'with-editor-server-window-alist
             (cons git-commit-filename-regexp 'switch-to-buffer))

;;; Message Utilities

(defun magit-commit-message-buffer ()
  (let* ((find-file-visit-truename t) ; git uses truename of COMMIT_EDITMSG
         (topdir (magit-toplevel)))
    (--first (equal topdir (with-current-buffer it
                             (and git-commit-mode (magit-toplevel))))
             (append (buffer-list (selected-frame))
                     (buffer-list)))))

(defvar magit-commit-add-log-insert-function 'magit-commit-add-log-insert
  "Used by `magit-commit-add-log' to insert a single entry.")

(defun magit-commit-add-log ()
  "Add a stub for the current change into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `magit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (pcase-let* ((hunk (and (magit-section-match 'hunk)
                          (magit-current-section)))
               (log  (magit-commit-message-buffer))
               (`(,buf ,pos) (magit-diff-visit-file--noselect)))
    (unless log
      (unless (magit-commit-assert nil)
        (user-error "Abort"))
      (magit-commit-create)
      (while (not (setq log (magit-commit-message-buffer)))
        (sit-for 0.01)))
    (magit--with-temp-position buf pos
      (funcall magit-commit-add-log-insert-function log
               (magit-file-relative-name)
               (and hunk (add-log-current-defun))))))

(defun magit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (undo-boundary)
    (goto-char (point-max))
    (while (re-search-backward (concat "^" comment-start) nil t))
    (save-restriction
      (narrow-to-region (point-min) (point))
      (cond ((re-search-backward (format "* %s\\(?: (\\([^)]+\\))\\)?: " file)
                                 nil t)
             (when (equal (match-string 1) defun)
               (setq defun nil))
             (re-search-forward ": "))
            (t
             (when (re-search-backward "^[\\*(].+\n" nil t)
               (goto-char (match-end 0)))
             (while (re-search-forward "^[^\\*\n].*\n" nil t))
             (if defun
                 (progn (insert (format "* %s (%s): \n" file defun))
                        (setq defun nil))
               (insert (format "* %s: \n" file)))
             (backward-char)
             (unless (looking-at "\n[\n\\']")
               (insert ?\n)
               (backward-char))))
      (when defun
        (forward-line)
        (let ((limit (save-excursion
                       (and (re-search-forward "^\\*" nil t)
                            (point)))))
          (unless (or (looking-back (format "(%s): " defun)
                                    (line-beginning-position))
                      (re-search-forward (format "^(%s): " defun) limit t))
            (while (re-search-forward "^[^\\*\n].*\n" limit t))
            (insert (format "(%s): \n" defun))
            (backward-char)))))))

;;; _
(provide 'magit-commit)
;;; magit-commit.el ends here
