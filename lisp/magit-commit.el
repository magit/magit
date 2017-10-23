;;; magit-commit.el --- create Git commits  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2017  The Magit Project Contributors
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
(declare-function epg-sub-key-id 'epg)
(declare-function epg-key-sub-key-list 'epg)
(declare-function epg-key-user-id-list 'epg)
(declare-function epg-user-id-string 'epg)
(declare-function epg-decode-dn 'epg)
(declare-function epg-list-keys 'epg)

;;; Options

(defcustom magit-commit-arguments nil
  "The arguments used when committing."
  :group 'magit-git-arguments
  :type '(repeat (string :tag "Argument")))

(defcustom magit-commit-ask-to-stage 'verbose
  "Whether to ask to stage everything when committing and nothing is staged."
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

;;; Popup

(defun magit-commit-popup (&optional arg)
  "Popup console for commit commands."
  (interactive "P")
  (--if-let (magit-commit-message-buffer)
      (switch-to-buffer it)
    (magit-invoke-popup 'magit-commit-popup nil arg)))

(defvar magit-commit-popup
  '(:variable magit-commit-arguments
    :man-page "git-commit"
    :switches ((?a "Stage all modified and deleted files"   "--all")
               (?e "Allow empty commit"                     "--allow-empty")
               (?v "Show diff of changes to be committed"   "--verbose")
               (?h "Disable hooks"                          "--no-verify")
               (?s "Add Signed-off-by line"                 "--signoff")
               (?R "Claim authorship and reset author date" "--reset-author"))
    :options  ((?A "Override the author"  "--author=")
               (?S "Sign using gpg"       "--gpg-sign=" magit-read-gpg-secret-key)
               (?C "Reuse commit message" "--reuse-message="
                   magit-read-reuse-message))
    :actions  ((?c "Commit"         magit-commit)
               (?e "Extend"         magit-commit-extend)
               (?f "Fixup"          magit-commit-fixup)
               (?F "Instant Fixup"  magit-commit-instant-fixup) nil
               (?w "Reword"         magit-commit-reword)
               (?s "Squash"         magit-commit-squash)
               (?S "Instant Squash" magit-commit-instant-squash) nil
               (?a "Amend"          magit-commit-amend)
               (?A "Augment"        magit-commit-augment))
    :max-action-columns 4
    :default-action magit-commit))

(magit-define-popup-keys-deferred 'magit-commit-popup)

(defun magit-commit-arguments nil
  (if (eq magit-current-popup 'magit-commit-popup)
      magit-current-popup-args
    magit-commit-arguments))

(defvar magit-gpg-secret-key-hist nil)

(defun magit-read-gpg-secret-key (prompt &optional _initial-input)
  (require 'epa)
  (let ((keys (--map (concat (epg-sub-key-id (car (epg-key-sub-key-list it)))
                             " "
                             (-when-let (id-obj (car (epg-key-user-id-list it)))
                               (let    ((id-str (epg-user-id-string id-obj)))
                                 (if (stringp id-str)
                                     id-str
                                   (epg-decode-dn id-obj)))))
                     (epg-list-keys (epg-make-context epa-protocol) nil t))))
    (car (split-string (magit-completing-read
                        prompt keys nil nil nil 'magit-gpg-secret-key-hist
                        (car (or magit-gpg-secret-key-hist keys)))
                       " "))))

(defun magit-read-reuse-message (prompt &optional default)
  (magit-completing-read prompt (magit-list-refnames)
                         nil nil nil 'magit-revision-history
                         (or default
                             (and (magit-rev-verify "ORIG_HEAD")
                                  "ORIG_HEAD"))))

;;; Commands

;;;###autoload
(defun magit-commit (&optional args)
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
  (let ((process-environment process-environment))
    (unless override-date
      (push (magit-rev-format "GIT_COMMITTER_DATE=%cD") process-environment))
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
  (-when-let (args (magit-commit-assert args t))
    (when commit
      (when (and rebase (not (magit-rev-ancestor-p commit "HEAD")))
        (magit-read-char-case
            (format "%s isn't an ancestor of HEAD.  " commit) nil
          (?c "[c]reate without rebasing" (setq rebase nil))
          (?s "[s]elect other"            (setq commit nil))
          (?a "[a]bort"                   (user-error "Quit")))))
    (when commit
      (setq commit (magit-rebase-interactive-assert commit)))
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
            (magit-rebase-interactive-1 commit
                (list "--autosquash" "--autostash")
              "" "true" t)))
        (format "Type %%p on a commit to %s into it,"
                (substring option 2))
        nil nil (list "--graph"))
      (when magit-commit-show-diff
        (let ((magit-display-buffer-noselect t))
          (apply #'magit-diff-staged nil (magit-diff-arguments)))))))

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
    (prog1 (when (y-or-n-p "Nothing staged.  Stage and commit everything? ")
             (magit-run-git "add" "-u" ".")
             (or args (list "--")))
      (when (and (eq magit-commit-ask-to-stage 'verbose)
                 (derived-mode-p 'magit-diff-mode))
        (magit-mode-bury-buffer))))
   (t
    (user-error "Nothing staged"))))

;;; Pending Diff

(defun magit-commit-diff ()
  (-when-let (fn (and git-commit-mode
                      magit-commit-show-diff
                      (cl-case last-command
                        (magit-commit
                         (apply-partially 'magit-diff-staged nil))
                        (magit-commit-all
                         (apply-partially 'magit-diff-working-tree nil))
                        ((magit-commit-amend
                          magit-commit-reword
                          magit-rebase-reword-commit)
                         'magit-diff-while-amending))))
    (-when-let (diff-buffer (magit-mode-get-buffer 'magit-diff-mode))
      ;; This window just started displaying the commit message
      ;; buffer.  Without this that buffer would immediately be
      ;; replaced with the diff buffer.  See #2632.
      (unrecord-window-buffer nil diff-buffer))
    (condition-case nil
        (let ((magit-inhibit-save-previous-winconf 'unset)
              (magit-display-buffer-noselect t)
              (inhibit-quit nil))
          (message "Diffing changes to be committed (C-g to abort diffing)")
          (funcall fn (car (magit-diff-arguments))))
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
  (let ((hunk (magit-section-when 'hunk it))
        (log (magit-commit-message-buffer)) buf pos)
    (save-window-excursion
      (call-interactively #'magit-diff-visit-file)
      (setq buf (current-buffer))
      (setq pos (point)))
    (unless log
      (unless (magit-commit-assert nil)
        (user-error "Abort"))
      (magit-commit)
      (while (not (setq log (magit-commit-message-buffer)))
        (sit-for 0.01)))
    (save-excursion
      (with-current-buffer buf
        (goto-char pos)
        (funcall magit-commit-add-log-insert-function log
                 (magit-file-relative-name)
                 (and hunk (add-log-current-defun)))))))

(defun magit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (undo-boundary)
    (goto-char (point-max))
    (while (re-search-backward (concat "^" comment-start) nil t))
    (cond ((re-search-backward (format "* %s\\(?: (\\([^)]+\\))\\)?: " file)
                               nil t)
           (when (equal (match-string 1) defun)
             (setq defun nil))
           (re-search-forward ": "))
          (t
           (when (re-search-backward "^[\\*(].+\n" nil t)
             (goto-char (match-end 0)))
           (while (re-search-forward "^[^\\*#\n].*\n" nil t))
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
          (while (re-search-forward "^[^\\*#\n].*\n" limit t))
          (insert (format "(%s): \n" defun))
          (backward-char))))))

(provide 'magit-commit)
;;; magit-commit.el ends here
