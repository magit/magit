;;; magit-commit.el --- create Git commits

;; Copyright (C) 2008-2015  The Magit Project Contributors
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

(defcustom magit-commit-ask-to-stage t
  "Whether to ask to stage everything when committing and nothing is staged."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-extend-override-date nil
  "Whether using `magit-commit-extend' changes the committer date."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-reword-override-date nil
  "Whether using `magit-commit-reword' changes the committer date."
  :package-version '(magit . "2.1.0")
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

;;; Code

;;;###autoload (autoload 'magit-commit-popup "magit-commit" nil t)
(with-no-warnings ; quiet byte-compiler
(magit-define-popup magit-commit-popup
  "Popup console for commit commands."
  'magit-commands
  :man-page "git-commit"
  :switches '((?a "Stage all modified and deleted files"   "--all")
              (?e "Allow empty commit"                     "--allow-empty")
              (?v "Show diff of changes to be committed"   "--verbose")
              (?n "Bypass git hooks"                       "--no-verify")
              (?s "Add Signed-off-by line"                 "--signoff")
              (?R "Claim authorship and reset author date" "--reset-author"))
  :options  '((?A "Override the author"  "--author="        read-from-minibuffer)
              (?S "Sign using gpg"       "--gpg-sign="      magit-read-gpg-secret-key)
              (?C "Reuse commit message" "--reuse-message=" read-from-minibuffer))
  :actions  '((?c "Commit"         magit-commit)
              (?e "Extend"         magit-commit-extend)
              (?f "Fixup"          magit-commit-fixup)
              (?F "Instant Fixup"  magit-commit-instant-fixup)
              (?a "Amend"          magit-commit-amend)
              (?w "Reword"         magit-commit-reword)
              (?s "Squash"         magit-commit-squash)
              (?S "Instant Squash" magit-commit-instant-squash))
  :max-action-columns 4
  :default-action 'magit-commit))

(defun magit-commit-message-buffer ()
  (let ((topdir (magit-toplevel)))
    (--first (equal topdir (with-current-buffer it
                             (and git-commit-mode (magit-toplevel))))
             (append (buffer-list (selected-frame))
                     (buffer-list)))))

(defadvice magit-commit-popup (around pop-to-ongoing activate)
  (--if-let (magit-commit-message-buffer) (switch-to-buffer it) ad-do-it))

;;;###autoload
(defun magit-commit (&optional args)
  "Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.
\n(git commit [--amend] ARGS)"
  (interactive (if current-prefix-arg
                   (list (cons "--amend" (magit-commit-arguments)))
                 (list (magit-commit-arguments))))
  (when (setq args (magit-commit-assert args))
    (magit-run-git-with-editor "commit" args)))

;;;###autoload
(defun magit-commit-amend (&optional args)
  "Amend the last commit.
\n(git commit --amend ARGS)"
  (interactive (list (magit-commit-arguments)))
  (magit-run-git-with-editor "commit" "--amend" args))

;;;###autoload
(defun magit-commit-extend (&optional args override-date)
  "Amend the last commit, without editing the message.
With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-extend-override-date' can be
used to inverse the meaning of the prefix argument.
\n(git commit --amend --no-edit)"
  (interactive (list (magit-commit-arguments)
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (when (setq args (magit-commit-assert args (not override-date)))
    (let ((process-environment process-environment))
      (unless override-date
        (setenv "GIT_COMMITTER_DATE" (magit-rev-format "%cd")))
      (magit-run-git-with-editor "commit" "--amend" "--no-edit" args))))

;;;###autoload
(defun magit-commit-reword (&optional args override-date)
  "Reword the last commit, ignoring staged changes.

With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-rewrite-override-date' can be
used to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.
\n(git commit --amend --only)"
  (interactive (list (magit-commit-arguments)
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (let ((process-environment process-environment))
    (unless override-date
      (setenv "GIT_COMMITTER_DATE" (magit-rev-format "%cd")))
    (magit-run-git-with-editor "commit" "--amend" "--only" args)))

;;;###autoload
(defun magit-commit-fixup (&optional commit args confirm)
  "Create a fixup commit.
With a prefix argument the target commit has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.
\n(git commit --no-edit --fixup=COMMIT [ARGS])"
  (interactive (magit-commit-squash-read-args))
  (magit-commit-squash-internal 'magit-commit-fixup "--fixup"
                                commit args confirm))

;;;###autoload
(defun magit-commit-squash (&optional commit args confirm)
  "Create a squash commit.
With a prefix argument the target commit has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.
\n(git commit --no-edit --squash=COMMIT [ARGS])"
  (interactive (magit-commit-squash-read-args))
  (magit-commit-squash-internal 'magit-commit-squash "--squash"
                                commit args confirm))

;;;###autoload
(defun magit-commit-instant-fixup (&optional commit args)
  "Create a fixup commit and instantly rebase.
\n(git commit --no-edit --fixup=COMMIT ARGS;
 git rebase -i COMMIT^ --autosquash --autostash)"
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal
   (lambda (c a)
     (when (setq c (magit-commit-fixup c a))
       (magit-rebase-autosquash (concat c "^") (list "--autostash"))))
   "--fixup" commit args t))

;;;###autoload
(defun magit-commit-instant-squash (&optional commit args)
  "Create a squash commit and instantly rebase.
\n(git commit --no-edit --squash=COMMIT ARGS;
 git rebase -i COMMIT^ --autosquash --autostash)"
  (interactive (list (magit-commit-at-point)
                     (magit-commit-arguments)))
  (magit-commit-squash-internal
   (lambda (c a)
     (when (setq c (magit-commit-squash c a))
       (magit-rebase-autosquash (concat c "^") (list "--autostash"))))
   "--squash" commit args t))

(defun magit-commit-squash-read-args ()
  (list (magit-commit-at-point)
        (magit-commit-arguments)
        (or current-prefix-arg magit-commit-squash-confirm)))

(defun magit-commit-squash-internal (fn option commit args confirm)
  (when (setq args (magit-commit-assert args t))
    (if (and commit (not confirm))
        (let ((magit-diff-auto-show nil))
          (magit-run-git-with-editor "commit" "--no-edit"
                                     (concat option "=" commit) args)
          commit)
      (magit-log-select
        `(lambda (commit) (,fn commit (list ,@args)))
        "Type %p on the commit to squash/fixup into it,")
      (when (magit-diff-auto-show-p 'log-select)
        (let ((magit-diff-switch-buffer-function 'display-buffer))
          (magit-diff-staged))))))

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
    (when (magit-diff-auto-show-p 'stage-all)
      (magit-diff-unstaged))
    (prog1 (when (y-or-n-p "Nothing staged.  Stage and commit everything? ")
             (magit-run-git "add" "-u" ".")
             (or args (list "--")))
      (when (and (magit-diff-auto-show-p 'stage-all)
                 (derived-mode-p 'magit-diff-mode))
        (magit-mode-bury-buffer))))
   (t
    (user-error "Nothing staged"))))

(defun magit-commit-diff ()
  (--when-let (and git-commit-mode
                   (magit-diff-auto-show-p 'commit)
                   (pcase last-command
                     (`magit-commit        'magit-diff-staged)
                     (`magit-commit-amend  'magit-diff-while-amending)
                     (`magit-commit-reword 'magit-diff-while-amending)))
    (setq with-editor-previous-winconf (current-window-configuration))
    (let ((magit-inhibit-save-previous-winconf 'unset)
          (magit-diff-switch-buffer-function 'display-buffer))
      (funcall it))))

(add-hook 'server-switch-hook 'magit-commit-diff)

(add-to-list 'with-editor-server-window-alist
             (cons git-commit-filename-regexp 'switch-to-buffer))

(defvar magit-gpg-secret-key-hist nil)

(defun magit-read-gpg-secret-key (prompt &optional initial-input)
  (require 'epa)
  (let ((keys (--map (list (epg-sub-key-id (car (epg-key-sub-key-list it)))
                           (-when-let (id-obj (car (epg-key-user-id-list it)))
                             (let    ((id-str (epg-user-id-string id-obj)))
                               (if (stringp id-str)
                                   id-str
                                 (epg-decode-dn id-obj)))))
                     (epg-list-keys (epg-make-context epa-protocol) nil t))))
    (magit-completing-read prompt keys nil nil nil 'magit-gpg-secret-key-hist
                           (car (or magit-gpg-secret-key-hist keys)))))

(defvar magit-commit-add-log-insert-function 'magit-commit-add-log-insert
  "Used by `magit-commit-add-log' to insert a single entry.")

(defun magit-commit-add-log ()
  "Add a stub for the current hunk into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `magit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (let ((log (magit-commit-message-buffer)) buf pos)
    (save-window-excursion
      (call-interactively #'magit-diff-visit-file)
      (setq buf (current-buffer)
            pos (point)))
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
                 (file-relative-name buffer-file-name (magit-toplevel))
                 (add-log-current-defun))))))

(defun magit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (goto-char (point-min))
    (cond ((not (re-search-forward (format "^\\* %s" (regexp-quote file))
                                   nil t))
           ;; No entry for file, create it.
           (goto-char (point-max))
           (forward-comment -1000)
           (unless (or (bobp) (looking-back "\\(\\*[^\n]+\\|\n\\)" nil))
             (insert "\n"))
           (insert (format "\n* %s" file))
           (when defun
             (insert (format " (%s)" defun)))
           (insert ": "))
          (defun
           ;; found entry for file, look for defun
           (let ((limit (save-excursion
                          (or (and (re-search-forward "^\\* " nil t)
                                   (match-beginning 0))
                              (progn (goto-char (point-max))
                                     (forward-comment -1000)
                                     (point))))))
             (cond ((re-search-forward
                     (format "(.*\\_<%s\\_>.*):" (regexp-quote defun))
                     limit t)
                    ;; found it, goto end of current entry
                    (if (re-search-forward "^(" limit t)
                        (backward-char 2)
                      (goto-char limit)))
                   (t
                    ;; not found, insert new entry
                    (goto-char limit)
                    (if (bolp)
                        (open-line 1)
                      (newline))
                    (insert (format "(%s): " defun))))))
          (t
           ;; found entry for file, look for its beginning
           (when (looking-at ":")
             (forward-char 2))))))

;;; magit-commit.el ends soon
(provide 'magit-commit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-commit.el ends here
