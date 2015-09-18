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

(defcustom magit-commit-arguments nil
  "The arguments used when committing."
  :group 'magit-commands
  :type '(repeat (string :tag "Argument")))

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
               (?n "Bypass git hooks"                       "--no-verify")
               (?s "Add Signed-off-by line"                 "--signoff")
               (?R "Claim authorship and reset author date" "--reset-author"))
    :options  ((?A "Override the author"  "--author="        read-from-minibuffer)
               (?S "Sign using gpg"       "--gpg-sign="      magit-read-gpg-secret-key)
               (?C "Reuse commit message" "--reuse-message=" read-from-minibuffer))
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
    :default-action 'magit-commit))

(magit-define-popup-keys-deferred 'magit-commit-popup)

(defun magit-commit-arguments nil
  (if (eq magit-current-popup 'magit-commit-popup)
      magit-current-popup-args
    magit-commit-arguments))

(defun magit-commit-message-buffer ()
  (let ((topdir (magit-toplevel)))
    (--first (equal topdir (with-current-buffer it
                             (and git-commit-mode (magit-toplevel))))
             (append (buffer-list (selected-frame))
                     (buffer-list)))))

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
                         (not magit-commit-extend-override-date)
                       magit-commit-extend-override-date)))
  (when (setq args (magit-commit-assert args (not override-date)))
    (let ((process-environment process-environment))
      (unless override-date
        (setenv "GIT_COMMITTER_DATE" (magit-rev-format "%cD")))
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
      (setenv "GIT_COMMITTER_DATE" (magit-rev-format "%cD")))
    (magit-run-git-with-editor "commit" "--amend" "--only" args)))

;;;###autoload
(defun magit-commit-fixup (&optional commit)
  "Create a fixup commit.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'."
  (interactive (list (magit-commit-at-point)))
  (magit-commit-squash-internal "--fixup" commit))

;;;###autoload
(defun magit-commit-squash (&optional commit)
  "Create a squash commit, without editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'."
  (interactive (list (magit-commit-at-point)))
  (magit-commit-squash-internal "--squash" commit))

;;;###autoload
(defun magit-commit-augment (&optional commit)
  "Create a squash commit, editing the squash message.

With a prefix argument the target COMMIT has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'."
  (interactive (list (magit-commit-at-point)))
  (magit-commit-squash-internal "--squash" commit nil t))

;;;###autoload
(defun magit-commit-instant-fixup (&optional commit)
  "Create a fixup commit targeting COMMIT and instantly rebase."
  (interactive (list (magit-commit-at-point)))
  (magit-commit-squash-internal "--fixup" commit t))

;;;###autoload
(defun magit-commit-instant-squash (&optional commit)
  "Create a squash commit targeting COMMIT and instantly rebase."
  (interactive (list (magit-commit-at-point)))
  (magit-commit-squash-internal "--squash" commit t))

(defun magit-commit-squash-internal (option commit &optional rebase edit confirmed)
  (-when-let (args (magit-commit-assert (magit-commit-arguments) t))
    (if (and commit
             (or confirmed
                 (not (or rebase
                          current-prefix-arg
                          magit-commit-squash-confirm))))
        (let ((magit-diff-auto-show nil))
          (magit-run-git-with-editor "commit"
                                     (unless edit "--no-edit")
                                     (concat option "=" commit)
                                     args))
      (magit-log-select
        `(lambda (commit)
           (magit-commit-squash-internal ,option commit ,rebase ,edit t)
           ,@(when rebase
               `((magit-rebase-interactive-1 commit
                     (list "--autosquash" "--autostash")
                   "" "true"))))
        (format "Type %%p on a commit to %s into it,"
                (substring option 2)))
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
                     (`magit-commit
                      (apply-partially 'magit-diff-staged nil))
                     (`magit-commit-amend  'magit-diff-while-amending)
                     (`magit-commit-reword 'magit-diff-while-amending)))
    (setq with-editor-previous-winconf (current-window-configuration))
    (let ((magit-inhibit-save-previous-winconf 'unset)
          (magit-diff-switch-buffer-function
           (lambda (buffer) (display-buffer buffer t))))
      (funcall it (car (magit-diff-arguments))))))

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
  "Add a stub for the current change into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `magit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (let ((hunk (magit-section-when 'hunk it))
        (log (magit-commit-message-buffer)) buf pos)
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

;;; magit-commit.el ends soon
(provide 'magit-commit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-commit.el ends here
