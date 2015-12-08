;;; magit-sequence.el --- history manipulation in Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2015  The Magit Project Contributors
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

;; Support for Git commands that replay commits and help the user make
;; changes along the way.  Supports `cherry-pick', `revert', `rebase',
;; `rebase--interactive' and `am'.

;;; Code:

(require 'magit)

;;; Options
;;;; Faces

(defface magit-sequence-pick
  '((t :inherit default))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-stop
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background dark))  :foreground "DarkSeaGreen2"))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-part
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background dark))  :foreground "LightGoldenrod2"))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-head
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background dark))  :foreground "LightSkyBlue1"))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-drop
  '((((class color) (background light)) :foreground "IndianRed")
    (((class color) (background dark))  :foreground "IndianRed"))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-done
  '((t :inherit magit-hash))
  "Face used in sequence sections."
  :group 'magit-faces)

(defface magit-sequence-onto
  '((t :inherit magit-sequence-done))
  "Face used in sequence sections."
  :group 'magit-faces)

;;; Common

;;;###autoload
(defun magit-sequencer-continue ()
  "Resume the current cherry-pick or revert sequence."
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (if (magit-anything-unstaged-p t)
          (user-error "Cannot continue due to unstaged changes")
        (magit-run-git-sequencer
         (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--continue"))
    (user-error "No cherry-pick or revert in progress")))

;;;###autoload
(defun magit-sequencer-skip ()
  "Skip the stopped at commit during a cherry-pick or revert sequence."
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (progn (magit-call-git "reset" "--hard")
             (magit-sequencer-continue))
    (user-error "No cherry-pick or revert in progress")))

;;;###autoload
(defun magit-sequencer-abort ()
  "Abort the current cherry-pick or revert sequence.
This discards all changes made since the sequence started."
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (magit-run-git-sequencer
       (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--abort")
    (user-error "No cherry-pick or revert in progress")))

(defun magit-sequencer-in-progress-p ()
  (or (magit-cherry-pick-in-progress-p)
      (magit-revert-in-progress-p)))

;;; Cherry-Pick

;;;###autoload (autoload 'magit-cherry-pick-popup "magit-sequence" nil t)
(magit-define-popup magit-cherry-pick-popup
  "Popup console for cherry-pick commands."
  'magit-commands
  :man-page "git-cherry-pick"
  :switches '((?s "Add Signed-off-by lines"            "--signoff")
              (?e "Edit commit messages"               "--edit")
              (?x "Reference cherry in commit message" "-x")
              (?F "Attempt fast-forward"               "--ff")
              (?m "Reply merge relative to parent"     "--mainline="))
  :options  '((?s "Strategy" "--strategy="))
  :actions  '((?A "Cherry Pick"  magit-cherry-pick)
              (?a "Cherry Apply" magit-cherry-apply))
  :sequence-actions '((?A "Continue" magit-sequencer-continue)
                      (?s "Skip"     magit-sequencer-skip)
                      (?a "Abort"    magit-sequencer-abort))
  :sequence-predicate 'magit-sequencer-in-progress-p
  :default-arguments '("--ff"))

(defun magit-cherry-pick-read-args (prompt)
  (list (or (nreverse (magit-region-values 'commit))
            (magit-read-other-branch-or-commit prompt))
        (magit-cherry-pick-arguments)))

;;;###autoload
(defun magit-cherry-pick (commit &optional args)
  "Cherry-pick COMMIT.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting."
  (interactive (magit-cherry-pick-read-args "Cherry-pick"))
  (magit-assert-one-parent (car (if (listp commit)
                                    commit
                                  (split-string commit "\\.\\.")))
                           "cherry-pick")
  (magit-run-git-sequencer "cherry-pick" args commit))

;;;###autoload
(defun magit-cherry-apply (commit &optional args)
  "Apply the changes in COMMIT but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting."
  (interactive (magit-cherry-pick-read-args "Apply changes from commit"))
  (magit-assert-one-parent commit "cherry-pick")
  (magit-run-git-sequencer "cherry-pick" "--no-commit"
                           (remove "--ff" args) commit))

(defun magit-cherry-pick-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (file-exists-p (magit-git-dir "CHERRY_PICK_HEAD")))

;;; Revert

;;;###autoload (autoload 'magit-revert-popup "magit-sequence" nil t)
(magit-define-popup magit-revert-popup
  "Popup console for revert commands."
  'magit-commands
  :man-page "git-revert"
  :switches '((?s "Add Signed-off-by lines" "--signoff"))
  :options  '((?s "Strategy" "--strategy="))
  :actions  '((?V "Revert commit(s)" magit-revert)
              (?v "Revert changes"   magit-revert-no-commit))
  :sequence-actions '((?V "Continue" magit-sequencer-continue)
                      (?s "Skip"     magit-sequencer-skip)
                      (?a "Abort"    magit-sequencer-abort))
  :sequence-predicate 'magit-sequencer-in-progress-p)

(defun magit-revert-read-args (prompt)
  (list (or (magit-region-values 'commit)
            (magit-read-branch-or-commit prompt))
        (magit-revert-arguments)))

;;;###autoload
(defun magit-revert (commit &optional args)
  "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (magit-revert-read-args "Revert commit"))
  (magit-assert-one-parent commit "revert")
  (magit-run-git-sequencer "revert" args commit))

;;;###autoload
(defun magit-revert-no-commit (commit &optional args)
  "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (magit-revert-read-args "Revert changes"))
  (magit-assert-one-parent commit "revert")
  (magit-run-git-sequencer "revert" "--no-commit" args commit))

(defun magit-revert-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (file-exists-p (magit-git-dir "REVERT_HEAD")))

;;; Patch

;;;###autoload (autoload 'magit-am-popup "magit-sequence" nil t)
(magit-define-popup magit-am-popup
  "Popup console for mailbox commands."
  'magit-commands
  :man-page "git-am"
  :switches '((?3 "Fall back on 3way merge"           "--3way")
              (?s "Add Signed-off-by lines"           "--signoff")
              (?c "Remove text before scissors line"  "--scissors")
              (?k "Inhibit removal of email cruft"    "--keep")
              (?b "Limit removal of email cruft"      "--keep-non-patch")
              (?d "Use author date as committer date"
                  "--committer-date-is-author-date")
              (?D "Use committer date as author date" "--ignore-date"))
  :options  '((?p "Remove leading slashes from paths" "-p"
                  magit-popup-read-number))
  :actions  '((?w "Apply patches" magit-am-apply-patches)
              (?m "Apply maildir" magit-am-apply-maildir))
  :default-arguments '("--3way")
  :default-actions 'magit-am-apply-patches
  :sequence-actions '((?w "Continue" magit-am-continue)
                      (?s "Skip"     magit-am-skip)
                      (?a "Abort"    magit-am-abort))
  :sequence-predicate 'magit-am-in-progress-p)

;;;###autoload
(defun magit-am-apply-patches (&optional files args)
  "Apply the patches FILES."
  (interactive (list (or (magit-region-values 'file)
                         (list (let ((default (magit-file-at-point)))
                                 (read-file-name
                                  (if default
                                      (format "Apply patch (%s): " default)
                                    "Apply patch: ")
                                  nil default))))
                     (magit-am-arguments)))
  (magit-run-git-sequencer "am" args "--" (mapcar 'expand-file-name files)))

;;;###autoload
(defun magit-am-apply-maildir (&optional maildir args)
  "Apply the patches from MAILDIR."
  (interactive (list (read-file-name "Apply mbox or Maildir: ")
                     (magit-am-arguments)))
  (magit-run-git-sequencer "am" args (expand-file-name maildir)))

;;;###autoload
(defun magit-am-continue ()
  "Resume the current patch applying sequence."
  (interactive)
  (if (magit-am-in-progress-p)
      (if (magit-anything-unstaged-p t)
          (error "Cannot continue due to unstaged changes")
        (magit-run-git-sequencer "am" "--continue"))
    (user-error "Not applying any patches")))

;;;###autoload
(defun magit-am-skip ()
  "Skip the stopped at patch during a patch applying sequence."
  (interactive)
  (if (magit-am-in-progress-p)
      (magit-run-git-sequencer "am" "--skip")
    (user-error "Not applying any patches")))

;;;###autoload
(defun magit-am-abort ()
  "Abort the current patch applying sequence.
This discards all changes made since the sequence started."
  (interactive)
  (if (magit-am-in-progress-p)
      (magit-run-git "am" "--abort")
    (user-error "Not applying any patches")))

(defun magit-am-in-progress-p ()
  (file-exists-p (magit-git-dir "rebase-apply/applying")))

;;; Rebase

;;;###autoload (autoload 'magit-rebase-popup "magit-sequence" nil t)
(magit-define-popup magit-rebase-popup
  "Key menu for rebasing."
  'magit-commands
  :man-page "git-rebase"
  :switches '((?k "Keep empty commits"    "--keep-empty")
              (?p "Preserve merges"       "--preserve-merges")
              (?c "Lie about author date" "--committer-date-is-author-date")
              (?a "Autosquash"            "--autosquash")
              (?A "Autostash"             "--autostash")
              (?i "Interactive"           "--interactive"))
  :actions  '((lambda ()
                (concat (propertize "Rebase " 'face 'magit-popup-heading)
                        (propertize (or (magit-get-current-branch) "HEAD")
                                    'face 'magit-branch-local)
                        (propertize " onto" 'face 'magit-popup-heading)))
              (?p magit-get-push-branch    magit-rebase-onto-pushremote)
              (?u magit-get-tracked-branch magit-rebase-onto-upstream)
              (?e "elsewhere"              magit-rebase)
              "Rebase"
              (?i "interactively"      magit-rebase-interactive)
              (?m "to edit a commit"   magit-rebase-edit-commit)
              (?s "subset"             magit-rebase-subset)
              (?w "to reword a commit" magit-rebase-reword-commit) nil
              (?f "to autosquash"      magit-rebase-autosquash))
  :sequence-actions '((?r "Continue" magit-rebase-continue)
                      (?s "Skip"     magit-rebase-skip)
                      (?e "Edit"     magit-rebase-edit)
                      (?a "Abort"    magit-rebase-abort))
  :sequence-predicate 'magit-rebase-in-progress-p
  :max-action-columns 2)

(defun magit-git-rebase (target args)
  (magit-run-git-sequencer "rebase" target args))

;;;###autoload
(defun magit-rebase-onto-pushremote (args)
  "Rebase the current branch onto `branch.<name>.pushRemote'.
If that variable is unset, then rebase onto `remote.pushDefault'."
  (interactive (list (magit-rebase-arguments)))
  (--if-let (magit-get-current-branch)
      (-if-let (remote (magit-get-push-remote it))
          (if (member remote (magit-list-remotes))
              (magit-git-rebase (concat remote "/" it) args)
            (user-error "Remote `%s' doesn't exist" remote))
        (user-error "No push-remote is configured for %s" it))
    (user-error "No branch is checked out")))

;;;###autoload
(defun magit-rebase-onto-upstream (args)
  "Rebase the current branch onto its upstream branch."
  (interactive (list (magit-rebase-arguments)))
  (--if-let (magit-get-current-branch)
      (-if-let (target (magit-get-tracked-branch it))
          (magit-git-rebase target args)
        (user-error "No upstream is configured for %s" it))
    (user-error "No branch is checked out")))

;;;###autoload
(defun magit-rebase (target args)
  "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from head but not from the
selected branch TARGET are being rebased."
  (interactive (list (magit-read-other-branch-or-commit
                      "Rebase onto"
                      (magit-get-current-branch))
                     (magit-rebase-arguments)))
  (message "Rebasing...")
  (magit-git-rebase target args)
  (message "Rebasing...done"))

;;;###autoload
(defun magit-rebase-subset (newbase start args)
  "Rebase a subset of the current branches history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits."
  (interactive (list (magit-read-other-branch-or-commit
                      "Rebase subset onto"
                      (magit-get-current-branch)
                      (magit-get-tracked-branch))
                     nil
                     (magit-rebase-arguments)))
  (if start
      (progn (message "Rebasing...")
             (magit-run-git-sequencer "rebase" "--onto" newbase start args)
             (message "Rebasing...done"))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-subset ,newbase (concat commit "^") (list ,@args)))
      (concat "Type %p on a commit to rebase it "
              "and commits above it onto " newbase ","))))

(defun magit-rebase-interactive-1 (commit args message &optional editor)
  (declare (indent 2))
  (when commit
    (if (eq commit :merge-base)
        (setq commit (--if-let (magit-get-tracked-branch)
                         (magit-git-string "merge-base" it "HEAD")
                       nil))
      (when (magit-git-failure "merge-base" "--is-ancestor" commit "HEAD")
        (user-error "%s isn't an ancestor of HEAD" commit))
      (if (magit-commit-parents commit)
          (setq commit (concat commit "^"))
        (setq args (cons "--root" args)))))
  (when (and commit
             (magit-git-lines "rev-list" "--merges" (concat commit "..HEAD")))
    (magit-read-char-case "Proceed despite merge in rebase range?  " nil
      (?c "[c]ontinue")
      (?s "[s]elect other" (setq commit nil))
      (?a "[a]bort" (user-error "Quit"))))
  (if commit
      (let ((process-environment process-environment))
        (when editor
          (setenv "GIT_SEQUENCE_EDITOR" editor))
        (magit-run-git-sequencer "rebase" "-i" args
                                 (unless (member "--root" args) commit)))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-interactive-1 commit (list ,@args) ,message ,editor))
      message)))

;;;###autoload
(defun magit-rebase-interactive (commit args)
  "Start an interactive rebase sequence."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to rebase it and all commits above it,"))

;;;###autoload
(defun magit-rebase-autosquash (args)
  "Combine squash and fixup commits with their intended targets."
  (interactive (list (magit-rebase-arguments)))
  (magit-rebase-interactive-1 :merge-base (cons "--autosquash" args)
    "Type %p on a commit to squash into it and then rebase as necessary,"
    "true"))

;;;###autoload
(defun magit-rebase-edit-commit (commit args)
  "Edit a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to edit it,"
    "perl -i -p -e '++$x if not $x and s/^pick/edit/'"))

;;;###autoload
(defun magit-rebase-reword-commit (commit args)
  "Reword a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to reword its message,"
    "perl -i -p -e '++$x if not $x and s/^pick/reword/'"))

;;;###autoload
(defun magit-rebase-continue ()
  "Restart the current rebasing operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (if (magit-anything-unstaged-p t)
          (user-error "Cannot continue rebase with unstaged changes")
        (magit-run-git-sequencer "rebase" "--continue"))
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-skip ()
  "Skip the current commit and restart the current rebase operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-run-git-sequencer "rebase" "--skip")
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-edit ()
  "Edit the todo list of the current rebase operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-run-git-sequencer "rebase" "--edit-todo")
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-run-git "rebase" "--abort")
    (user-error "No rebase in progress")))

(defun magit-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (or (file-exists-p (magit-git-dir "rebase-merge"))
      (file-exists-p (magit-git-dir "rebase-apply/onto"))))

;;; Sections

(defun magit-insert-sequencer-sequence ()
  "Insert section for the on-going cherry-pick or revert sequence.
If no such sequence is in progress, do nothing."
  (let ((picking (magit-cherry-pick-in-progress-p)))
    (when (or picking (magit-revert-in-progress-p))
      (magit-insert-section (sequence)
        (magit-insert-heading (if picking "Cherry Picking" "Reverting"))
        (-when-let (lines (cdr (magit-file-lines (magit-git-dir "sequencer/todo"))))
          (dolist (line (nreverse lines))
            (when (string-match "^\\(pick\\|revert\\) \\([^ ]+\\) \\(.*\\)$" line)
              (magit-bind-match-strings (cmd hash msg) line
                (magit-insert-section (commit hash)
                  (insert (propertize cmd 'face 'magit-sequence-pick)
                          " " (propertize hash 'face 'magit-hash)
                          " " msg "\n"))))))
        (magit-sequence-insert-sequence
         (magit-file-line (magit-git-dir (if picking
                                             "CHERRY_PICK_HEAD"
                                           "REVERT_HEAD")))
         (magit-file-line (magit-git-dir "sequencer/head")))
        (insert "\n")))))

(defun magit-insert-am-sequence ()
  "Insert section for the on-going patch applying sequence.
If no such sequence is in progress, do nothing."
  (when (magit-am-in-progress-p)
    (magit-insert-section (rebase-sequence)
      (magit-insert-heading "Applying patches")
      (let ((patches (nreverse (magit-rebase-patches)))
            patch commit)
        (while patches
          (setq patch (pop patches)
                commit (magit-rev-verify-commit
                        (cadr (split-string (magit-file-line patch)))))
          (cond ((and commit patches)
                 (magit-sequence-insert-commit
                  "pick" commit 'magit-sequence-pick))
                (patches
                 (magit-sequence-insert-am-patch
                  "pick" patch 'magit-sequence-pick))
                (commit
                 (magit-sequence-insert-sequence commit "ORIG_HEAD"))
                (t
                 (magit-sequence-insert-am-patch
                  "stop" patch 'magit-sequence-stop)
                 (magit-sequence-insert-sequence nil "ORIG_HEAD")))))
      (insert ?\n))))

(defun magit-sequence-insert-am-patch (type patch face)
  (magit-insert-section (file patch)
    (insert (propertize type 'face face)
            ?\s (propertize (file-name-nondirectory patch) 'face 'magit-hash)
            ?\n)))

(defun magit-insert-rebase-sequence ()
  "Insert section for the on-going rebase sequence.
If no such sequence is in progress, do nothing."
  (when (magit-rebase-in-progress-p)
    (let* ((interactive (file-directory-p (magit-git-dir "rebase-merge")))
           (dir  (if interactive "rebase-merge/" "rebase-apply/"))
           (name (-> (concat dir "head-name") magit-git-dir magit-file-line))
           (onto (-> (concat dir "onto")      magit-git-dir magit-file-line))
           (onto (or (magit-rev-name onto name)
                     (magit-rev-name onto "refs/heads/*") onto))
           (name (or (magit-rev-name name "refs/heads/*") name)))
      (magit-insert-section (rebase-sequence)
        (magit-insert-heading (format "Rebasing %s onto %s" name onto))
        (if interactive
            (magit-rebase-insert-merge-sequence)
          (magit-rebase-insert-apply-sequence))
        (magit-sequence-insert-sequence
         (magit-file-line
          (magit-git-dir
           (concat dir (if interactive "stopped-sha" "original-commit"))))
         onto (--map (cadr (split-string it))
                     (magit-file-lines (magit-git-dir "rebase-merge/done"))))
        (insert ?\n)))))

(defun magit-rebase-insert-merge-sequence ()
  (dolist (line (nreverse
                 (magit-file-lines
                  (magit-git-dir "rebase-merge/git-rebase-todo"))))
    (when (string-match "^\\([^# ]+\\) \\([^ ]+\\) .*$" line)
      (magit-bind-match-strings (action hash) line
        (magit-sequence-insert-commit action hash 'magit-sequence-pick)))))

(defun magit-rebase-insert-apply-sequence ()
  (dolist (patch (nreverse (cdr (magit-rebase-patches))))
    (magit-sequence-insert-commit
     "pick" (cadr (split-string (magit-file-line patch))) 'magit-sequence-pick)))

(defun magit-rebase-patches ()
  (directory-files (magit-git-dir "rebase-apply") t "^[0-9]\\{4\\}$"))

(defun magit-sequence-insert-sequence (stop onto &optional orig)
  (let ((head (magit-rev-parse "HEAD")) done)
    (setq onto (if onto (magit-rev-parse onto) head))
    (setq done (magit-git-lines "log" "--format=%H" (concat onto "..HEAD")))
    (when (and stop (not (member stop done)))
      (let ((id (magit-patch-id stop)))
        (--if-let (--first (equal (magit-patch-id it) id) done)
            (setq stop it)
          (cond
           ((--first (magit-rev-equal it stop) done)
            ;; The commit's testament has been executed.
            (magit-sequence-insert-commit "void" stop 'magit-sequence-drop))
           ;; The faith of the commit is still undecided...
           ((magit-anything-unmerged-p)
            ;; ...and time travel isn't for the faint of heart.
            (magit-sequence-insert-commit "join" stop 'magit-sequence-part))
           ((magit-anything-modified-p t)
            ;; ...and the dust hasn't settled yet...
            (magit-sequence-insert-commit
             (let ((staged   (magit-commit-tree "oO" nil "HEAD"))
                   (unstaged (magit-commit-worktree "oO" "--reset")))
               (cond
                ;; ...but we could end up at the same tree just by committing.
                ((or (magit-rev-equal staged   stop)
                     (magit-rev-equal unstaged stop)) "goal")
                ;; ...but the changes are still there, untainted.
                ((or (equal (magit-patch-id staged)   id)
                     (equal (magit-patch-id unstaged) id)) "same")
                ;; ...and some changes are gone and/or others were added.
                (t "work")))
             stop 'magit-sequence-part))
           ;; The commit is definitely gone...
           ((--first (magit-rev-equal it stop) done)
            ;; ...but all of its changes are still in effect.
            (magit-sequence-insert-commit "poof" stop 'magit-sequence-drop))
           (t
            ;; ...and some changes are gone and/or other changes were added.
            (magit-sequence-insert-commit "gone" stop 'magit-sequence-drop)))
          (setq stop nil))))
    (dolist (rev done)
      (apply 'magit-sequence-insert-commit
             (cond ((equal rev stop)
                    ;; ...but its reincarnation lives on.
                    ;; Or it didn't die in the first place.
                    (list (if (and (equal rev head)
                                   (equal (magit-patch-id (concat stop "^"))
                                          (magit-patch-id (car (last orig 2)))))
                              "stop" ; We haven't done anything yet.
                            "same")  ; There are new commits.
                          rev (if (equal rev head)
                                  'magit-sequence-head
                                'magit-sequence-stop)))
                   ((equal rev head)
                    (list "done" rev 'magit-sequence-head))
                   (t
                    (list "done" rev 'magit-sequence-done)))))
    (magit-sequence-insert-commit "onto" onto
                                (if (equal onto head)
                                    'magit-sequence-head
                                  'magit-sequence-onto))))

(defun magit-sequence-insert-commit (type hash face)
  (magit-insert-section (commit hash)
    (insert (propertize type 'face face)    ?\s
            (magit-format-rev-summary hash) ?\n)))

;;; magit-sequence.el ends soon
(provide 'magit-sequence)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-sequence.el ends here
