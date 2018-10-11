;;; magit-sequence.el --- history manipulation in Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2018  The Magit Project Contributors
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

(defface magit-sequence-exec
  '((t :inherit magit-hash))
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

(defvar magit-perl-executable "perl"
  "The Perl executable.")

;;;###autoload (autoload 'magit-cherry-pick-popup "magit-sequence" nil t)
(magit-define-popup magit-cherry-pick-popup
  "Popup console for cherry-pick commands."
  :man-page "git-cherry-pick"
  :switches '((?s "Add Signed-off-by lines"            "--signoff")
              (?e "Edit commit messages"               "--edit")
              (?x "Reference cherry in commit message" "-x")
              (?F "Attempt fast-forward"               "--ff"))
  :options  '((?s "Strategy"                        "--strategy=")
              (?m "Replay merge relative to parent" "--mainline="))
  :actions  '("Apply here"
              (?A "Pick"    magit-cherry-pick)
              (?a "Apply"   magit-cherry-apply)
              (?h "Harvest" magit-cherry-harvest)
              "Apply elsewhere"
              (?d "Donate"  magit-cherry-donate)
              (?n "Spinout" magit-cherry-spinout)
              (?s "Spinoff" magit-cherry-spinoff))
  :sequence-actions '((?A "Continue" magit-sequencer-continue)
                      (?s "Skip"     magit-sequencer-skip)
                      (?a "Abort"    magit-sequencer-abort))
  :sequence-predicate 'magit-sequencer-in-progress-p
  :default-arguments '("--ff"))

(defun magit-cherry-pick-read-args (prompt)
  (list (or (nreverse (magit-region-values 'commit))
            (magit-read-other-branch-or-commit prompt))
        (magit-cherry-pick-arguments)))

(defun magit--cherry-move-read-args (verb away fn)
  (declare (indent defun))
   (let ((commits (or (nreverse (magit-region-values 'commit))
                      (list (funcall (if away
                                         'magit-read-branch-or-commit
                                       'magit-read-other-branch-or-commit)
                                     (format "%s cherry" (capitalize verb))))))
         (current (magit-get-current-branch)))
     (unless current
       (user-error "Cannot %s cherries while HEAD is detached" verb))
     (let ((reachable (magit-rev-ancestor-p (car commits) current))
           (msg "Cannot %s cherries that %s reachable from HEAD"))
       (pcase (list away reachable)
         (`(nil t) (user-error msg verb "are"))
         (`(t nil) (user-error msg verb "are not"))))
     `(,commits
       ,@(funcall fn commits)
       ,(magit-cherry-pick-arguments))))

(defun magit--cherry-spinoff-read-args (verb)
  (magit--cherry-move-read-args verb t
    (lambda (commits)
      (butlast (magit-branch-read-args
                (format "Create branch from %s cherries" commits))))))

;;;###autoload
(defun magit-cherry-pick (commits &optional args)
  "Copy COMMITS from another branch onto the current branch.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then pick all of them,
without prompting."
  (interactive (magit-cherry-pick-read-args "Cherry-pick"))
  (magit--cherry-pick commits args))

;;;###autoload
(defun magit-cherry-apply (commits &optional args)
  "Apply the changes in COMMITS but do not commit them.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then apply all of them,
without prompting."
  (interactive (magit-cherry-pick-read-args "Apply changes from commit"))
  (magit--cherry-pick commits (cons "--no-commit" (remove "--ff" args))))

;;;###autoload
(defun magit-cherry-harvest (commits branch &optional args)
  "Move COMMITS from another BRANCH onto the current branch.
Remove the COMMITS from BRANCH and stay on the current branch.
If a conflict occurs, then you have to fix that and finish the
process manually."
  (interactive
   (magit--cherry-move-read-args "harvest" nil
     (lambda (commits)
       (list (let ((branches (magit-list-containing-branches (car commits))))
               (pcase (length branches)
                 (0 nil)
                 (1 (car branches))
                 (_ (magit-completing-read
                     (format "Remove %s cherries from branch" (length commits))
                     branches nil t))))))))
  (magit--cherry-move commits branch (magit-get-current-branch) args nil t))

;;;###autoload
(defun magit-cherry-donate (commits branch &optional args)
  "Move COMMITS from the current branch onto another existing BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually."
  (interactive
   (magit--cherry-move-read-args "donate" t
     (lambda (commits)
       (list (magit-read-other-branch (format "Move %s cherries to branch"
                                              (length commits)))))))
  (magit--cherry-move commits (magit-get-current-branch) branch args))

;;;###autoload
(defun magit-cherry-spinout (commits branch start-point &optional args)
  "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and stay on that branch.
If a conflict occurs, then you have to fix that and finish the
process manually."
  (interactive (magit--cherry-spinoff-read-args "spinout"))
  (magit--cherry-move commits (magit-get-current-branch) branch args
                      start-point))

;;;###autoload
(defun magit-cherry-spinoff (commits branch start-point &optional args)
  "Move COMMITS from the current branch onto a new BRANCH.
Remove COMMITS from the current branch and checkout BRANCH.
If a conflict occurs, then you have to fix that and finish
the process manually."
  (interactive (magit--cherry-spinoff-read-args "spinoff"))
  (magit--cherry-move commits (magit-get-current-branch) branch args
                      start-point t))

(defun magit--cherry-move (commits src dst args
                                   &optional start-point checkout-dst)
  (let ((current (magit-get-current-branch)))
    (unless (magit-branch-p dst)
      (let ((magit-process-raise-error t))
        (magit-call-git "branch" dst start-point))
      (--when-let (magit-get-indirect-upstream-branch start-point)
        (magit-call-git "branch" "--set-upstream-to" it dst)))
    (unless (equal dst current)
      (let ((magit-process-raise-error t))
        (magit-call-git "checkout" dst)))
    (if (not src) ; harvest only
        (magit--cherry-pick commits args)
      (let ((tip (car (last commits)))
            (keep (concat (car commits) "^")))
        (magit--cherry-pick commits args)
        (set-process-sentinel
         magit-this-process
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (if (> (process-exit-status process) 0)
                 (magit-process-sentinel process event)
               (process-put process 'inhibit-refresh t)
               (magit-process-sentinel process event)
               (cond
                ((magit-rev-equal tip src)
                 (magit-call-git "update-ref"
                                 "-m" (format "reset: moving to %s" keep)
                                 (magit-ref-fullname src)
                                 keep tip)
                 (if (not checkout-dst)
                     (magit-run-git "checkout" src)
                   (magit-refresh)))
                (t
                 (magit-git "checkout" src)
                 (let ((process-environment process-environment))
                   (push (format "%s=%s -i -ne '/^pick (%s)/ or print'"
                                 "GIT_SEQUENCE_EDITOR"
                                 magit-perl-executable
                                 (mapconcat #'magit-rev-abbrev commits "|"))
                         process-environment)
                   (magit-run-git-sequencer "rebase" "-i" keep))
                 (when checkout-dst
                   (set-process-sentinel
                    magit-this-process
                    (lambda (process event)
                      (when (memq (process-status process) '(exit signal))
                        (if (> (process-exit-status process) 0)
                            (magit-process-sentinel process event)
                          (process-put process 'inhibit-refresh t)
                          (magit-process-sentinel process event)
                          (magit-run-git "checkout" dst))))))))))))))))

(defun magit--cherry-pick (commits args &optional revert)
  (let ((command (if revert "revert" "cherry-pick")))
    (when (stringp commits)
      (setq commits (if (string-match-p "\\.\\." commits)
                        (split-string commits "\\.\\.")
                      (list commits))))
    (magit-run-git-sequencer
     (if revert "revert" "cherry-pick")
     (pcase-let ((`(,merge ,non-merge)
                  (-separate 'magit-merge-commit-p commits)))
       (cond
        ((not merge)
         (--remove (string-prefix-p "--mainline=" it) args))
        (non-merge
         (user-error "Cannot %s merge and non-merge commits at once"
                     command))
        ((--first (string-prefix-p "--mainline=" it) args)
         args)
        (t
         (cons (format "--mainline=%s"
                       (read-number "Replay merges relative to parent: "))
               args))))
     commits)))

(defun magit-cherry-pick-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (file-exists-p (magit-git-dir "CHERRY_PICK_HEAD")))

;;; Revert

;;;###autoload (autoload 'magit-revert-popup "magit-sequence" nil t)
(magit-define-popup magit-revert-popup
  "Popup console for revert commands."
  :man-page "git-revert"
  :switches '((?s "Add Signed-off-by lines"   "--signoff")
              (?e "Edit commit message"       "--edit")
              (?E "Don't edit commit message" "--no-edit"))
  :options  '((?s "Strategy"       "--strategy=")
              (?S "Sign using gpg" "--gpg-sign=" magit-read-gpg-secret-key)
              (?m "Replay merge relative to parent" "--mainline="))
  :actions  '((?V "Revert commit(s)" magit-revert-and-commit)
              (?v "Revert changes"   magit-revert-no-commit))
  :sequence-actions '((?V "Continue" magit-sequencer-continue)
                      (?s "Skip"     magit-sequencer-skip)
                      (?a "Abort"    magit-sequencer-abort))
  :sequence-predicate 'magit-sequencer-in-progress-p
  :default-arguments '("--edit"))

(defun magit-revert-read-args (prompt)
  (list (or (magit-region-values 'commit)
            (magit-read-branch-or-commit prompt))
        (magit-revert-arguments)))

;;;###autoload
(defun magit-revert-and-commit (commit &optional args)
  "Revert COMMIT by creating a new commit.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (magit-revert-read-args "Revert commit"))
  (magit--cherry-pick commit args t))

;;;###autoload
(defun magit-revert-no-commit (commit &optional args)
  "Revert COMMIT by applying it in reverse to the worktree.
Prompt for a commit, defaulting to the commit at point.  If
the region selects multiple commits, then revert all of them,
without prompting."
  (interactive (magit-revert-read-args "Revert changes"))
  (magit--cherry-pick commit (cons "--no-commit" args) t))

(defun magit-revert-in-progress-p ()
  ;; .git/sequencer/todo does not exist when there is only one commit left.
  (file-exists-p (magit-git-dir "REVERT_HEAD")))

;;; Patch

;;;###autoload (autoload 'magit-am-popup "magit-sequence" nil t)
(magit-define-popup magit-am-popup
  "Popup console for mailbox commands."
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
  :actions  '((?m "Apply maildir"     magit-am-apply-maildir)
              (?w "Apply patches"     magit-am-apply-patches)
              (?a "Apply plain patch" magit-patch-apply-popup))
  :default-arguments '("--3way")
  :default-actions 'magit-am-apply-patches
  :max-action-columns 1
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
  (magit-run-git-sequencer "am" args "--"
                           (--map (magit-convert-filename-for-git
                                   (expand-file-name it))
                                  files)))

;;;###autoload
(defun magit-am-apply-maildir (&optional maildir args)
  "Apply the patches from MAILDIR."
  (interactive (list (read-file-name "Apply mbox or Maildir: ")
                     (magit-am-arguments)))
  (magit-run-git-sequencer "am" args (magit-convert-filename-for-git
                                      (expand-file-name maildir))))

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
  :man-page "git-rebase"
  :switches '((?k "Keep empty commits"       "--keep-empty")
              (?p "Preserve merges"          "--preserve-merges")
              (?c "Lie about committer date" "--committer-date-is-author-date")
              (?a "Autosquash"               "--autosquash")
              (?A "Autostash"                "--autostash")
              (?i "Interactive"              "--interactive")
              (?h "Disable hooks"            "--no-verify"))
  :actions  '((lambda ()
                (concat (propertize "Rebase " 'face 'magit-popup-heading)
                        (propertize (or (magit-get-current-branch) "HEAD")
                                    'face 'magit-branch-local)
                        (propertize " onto" 'face 'magit-popup-heading)))
              (?p (lambda ()
                    (--when-let (magit-get-push-branch) (concat it "\n")))
                  magit-rebase-onto-pushremote)
              (?u (lambda ()
                    (--when-let (magit-get-upstream-branch) (concat it "\n")))
                  magit-rebase-onto-upstream)
              (?e "elsewhere" magit-rebase-branch)
              "Rebase"
              (?i "interactively"      magit-rebase-interactive)
              (?m "to modify a commit" magit-rebase-edit-commit)
              (?s "a subset"           magit-rebase-subset)
              (?w "to reword a commit" magit-rebase-reword-commit) nil
              (?k "to remove a commit" magit-rebase-remove-commit) nil
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
      (if-let ((remote (magit-get-push-remote it)))
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
      (if-let ((target (magit-get-upstream-branch it)))
          (magit-git-rebase target args)
        (user-error "No upstream is configured for %s" it))
    (user-error "No branch is checked out")))

;;;###autoload
(defun magit-rebase-branch (target args)
  "Rebase the current branch onto a branch read in the minibuffer.
All commits that are reachable from `HEAD' but not from the
selected branch TARGET are being rebased."
  (interactive (list (magit-read-other-branch-or-commit "Rebase onto")
                     (magit-rebase-arguments)))
  (message "Rebasing...")
  (magit-git-rebase target args)
  (message "Rebasing...done"))

;;;###autoload
(defun magit-rebase-subset (newbase start args)
  "Rebase a subset of the current branch's history onto a new base.
Rebase commits from START to `HEAD' onto NEWBASE.
START has to be selected from a list of recent commits."
  (interactive (list (magit-read-other-branch-or-commit
                      "Rebase subset onto" nil
                      (magit-get-upstream-branch))
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

(defun magit-rebase-interactive-1
    (commit args message &optional editor delay-edit-confirm noassert confirm)
  (declare (indent 2))
  (when commit
    (if (eq commit :merge-base)
        (setq commit (--if-let (magit-get-upstream-branch)
                         (magit-git-string "merge-base" it "HEAD")
                       nil))
      (unless (magit-rev-ancestor-p commit "HEAD")
        (user-error "%s isn't an ancestor of HEAD" commit))
      (if (magit-commit-parents commit)
          (setq commit (concat commit "^"))
        (setq args (cons "--root" args)))))
  (when (and commit (not noassert))
    (setq commit (magit-rebase-interactive-assert commit delay-edit-confirm)))
  (if (and commit (not confirm))
      (let ((process-environment process-environment))
        (when editor
          (push (concat "GIT_SEQUENCE_EDITOR=" editor) process-environment))
        (magit-run-git-sequencer "rebase" "-i" args
                                 (unless (member "--root" args) commit)))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-interactive-1 commit (list ,@args)
           ,message ,editor ,delay-edit-confirm ,noassert))
      message)))

(defvar magit--rebase-published-symbol nil)
(defvar magit--rebase-public-edit-confirmed nil)

(defun magit-rebase-interactive-assert (since &optional delay-edit-confirm)
  (let* ((commit (if (string-suffix-p "^" since)
                     ;; If SINCE is "REV^", then the user selected
                     ;; "REV", which is the first commit that will
                     ;; be replaced. (from^..to] <=> [from..to].
                     (substring since 0 -1)
                   ;; The "--root" argument is being used.
                   since))
         (branches (magit-list-publishing-branches commit)))
    (setq magit--rebase-public-edit-confirmed
          (delete (magit-toplevel) magit--rebase-public-edit-confirmed))
    (when (and branches
               (or (not delay-edit-confirm)
                   ;; The user might have stopped at a published commit
                   ;; merely to add new commits *after* it.  Try not to
                   ;; ask users whether they really want to edit public
                   ;; commits, when they don't actually intend to do so.
                   (not (--all-p (magit-rev-equal it commit) branches))))
      (let ((m1 "Some of these commits have already been published to ")
            (m2 ".\nDo you really want to modify them"))
        (magit-confirm (or magit--rebase-published-symbol 'rebase-published)
          (concat m1 "%s" m2)
          (concat m1 "%i public branches" m2)
          nil branches))
      (push (magit-toplevel) magit--rebase-public-edit-confirmed)))
  (if (magit-git-lines "rev-list" "--merges" (concat since "..HEAD"))
      (magit-read-char-case "Proceed despite merge in rebase range?  " nil
        (?c "[c]ontinue" since)
        (?s "[s]elect other" nil)
        (?a "[a]bort" (user-error "Quit")))
    since))

;;;###autoload
(defun magit-rebase-interactive (commit args)
  "Start an interactive rebase sequence."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to rebase it and all commits above it,"
    nil t))

;;;###autoload
(defun magit-rebase-autosquash (args)
  "Combine squash and fixup commits with their intended targets."
  (interactive (list (magit-rebase-arguments)))
  (magit-rebase-interactive-1 :merge-base (cons "--autosquash" args)
    "Type %p on a commit to squash into it and then rebase as necessary,"
    "true" nil t))

;;;###autoload
(defun magit-rebase-edit-commit (commit args)
  "Edit a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to edit it,"
    (concat magit-perl-executable
            " -i -p -e '++$x if not $x and s/^pick/edit/'")
    t))

;;;###autoload
(defun magit-rebase-reword-commit (commit args)
  "Reword a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to reword its message,"
    (concat magit-perl-executable
            " -i -p -e '++$x if not $x and s/^pick/reword/'")))

;;;###autoload
(defun magit-rebase-remove-commit (commit args)
  "Remove a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (magit-rebase-interactive-1 commit args
    "Type %p on a commit to remove it,"
    (concat magit-perl-executable
            " -i -p -e '++$x if not $x and s/^pick/# pick/'")
    nil nil t))

;;;###autoload
(defun magit-rebase-continue (&optional noedit)
  "Restart the current rebasing operation.
In some cases this pops up a commit message buffer for you do
edit.  With a prefix argument the old message is reused as-is."
  (interactive "P")
  (if (magit-rebase-in-progress-p)
      (if (magit-anything-unstaged-p t)
          (user-error "Cannot continue rebase with unstaged changes")
        (when (and (magit-anything-staged-p)
                   (file-exists-p (magit-git-dir "rebase-merge"))
                   (not (member (magit-toplevel)
                                magit--rebase-public-edit-confirmed)))
          (magit-commit-amend-assert))
        (if noedit
            (let ((process-environment process-environment))
              (push "GIT_EDITOR=true" process-environment)
              (magit-run-git-async (magit--rebase-resume-command) "--continue")
              (set-process-sentinel magit-this-process
                                    #'magit-sequencer-process-sentinel)
              magit-this-process)
          (magit-run-git-sequencer (magit--rebase-resume-command) "--continue")))
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-skip ()
  "Skip the current commit and restart the current rebase operation."
  (interactive)
  (unless (magit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (magit-run-git-sequencer (magit--rebase-resume-command) "--skip"))

;;;###autoload
(defun magit-rebase-edit ()
  "Edit the todo list of the current rebase operation."
  (interactive)
  (unless (magit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (magit-run-git-sequencer "rebase" "--edit-todo"))

;;;###autoload
(defun magit-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (unless (magit-rebase-in-progress-p)
    (user-error "No rebase in progress"))
  (magit-confirm 'abort-rebase "Abort this rebase")
  (magit-run-git (magit--rebase-resume-command) "--abort"))

(defun magit-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (or (file-exists-p (magit-git-dir "rebase-merge"))
      (file-exists-p (magit-git-dir "rebase-apply/onto"))))

(defun magit--rebase-resume-command ()
  (if (file-exists-p (magit-git-dir "rebase-recursive")) "rbr" "rebase"))

;;; Sections

(defun magit-insert-sequencer-sequence ()
  "Insert section for the on-going cherry-pick or revert sequence.
If no such sequence is in progress, do nothing."
  (let ((picking (magit-cherry-pick-in-progress-p)))
    (when (or picking (magit-revert-in-progress-p))
      (magit-insert-section (sequence)
        (magit-insert-heading (if picking "Cherry Picking" "Reverting"))
        (when-let ((lines
                    (cdr (magit-file-lines (magit-git-dir "sequencer/todo")))))
          (dolist (line (nreverse lines))
            (when (string-match
                   "^\\(pick\\|revert\\) \\([^ ]+\\) \\(.*\\)$" line)
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
          (setq patch (pop patches))
          (setq commit (magit-rev-verify-commit
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
    (let ((title
           (with-temp-buffer
             (insert-file-contents patch nil nil 4096)
             (unless (re-search-forward "^Subject: " nil t)
               (goto-char (point-min)))
             (buffer-substring (point) (line-end-position)))))
      (insert (propertize type 'face face)
              ?\s (propertize (file-name-nondirectory patch) 'face 'magit-hash)
              ?\s title
              ?\n))))

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
            (magit-rebase-insert-merge-sequence onto)
          (magit-rebase-insert-apply-sequence onto))
        (insert ?\n)))))

(defun magit-rebase-insert-merge-sequence (onto)
  (let (exec)
    (dolist (line (nreverse
                   (magit-file-lines
                    (magit-git-dir "rebase-merge/git-rebase-todo"))))
      (cond ((string-prefix-p "exec" line)
             (setq exec (substring line 5)))
            ((string-match (format "^\\([^%c ]+\\) \\([^ ]+\\) .*$"
                                   (string-to-char
                                    (or (magit-get "core.commentChar") "#")))
                           line)
             (magit-bind-match-strings (action hash) line
               (unless (equal action "exec")
                 (magit-sequence-insert-commit
                  action hash 'magit-sequence-pick exec)))
             (setq exec nil)))))
  (magit-sequence-insert-sequence
   (magit-file-line (magit-git-dir "rebase-merge/stopped-sha"))
   onto
   (--when-let (magit-file-lines (magit-git-dir "rebase-merge/done"))
     (cadr (split-string (car (last it)))))))

(defun magit-rebase-insert-apply-sequence (onto)
  (let ((rewritten
         (--map (car (split-string it))
                (magit-file-lines (magit-git-dir "rebase-apply/rewritten"))))
        (stop (magit-file-line (magit-git-dir "rebase-apply/original-commit"))))
    (dolist (patch (nreverse (cdr (magit-rebase-patches))))
      (let ((hash (cadr (split-string (magit-file-line patch)))))
        (unless (or (member hash rewritten)
                    (equal hash stop))
          (magit-sequence-insert-commit "pick" hash 'magit-sequence-pick)))))
  (magit-sequence-insert-sequence
   (magit-file-line (magit-git-dir "rebase-apply/original-commit"))
   onto))

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
             (let* ((magit--refresh-cache nil)
                    (staged   (magit-commit-tree "oO" nil "HEAD"))
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
                                   (equal (magit-patch-id rev)
                                          (magit-patch-id orig)))
                              "stop" ; We haven't done anything yet.
                            "like")  ; There are new commits.
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

(defun magit-sequence-insert-commit (type hash face &optional exec)
  (magit-insert-section (commit hash)
    (magit-insert-heading
      (propertize type 'face face)    "\s"
      (magit-format-rev-summary hash) "\n")
    (when exec
      (insert (propertize "exec" 'face 'magit-sequence-onto) "\s" exec "\n"))))

(provide 'magit-sequence)
;;; magit-sequence.el ends here
