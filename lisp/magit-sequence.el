;;; magit-sequence.el --- history manipulation in Magit

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
      (if (magit-anything-unstaged-p)
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
  'magit-commands 'magit-popup-sequence-mode
  :man-page "git-cherry-pick"
  :switches '((?s "Add Signed-off-by lines"            "--signoff")
              (?e "Edit commit messages"               "--edit")
              (?x "Reference cherry in commit message" "-x")
              (?F "Attempt fast-forward"               "--ff")
              (?m "Reply merge relative to parent"     "--mainline="))
  :options  '((?s "Strategy" "--strategy=" read-from-minibuffer))
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
  (interactive (magit-cherry-pick-read-args "Apply commit"))
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
  'magit-commands 'magit-popup-sequence-mode
  :man-page "git-revert"
  :switches '((?s "Add Signed-off-by lines" "--signoff"))
  :options  '((?s "Strategy" "--strategy="  read-from-minibuffer))
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
  'magit-commands 'magit-popup-sequence-mode
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
                         (list (read-file-name "Apply patch: " nil
                                               (magit-file-at-point))))
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
      (if (magit-anything-unstaged-p)
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
  'magit-commands 'magit-popup-sequence-mode
  :man-page "git-rebase"
  :switches '((?k "Keep empty commits" "--keep-empty")
              (?p "Preserve merges" "--preserve-merges")
              (?c "Lie about author date" "--committer-date-is-author-date")
              (?a "Autosquash" "--autosquash")
              (?A "Autostash" "--autostash"))
  :actions  '((?r "Rebase"             magit-rebase)
              (?f "Autosquash"         magit-rebase-autosquash)
              (?o "Rebase subset"      magit-rebase-from)
              nil
              (?e "Rebase interactive" magit-rebase-interactive)
              (?s "Edit commit"        magit-rebase-edit-commit)
              (?l "Rebase unpushed"    magit-rebase-unpushed)
              (?w "Reword commit"      magit-rebase-reword-commit))
  :sequence-actions '((?r "Continue" magit-rebase-continue)
                      (?s "Skip"     magit-rebase-skip)
                      (?e "Edit"     magit-rebase-edit)
                      (?a "Abort"    magit-rebase-abort))
  :sequence-predicate 'magit-rebase-in-progress-p
  :max-action-columns 2)

;;;###autoload
(defun magit-rebase (upstream &optional args)
  "Start a non-interactive rebase sequence.
All commits not in UPSTREAM are rebased.
\n(git rebase UPSTREAM[^] [ARGS])"
  (interactive (list (magit-read-other-branch-or-commit
                      "Rebase onto"
                      (magit-get-current-branch)
                      (magit-get-tracked-branch))
                     (magit-rebase-arguments)))
  (message "Rebasing...")
  (magit-run-git-sequencer "rebase" upstream args)
  (message "Rebasing...done"))

;;;###autoload
(defun magit-rebase-from (newbase start &optional args)
  "Start a non-interactive rebase sequence.
Commits from START to `HEAD' onto NEWBASE.  START has to be
selected from a list of recent commits.
\n(git rebase --onto NEWBASE START[^] [ARGS])"
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
         (magit-rebase-from ,newbase (concat commit "^") (list ,@args)))
      (concat "Type %p on a commit to rebase it "
              "and commits above it onto " newbase ","))))

;;;###autoload
(defun magit-rebase-interactive (commit &optional args)
  "Start an interactive rebase sequence.
\n(git rebase -i COMMIT[^] [ARGS])"
  (interactive (let ((commit (magit-commit-at-point)))
                 (list (and commit (concat commit "^"))
                       (magit-rebase-arguments))))
  (if (setq commit (magit-rebase-interactive-assert commit))
      (magit-run-git-sequencer "rebase" "-i" commit args)
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-interactive (concat commit "^") (list ,@args)))
      "Type %p on a commit to interactively rebase it and all commits above it,")))

(defun magit-rebase-unpushed (commit &optional args)
  "Start an interactive rebase sequence of all unpushed commits.
\n(git rebase -i UPSTREAM [ARGS])"
  (interactive (list (magit-get-tracked-branch)
                     (magit-rebase-arguments)))
  (if (setq commit (magit-rebase-interactive-assert commit))
      (magit-run-git-sequencer "rebase" "-i" commit args)
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-interactive (concat commit "^") (list ,@args)))
      "Type %p on a commit to interactively rebase it and all commits above it,")))

;;;###autoload
(defun magit-rebase-autosquash (commit &optional args)
  "Combine squash and fixup commits with their intended targets.
\n(git rebase -i COMMIT[^] --autosquash [ARGS])"
  (interactive (list (--when-let (magit-get-tracked-branch)
                       (magit-git-string "merge-base" it "HEAD"))
                     (magit-rebase-arguments)))
  (if (setq commit (magit-rebase-interactive-assert commit))
      (let ((process-environment process-environment))
        (setenv "GIT_SEQUENCE_EDITOR" "true")
        (magit-run-git-sequencer "rebase" "-i" commit "--autosquash" args))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-autosquash (concat commit "^") (list ,@args)))
      "Type %p on a commit to squash into it and commits above it,")))

;;;###autoload
(defun magit-rebase-edit-commit (commit &optional args)
  "Edit a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (if (setq commit (magit-rebase-interactive-assert commit))
      (let ((process-environment process-environment))
        (setenv "GIT_SEQUENCE_EDITOR"
                "perl -i -p -e '++$x if not $x and s/^pick/edit/'")
        (magit-run-git-sequencer "rebase" "-i" (concat commit "^") args))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-edit-commit commit (list ,@args)))
      "Type %p on a commit to edit it,")))

;;;###autoload
(defun magit-rebase-reword-commit (commit &optional args)
  "Reword a single older commit using rebase."
  (interactive (list (magit-commit-at-point)
                     (magit-rebase-arguments)))
  (if (setq commit (magit-rebase-interactive-assert commit))
      (let ((process-environment process-environment))
        (setenv "GIT_SEQUENCE_EDITOR"
                "perl -i -p -e '++$x if not $x and s/^pick/reword/'")
        (magit-run-git-sequencer "rebase" "-i" (concat commit "^") args))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-reword-commit commit (list ,@args)))
      "Type %p on a commit to reword its message,")))

(defun magit-rebase-interactive-assert (commit)
  (when commit
    (if (magit-git-lines "rev-list" "--merges" (concat commit "..HEAD"))
        (magit-read-char-case "Proceed despite merge in rebase range?  " nil
          (?c "[c]ontinue" commit)
          (?s "[s]elect other" nil)
          (?a "[a]bort" (user-error "Quit")))
      commit)))

;;;###autoload
(defun magit-rebase-continue ()
  "Restart the current rebasing operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (if (magit-anything-unstaged-p)
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
      (let* ((patches (magit-rebase-patches))
             (stop (magit-file-line (car patches))))
        (if (string-match "^From \\([^ ]\\{40\\}\\)" stop)
            (progn (setq stop (match-string 1 stop))
                   (magit-rebase-insert-apply-sequence)
                   (magit-sequence-insert-sequence stop "ORIG_HEAD"))
          (setq  patches (nreverse patches))
          (while patches
            (let ((patch (pop patches)))
              (magit-insert-section (file patch)
                (insert (if patches
                            (propertize "pick" 'face 'magit-sequence-pick)
                          (propertize "stop" 'face 'magit-sequence-stop))
                        ?\s (propertize (file-name-nondirectory patch)
                                        'face 'magit-hash)
                        ?\n))))
          (magit-sequence-insert-sequence nil "ORIG_HEAD")))
      (insert ?\n))))

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
  (setq  onto (magit-rev-parse onto))
  (let ((head (magit-rev-parse "HEAD"))
        (done (magit-git-lines "log" "--format=%H" (concat onto "..HEAD"))))
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
                   (unstaged (magit-commit-worktree "oO")))
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
