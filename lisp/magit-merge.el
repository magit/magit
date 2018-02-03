;;; magit-merge.el --- merge functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
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

;; This library implements merge commands.

;;; Code:

(require 'magit)

;;; Commands

;;;###autoload (autoload 'magit-merge-popup "magit" nil t)
(magit-define-popup magit-merge-popup
  "Popup console for merge commands."
  :man-page "git-merge"
  :switches '((?f "Fast-forward only" "--ff-only")
              (?n "No fast-forward"   "--no-ff"))
  :options  '((?s "Strategy" "--strategy="))
  :actions  '((?m "Merge"                  magit-merge)
              (?p "Preview merge"          magit-merge-preview)
              (?e "Merge and edit message" magit-merge-editmsg) nil
              (?n "Merge but don't commit" magit-merge-nocommit)
              (?s "Squash merge"           magit-merge-squash)
              (?a "Absorb"                 magit-merge-absorb)
              (?i "Merge into"             magit-merge-into))
  :sequence-actions   '((?m "Commit merge" magit-commit)
                        (?a "Abort merge"  magit-merge-abort))
  :sequence-predicate 'magit-merge-in-progress-p
  :default-action 'magit-merge
  :max-action-columns 2)

;;;###autoload
(defun magit-merge (rev &optional args nocommit)
  "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)
                     current-prefix-arg))
  (magit-merge-assert)
  (magit-run-git-async "merge" (if nocommit "--no-commit" "--no-edit") args rev))

;;;###autoload
(defun magit-merge-editmsg (rev &optional args)
  "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.
\n(git merge --edit --no-ff [ARGS] REV)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)))
  (magit-merge-assert)
  (cl-pushnew "--no-ff" args :test #'equal)
  (apply #'magit-run-git-with-editor "merge" "--edit"
         (append args (list rev))))

;;;###autoload
(defun magit-merge-nocommit (rev &optional args)
  "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.
\n(git merge --no-commit --no-ff [ARGS] REV)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)))
  (magit-merge-assert)
  (cl-pushnew "--no-ff" args :test #'equal)
  (magit-run-git-async "merge" "--no-commit" args rev))

;;;###autoload
(defun magit-merge-into (branch &optional args)
  "Merge the current branch into BRANCH and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `magit-branch-pull-request' was used to create the merged
branch, then also remove the respective remote branch."
  (interactive (list (magit-read-other-branch
                      (format "Merge `%s' into" (magit-get-current-branch)))
                     (magit-merge-arguments)))
  (let ((current (magit-get-current-branch)))
    (when (zerop (magit-call-git "checkout" branch))
      (magit--merge-absort current args))))

;;;###autoload
(defun magit-merge-absorb (branch &optional args)
  "Merge BRANCH into the current branch and remove the former.

Before merging, force push the source branch to its push-remote,
provided the respective remote branch already exists, ensuring
that the respective pull-request (if any) won't get stuck on some
obsolete version of the commits that are being merged.  Finally
if `magit-branch-pull-request' was used to create the merged
branch, then also remove the respective remote branch."
  (interactive (list (magit-read-other-local-branch "Absorb branch")
                     (magit-merge-arguments)))
  (magit--merge-absort branch args))

(defun magit--merge-absort (branch args)
  (when (equal branch "master")
    (unless (yes-or-no-p
             "Do you really wanto to merge `master' into another branch? ")
      (user-error "Abort")))
  (-if-let (target (magit-get-push-branch branch t))
      (progn
        (magit-git-push branch target (list "--force-with-lease"))
        (set-process-sentinel
         magit-this-process
         (lambda (process event)
           (when (memq (process-status process) '(exit signal))
             (if (not (zerop (process-exit-status process)))
                 (magit-process-sentinel process event)
               (process-put process 'inhibit-refresh t)
               (magit-process-sentinel process event)
               (magit--merge-absort-1 branch args))))))
    (magit--merge-absort-1 branch args)))

(defun magit--merge-absort-1 (branch args)
  (magit-run-git-async "merge" args "--no-edit" branch)
  (set-process-sentinel
   magit-this-process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (if (> (process-exit-status process) 0)
           (magit-process-sentinel process event)
         (process-put process 'inhibit-refresh t)
         (magit-process-sentinel process event)
         (magit-branch-maybe-delete-pr-remote branch)
         (magit-branch-unset-pushRemote branch)
         (magit-run-git "branch" "-D" branch))))))

;;;###autoload
(defun magit-merge-squash (rev)
  "Squash commit REV into the current branch; don't create a commit.
\n(git merge --squash REV)"
  (interactive (list (magit-read-other-branch-or-commit "Squash")))
  (magit-merge-assert)
  (magit-run-git-async "merge" "--squash" rev))

;;;###autoload
(defun magit-merge-preview (rev)
  "Preview result of merging REV into the current branch."
  (interactive (list (magit-read-other-branch-or-commit "Preview merge")))
  (magit-mode-setup #'magit-merge-preview-mode rev))

(define-derived-mode magit-merge-preview-mode magit-diff-mode "Magit Merge"
  "Mode for previewing a merge."
  :group 'magit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun magit-merge-preview-refresh-buffer (rev)
  (let* ((branch (magit-get-current-branch))
         (head (or branch (magit-rev-verify "HEAD"))))
    (magit-set-header-line-format (format "Preview merge of %s into %s"
                                          rev
                                          (or branch "HEAD")))
    (magit-insert-section (diffbuf)
      (magit-git-wash #'magit-diff-wash-diffs
        "merge-tree" (magit-git-string "merge-base" head rev) head rev))))

;;;###autoload
(defun magit-merge-abort ()
  "Abort the current merge operation.
\n(git merge --abort)"
  (interactive)
  (unless (file-exists-p (magit-git-dir "MERGE_HEAD"))
    (user-error "No merge in progress"))
  (magit-confirm 'abort-merge)
  (magit-run-git-async "merge" "--abort"))

(defun magit-checkout-stage (file arg)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((file (magit-completing-read "Checkout file"
                                      (magit-tracked-files) nil nil nil
                                      'magit-read-file-hist
                                      (magit-current-file))))
     (cond ((member file (magit-unmerged-files))
            (list file (magit-checkout-read-stage file)))
           ((yes-or-no-p (format "Restore conflicts in %s? " file))
            (list file "--merge"))
           (t
            (user-error "Quit")))))
  (pcase (cons arg (cddr (car (magit-file-status file))))
    ((or `("--ours"   ?D ,_)
         `("--theirs" ,_ ?D))
     (magit-run-git "rm" "--" file))
    (_ (if (equal arg "--merge")
           ;; This fails if the file was deleted on one
           ;; side.  And we cannot do anything about it.
           (magit-run-git "checkout" "--merge" "--" file)
         (magit-call-git "checkout" arg "--" file)
         (magit-run-git "add" "-u" "--" file)))))

;;; Utilities

(defun magit-merge-in-progress-p ()
  (file-exists-p (magit-git-dir "MERGE_HEAD")))

(defun magit--merge-range (&optional head)
  (unless head
    (setq head (magit-get-shortname
                (car (magit-file-lines (magit-git-dir "MERGE_HEAD"))))))
  (and head
       (concat (magit-git-string "merge-base" "--octopus" "HEAD" head)
               ".." head)))

(defun magit-merge-assert ()
  (or (not (magit-anything-modified-p t))
      (magit-confirm 'merge-dirty
        "Merging with dirty worktree is risky.  Continue")))

(defun magit-checkout-read-stage (file)
  (magit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

;;; Sections

(defvar magit-unmerged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] 'magit-diff-dwim)
    map)
  "Keymap for `unmerged' sections.")

(defun magit-insert-merge-log ()
  "Insert section for the on-going merge.
Display the heads that are being merged.
If no merge is in progress, do nothing."
  (when (magit-merge-in-progress-p)
    (let* ((heads (mapcar #'magit-get-shortname
                          (magit-file-lines (magit-git-dir "MERGE_HEAD"))))
           (range (magit--merge-range (car heads))))
      (magit-insert-section (unmerged range)
        (magit-insert-heading
          (format "Merging %s:" (mapconcat #'identity heads ", ")))
        (magit-insert-log
         range
         (let ((args magit-log-section-arguments))
           (unless (member "--decorate=full" magit-log-section-arguments)
             (push "--decorate=full" args))
           args))))))

(provide 'magit-merge)
;;; magit-merge.el ends here
