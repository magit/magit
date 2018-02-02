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

;;;###autoload (autoload 'magit-merge-popup "magit" nil t)
(magit-define-popup magit-merge-popup
  "Popup console for merge commands."
  :man-page "git-merge"
  :switches '((?f "Fast-forward only" "--ff-only")
              (?n "No fast-forward"   "--no-ff"))
  :options  '((?s "Strategy" "--strategy="))
  :actions  '((?m "Merge"                  magit-merge)
              (?p "Preview merge"          magit-merge-preview)
              (?e "Merge and edit message" magit-merge-editmsg)
              (?s "Squash merge"           magit-merge-squash)
              (?n "Merge but don't commit" magit-merge-nocommit))
  :sequence-actions   '((?m "Commit merge" magit-commit)
                        (?a "Abort merge"  magit-merge-abort))
  :sequence-predicate 'magit-merge-state
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

(defun magit-merge-state ()
  (file-exists-p (magit-git-dir "MERGE_HEAD")))

(defun magit-merge-assert ()
  (or (not (magit-anything-modified-p t))
      (magit-confirm 'merge-dirty
        "Merging with dirty worktree is risky.  Continue")))

(defun magit-checkout-read-stage (file)
  (magit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

(defun magit-insert-merge-log ()
  "Insert section for the on-going merge.
Display the heads that are being merged.
If no merge is in progress, do nothing."
  (-when-let (heads (mapcar 'magit-get-shortname
                            (magit-file-lines (magit-git-dir "MERGE_HEAD"))))
    (magit-insert-section (commit (car heads))
      (magit-insert-heading
        (format "Merging %s:" (mapconcat 'identity heads ", ")))
      (magit-insert-log
       (concat (magit-git-string "merge-base" "--octopus" "HEAD" (car heads))
               ".." (car heads))
       (let ((args magit-log-section-arguments))
         (unless (member "--decorate=full" magit-log-section-arguments)
           (push "--decorate=full" args))
         args)))))

(provide 'magit-merge)
;;; magit-merge.el ends here
