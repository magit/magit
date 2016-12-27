;;; magit-worktree.el --- worktree support  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Magit Project Contributors
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

;; This library implements support for `git-worktree'.

;;; Code:

(require 'magit)

;;; Commands

;;;###autoload
(defun magit-worktree-checkout (path branch)
  "Checkout BRANCH in a new worktree at PATH."
  (interactive
   (let ((branch (magit-read-local-branch "Checkout")))
     (list (read-directory-name (format "Checkout %s in new worktree: " branch))
           branch)))
  (magit-run-git "worktree" "add" (expand-file-name path) branch)
  (magit-diff-visit-directory path))

;;;###autoload
(defun magit-worktree-branch (path branch start-point &optional force)
  "Create a new BRANCH and check it out in a new worktree at PATH."
  (interactive
   `(,(read-directory-name "Create worktree: ")
     ,@(butlast (magit-branch-read-args "Create and checkout branch"))
     ,current-prefix-arg))
  (magit-run-git "worktree" "add" (if force "-B" "-b")
                 branch (expand-file-name path) start-point)
  (magit-diff-visit-directory path))

(defun magit-worktree-delete (worktree)
  "Delete a worktree, defaulting to the worktree at point.
The primary worktree cannot be deleted."
  (interactive
   (list (magit-completing-read "Delete worktree"
                                (cdr (magit-list-worktrees))
                                nil t nil nil
                                (magit-section-when (worktree)))))
  (if (file-directory-p (expand-file-name ".git" worktree))
      (user-error "Deleting %s would delete the shared .git directory" worktree)
    (let ((primary (file-name-as-directory (caar (magit-list-worktrees)))))
      (when (if magit-delete-by-moving-to-trash
                (magit-confirm-files 'trash (list "worktree"))
              (magit-confirm-files 'delete (list "worktree")))
        (let ((delete-by-moving-to-trash magit-delete-by-moving-to-trash))
          (delete-directory worktree t magit-delete-by-moving-to-trash))
        (if (file-exists-p default-directory)
            (magit-run-git "worktree" "prune")
          (let ((default-directory primary))
            (magit-run-git "worktree" "prune")))))))

(defun magit-worktree-status (worktree)
  "Show the status for the worktree at point.
If there is no worktree at point, then read one in the
minibuffer.  If the worktree at point is the one whose
status is already being displayed in the current buffer,
then show it in Dired instead."
  (interactive
   (list (or (magit-section-when (worktree))
             (magit-completing-read
              "Show status for worktree"
              (cl-delete (directory-file-name (magit-toplevel))
                         (magit-list-worktrees)
                         :test #'equal :key #'car)))))
  (magit-diff-visit-directory worktree))

;;; Sections

(defvar magit-worktree-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]  'magit-worktree-status)
    (define-key map [remap magit-delete-thing] 'magit-worktree-delete)
    map)
  "Keymap for `worktree' sections.")

(defun magit-insert-worktrees ()
  "Insert sections for all worktrees.
If there is only one worktree, then insert nothing."
  (let ((worktrees (magit-list-worktrees)))
    (when (> (length worktrees) 1)
      (magit-insert-section (worktrees)
        (magit-insert-heading "Worktrees:")
        (let* ((cols
                (mapcar (-lambda ((path barep commit branch))
                          (cons (cond
                                 (branch (propertize branch
                                                     'face 'magit-branch-local))
                                 (commit (propertize (magit-rev-abbrev commit)
                                                     'face 'magit-hash))
                                 (barep  "(bare)"))
                                path))
                        worktrees))
               (align (1+ (-max (--map (string-width (car it)) cols)))))
          (pcase-dolist (`(,head . ,path) cols)
            (magit-insert-section (worktree path)
              (insert head)
              (indent-to align)
              (insert (let ((r (file-relative-name path))
                            (a (abbreviate-file-name path)))
                        (if (< (string-width r) (string-width a)) r a)))
              (insert ?\n))))
        (insert ?\n)))))

(provide 'magit-worktree)
;;; magit-worktree.el ends here
