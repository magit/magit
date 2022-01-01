;;; magit-worktree.el --- worktree support  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2022  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;;; Options

(defcustom magit-worktree-read-directory-name-function 'read-directory-name
  "Function used to read a directory for worktree commands.
This is called with one argument, the prompt, and can be used
to e.g. use a base directory other than `default-directory'.
Used by `magit-worktree-checkout' and `magit-worktree-branch'."
  :package-version '(magit . "3.0.0")
  :group 'magit-commands
  :type 'function)

;;; Commands

;;;###autoload (autoload 'magit-worktree "magit-worktree" nil t)
(transient-define-prefix magit-worktree ()
  "Act on a worktree."
  :man-page "git-worktree"
  [["Create new"
    ("b" "worktree"              magit-worktree-checkout)
    ("c" "branch and worktree"   magit-worktree-branch)]
   ["Commands"
    ("m" "Move worktree"         magit-worktree-move)
    ("k" "Delete worktree"       magit-worktree-delete)
    ("g" "Visit worktree"        magit-worktree-status)]])

;;;###autoload
(defun magit-worktree-checkout (path branch)
  "Checkout BRANCH in a new worktree at PATH."
  (interactive
   (let ((branch (magit-read-branch-or-commit "Checkout")))
     (list (funcall magit-worktree-read-directory-name-function
                    (format "Checkout %s in new worktree: " branch))
           branch)))
  (magit-run-git "worktree" "add" (magit--expand-worktree path) branch)
  (magit-diff-visit-directory path))

;;;###autoload
(defun magit-worktree-branch (path branch start-point &optional force)
  "Create a new BRANCH and check it out in a new worktree at PATH."
  (interactive
   `(,(funcall magit-worktree-read-directory-name-function
               "Create worktree: ")
     ,@(magit-branch-read-args "Create and checkout branch")
     ,current-prefix-arg))
  (magit-run-git "worktree" "add" (if force "-B" "-b")
                 branch (magit--expand-worktree path) start-point)
  (magit-diff-visit-directory path))

;;;###autoload
(defun magit-worktree-move (worktree path)
  "Move WORKTREE to PATH."
  (interactive
   (list (magit-completing-read "Move worktree"
                                (cdr (magit-list-worktrees))
                                nil t nil nil
                                (magit-section-value-if 'worktree))
         (funcall magit-worktree-read-directory-name-function
                  "Move worktree to: ")))
  (if (file-directory-p (expand-file-name ".git" worktree))
      (user-error "You may not move the main working tree")
    (let ((preexisting-directory (file-directory-p path)))
      (when (and (zerop (magit-call-git "worktree" "move" worktree
                                        (magit--expand-worktree path)))
                 (not (file-exists-p default-directory))
                 (derived-mode-p 'magit-status-mode))
        (kill-buffer)
        (magit-diff-visit-directory
         (if preexisting-directory
             (concat (file-name-as-directory path)
                     (file-name-nondirectory worktree))
           path)))
      (magit-refresh))))

(defun magit-worktree-delete (worktree)
  "Delete a worktree, defaulting to the worktree at point.
The primary worktree cannot be deleted."
  (interactive
   (list (magit-completing-read "Delete worktree"
                                (cdr (magit-list-worktrees))
                                nil t nil nil
                                (magit-section-value-if 'worktree))))
  (if (file-directory-p (expand-file-name ".git" worktree))
      (user-error "Deleting %s would delete the shared .git directory" worktree)
    (let ((primary (file-name-as-directory (caar (magit-list-worktrees)))))
      (magit-confirm-files (if magit-delete-by-moving-to-trash 'trash 'delete)
                           (list "worktree"))
      (when (file-exists-p worktree)
        (let ((delete-by-moving-to-trash magit-delete-by-moving-to-trash))
          (delete-directory worktree t magit-delete-by-moving-to-trash)))
      (if (file-exists-p default-directory)
          (magit-run-git "worktree" "prune")
        (let ((default-directory primary))
          (magit-run-git "worktree" "prune"))
        (when (derived-mode-p 'magit-status-mode)
          (kill-buffer)
          (magit-status-setup-buffer primary))))))

(defun magit-worktree-status (worktree)
  "Show the status for the worktree at point.
If there is no worktree at point, then read one in the
minibuffer.  If the worktree at point is the one whose
status is already being displayed in the current buffer,
then show it in Dired instead."
  (interactive
   (list (or (magit-section-value-if 'worktree)
             (magit-completing-read
              "Show status for worktree"
              (cl-delete (directory-file-name (magit-toplevel))
                         (magit-list-worktrees)
                         :test #'equal :key #'car)))))
  (magit-diff-visit-directory worktree))

(defun magit--expand-worktree (path)
  (magit-convert-filename-for-git (expand-file-name path)))

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
                (mapcar
                 (pcase-lambda (`(,path ,barep ,commit ,branch))
                   (cons (cond
                          (branch (propertize
                                   branch 'font-lock-face
                                   (if (equal branch (magit-get-current-branch))
                                       'magit-branch-current
                                     'magit-branch-local)))
                          (commit (propertize (magit-rev-abbrev commit)
                                              'font-lock-face 'magit-hash))
                          (barep  "(bare)"))
                         path))
                 worktrees))
               (align (1+ (-max (--map (string-width (car it)) cols)))))
          (pcase-dolist (`(,head . ,path) cols)
            (magit-insert-section (worktree path)
              (insert head)
              (insert (make-string (- align (length head)) ?\s))
              (insert (let ((r (file-relative-name path))
                            (a (abbreviate-file-name path)))
                        (if (< (string-width r) (string-width a)) r a)))
              (insert ?\n))))
        (insert ?\n)))))

;;; _
(provide 'magit-worktree)
;;; magit-worktree.el ends here
