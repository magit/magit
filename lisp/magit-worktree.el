;;; magit-worktree.el --- Worktree support  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2025 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements support for `git-worktree'.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-read-worktree-directory-function
  #'magit-read-worktree-directory-sibling
  "Function used to read the directory to be used as a new worktree.
This is called with two arguments, the prompt and the branch to be
checked out.  When not checking out a branch then use nil for the
second argument."
  :package-version '(magit . "4.3.9")
  :group 'magit-commands
  :type `(radio (function-item ,#'magit-read-worktree-directory)
                (function-item ,#'magit-read-worktree-directory-nested)
                (function-item ,#'magit-read-worktree-directory-sibling)
                (function-item ,#'magit-read-worktree-directory-offsite)
                function))

(defcustom magit-read-worktree-offsite-directory
  (expand-file-name "wtrees/" (or (getenv "XDG_DATA_HOME") "~/.local/share"))
  "Base directory used by `magit-read-worktree-directory-offsite'.
That function is suitable as `magit-read-worktree-directory-function',
but is not used by default."
  :package-version '(magit . "4.3.9")
  :group 'magit-commands
  :type 'directory)

(defvar magit-worktree-read-directory-name-function nil
  "Like `magit-read-worktree-directory-function' but takes only one argument.")
(make-obsolete-variable 'magit-worktree-read-directory-name-function
                        'magit-read-worktree-directory-function
                        "Magit 4.3.9")

;;; Functions

(defun magit-read-worktree-directory (prompt _branch)
  "Call `read-directory-name' with PROMPT, but ignoring _BRANCH."
  (read-directory-name prompt))

(defun magit-read-worktree-directory-nested (prompt branch)
  "Call `read-directory-name' in current worktree.
For `read-directory-name's INITIAL argument use a string based on
BRANCH, replacing slashes with dashes.  If BRANCH is nil, use nil
as INITIAL.  Always forward PROMPT as-is."
  (read-directory-name prompt nil nil nil
                       (and branch (string-replace "/" "-" branch))))

(defun magit-read-worktree-directory-sibling (prompt branch)
  "Call `read-directory-name' in parent directory of current worktree.
For `read-directory-name's INITIAL argument use a string based on the
name of the current worktree and BRANCH.  Use \"PREFIX_BRANCH\" where
PREFIX is the name of the current worktree, up to the first underscore,
and slashes in BRANCH are replaced with dashes.  If BRANCH is nil use
just \"PREFIX_\".  Always forward PROMPT as-is."
  (let* ((path (directory-file-name default-directory))
         (name (file-name-nondirectory path)))
    (read-directory-name
     prompt (file-name-directory path) nil nil
     (concat (if (string-match "_" name)
                 (substring name 0 (match-beginning 0))
               name)
             "_"
             (and branch (string-replace "/" "-" branch))))))

(defun magit-read-worktree-directory-offsite (prompt branch)
  "Call `read-directory-name' in a directory shared by all repositories.

Option `magit-read-worktree-offsite-directory' specifies that shared
base directory.

For `read-directory-name's INITIAL argument use a string based on the
name of the current worktree and BRANCH.  Use \"PREFIX_BRANCH\" where
PREFIX is the name of the current worktree, up to the first underscore,
and slashes in BRANCH are replaced with dashes.  If BRANCH is nil use
just \"PREFIX_\".  Always forward PROMPT as-is."
  (mkdir magit-read-worktree-offsite-directory t)
  (read-directory-name
   prompt magit-read-worktree-offsite-directory nil nil
   (let* ((name (file-name-nondirectory (directory-file-name default-directory)))
          (name (if (string-match "_" name)
                    (substring name 0 (match-beginning 0))
                  name))
          (name (concat name "_")))
     (if branch
         (concat name (string-replace "/" "-" branch))
       (file-name-nondirectory
        (make-temp-name
         (expand-file-name name magit-read-worktree-offsite-directory)))))))

(defun magit--read-worktree-directory (rev branchp)
  (let ((default-directory (magit-toplevel))
        (prompt (format "Checkout %s in new worktree: " rev)))
    (if magit-worktree-read-directory-name-function
        (funcall magit-worktree-read-directory-name-function prompt)
      (funcall magit-read-worktree-directory-function
               prompt (and branchp rev)))))

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
(defun magit-worktree-checkout (directory commit)
  "Checkout COMMIT in a new worktree in DIRECTORY.
COMMIT may, but does not have to be, a local branch.
Interactively, use `magit-read-worktree-directory-function'."
  (interactive
   (let ((commit (magit-read-branch-or-commit
                  "In new worktree; checkout" nil
                  (mapcar #'caddr (magit-list-worktrees)))))
     (list (magit--read-worktree-directory commit (magit-local-branch-p commit))
           commit)))
  (when (zerop (magit-run-git "worktree" "add"
                              (magit--expand-worktree directory) commit))
    (magit-diff-visit-directory directory)))

;;;###autoload
(defun magit-worktree-branch (directory branch start-point)
  "Create a new BRANCH and check it out in a new worktree at DIRECTORY.
Interactively, use `magit-read-worktree-directory-function'."
  (interactive
   (pcase-let
       ((`(,branch ,start-point)
         (magit-branch-read-args "In new worktree; checkout new branch")))
     (list (magit--read-worktree-directory branch t)
           branch start-point)))
  (when (zerop (magit-run-git "worktree" "add" "-b" branch
                              (magit--expand-worktree directory) start-point))
    (magit-diff-visit-directory directory)))

;;;###autoload
(defun magit-worktree-move (worktree directory)
  "Move existing WORKTREE directory to DIRECTORY."
  (interactive
   (list (magit-completing-read "Move worktree"
                                (cdr (magit-list-worktrees))
                                nil t nil nil
                                (magit-section-value-if 'worktree))
         (read-directory-name "Move worktree to: ")))
  (if (file-directory-p (expand-file-name ".git" worktree))
      (user-error "You may not move the main working tree")
    (let ((preexisting-directory (file-directory-p directory)))
      (when (and (zerop (magit-call-git "worktree" "move" worktree
                                        (magit--expand-worktree directory)))
                 (not (file-exists-p default-directory))
                 (derived-mode-p 'magit-status-mode))
        (kill-buffer)
        (magit-diff-visit-directory
         (if preexisting-directory
             (concat (file-name-as-directory directory)
                     (file-name-nondirectory worktree))
           directory)))
      (magit-refresh))))

(defun magit-worktree-delete (worktree)
  "Delete a worktree, defaulting to the worktree at point.
The primary worktree cannot be deleted."
  (interactive
   (list (magit-completing-read "Delete worktree"
                                (mapcar #'car (cdr (magit-list-worktrees)))
                                nil t nil nil
                                (magit-section-value-if 'worktree))))
  (if (file-directory-p (expand-file-name ".git" worktree))
      (user-error "Deleting %s would delete the shared .git directory" worktree)
    (let ((primary (file-name-as-directory (caar (magit-list-worktrees)))))
      (magit-confirm-files (if magit-delete-by-moving-to-trash 'trash 'delete)
                           (list worktree))
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

(defun magit--expand-worktree (directory)
  (magit-convert-filename-for-git (expand-file-name directory)))

;;; Sections

(defvar-keymap magit-worktree-section-map
  :doc "Keymap for `worktree' sections."
  "<remap> <magit-delete-thing>" #'magit-worktree-delete
  "<remap> <magit-visit-thing>"  #'magit-worktree-status
  "<4>" (magit-menu-item "Worktree commands..." #'magit-worktree)
  "<3>" '(menu-item "--")
  "<2>" (magit-menu-item "Delete %m" #'magit-worktree-delete)
  "<1>" (magit-menu-item "Visit %s" #'magit-worktree-status))

(defun magit-insert-worktrees ()
  "Insert sections for all worktrees.
If there is only one worktree, then insert nothing."
  (let ((worktrees (magit-list-worktrees)))
    (when (length> worktrees 1)
      (magit-insert-section (worktrees)
        (magit-insert-heading t "Worktrees")
        (let* ((cols
                (mapcar
                 (lambda (config)
                   (pcase-let ((`(,_ ,commit ,branch ,bare) config))
                     (cons (cond
                            (branch
                             (propertize
                              branch 'font-lock-face
                              (if (equal branch (magit-get-current-branch))
                                  'magit-branch-current
                                'magit-branch-local)))
                            (commit
                             (propertize (magit-rev-abbrev commit)
                                         'font-lock-face 'magit-hash))
                            (bare "(bare)"))
                           config)))
                 worktrees))
               (align (1+ (apply #'max (mapcar (##string-width (car %)) cols)))))
          (pcase-dolist (`(,head . ,config) cols)
            (magit--insert-worktree
             config
             (concat head (make-string (- align (length head)) ?\s)))))
        (insert ?\n)))))

(defun magit--insert-worktree (config head)
  "Insert worktree section for CONFIG.
See `magit-list-worktrees' for the format of CONFIG.  HEAD is
a prettified reference or revision representing the worktree,
with padding for alignment."
  ;; #4926 Before changing the signature, inform @vermiculus.
  (let ((path (car config)))
    (magit-insert-section (worktree path)
      (insert head)
      (insert (let ((relative (file-relative-name path))
                    (absolute (abbreviate-file-name path)))
                (if (or (> (string-width relative) (string-width absolute))
                        (equal relative "./"))
                    absolute
                  relative)))
      (insert ?\n))))

;;; _
(provide 'magit-worktree)
;;; magit-worktree.el ends here
