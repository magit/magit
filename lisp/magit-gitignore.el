;;; magit-gitignore.el --- intentionally untracked files  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2019  The Magit Project Contributors
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

;; This library implements gitignore commands.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'magit)

;;; Commands

;;;###autoload (autoload 'magit-gitignore "magit-gitignore" nil t)
(define-transient-command magit-gitignore ()
  "Instruct Git to ignore a file or pattern."
  :man-page "gitignore"
  ["Gitignore"
   ("t" "shared at toplevel (.gitignore)"
    magit-gitignore-in-topdir)
   ("p" "privately (.git/info/exclude)"
    magit-gitignore-in-gitdir)
   ])

;;;###autoload
(defun magit-gitignore-in-topdir (rule)
  "Add the Git ignore RULE to the top-level \".gitignore\" file.
Since this file is tracked, it is shared with other clones of the
repository.  Also stage the file."
  (interactive (list (magit-gitignore-read-pattern)))
  (magit-with-toplevel
    (magit--gitignore rule ".gitignore")
    (magit-run-git "add" ".gitignore")))

;;;###autoload
(defun magit-gitignore-in-gitdir (rule)
  "Add the Git ignore RULE to \"$GIT_DIR/info/exclude\".
Rules in that file only affects this clone of the repository."
  (interactive (list (magit-gitignore-read-pattern)))
  (magit--gitignore rule (magit-git-dir "info/exclude"))
  (magit-refresh))

(defun magit--gitignore (rule file)
  (when-let ((directory (file-name-directory file)))
    (make-directory directory t))
  (with-temp-buffer
    (when (file-exists-p file)
      (insert-file-contents file))
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))
    (insert (replace-regexp-in-string "\\(\\\\*\\)" "\\1\\1" rule))
    (insert "\n")
    (write-region nil nil file)))

(defun magit-gitignore-read-pattern ()
  (let* ((default (magit-current-file))
         (choices
          (delete-dups
           (--mapcat
            (cons (concat "/" it)
                  (when-let ((ext (file-name-extension it)))
                    (list (concat "/" (file-name-directory "foo") "*." ext)
                          (concat "*." ext))))
            (magit-untracked-files)))))
    (when default
      (setq default (concat "/" default))
      (unless (member default choices)
        (setq default (concat "*." (file-name-extension default)))
        (unless (member default choices)
          (setq default nil))))
    (magit-completing-read "File or pattern to ignore"
                           choices nil nil nil nil default)))

;;; _
(provide 'magit-gitignore)
;;; magit-gitignore.el ends here
