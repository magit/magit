;;; magit-gitignore.el --- intentionally untracked files  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2018  The Magit Project Contributors
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

;;;###autoload (autoload 'magit-gitignore-popup "magit-gitignore" nil t)
(magit-define-popup magit-gitignore-popup
  "Popup console for gitignore commands."
  :man-page "gitignore"
  :actions '((?l "ignore locally"  magit-gitignore-locally)
             (?g "ignore in .gitignore" magit-gitignore-globally)
             (?s "ignore system-wide" magit-gitignore-system))
  :max-action-columns 1)

;;;###autoload
(defun magit-gitignore-globally (file-or-pattern)
  "Instruct Git to ignore FILE-OR-PATTERN in all checkouts of this repository.

Magit will edit the .gitignore file in the project's working directory root."
  (interactive (list (magit-gitignore-read-pattern nil)))
  (magit--gitignore file-or-pattern nil))

;;;###autoload
(defun magit-gitignore-locally (file-or-pattern)
  "Instruct Git to ignore FILE-OR-PATTERN in the current checkout only.

Magit will edit the info/exclude file in the project's git directory."
  (interactive (list (magit-gitignore-read-pattern " locally")))
  (magit--gitignore file-or-pattern t))

;;;###autoload
(defun magit-gitignore-system (file-or-pattern)
  "Instruct Git to ignore FILE-OR-PATTERN in all repositories on this system.

Magit will edit the file indicated in git's core.excludesfile setting."
  (interactive (list (magit-gitignore-read-pattern " system-wide")))
  (magit--gitignore file-or-pattern 'system))

(defun magit--gitignore (file-or-pattern local)
  (let ((gitignore
         (cond
          ((eq local 'system)
           (or (magit-get "core.excludesfile")
               (error "core.excludesfile is not set")))
          (local
           (magit-git-dir (convert-standard-filename "info/exclude")))
          (t
           (expand-file-name ".gitignore" (magit-toplevel))))))
    (make-directory (file-name-directory gitignore) t)
    (with-temp-buffer
      (when (file-exists-p gitignore)
        (insert-file-contents gitignore))
      (goto-char (point-max))
      (unless (bolp)
        (insert "\n"))
      (insert (replace-regexp-in-string "\\(\\\\*\\)" "\\1\\1" file-or-pattern))
      (insert "\n")
      (write-region nil nil gitignore))
    (if local
        (magit-refresh)
      (magit-run-git "add" ".gitignore"))))

(defun magit-gitignore-read-pattern (message-suffix)
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
    (magit-completing-read (concat "File or pattern to ignore" message-suffix)
                           choices nil nil nil nil default)))

;;; _
(provide 'magit-gitignore)
;;; magit-gitignore.el ends here
