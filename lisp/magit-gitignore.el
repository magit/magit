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
             (?g "ignore globally" magit-gitignore-globally))
  :max-action-columns 1)

;;;###autoload
(defun magit-gitignore-globally (file-or-pattern)
  "Instruct Git to globally ignore FILE-OR-PATTERN."
  (interactive (list (magit-gitignore-read-pattern nil)))
  (magit--gitignore file-or-pattern nil))

;;;###autoload
(defun magit-gitignore-locally (file-or-pattern)
  "Instruct Git to locally ignore FILE-OR-PATTERN."
  (interactive (list (magit-gitignore-read-pattern t)))
  (magit--gitignore file-or-pattern t))

(defun magit--gitignore (file-or-pattern local)
  (let ((gitignore
         (if local
             (magit-git-dir (convert-standard-filename "info/exclude"))
           (expand-file-name ".gitignore" (magit-toplevel)))))
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

(defun magit-gitignore-read-pattern (local)
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
    (magit-completing-read (concat "File or pattern to ignore"
                                   (and local " locally"))
                           choices nil nil nil nil default)))

;;; _
(provide 'magit-gitignore)
;;; magit-gitignore.el ends here
