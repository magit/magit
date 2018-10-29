;;; magit-libgit.el --- Libgit functionality       -*- lexical-binding: t -*-

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

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'subr-x)

(require 'magit-git)

(if (eq (magit--git-implementation) 'libgit)
    (condition-case err
        (require 'libgit)
      (error
       (setq magit-inhibit-libgit 'error)
       (message "Error while loading `magit-libgit': %S" err)))
  ;; If `libegit2' isn't available, then this file is compiled
  ;; anyway.  Don't spew warnings.  Sort alphabetically.
  (declare-function libgit-repository-bare-p "libegit2")
  (declare-function libgit-repository-open "libegit2")
  )

;;; Utilities

(defun magit-libgit-repo (&optional directory)
  (and-let* ((default-directory
               (let ((magit-inhibit-libgit t))
                 (magit-gitdir directory))))
    (magit--with-refresh-cache
        (cons default-directory 'magit-libgit-repo)
      (libgit-repository-open default-directory))))

;;; Methods

;;; _
(provide 'magit-libgit)
;;; magit-libgit.el ends here

