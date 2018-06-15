;;; magit/forge.el ---                            -*- lexical-binding: t -*-

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

(require 'magit)
(require 'magit/forge/db)
(require 'magit/forge/core)

(provide 'magit/forge)

(require 'magit/forge/post)
(require 'magit/forge/topic)
(require 'magit/forge/issue)
(require 'magit/forge/pullreq)

(require 'magit/forge/github)
(require 'magit/forge/gitlab)

;;; Commands

;;;###autoload
(defun magit-forge-pull ()
  "Pull topics from the forge project of the current repository."
  (interactive)
  (if-let (prj (magit-forge-get-project t))
      (with-slots (owner name) prj
        (message "Pulling project %s/%s..." owner name)
        (magit-forge--pull-issues prj)
        (magit-forge--pull-pullreqs prj)
        (oset prj sparse-p nil)
        (magit-refresh)
        (message "Pulling project %s/%s...done" owner name))
    (error "Cannot determine forge project for %s" (magit-toplevel))))

;;;###autoload
(defun magit-forge-reset-database ()
  "Move the current database file to the trash.
This is useful after the database scheme has changed, which will
happen a few times while the forge functionality is still under
heavy development."
  (interactive)
  (when (and (file-exists-p magit-forge-database-file)
             (yes-or-no-p "Really trash Magit's database file? "))
    (when magit--db-connection
      (emacsql-close magit--db-connection))
    (delete-file magit-forge-database-file t)
    (magit-refresh)))

;;; magit/forge.el ends here
