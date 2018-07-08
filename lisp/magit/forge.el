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
(defun magit-forge-add-pullreq-refspec ()
  "Configure Git to fetch all pull-requests.
This is done by adding \"+refs/pull/*/head:refs/pullreqs/*\"
to the value of `remote.REMOTE.fetch', where REMOTE is the
upstream remote.  Also fetch from REMOTE."
  (interactive)
  (let* ((project (magit-forge-get-project t))
         (remote  (oref project remote))
         (fetch   (magit-get-all "remote" remote "fetch"))
         (refspec (oref-default project pullreq-refspec)))
    (if (member refspec fetch)
        (message "Pull-request refspec is already active")
      (magit-call-git "config" "--add"
                      (format "remote.%s.fetch" remote)
                      refspec)
      (magit-git-fetch remote (magit-fetch-arguments)))))

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

;;;###autoload
(defun magit-browse-commit (rev)
  "Visit the url corresponding to REV using a browser."
  (interactive
   (list (or (magit-completing-read "Browse commit"
                                    (magit-list-branch-names)
                                    nil nil nil 'magit-revision-history
                                    (magit-branch-or-commit-at-point))
             (user-error "Nothing selected"))))
  ;; FIXME This assumes that the commit is available on the upstream,
  ;; but it might only be available in another remote or even only
  ;; locally.
  (browse-url (magit-forge--format-url
               (magit-forge-get-project t)
               'commit-url-format
               `((?r . ,(magit-rev-verify-commit rev))))))

;;;###autoload
(defun magit-browse-branch (branch)
  "Visit the url corresponding BRANCH using a browser."
  (interactive (list (magit-read-branch "Browse branch")))
  (let (remote)
    (if (magit-remote-branch-p branch)
        (let ((cons (magit-split-branch-name branch)))
          (setq remote (car cons))
          (setq branch (cdr cons)))
      (or (setq remote (or (magit-get-push-remote branch)
                           (magit-get-upstream-remote branch)))
          (user-error "Cannot determine remote for %s" branch)))
    (browse-url (magit-forge--format-remote-url remote 'branch-url-format
                                                `((?r . ,branch))))))

;;;###autoload
(defun magit-browse-remote (remote)
  "Visit the url corresponding to REMOTE using a browser."
  (interactive (list (magit-read-remote "Browse remote")))
  (browse-url (magit-forge--format-remote-url remote 'remote-url-format)))

;; TODO move me
(defun magit-forge--format-remote-url (remote slot &optional spec)
  (if-let (parts (magit-forge--split-remote-url remote))
      ;; FIXME(?) This assumes that a fork is located on the same
      ;; forge as the upstream.
      (magit-forge--format-url (magit-forge-get-project t) slot
                               `(,@spec
                                 (?h . ,(nth 0 parts))
                                 (?o . ,(nth 1 parts))
                                 (?n . ,(nth 2 parts))))
    (user-error "Cannot browse non-forge remote %s" remote)))

;;; magit/forge.el ends here
