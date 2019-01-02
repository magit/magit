;;; magit-fetch.el --- download objects and refs  -*- lexical-binding: t -*-

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

;; This library implements fetch commands.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-fetch-modules-jobs 4
  "Number of submodules to fetch in parallel.
Ignored for Git versions before v2.8.0."
  :package-version '(magit . "2.12.0")
  :group 'magit-commands
  :type '(choice (const :tag "one at a time" nil) number))

;;; Commands

;;;###autoload (autoload 'magit-fetch-popup "magit-fetch" nil t)
(magit-define-popup magit-fetch-popup
  "Popup console for fetch commands."
  :man-page "git-fetch"
  :switches '((?p "Prune deleted branches" "--prune"))
  :actions  '("Configure"
              (?C "variables..."           magit-branch-config-popup)
              "Fetch from"
              (?p magit-get-push-remote    magit-fetch-from-pushremote)
              (?u magit-get-remote         magit-fetch-from-upstream)
              (?e "elsewhere"              magit-fetch-other)
              (?a "all remotes"            magit-fetch-all)
              "Fetch"
              (?o "another branch"         magit-fetch-branch)
              (?r "explicit refspec"       magit-fetch-refspec)
              (?m "submodules"             magit-fetch-modules))
  :default-action 'magit-fetch
  :max-action-columns 1)

(defun magit-git-fetch (remote args)
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "fetch" remote args))

;;;###autoload
(defun magit-fetch-from-pushremote (args)
  "Fetch from the push-remote of the current branch."
  (interactive (list (magit-fetch-arguments)))
  (--if-let (magit-get-push-remote)
      (magit-git-fetch it args)
    (--if-let (magit-get-current-branch)
        (user-error "No push-remote is configured for %s" it)
      (user-error "No branch is checked out"))))

;;;###autoload
(defun magit-fetch-from-upstream (args)
  "Fetch from the upstream repository of the current branch."
  (interactive (list (magit-fetch-arguments)))
  (--if-let (magit-get-remote)
      (magit-git-fetch it args)
    (--if-let (magit-get-current-branch)
        (user-error "No upstream is configured for %s" it)
      (user-error "No branch is checked out"))))

;;;###autoload
(defun magit-fetch-other (remote args)
  "Fetch from another repository."
  (interactive (list (magit-read-remote "Fetch remote")
                     (magit-fetch-arguments)))
  (magit-git-fetch remote args))

;;;###autoload
(defun magit-fetch-branch (remote branch args)
  "Fetch a BRANCH from a REMOTE."
  (interactive
   (let ((remote (magit-read-remote-or-url "Fetch from remote or url")))
     (list remote
           (magit-read-remote-branch "Fetch branch" remote)
           (magit-fetch-arguments))))
  (magit-git-fetch remote (cons branch args)))

;;;###autoload
(defun magit-fetch-refspec (remote refspec args)
  "Fetch a REFSPEC from a REMOTE."
  (interactive
   (let ((remote (magit-read-remote-or-url "Fetch from remote or url")))
     (list remote
           (magit-read-refspec "Fetch using refspec" remote)
           (magit-fetch-arguments))))
  (magit-git-fetch remote (cons refspec args)))

;;;###autoload
(defun magit-fetch-all (args)
  "Fetch from all remotes."
  (interactive (list (cl-intersection (magit-fetch-arguments)
                                      (list "--verbose" "--prune")
                                      :test #'equal)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "remote" "update" args))

;;;###autoload
(defun magit-fetch-all-prune ()
  "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote."
  (interactive)
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "remote" "update" "--prune"))

;;;###autoload
(defun magit-fetch-all-no-prune ()
  "Fetch from all remotes."
  (interactive)
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "remote" "update"))

;;;###autoload
(defun magit-fetch-modules (&optional all)
  "Fetch all submodules.

Option `magit-fetch-modules-jobs' controls how many submodules
are being fetched in parallel.  Also fetch the super-repository,
because `git-fetch' does not support not doing that.  With a
prefix argument fetch all remotes."
  (interactive "P")
  (magit-with-toplevel
    (magit-run-git-async
     "fetch" "--verbose" "--recurse-submodules"
     (and magit-fetch-modules-jobs
          (version<= "2.8.0" (magit-git-version))
          (list "-j" (number-to-string magit-fetch-modules-jobs)))
     (and all "--all"))))

;;; _
(provide 'magit-fetch)
;;; magit-fetch.el ends here
