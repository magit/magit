;;; magit-pull.el --- update local objects and refs  -*- lexical-binding: t -*-

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

;; This library implements pull commands.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-pull-or-fetch nil
  "Whether `magit-pull' also offers some fetch suffixes."
  :package-version '(magit . "2.91.0")
  :group 'magit-commands
  :type 'boolean)

;;; Commands

;;;###autoload (autoload 'magit-pull "magit-pull" nil t)
(define-transient-command magit-pull ()
  "Pull from another repository."
  :man-page "git-pull"
  [:description
   (lambda ()
     (if-let ((branch (magit-get-current-branch)))
         (concat
          (propertize "Pull into " 'face 'transient-heading)
          (propertize branch       'face 'magit-branch-local)
          (propertize " from"      'face 'transient-heading))
       (propertize "Pull from" 'face 'transient-heading)))
   ("p" magit-pull-from-pushremote
    :if 'magit-get-push-branch
    :description 'magit-get-push-branch)
   ("u" magit-pull-from-upstream
    :if 'magit-get-upstream-branch
    :description 'magit-get-upstream-branch)
   ("e" "elsewhere"         magit-pull-branch)]
  ["Fetch from"
   :if-non-nil magit-pull-or-fetch
   ("f" "remotes"           magit-fetch-all-no-prune)
   ("F" "remotes and prune" magit-fetch-all-prune)]
  ["Fetch"
   :if-non-nil magit-pull-or-fetch
   ("o" "another branch"    magit-fetch-branch)
   ("s" "explicit refspec"  magit-fetch-refspec)
   ("m" "submodules"        magit-fetch-modules)]
  ["Configure"
   ("r" magit-branch.<branch>.rebase :if magit-get-current-branch)
   ("C" "variables..." magit-branch-configure)]
  (interactive)
  (transient-setup 'magit-pull nil nil :scope (magit-get-current-branch)))

(defun magit-pull-arguments ()
  (transient-args 'magit-pull))

(defun magit-git-pull (source args)
  (run-hooks 'magit-credential-hook)
  (pcase-let ((`(,remote . ,branch)
               (magit-split-branch-name source)))
    (magit-run-git-with-editor "pull" args remote branch)))

;;;###autoload
(defun magit-pull-from-pushremote (args)
  "Pull from the push-remote of the current branch."
  (interactive (list (magit-pull-arguments)))
  (--if-let (magit-get-push-branch)
      (magit-git-pull it args)
    (--if-let (magit-get-current-branch)
        (user-error "No push-remote is configured for %s" it)
      (user-error "No branch is checked out"))))

;;;###autoload
(defun magit-pull-from-upstream (args)
  "Pull from the upstream of the current branch."
  (interactive (list (magit-pull-arguments)))
  (--if-let (magit-get-upstream-branch)
      (magit-git-pull it args)
    (--if-let (magit-get-current-branch)
        (user-error "No upstream is configured for %s" it)
      (user-error "No branch is checked out"))))

;;;###autoload
(defun magit-pull-branch (source args)
  "Pull from a branch read in the minibuffer."
  (interactive (list (magit-read-remote-branch "Pull" nil nil nil t)
                     (magit-pull-arguments)))
  (magit-git-pull source args))

;;; _
(provide 'magit-pull)
;;; magit-pull.el ends here
