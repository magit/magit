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

;;; Commands

;;;###autoload (autoload 'magit-pull-popup "magit-pull" nil t)
(magit-define-popup magit-pull-popup
  "Popup console for pull commands."
  :man-page "git-pull"
  :variables '("Configure"
               (?r "branch.%s.rebase"
                   magit-cycle-branch*rebase
                   magit-pull-format-branch*rebase)
               (?C "variables..." magit-branch-config-popup))
  :actions '((lambda ()
               (--if-let (magit-get-current-branch)
                   (concat
                    (propertize "Pull into " 'face 'magit-popup-heading)
                    (propertize it           'face 'magit-branch-local)
                    (propertize " from"      'face 'magit-popup-heading))
                 (propertize "Pull from" 'face 'magit-popup-heading)))
             (?p magit-get-push-branch     magit-pull-from-pushremote)
             (?u magit-get-upstream-branch magit-pull-from-upstream)
             (?e "elsewhere"               magit-pull-branch))
  :default-action 'magit-pull
  :max-action-columns 1)

;;;###autoload (autoload 'magit-pull-and-fetch-popup "magit-pull" nil t)
(magit-define-popup magit-pull-and-fetch-popup
  "Popup console for pull and fetch commands.

This popup is intended as a replacement for the separate popups
`magit-pull-popup' and `magit-fetch-popup'.  To use it, add this
to your init file:

  (with-eval-after-load \\='magit-remote
    (define-key magit-mode-map \"f\" \\='magit-pull-and-fetch-popup)
    (define-key magit-mode-map \"F\" nil))

The combined popup does not offer all commands and arguments
available from the individual popups.  Instead of the argument
`--prune' and the command `magit-fetch-all' it uses two commands
`magit-fetch-prune' and `magit-fetch-no-prune'.  And the commands
`magit-fetch-from-pushremote' and `magit-fetch-from-upstream' are
missing.  To add them use something like:

  (with-eval-after-load \\='magit-remote
    (magit-define-popup-action \\='magit-pull-and-fetch-popup ?U
      \\='magit-get-upstream-branch
      \\='magit-fetch-from-upstream-remote ?F)
    (magit-define-popup-action \\='magit-pull-and-fetch-popup ?P
      \\='magit-get-push-branch
      \\='magit-fetch-from-push-remote ?F))"
  :man-page "git-pull"
  :variables '("Configure"
               (?r "branch.%s.rebase"
                   magit-cycle-branch*rebase
                   magit-pull-format-branch*rebase)
               (?C "variables..." magit-branch-config-popup))
  :actions '((lambda ()
               (--if-let (magit-get-current-branch)
                   (concat
                    (propertize "Pull into " 'face 'magit-popup-heading)
                    (propertize it           'face 'magit-branch-local)
                    (propertize " from"      'face 'magit-popup-heading))
                 (propertize "Pull from" 'face 'magit-popup-heading)))
             (?p magit-get-push-branch     magit-pull-from-pushremote)
             (?u magit-get-upstream-branch magit-pull-from-upstream)
             (?e "elsewhere"               magit-pull-branch)
             "Fetch from"
             (?f "remotes"           magit-fetch-all-no-prune)
             (?F "remotes and prune" magit-fetch-all-prune)
             "Fetch"
             (?o "another branch"    magit-fetch-branch)
             (?s "explicit refspec"  magit-fetch-refspec)
             (?m "submodules"        magit-fetch-modules))
  :default-action 'magit-fetch
  :max-action-columns 1)

(defun magit-pull-format-branch*rebase ()
  (magit--format-popup-variable:choices
   (format "branch.%s.rebase" (or (magit-get-current-branch) "<name>"))
   '("true" "false")
   "false" "pull.rebase"))

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
      (progn (run-hooks 'magit-credential-hook)
             (magit-run-git-with-editor
              "pull" args (car (magit-split-branch-name it))))
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
