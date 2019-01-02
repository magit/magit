;;; magit-push.el --- update remote objects and refs  -*- lexical-binding: t -*-

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

;; This library implements push commands.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'magit)

;;; Options

(defcustom magit-push-current-set-remote-if-missing t
  "Whether to configure missing remotes before pushing.

When nil, then the command `magit-push-current-to-pushremote' and
`magit-push-current-to-upstream' do not appear in the push popup
if the push-remote resp. upstream is not configured.  If the user
invokes one of these commands anyway, then it raises an error.

When non-nil, then these commands always appear in the push
popup.  But if the required configuration is missing, then they
do appear in a way that indicates that this is the case.  If the
user invokes one of them, then it asks for the necessary
configuration, stores the configuration, and then uses it to push
a first time.

This option also affects whether the argument `--set-upstream' is
available in the popup.  If the value is t, then that argument is
redundant.  But note that changing the value of this option does
not take affect immediately, the argument will only be added or
removed after restarting Emacs."
  :package-version '(magit . "2.6.0")
  :group 'magit-commands
  :type '(choice (const :tag "don't set" nil)
                 (const :tag "set branch.<name>.pushRemote" t)
                 (const :tag "set remote.pushDefault" default)))

;;; Commands

;;;###autoload (autoload 'magit-push-popup "magit-push" nil t)
(magit-define-popup magit-push-popup
  "Popup console for push commands."
  :man-page "git-push"
  :switches `((?f "Force with lease" "--force-with-lease")
              (?F "Force"            "--force")
              (?h "Disable hooks"    "--no-verify")
              (?d "Dry run"          "--dry-run")
              ,@(and (not magit-push-current-set-remote-if-missing)
                     '((?u "Set upstream"  "--set-upstream"))))
  :actions '("Configure"
             (?C "variables..."      magit-branch-config-popup)
             (lambda ()
               (--when-let (magit-get-current-branch)
                 (concat (propertize "Push " 'face 'magit-popup-heading)
                         (propertize it      'face 'magit-branch-local)
                         (propertize " to"   'face 'magit-popup-heading))))
             (?p magit--push-current-to-pushremote-desc
                 magit-push-current-to-pushremote)
             (?u magit--push-current-to-upstream-desc
                 magit-push-current-to-upstream)
             (?e "elsewhere\n"       magit-push-current)
             "Push"
             (?o "another branch"    magit-push-other)
             (?T "a tag"             magit-push-tag)
             (?r "explicit refspecs" magit-push-refspecs)
             (?t "all tags"          magit-push-tags)
             (?m "matching branches" magit-push-matching))
  :max-action-columns 2)

(defun magit-git-push (branch target args)
  (run-hooks 'magit-credential-hook)
  (pcase-let ((`(,remote . ,target)
               (magit-split-branch-name target)))
    (magit-run-git-async "push" "-v" args remote
                         (format "%s:refs/heads/%s" branch target))))

;;;###autoload
(defun magit-push-current-to-pushremote (args &optional push-remote)
  "Push the current branch to `branch.<name>.pushRemote'.
If that variable is unset, then push to `remote.pushDefault'.

When `magit-push-current-set-remote-if-missing' is non-nil and
the push-remote is not configured, then read the push-remote from
the user, set it, and then push to it.  With a prefix argument
the push-remote can be changed before pushed to it."
  (interactive
   (list (magit-push-arguments)
         (and (magit--push-current-set-pushremote-p current-prefix-arg)
              (magit-read-remote
               (if (eq magit-push-current-set-remote-if-missing 'default)
                   "Set `remote.pushDefault' and push there"
                 (format "Set `branch.%s.pushRemote' and push there"
                         (magit-get-current-branch)))))))
  (--if-let (magit-get-current-branch)
      (progn (when push-remote
               (setf (magit-get
                      (if (eq magit-push-current-set-remote-if-missing 'default)
                          "remote.pushDefault"
                        (format "branch.%s.pushRemote" it)))
                     push-remote))
             (if-let ((remote (magit-get-push-remote it)))
                 (if (member remote (magit-list-remotes))
                     (magit-git-push it (concat remote "/" it) args)
                   (user-error "Remote `%s' doesn't exist" remote))
               (user-error "No push-remote is configured for %s" it)))
    (user-error "No branch is checked out")))

(defun magit--push-current-set-pushremote-p (&optional change)
  (and (or change
           (and magit-push-current-set-remote-if-missing
                (not (magit-get-push-remote))))
       (magit-get-current-branch)))

(defun magit--push-current-to-pushremote-desc ()
  (--if-let (magit-get-push-branch)
      (concat (magit-branch-set-face it) "\n")
    (and (magit--push-current-set-pushremote-p)
         (concat
          (propertize (if (eq magit-push-current-set-remote-if-missing 'default)
                          "pushDefault"
                        "pushRemote")
                      'face 'bold)
          ", after setting that\n"))))

;;;###autoload
(defun magit-push-current-to-upstream (args &optional upstream)
  "Push the current branch to its upstream branch.

When `magit-push-current-set-remote-if-missing' is non-nil and
the upstream is not configured, then read the upstream from the
user, set it, and then push to it.  With a prefix argument the
upstream can be changed before pushed to it."
  (interactive
   (list (magit-push-arguments)
         (and (magit--push-current-set-upstream-p current-prefix-arg)
              (magit-read-upstream-branch))))
  (--if-let (magit-get-current-branch)
      (progn
        (when upstream
          (magit-set-upstream-branch it upstream))
        (if-let ((target (magit-get-upstream-branch it)))
            (magit-git-push it target args)
          (user-error "No upstream is configured for %s" it)))
    (user-error "No branch is checked out")))

(defun magit--push-current-set-upstream-p (&optional change)
  (and (or change
           (and magit-push-current-set-remote-if-missing
                (not (magit-get-upstream-branch))))
       (magit-get-current-branch)))

(defun magit--push-current-to-upstream-desc ()
  (--if-let (magit-get-upstream-branch)
      (concat (magit-branch-set-face it) "\n")
    (and (magit--push-current-set-upstream-p)
         (concat (propertize "@{upstream}" 'face 'bold)
                 ", after setting that\n"))))

;;;###autoload
(defun magit-push-current (target args)
  "Push the current branch to a branch read in the minibuffer."
  (interactive
   (--if-let (magit-get-current-branch)
       (list (magit-read-remote-branch (format "Push %s to" it)
                                       nil nil it 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (magit-git-push (magit-get-current-branch) target args))

;;;###autoload
(defun magit-push-other (source target args)
  "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer."
  (interactive
   (let ((source (magit-read-local-branch-or-commit "Push")))
     (list source
           (magit-read-remote-branch
            (format "Push %s to" source) nil
            (if (magit-local-branch-p source)
                (or (magit-get-push-branch source)
                    (magit-get-upstream-branch source))
              (and (magit-rev-ancestor-p source "HEAD")
                   (or (magit-get-push-branch)
                       (magit-get-upstream-branch))))
            source 'confirm)
           (magit-push-arguments))))
  (magit-git-push source target args))

(defvar magit-push-refspecs-history nil)

;;;###autoload
(defun magit-push-refspecs (remote refspecs args)
  "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used."
  (interactive
   (list (magit-read-remote "Push to remote")
         (split-string (magit-completing-read-multiple
                        "Push refspec,s"
                        (cons "HEAD" (magit-list-local-branch-names))
                        nil nil 'magit-push-refspecs-history)
                       crm-default-separator t)
         (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args remote refspecs))

;;;###autoload
(defun magit-push-matching (remote &optional args)
  "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation."
  (interactive (list (magit-read-remote "Push matching branches to" nil t)
                     (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args remote ":"))

;;;###autoload
(defun magit-push-tags (remote &optional args)
  "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default."
  (interactive (list (magit-read-remote "Push tags to remote" nil t)
                     (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" remote "--tags" args))

;;;###autoload
(defun magit-push-tag (tag remote &optional args)
  "Push a tag to another repository."
  (interactive
   (let  ((tag (magit-read-tag "Push tag")))
     (list tag (magit-read-remote (format "Push %s to remote" tag) nil t)
           (magit-push-arguments))))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" remote tag args))

;;;###autoload
(defun magit-push-implicitly (args)
  "Push somewhere without using an explicit refspec.

This command simply runs \"git push -v [ARGS]\".  ARGS are the
arguments specified in the popup buffer.  No explicit refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

To add this command to the push popup add this to your init file:

  (with-eval-after-load \\='magit-remote
    (magit-define-popup-action \\='magit-push-popup ?P
      \\='magit-push-implicitly--desc
      \\='magit-push-implicitly ?p t))

The function `magit-push-implicitly--desc' attempts to predict
what this command will do.  The value it returns is displayed in
the popup buffer."
  (interactive (list (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args))

(defun magit-push-implicitly--desc ()
  (let ((default (magit-get "push.default")))
    (unless (equal default "nothing")
      (or (when-let ((remote (or (magit-get-remote)
                                 (magit-remote-p "origin")))
                     (refspec (magit-get "remote" remote "push")))
            (format "%s using %s"
                    (propertize remote  'face 'magit-branch-remote)
                    (propertize refspec 'face 'bold)))
          (--when-let (and (not (magit-get-push-branch))
                           (magit-get-upstream-branch))
            (format "%s aka %s\n"
                    (magit-branch-set-face it)
                    (propertize "@{upstream}" 'face 'bold)))
          (--when-let (magit-get-push-branch)
            (format "%s aka %s\n"
                    (magit-branch-set-face it)
                    (propertize "pushRemote" 'face 'bold)))
          (--when-let (magit-get-@{push}-branch)
            (format "%s aka %s\n"
                    (magit-branch-set-face it)
                    (propertize "@{push}" 'face 'bold)))
          (format "using %s (%s is %s)\n"
                  (propertize "git push"     'face 'bold)
                  (propertize "push.default" 'face 'bold)
                  (propertize default        'face 'bold))))))

;;;###autoload
(defun magit-push-to-remote (remote args)
  "Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

To add this command to the push popup add this to your init file:

  (with-eval-after-load \\='magit-remote
    (magit-define-popup-action \\='magit-push-popup ?r
      \\='magit-push-to-remote--desc
      \\='magit-push-to-remote ?p t))"
  (interactive (list (magit-read-remote "Push to remote")
                     (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args remote))

(defun magit-push-to-remote--desc ()
  (format "using %s\n" (propertize "git push <remote>" 'face 'bold)))

;;; _
(provide 'magit-push)
;;; magit-push.el ends here
