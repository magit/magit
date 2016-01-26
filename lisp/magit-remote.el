;;; magit-remote.el --- transfer Git commits  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2016  The Magit Project Contributors
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

;; This library implements support for interacting with remote
;; repositories.  Commands for cloning, fetching, pulling, and
;; pushing are defined here.

;;; Code:

(require 'magit)

;;; Clone

(defcustom magit-clone-set-remote-head nil
  "Whether cloning creates the symbolic-ref `<remote>/HEAD'."
  :package-version '(magit . "2.4.2")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-clone-set-remote.pushDefault 'ask
  "Whether to set the value of `remote.pushDefault' after cloning.

If t, then set without asking.  If nil, then don't set.  If
`ask', then ask."
  :package-version '(magit . "2.4.0")
  :group 'magit-commands
  :type '(choice (const :tag "set" t)
                 (const :tag "ask" ask)
                 (const :tag "don't set" nil)))

;;;###autoload
(defun magit-clone (repository directory)
  "Clone the REPOSITORY to DIRECTORY.
Then show the status buffer for the new repository."
  (interactive
   (let  ((url (magit-read-string-ns "Clone repository")))
     (list url (read-directory-name
                "Clone to: " nil nil nil
                (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                     (match-string 1 url))))))
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (message "Cloning %s..." repository)
  (when (= (magit-call-git "clone" repository
                           ;; Stop cygwin git making a "c:" directory.
                           (magit-convert-git-filename directory))
           0)
    (when (or (eq  magit-clone-set-remote.pushDefault t)
              (and magit-clone-set-remote.pushDefault
                   (y-or-n-p "Set `remote.pushDefault' to \"origin\"? ")))
      (let ((default-directory directory))
        (magit-call-git "config" "remote.pushDefault" "origin")))
    (unless magit-clone-set-remote-head
      (magit-remote-unset-head "origin"))
    (message "Cloning %s...done" repository)
    (magit-status-internal directory)))

;;; Setup

(defcustom magit-remote-add-set-remote.pushDefault 'ask-if-unset
  "Whether to set the value of `remote.pushDefault' after adding a remote.

If `ask', then always ask.  If `ask-if-unset', then ask, but only
if the variable isn't set already.  If nil, then don't ever set.
If the value is a string, then set without asking, provided the
name of the name of the added remote is equal to that string and
the variable isn't already set."
  :package-version '(magit . "2.4.0")
  :group 'magit-commands
  :type '(choice (const  :tag "ask if unset" ask-if-unset)
                 (const  :tag "always ask" ask)
                 (string :tag "set if named")
                 (const  :tag "don't set")))

;;;###autoload (autoload 'magit-remote-popup "magit-remote" nil t)
(magit-define-popup magit-remote-popup
  "Popup console for remote commands."
  'magit-commands nil nil
  :man-page "git-remote"
  :actions  '((?a "Add"     magit-remote-add)
              (?r "Rename"  magit-remote-rename)
              (?k "Remove"  magit-remote-remove)
              (?u "Set url" magit-remote-set-url)))

(defun magit-read-url (prompt &optional initial-input)
  (let ((url (magit-read-string-ns prompt initial-input)))
    (if (string-prefix-p "~" url)
        (expand-file-name url)
      url)))

;;;###autoload
(defun magit-remote-add (remote url)
  "Add a remote named REMOTE and fetch it."
  (interactive (list (magit-read-string-ns "Remote name")
                     (magit-read-url "Remote url")))
  (if (pcase (list magit-remote-add-set-remote.pushDefault
                   (magit-get "remote.defaultPush"))
        (`(,(pred stringp) ,_) t)
        ((or `(ask ,_) `(ask-if-unset nil))
         (y-or-n-p (format "Set `remote.pushDefault' to \"%s\"? " remote))))
      (progn (magit-call-git "remote" "add" "-f" remote url)
             (magit-call-git "config" "remote.pushDefault" remote)
             (magit-refresh))
    (magit-run-git-async "remote" "add" "-f" remote url)))

;;;###autoload
(defun magit-remote-rename (old new)
  "Rename the remote named OLD to NEW."
  (interactive
   (let  ((remote (magit-read-remote "Rename remote")))
     (list remote (magit-read-string-ns (format "Rename %s to" remote)))))
  (unless (string= old new)
    (magit-run-git "remote" "rename" old new)))

;;;###autoload
(defun magit-remote-set-url (remote url)
  "Change the url of the remote named REMOTE to URL."
  (interactive
   (let  ((remote (magit-read-remote "Set url of remote")))
     (list remote (magit-read-url
                   "Url" (magit-get "remote" remote "url")))))
  (magit-run-git "remote" "set-url" remote url))

;;;###autoload
(defun magit-remote-remove (remote)
  "Delete the remote named REMOTE."
  (interactive (list (magit-read-remote "Delete remote")))
  (magit-run-git "remote" "rm" remote))

;;;###autoload
(defun magit-remote-set-head (remote &optional branch)
  "Set the local representation of REMOTE's default branch.
Query REMOTE and set the symbolic-ref refs/remotes/<remote>/HEAD
accordingly.  With a prefix argument query for the branch to be
used, which allows you to select an incorrect value if you fancy
doing that."
  (interactive
   (let  ((remote (magit-read-remote "Set HEAD for remote")))
     (list remote
           (and current-prefix-arg
                (magit-read-remote-branch (format "Set %s/HEAD to" remote)
                                          remote nil nil t)))))
  (magit-run-git "remote" "set-head" remote (or branch "--auto")))

;;;###autoload
(defun magit-remote-unset-head (remote)
  "Unset the local representation of REMOTE's default branch.
Delete the symbolic-ref \"refs/remotes/<remote>/HEAD\"."
  (interactive (list (magit-read-remote "Unset HEAD for remote")))
  (magit-run-git "remote" "set-head" remote "--delete"))

;;; Fetch

;;;###autoload (autoload 'magit-fetch-popup "magit-remote" nil t)
(magit-define-popup magit-fetch-popup
  "Popup console for fetch commands."
  'magit-commands
  :man-page "git-fetch"
  :switches '((?p "Prune deleted branches" "--prune"))
  :actions  '("Fetch from"
              (?p magit-get-push-remote    magit-fetch-from-pushremote)
              (?u magit-get-remote         magit-fetch-from-upstream)
              (?e "elsewhere"              magit-fetch)
              (?a "all remotes"            magit-fetch-all)
              "Fetch"
              (?m "submodules"             magit-submodule-fetch))
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
(defun magit-fetch (remote args)
  "Fetch from another repository."
  (interactive (list (magit-read-remote "Fetch remote")
                     (magit-fetch-arguments)))
  (magit-git-fetch remote args))

;;;###autoload
(defun magit-fetch-all (args)
  "Fetch from all remotes."
  (interactive (list (magit-fetch-arguments)))
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

;;; Pull

;;;###autoload (autoload 'magit-pull-popup "magit-remote" nil t)
(magit-define-popup magit-pull-popup
  "Popup console for pull commands."
  'magit-commands
  :man-page "git-pull"
  :variables '("Variables"
               (?r "branch.%s.rebase"
                   magit-cycle-branch*rebase
                   magit-pull-format-branch*rebase))
  :actions '((lambda ()
               (--if-let (magit-get-current-branch)
                   (concat
                    (propertize "Pull into " 'face 'magit-popup-heading)
                    (propertize it           'face 'magit-branch-local)
                    (propertize " from"      'face 'magit-popup-heading))
                 (propertize "Pull from" 'face 'magit-popup-heading)))
             (?p magit-get-push-branch     magit-pull-from-pushremote)
             (?u magit-get-upstream-branch magit-pull-from-upstream)
             (?e "elsewhere"               magit-pull))
  :default-action 'magit-pull
  :max-action-columns 1)

;;;###autoload (autoload 'magit-pull-and-fetch-popup "magit-remote" nil t)
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
  'magit-commands
  :man-page "git-pull"
  :variables '("Pull variables"
               (?r "branch.%s.rebase"
                   magit-cycle-branch*rebase
                   magit-pull-format-branch*rebase))
  :actions '((lambda ()
               (--if-let (magit-get-current-branch)
                   (concat
                    (propertize "Pull into " 'face 'magit-popup-heading)
                    (propertize it           'face 'magit-branch-local)
                    (propertize " from"      'face 'magit-popup-heading))
                 (propertize "Pull from" 'face 'magit-popup-heading)))
             (?p magit-get-push-branch     magit-pull-from-pushremote)
             (?u magit-get-upstream-branch magit-pull-from-upstream)
             (?e "elsewhere"               magit-pull)
             "Fetch from"
             (?f "remotes"           magit-fetch-all-no-prune)
             (?F "remotes and prune" magit-fetch-all-prune)
             "Fetch"
             (?m "submodules"        magit-submodule-fetch))
  :default-action 'magit-fetch
  :max-action-columns 1)

(defun magit-pull-format-branch*rebase ()
  (magit-popup-format-variable (format "branch.%s.rebase"
                                       (or (magit-get-current-branch) "<name>"))
                               '("true" "false")
                               "false" "pull.rebase"))

(defun magit-git-pull (source args)
  (run-hooks 'magit-credential-hook)
  (-let [(remote . branch)
         (magit-split-branch-name source)]
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
(defun magit-pull (source args)
  "Pull from a branch read in the minibuffer."
  (interactive (list (magit-read-remote-branch "Pull" nil nil nil t)
                     (magit-pull-arguments)))
  (magit-git-pull source args))

;;; Push

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
  :package-version '(magit . "2.4.0")
  :group 'magit-commands
  :type 'boolean)

;;;###autoload (autoload 'magit-push-popup "magit-remote" nil t)
(magit-define-popup magit-push-popup
  "Popup console for push commands."
  'magit-commands
  :man-page "git-push"
  :switches `((?f "Force"         "--force-with-lease")
              (?h "Disable hooks" "--no-verify")
              (?d "Dry run"       "--dry-run")
              ,@(and (not magit-push-current-set-remote-if-missing)
                     '((?u "Set upstream"  "--set-upstream"))))
  :actions '((lambda ()
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
             (?o "another branch"    magit-push)
             (?T "a tag"             magit-push-tag)
             (?m "matching branches" magit-push-matching)
             (?t "all tags"          magit-push-tags))
  :max-action-columns 2)

(defun magit-git-push (branch target args)
  (run-hooks 'magit-credential-hook)
  (-let [(remote . target)
         (magit-split-branch-name target)]
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
              (magit-read-remote (format "Set push-remote of %s and push there"
                                         (magit-get-current-branch))))))
  (--if-let (magit-get-current-branch)
      (progn (when push-remote
               (magit-call-git "config"
                               (format "branch.%s.pushRemote"
                                       (magit-get-current-branch))
                               push-remote))
             (-if-let (remote (magit-get-push-remote it))
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
         (concat (propertize "pushRemote" 'face 'bold)
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
          (magit-set-branch*merge/remote it upstream))
        (-if-let (target (magit-get-upstream-branch it))
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
(defun magit-push (source target args)
  "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer."
  (interactive
   (let ((source (magit-read-local-branch-or-commit "Push")))
     (list source
           (magit-read-remote-branch (format "Push %s to" source) nil
                                     (magit-get-upstream-branch source)
                                     source 'confirm)
           (magit-push-arguments))))
  (magit-git-push source target args))

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
      'magit-push-implicitly--desc
      'magit-push-implicitly ?p t))

The function `magit-push-implicitly--desc' attempts to predict
what this command will do, the value it returns is displayed in
the popup buffer."
  (interactive (list (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args))

(defun magit-push-implicitly--desc ()
  (let ((default (magit-get "push.default")))
    (unless (equal default "nothing")
      (or (-when-let* ((remote (or (magit-get-remote)
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
      'magit-push-to-remote--desc
      'magit-push-to-remote ?p t))"
  (interactive (list (magit-read-remote "Push to remote")
                     (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args remote))

(defun magit-push-to-remote--desc ()
  (format "using %s\n" (propertize "git push <remote>" 'face 'bold)))

;;; Email

;;;###autoload (autoload 'magit-patch-popup "magit-remote" nil t)
(magit-define-popup magit-patch-popup
  "Popup console for patch commands."
  'magit-commands
  :man-page "git-format-patch"
  :switches '("Switches for formatting patches"
              (?l "Add cover letter" "--cover-letter"))
  :options  '("Options for formatting patches"
              (?f "From"             "--from=")
              (?t "To"               "--to=")
              (?c "CC"               "--cc=")
              (?r "In reply to"      "--in-reply-to=")
              (?v "Reroll count"     "--reroll-count=")
              (?s "Thread style"     "--thread=")
              (?U "Context lines"    "-U")
              (?M "Detect renames"   "-M")
              (?C "Detect copies"    "-C")
              (?A "Diff algorithm"   "--diff-algorithm="
                  magit-diff-select-algorithm)
              (?o "Output directory" "--output-directory="))
  :actions  '((?p "Format patches"   magit-format-patch)
              (?r "Request pull"     magit-request-pull))
  :default-action 'magit-format-patch)

;;;###autoload
(defun magit-format-patch (range args)
  "Create patches for the commits in RANGE.
When a single commit is given for RANGE, create a patch for the
changes introduced by that commit (unlike 'git format-patch'
which creates patches for all commits that are reachable from
HEAD but not from the specified commit)."
  (interactive
   (list (-if-let (revs (magit-region-values 'commit))
             (concat (car (last revs)) "^.." (car revs))
           (let ((range (magit-read-range-or-commit "Format range or commit")))
             (if (string-match-p "\\.\\." range)
                 range
               (format "%s~..%s" range range))))
         (magit-patch-arguments)))
  (magit-call-git "format-patch" range args)
  (when (member "--cover-letter" args)
    (find-file
     (expand-file-name
      "0000-cover-letter.patch"
      (let ((topdir (magit-toplevel)))
        (or (--some (and (string-match "--output-directory=\\(.+\\)" it)
                         (expand-file-name (match-string 1 it) topdir))
                    args)
            topdir))))))

;;;###autoload
(defun magit-request-pull (url start end)
  "Request upstream to pull from you public repository.

URL is the url of your publically accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit."
  (interactive
   (list (magit-get "remote" (magit-read-remote "Remote") "url")
         (magit-read-branch-or-commit "Start" (magit-get-upstream-branch))
         (magit-read-branch-or-commit "End")))
  (let ((dir default-directory))
    ;; mu4e changes default-directory
    (compose-mail)
    (setq default-directory dir))
  (message-goto-body)
  (magit-git-insert "request-pull" start url end)
  (set-buffer-modified-p nil))

;;; magit-remote.el ends soon
(provide 'magit-remote)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-remote.el ends here
