;;; magit-remote.el --- transfer Git commits

;; Copyright (C) 2008-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

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

;;; Options

(defcustom magit-set-upstream-on-push nil
  "Whether `magit-push' may set upstream when pushing a branch.
This only applies if the branch does not have an upstream set yet.

nil        don't use --set-upstream.
t          ask if --set-upstream should be used.
`dontask'  always use --set-upstream.
`refuse'   refuse to push unless a remote branch has already been set."
  :group 'magit-commands
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Ask if not set" askifnotset)
                 (const :tag "Refuse" refuse)
                 (const :tag "Always" dontask)))

;;; Clone

;;;###autoload
(defun magit-clone (repository directory)
  "Clone the REPOSITORY to DIRECTORY."
  (interactive
   (let  ((url (magit-read-string "Clone repository")))
     (list url (read-directory-name
                "Clone to: " nil nil nil
                (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                     (match-string 1 url))))))
  (make-directory directory t)
  (magit-run-git "clone" repository directory))

;;; Setup

(magit-define-popup magit-remote-popup
  "Popup console for remote commands."
  'magit-popups
  :man-page "git-remote"
  :actions  '((?a "Add"    magit-remote-add)
              (?r "Rename" magit-remote-rename)
              (?k "Remove" magit-remote-remove)))

;;;###autoload
(defun magit-remote-add (remote url)
  "Add the REMOTE and fetch it.
\n(git remote add -f REMOTE URL)."
  (interactive (list (magit-read-string "Remote name")
                     (magit-read-string "Remote url")))
  (magit-run-git-async "remote" "add" "-f" remote url))

;;;###autoload
(defun magit-remote-remove (remote)
  "Delete the REMOTE.
\n(git remote rm REMOTE)."
  (interactive (list (magit-read-remote "Delete remote")))
  (magit-run-git "remote" "rm" remote))

;;;###autoload
(defun magit-remote-rename (old new)
  "Rename remote OLD to NEW.
\n(git remote rename OLD NEW)."
  (interactive
   (let  ((remote (magit-read-remote "Rename remote")))
     (list remote (magit-read-string (format "Rename remote '%s' to" remote)))))
  (unless (string= old new)
    (magit-run-git "remote" "rename" old new)))

;;; Fetch

(magit-define-popup magit-fetch-popup
  "Popup console for fetch commands."
  'magit-popups
  :man-page "git-fetch"
  :switches '((?p "Prune"   "--prune"))
  :actions  '((?f "Current" magit-fetch-current)
              (?o "Other"   magit-fetch)
              (?a "All"     magit-remote-update))
  :default-action 'magit-fetch-current)

;;;###autoload
(defun magit-fetch (remote &optional args)
  "Fetch from REMOTE."
  (interactive (list (magit-read-remote "Fetch remote")
                     magit-current-popup-args))
  (magit-run-git-async "fetch" remote args))

;;;###autoload
(defun magit-fetch-current (&optional args)
  "Fetch for the default remote.
If there is no default remote, ask for one."
  (interactive (list magit-current-popup-args))
  (magit-fetch (or (magit-get-current-remote)
                   (magit-read-remote "Fetch remote"))
               args))

;;;###autoload
(defun magit-remote-update ()
  "Update all remotes."
  (interactive)
  (magit-run-git-async "remote" "update" magit-current-popup-args))

;;; Pull

(magit-define-popup magit-pull-popup
  "Popup console for pull commands."
  'magit-popups
  :man-page "git-pull"
  :switches '((?f "Force"  "--force")
              (?r "Rebase" "--rebase"))
  :actions  '((?F "Pull"   magit-pull))
  :default-action 'magit-pull)

;;;###autoload
(defun magit-pull ()
  "Pull changes from a remote repository.

If there is no default remote, the user is prompted for one and
the chosen values is saved.  If there is no default merge branch,
the user is prompted for one and the chosen values is saved.

With a prefix argument, the default remote is not used and the
user is prompted for a remote.  With two prefix arguments, the
default merge branch is not used and the user is prompted for
a merge branch.  Values entered by the user because of prefix
arguments are not saved."
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (branch-remote (magit-get-current-remote))
         (branch-merge (magit-get "branch" branch "merge"))
         (branch-merge-name (and branch-merge
                                 (save-match-data
                                   (string-match "^refs/heads/\\(.+\\)"
                                                 branch-merge)
                                   (match-string 1 branch-merge))))
         (choose-remote (>= (prefix-numeric-value current-prefix-arg) 4))
         (choose-branch (>= (prefix-numeric-value current-prefix-arg) 16))
         (remote-needed (or choose-remote
                            (not branch-remote)))
         (branch-needed (or choose-branch
                            (not branch-merge-name)))
         (chosen-branch-remote
          (if remote-needed
              (magit-read-remote "Pull from remote" branch-remote)
            branch-remote))
         (chosen-branch-merge-name
          (if branch-needed
              (magit-read-remote-branch (format "Pull branch from remote %s"
                                                chosen-branch-remote)
                                        chosen-branch-remote)
            branch-merge-name)))
    (when (and (not branch-remote)
               (not choose-remote))
      (magit-set chosen-branch-remote "branch" branch "remote"))
    (when (and (not branch-merge-name)
               (not choose-branch))
      (magit-set (format "%s" chosen-branch-merge-name)
                 "branch" branch "merge"))
    (magit-run-git-async
     "pull" magit-current-popup-args
     (and choose-remote chosen-branch-remote)
     (and (or choose-remote choose-branch)
          (list (format "refs/heads/%s:refs/remotes/%s/%s"
                        chosen-branch-merge-name
                        chosen-branch-remote
                        chosen-branch-merge-name))))))

;;; Push

(magit-define-popup magit-push-popup
  "Popup console for push commands."
  'magit-popups
  :man-page "git-push"
  :switches '((?f "Force"         "--force")
              (?h "Disable hooks" "--no-verify")
              (?d "Dry run"       "-n")
              (?u "Set upstream"  "-u"))
  :actions  '((?P "Push"          magit-push)
              (?t "Push tags"     magit-push-tags))
  :default-action 'magit-push)

;;;###autoload
(defun magit-push-tags (remote &optional args)
  "Push all tags to a remote repository.
If only one remote exists, push to that.  Otherwise prompt for a
remote, offering the remote configured for the current branch as
default."
  (interactive (list (let ((remotes (magit-list-remotes)))
                       (if (= (length remotes) 1)
                           (car remotes)
                         (magit-read-remote "Push tags to remote")))
                     magit-current-popup-args))
  (magit-run-git-async "push" remote "--tags" args))

;;;###autoload
(defun magit-push (arg)
  "Push the current branch to a remote repository.

With a single prefix argument ask the user what remote to push
to.  With two or more prefix arguments also ask the user the
name of the remote branch to push to.

Otherwise use the remote and branch as configured using the
Git variables `branch.<name>.remote' and `branch.<name>.merge'.
If the former is undefined ask the user.  If the latter is
undefined push without specifing the remote branch explicitly.

Also see option `magit-set-upstream-on-push'."
  (interactive "P")
  (let* ((branch (or (magit-get-current-branch)
                     (user-error "Don't push a detached head.  That's gross")))
         (auto-remote (magit-get-current-remote))
         (used-remote (if (or arg (not auto-remote))
                          (magit-read-remote
                           (format "Push %s to remote" branch) auto-remote)
                        auto-remote))
         (auto-branch (and (equal used-remote auto-remote)
                           (magit-get "branch" branch "merge")))
         (used-branch (if (>= (prefix-numeric-value arg) 16)
                          (magit-read-remote-branch
                           (format "Push %s as branch" branch)
                           used-remote auto-branch)
                        auto-branch)))
    (cond ;; Pushing to what's already configured.
          ((and auto-branch
                (equal auto-branch used-branch)
                (equal auto-remote used-remote)))
          ;; Setting upstream because of magit-current-popup-args.
          ((member "-u" magit-current-popup-args))
          ;; Two prefix arguments; ignore magit-set-upstream-on-push.
          ((>= (prefix-numeric-value arg) 16)
           (and (yes-or-no-p "Set upstream while pushing? ")
                (setq magit-current-popup-args
                      (cons "-u" magit-current-popup-args))))
          ;; Else honor magit-set-upstream-on-push.
          ((eq magit-set-upstream-on-push 'refuse)
           (user-error "Not pushing since no upstream has been set."))
          ((or (eq magit-set-upstream-on-push 'dontask)
               (and (eq magit-set-upstream-on-push t)
                    (yes-or-no-p "Set upstream while pushing? ")))
           (setq magit-current-popup-args (cons "-u" magit-current-popup-args))))
    (magit-run-git-async
     "push" "-v" used-remote
     (if used-branch (format "%s:%s" branch used-branch) branch)
     magit-current-popup-args)))

;;; magit-remote.el ends soon
(provide 'magit-remote)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-remote.el ends here
