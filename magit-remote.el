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
  :actions  '((?a "Add"     magit-remote-add)
              (?r "Rename"  magit-remote-rename)
              (?k "Remove"  magit-remote-remove)
              (?u "Set url" magit-remote-set-url)))

;;;###autoload
(defun magit-remote-add (remote url)
  "Add a remote named REMOTE and fetch it."
  (interactive (list (magit-read-string "Remote name")
                     (magit-read-string "Remote url")))
  (magit-run-git-async "remote" "add" "-f" remote url))

;;;###autoload
(defun magit-remote-rename (old new)
  "Rename the remote named OLD to NEW."
  (interactive
   (let  ((remote (magit-read-remote "Rename remote")))
     (list remote (magit-read-string (format "Rename %s to" remote)))))
  (unless (string= old new)
    (magit-run-git "remote" "rename" old new)))

;;;###autoload
(defun magit-remote-set-url (remote url)
  "Change the url of the remote named REMOTE to URL."
  (interactive
   (let  ((remote (magit-read-remote "Rename remote")))
     (list remote (magit-read-string "Url" (magit-get "remote" remote "url")))))
  (magit-set url "remote" remote "url"))

;;;###autoload
(defun magit-remote-remove (remote)
  "Delete the remote named REMOTE."
  (interactive (list (magit-read-remote "Delete remote")))
  (magit-run-git "remote" "rm" remote))

;;; Fetch

(magit-define-popup magit-fetch-popup
  "Popup console for fetch commands."
  'magit-popups
  :man-page "git-fetch"
  :switches '((?p "Prune"      "--prune"))
  :actions  '((?f "Current"    magit-fetch-current)
              (?o "Other"      magit-fetch)
              (?a "All"        magit-fetch-all)
              (?m "Submodules" magit-submodule-fetch))
  :default-action 'magit-fetch-current)

;;;###autoload
(defun magit-fetch-current (remote &optional args)
  "Fetch from the upstream repository of the current branch.
If `HEAD' is detached or if the upstream is not configured,
then read the remote."
  (interactive (list (or (magit-get-remote)
                         (magit-read-remote "Fetch remote"))
                     (magit-fetch-arguments)))
  (magit-run-git-async "fetch" remote args))

;;;###autoload
(defun magit-fetch (remote &optional args)
  "Fetch from another repository."
  (interactive (list (magit-read-remote "Fetch remote")
                     (magit-fetch-arguments)))
  (magit-run-git-async "fetch" remote args))

;;;###autoload
(defun magit-fetch-all (&optional args)
  "Fetch from another repository."
  (interactive (list (magit-fetch-arguments)))
  (magit-run-git-async "remote" "update" args))

;;; Pull

(magit-define-popup magit-pull-popup
  "Popup console for pull commands."
  'magit-popups
  :man-page "git-pull"
  :switches '((?r "Rebase" "--rebase"))
  :actions  '((?F "Current" magit-pull-current)
              (?o "Other"   magit-pull))
  :default-action 'magit-pull-current)

;;;###autoload
(defun magit-pull-current (remote branch &optional args)
  "Fetch from another repository and merge into current branch."
  (interactive (magit-pull-read-args t))
  (magit-run-git-async "pull" args remote branch))

;;;###autoload
(defun magit-pull (remote branch &optional args)
  "Fetch from another repository and merge a fetched branch."
  (interactive (magit-pull-read-args))
  (magit-run-git-async "pull" args remote branch))

(defun magit-pull-read-args (&optional use-upstream)
  (let ((remote (magit-get-remote-branch)))
    (unless (and use-upstream remote)
      (setq remote (magit-read-remote-branch "Pull" nil remote nil t)))
    (list (car remote) (cdr remote) (magit-pull-arguments))))

;;; Push

(magit-define-popup magit-push-popup
  "Popup console for push commands."
  'magit-popups
  :man-page "git-push"
  :switches '((?f "Force safely"  "--force-with-lease") ; >= 1.8.5
              (?F "Force"         "--force")
              (?h "Disable hooks" "--no-verify")
              (?d "Dry run"       "--dry-run")
              (?u "Set upstream"  "--set-upstream"))
  :actions  '((?P "Current"   magit-push-current)
              (?o "Other"     magit-push)
              (?e "Elsewhere" magit-push-elsewhere)
              (?m "Matching"  magit-push-matching)
              (?t "Tags"      magit-push-tags))
  :default-action 'magit-push-current)

;;;###autoload
(defun magit-push-current (branch remote &optional remote-branch args)
  "Push the current branch to its upstream branch.
If the upstream isn't set, then read the remote branch."
  (interactive (magit-push-read-args t t))
  (magit-push branch remote remote-branch args))

;;;###autoload
(defun magit-push (branch remote &optional remote-branch args)
  "Push a branch to its upstream branch.
If the upstream isn't set, then read the remote branch."
  (interactive (magit-push-read-args t))
  (magit-run-git-async "push" "-v" args remote
                       (if remote-branch
                           (format "%s:refs/heads/%s" branch remote-branch)
                         branch)))

;;;###autoload
(defun magit-push-elsewhere (branch remote remote-branch &optional args)
  "Push a branch or commit to some remote branch.
Read the local and remote branch."
  (interactive (magit-push-read-args))
  (magit-push branch remote remote-branch args))

(defun magit-push-read-args (&optional use-upstream use-current)
  (let* ((local (or (and use-current (magit-get-current-branch))
                    (magit-completing-read
                     "Push" (--if-let (magit-commit-at-point)
                                (cons it (magit-list-local-branch-names))
                              (magit-list-local-branch-names))
                     nil nil nil 'magit-revision-history
                     (magit-local-branch-at-point))
                    (user-error "Nothing selected")))
         (remote (and (magit-branch-p local)
                      (magit-get-remote-branch local))))
    (unless (and use-upstream remote)
      (setq remote (magit-read-remote-branch (format "Push %s to" local)
                                             nil remote local 'confirm)))
    (list local (car remote) (cdr remote) (magit-push-arguments))))

;;;###autoload
(defun magit-push-matching (remote &optional args)
  "Push all matching branches to another repository.
If multiple remotes exit, then read one from the user.
If just one exists, use that without requiring confirmation."
  (interactive (list (magit-read-remote "Push matching branches to" nil t)))
  (magit-run-git-async "push" "-v" args remote ":"))

;;;###autoload
(defun magit-push-tags (remote &optional args)
  "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default."
  (interactive (list (magit-read-remote "Push tags to remote" nil t)
                     (magit-push-arguments)))
  (magit-run-git-async "push" remote "--tags" args))

;;; magit-remote.el ends soon
(provide 'magit-remote)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-remote.el ends here
