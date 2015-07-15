;;; magit-remote.el --- transfer Git commits

;; Copyright (C) 2008-2015  The Magit Project Contributors
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

;;;###autoload (autoload 'magit-remote-popup "magit-remote" nil t)
(magit-define-popup magit-remote-popup
  "Popup console for remote commands."
  'magit-commands nil nil
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
   (let  ((remote (magit-read-remote "Set url of remote")))
     (list remote (magit-read-string "Url" (magit-get "remote" remote "url")))))
  (magit-set url "remote" remote "url"))

;;;###autoload
(defun magit-remote-remove (remote)
  "Delete the remote named REMOTE."
  (interactive (list (magit-read-remote "Delete remote")))
  (magit-run-git "remote" "rm" remote))

;;; Fetch

;;;###autoload (autoload 'magit-fetch-popup "magit-remote" nil t)
(magit-define-popup magit-fetch-popup
  "Popup console for fetch commands."
  'magit-commands
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
  (magit-run-git-async-no-revert "fetch" remote args))

;;;###autoload
(defun magit-fetch (remote &optional args)
  "Fetch from another repository."
  (interactive (list (magit-read-remote "Fetch remote")
                     (magit-fetch-arguments)))
  (magit-run-git-async-no-revert "fetch" remote args))

;;;###autoload
(defun magit-fetch-all (&optional args)
  "Fetch from all configured remotes."
  (interactive (list (magit-fetch-arguments)))
  (magit-run-git-async-no-revert "remote" "update" args))

;;; Pull

;;;###autoload (autoload 'magit-pull-popup "magit-remote" nil t)
(magit-define-popup magit-pull-popup
  "Popup console for pull commands."
  'magit-commands
  :man-page "git-pull"
  :switches '((?r "Rebase" "--rebase"))
  :actions  '((?F "Current" magit-pull-current)
              (?o "Other"   magit-pull))
  :default-action 'magit-pull-current)

;;;###autoload
(defun magit-pull-current (remote branch &optional args)
  "Fetch and merge into current branch."
  (interactive (magit-pull-read-args t))
  (magit-run-git-async "pull" args
                       (and (not (equal remote (magit-get-remote)))
                            (not (equal branch (magit-get-remote-branch)))
                            (list remote branch))))

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

;;;###autoload (autoload 'magit-push-popup "magit-remote" nil t)
(magit-define-popup magit-push-popup
  "Popup console for push commands."
  'magit-commands
  :man-page "git-push"
  :switches '((?f "Force"         "--force-with-lease")
              (?d "Dry run"       "--dry-run")
              (?h "Disable hooks" "--no-verify")
              nil
              (?u "Set upstream"  "--set-upstream")
              (?p "Set publish"   "++set-publish"))
  :actions  '(
              (?e "Explicit..."   magit-push-explicit)
              (?p "Publish"       magit-push-publish)
              (?r "Publish..."    magit-push-publish-another)
              nil
              (?t "Tip"           magit-push-tip)
              (?T "Tip..."        magit-push-tip-to)
              nil nil nil
              (?d "Default"       magit-push-default)
              (?u "Upstream"      magit-push-upstream)
              (?U "Upstream..."   magit-push-upstream-another)
              nil
              (?c "Current"       magit-push-current)
              (?C "Another..."    magit-push-another)
              nil
              (?u "Simple"        magit-push-simple)
              (?U "Simple..."     magit-push-simple-another)
              nil
              (?m "Matching"      magit-push-matching)
              (?M "Matching..."   magit-push-matching-to-remote)
              )
  :default-action 'magit-push-explicit
  :max-action-columns 3)

(defun magit-push-run (mode refspec* args)
  (magit-run-git-async-no-revert
   (and mode (list "-c" (format "push.default=%s" mode)))
   "push" "-v" args refspec*))

;;;; Publish

;;;###autoload
(defun magit-push-publish (args)
  (interactive (list (magit-push-arguments)))
  )

;;;###autoload
(defun magit-push-publish-another (branch args)
  (interactive (list (magit-read-another-local-branch "Publish")
                     (magit-push-arguments)))
  )

;;;; Explicit

;;;###autoload
(defun magit-push-explicit (remote refspec* args)
  (interactive (list (magit-read-remote "Push to remote")
                     (magit-read-string "Push refspec*")
                     (magit-push-arguments)))
  ;; git push REMOTE <branch>
  ;; git push REMOTE <branch>:<branchB>
  ;; git push REMOTE <branch>:refs/heads/<branchB>
  ;; git push REMOTE HEAD:<branch>
  ;; ...
  )

;;;###autoload
(defun magit-push-tip (remote args)
  "Push the current branch to the same name on a remote.

Run `git push <REMOTE> HEAD'."
  (interactive
   (-if-let (branch (magit-get-current-branch))
       (list (magit-read-remote (format "Push %s to remote" branch))
             (magit-push-arguments))
     (user-error "Cannot use this push variant, HEAD is detached")))
  (magit-push-run nil (list remote "HEAD") args))

;;;###autoload
(defun magit-push-tip-to (remote remote-branch args)
  "Push the current branch to the another name on a remote.

Run `git push <REMOTE> HEAD:<REMOTE-BRANCH>'."
  (interactive
   (-if-let (branch (magit-get-current-branch))
       (list (magit-read-remote (format "Push %s to remote" branch))
             (magit-read-string (format "Push %s to branch" branch))
             (magit-push-arguments))
     (user-error "Cannot use this push variant, HEAD is detached")))
  (magit-push-run nil (list remote (concat "HEAD:" remote-branch)) args))

;;;; Default

(defun magit-push-default (args)
  "Push according to the Git variable `push.default'.

Run `git push' without any refspec whatsoever."
  (interactive (list (magit-push-arguments)))
  (when (and (not (magit-get "push.default"))
             (version< (magit-git-version) "2.0"))
    (user-error (concat "Because you are using a Git version before v2.0, "
                        "you have to set `push.default', "
                        "to be able to use this command.")))
  (magit-push-run nil nil args))

;; TODO how does this pick the remote(s)?
;; TODO may this push the same branch to multiple remotes?
;;;###autoload
(defun magit-push-current (args)
  "Push the current branch to a branch with the same name.

Run `git -c push.default=current push'."
  (interactive (list (magit-push-arguments)))
  (unless (magit-get-current-branch)
    (user-error "Cannot use this push variant, HEAD is detached"))
  (magit-push-run "current" nil args))

;;;###autoload
(defun magit-push-another (branch args)
  "Read a branch and push it to a branch with the same name.

Conceptually this is like running:
  git checkout BRANCH
  git -c push.default=current push'
  git checkout -"
  (interactive (list (magit-read-another-local-branch "Push")
                     (magit-push-arguments)))
  ;; TODO (magit-push-run "current" branch args)
  )

;;;###autoload
(defun magit-push-upstream (args)
  "Push the current branch to its upstream.

Run `git -c push.default=upstream push'."
  (interactive (list (magit-push-arguments)))
  (unless (magit-get-current-branch)
    (user-error "Cannot use this push variant, HEAD is detached"))
  (magit-push-run "upstream" nil args))

;;;###autoload
(defun magit-push-upstream-another (branch args)
  "Read a BRANCH and push it to its upstream.

Conceptually this is like running:
  git checkout BRANCH
  git -c push.default=upstream push'
  git checkout -"
  (interactive (list (magit-read-another-local-branch "Push")
                     (magit-push-arguments)))
  (magit-push-run "upstream" branch args))

;;;###autoload
(defun magit-push-simple (args)
  "Push the current branch to its matching upstream.

Run `git -c push.default=upstream push'.

This is like `magit-push-upstream', but if the name of the
upstream branch doesn't match that of the local branch then
this refuses to push."
  (interactive (list (magit-push-arguments)))
  (unless (magit-get-current-branch)
    (user-error "Cannot use this push variant, HEAD is detached"))
  (magit-push-run "simple" nil args))

;;;###autoload
(defun magit-push-simple-another (branch args)
  "Read a BRANCH and push it to its matching upstream.

Conceptually this is like running:
  git checkout BRANCH
  git -c push.default=simple push'
  git checkout -

This is like `magit-push-upstream-another', but if the name
of the upstream branch doesn't match that of the local branch
then this refuses to push."
  (interactive (list (magit-read-another-local-branch "Push")
                     (magit-push-arguments)))
  (magit-push-run "simple" branch args))

;;; Matching

;; TODO Is this equivalent to `git push :'
;;;###autoload
(defun magit-push-matching (args)
  "Push all branches having the same name on both ends.

Run `git -c push.default=matching push'."
  (interactive (list (magit-read-remote "Push matching branches to" nil t)))
  (magit-push-run "matching" nil args))

;;;###autoload
(defun magit-push-matching-to-remote (remote args)
  "Read a REMOTE and push all matching branches to that remote.

Run `git push REMOTE :'."
  (interactive (list (magit-read-remote "Push matching branches to")
                     (magit-push-arguments)))
  (magit-push-run nil (list remote ":") args))

;;;; Tags

;;;###autoload
(defun magit-push-tags (remote &optional args)
  "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default."
  (interactive (list (magit-read-remote "Push tags to remote" nil t)
                     (magit-push-arguments)))
  (magit-push-run nil remote (cons "--tags" args)))

;;;###autoload
(defun magit-push-tag (tag remote &optional args)
  "Push a tag to another repository."
  (interactive
   (let  ((tag (magit-read-tag "Push tag")))
     (list tag (magit-read-remote (format "Push %s to remote" tag) nil t))))
  (magit-push-run nil (list remote tag) args))

;;; Email

;;;###autoload (autoload 'magit-patch-popup "magit-remote" nil t)
(magit-define-popup magit-patch-popup
  "Popup console for patch commands."
  'magit-commands
  :man-page "git-format-patch"
  :options  '((?f "From"             "--from=")
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
  "Create patches for the commits in RANGE."
  (interactive
   (list (-if-let (revs (magit-region-values 'commit))
             (concat (car (last revs)) "^.." (car revs))
           (let ((range (magit-read-range-or-commit "Format range or commit")))
             (if (string-match-p "\\.\\." range)
                 range
               (format "%s~..%s" range range))))
         (magit-patch-arguments)))
  (magit-run-git-no-revert "format-patch" range args))

;;;###autoload
(defun magit-request-pull (url start end)
  "Request upstream to pull from you public repository.

URL is the url of your publically accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit."
  (interactive
   (list (magit-get "remote" (magit-read-remote "Remote") "url")
         (magit-read-branch-or-commit "Start" (magit-get-tracked-branch))
         (magit-read-branch-or-commit "End")))
  (let ((dir default-directory))
    ;; mu4e changes default-directory
    (compose-mail)
    (setq default-directory dir))
  (message-goto-body)
  (let ((inhibit-magit-revert t))
    (magit-git-insert "request-pull" start url))
  (set-buffer-modified-p nil))

;;; magit-remote.el ends soon
(provide 'magit-remote)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-remote.el ends here
