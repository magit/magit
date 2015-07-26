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
     (list url (file-name-as-directory
                (expand-file-name
                 (read-directory-name
                  "Clone to: " nil nil nil
                  (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                       (match-string 1 url))))))))
  (make-directory directory t)
  (message "Cloning %s..." repository)
  (magit-call-git "clone" repository directory)
  (message "Cloning %s...done" repository)
  (magit-status-internal directory))

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
              (?h "Disable hooks" "--no-verify")
              (?d "Dry run"       "--dry-run")
              (?u "Set upstream"  "--set-upstream"))
  :actions  '((?P "Current"    magit-push-current)
              (?q "Quickly"    magit-push-quickly)
              (?t "Tags"       magit-push-tags)
              (?o "Other"      magit-push)
              (?i "Implicitly" magit-push-implicitly)
              (?T "Tag"        magit-push-tag)
              (?e "Elsewhere"  magit-push-elsewhere)
              (?m "Matching"   magit-push-matching))
  :default-action 'magit-push-current
  :max-action-columns 3)

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
  (magit-run-git-async-no-revert
   "push" "-v" args remote
   (if remote-branch
       (format "%s:refs/heads/%s" branch remote-branch)
     branch)))

;;;###autoload
(defun magit-push-elsewhere (branch remote remote-branch &optional args)
  "Push a branch or commit to some remote branch.
Read the local and remote branch."
  (interactive (magit-push-read-args nil nil t))
  (magit-push branch remote remote-branch args))

(defcustom magit-push-always-verify 'nag
  "Whether certain commands require verification before pushing.

Starting with v2.1.0 some of the push commands are supposed to
push to the configured upstream branch without requiring user
confirmation or offering to push somewhere else.

This has taken a few users by surprise, and they suggested that
we force users to opt-in to this behavior.  Unfortunately adding
this option means that now other users will complain about us
needlessly forcing them to set an option.  But what can you do?
\(I am not making this up.  This has happened in the past and it
is very frustrating.)

You should set the value of this option to nil, causing all push
commands to behave as intended:

`PP' Push the current branch to its upstream branch, no questions
     asked.  If no upstream branch is configured or if that is
     another local branch, then prompt for the remote and branch
     to push to.

`Po' Push another local branch (not the current branch) to its
     upstream branch.  If no upstream branch is configured or if
     that is another local branch, then prompt for the remote and
     branch to push to.

`Pe' Push any local branch to any remote branch.  This command
     isn't affected by this option.  It always asks which branch
     should be pushed (defaulting to the current branch) and then
     where that should be pushed (defaulting to the upstream
     branch of the previously selected branch).

There are other push commands besides these.  You should read
their doc-strings instead of blindly trying them out and then
being surprised if it turns out that they do something different
from what you expected.  For example inside the push popup type
`?i' to learn what \"implicitly\" means here.  Or to learn about
all push commands at once, consult the manual.

While I have your attention, I would also like to warn you that
pushing will be further improved in a later release (probably
v2.3.0), and that you might be surprised by some of these
changes, unless you read the documentation.

Setting this option to t makes little sense.  If you consider
doing that, then you should probably just use `Pe' instead of
`PP' or `Po'."
  :package-version '(magit . "2.2.0")
  :group 'magit-commands
  :type '(choice (const :tag "require verification and mention this option" nag)
                 (const :tag "require verification" t)
                 (const :tag "don't require verification" nil)))

(defun magit-push-read-args (&optional use-upstream use-current default-current)
  (let* ((current (magit-get-current-branch))
         (local (or (and use-current current)
                    (magit-completing-read
                     "Push" (--if-let (magit-commit-at-point)
                                (cons it (magit-list-local-branch-names))
                              (magit-list-local-branch-names))
                     nil nil nil 'magit-revision-history
                     (or (and default-current current)
                         (magit-local-branch-at-point)
                         (magit-commit-at-point)))
                    (user-error "Nothing selected")))
         (remote (and (magit-branch-p local)
                      (magit-get-remote-branch local))))
    (unless (and use-upstream remote (not magit-push-always-verify))
      (setq remote (magit-read-remote-branch
                    (concat
                     (format "Push %s to" local)
                     (and use-upstream remote
                          (eq magit-push-always-verify 'nag)
                          " [also see option magit-push-always-verify]"))
                    nil remote local 'confirm)))
    (list local (car remote) (cdr remote) (magit-push-arguments))))

;;;###autoload
(defun magit-push-quickly (&optional args)
  "Push the current branch to some remote.
When the Git variable `magit.pushRemote' is set, then push to
that remote.  If that variable is undefined or the remote does
not exist, then push to \"origin\".  If that also doesn't exist
then raise an error.  The local branch is pushed to the remote
branch with the same name."
  (interactive (list (magit-push-arguments)))
  (-if-let (branch (magit-get-current-branch))
      (-if-let (remote (or (magit-remote-p (magit-get "magit.pushRemote"))
                           (magit-remote-p "origin")))
          (magit-run-git-async-no-revert "push" "-v" args remote branch)
        (user-error "Cannot determine remote to push to"))
    (user-error "No branch is checked out")))

;;;###autoload
(defun magit-push-implicitly (&optional args)
  "Push without explicitly specifing what to push.
This runs `git push -v'.  What is being pushed depends on various
Git variables as described in the `git-push(1)' and `git-config(1)'
manpages."
  (interactive (list (magit-push-arguments)))
  (magit-run-git-async-no-revert "push" "-v" args))

;;;###autoload
(defun magit-push-matching (remote &optional args)
  "Push all matching branches to another repository.
If multiple remotes exit, then read one from the user.
If just one exists, use that without requiring confirmation."
  (interactive (list (magit-read-remote "Push matching branches to" nil t)))
  (magit-run-git-async-no-revert "push" "-v" args remote ":"))

(defun magit-push-tags (remote &optional args)
  "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default."
  (interactive (list (magit-read-remote "Push tags to remote" nil t)
                     (magit-push-arguments)))
  (magit-run-git-async-no-revert "push" remote "--tags" args))

;;;###autoload
(defun magit-push-tag (tag remote &optional args)
  "Push a tag to another repository."
  (interactive
   (let  ((tag (magit-read-tag "Push tag")))
     (list tag (magit-read-remote (format "Push %s to remote" tag) nil t))))
  (magit-run-git-async-no-revert "push" remote tag))


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
