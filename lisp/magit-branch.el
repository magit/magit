;;; magit-branch.el --- branch support  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
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

;; This library implements support for branches.  It defines popups
;; and commands for creating, checking out, manipulating, and
;; configuring branches.  Commands defined here are mainly concerned
;; with branches as pointers, commands that deal with what a branch
;; points at, are defined elsewhere.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-branch-read-upstream-first t
  "Whether to read upstream before name of new branch when creating a branch.

`nil'      Read the branch name first.
`t'        Read the upstream first.
`fallback' Read the upstream first, but if it turns out that the chosen
           value is not a valid upstream (because it cannot be resolved
           as an existing revision), then treat it as the name of the
           new branch and continue by reading the upstream next."
  :package-version '(magit . "2.2.0")
  :group 'magit-commands
  :type '(choice (const :tag "read branch name first" nil)
                 (const :tag "read upstream first" t)
                 (const :tag "read upstream first, with fallback" fallback)))

(defcustom magit-branch-prefer-remote-upstream nil
  "Whether to favor remote upstreams when creating new branches.

When a new branch is created, Magit offers the branch, commit, or
stash as the default starting point of the new branch.  If there
is no such thing at point, then it falls back to offer the
current branch as starting-point.  The user may then accept that
default or pick something else.

If the chosen starting-point is a branch, then it may also be set
as the upstream of the new branch, depending on the value of the
Git variable `branch.autoSetupMerge'.  By default this is done
for remote branches, but not for local branches.

You might prefer to always use some remote branch as upstream.
If the chosen starting-point is (1) a local branch, (2) whose
name matches a member of the value of this option, (3) the
upstream of that local branch is a remote branch with the same
name, and (4) that remote branch can be fast-forwarded to the
local branch, then the chosen branch is used as starting-point,
but its own upstream is used as the upstream of the new branch.

Members of this option's value are treated as branch names that
have to match exactly unless they contain a character that makes
them invalid as a branch name.  Recommended characters to use
to trigger interpretation as a regexp are \"*\" and \"^\".  Some
other characters which you might expect to be invalid, actually
are not, e.g. \".+$\" are all perfectly valid.  More precisely,
if `git check-ref-format --branch STRING' exits with a non-zero
status, then treat STRING as a regexp.

Assuming the chosen branch matches these conditions you would end
up with with e.g.:

  feature --upstream--> origin/master

instead of

  feature --upstream--> master --upstream--> origin/master

Which you prefer is a matter of personal preference.  If you do
prefer the former, then you should add branches such as \"master\",
\"next\", and \"maint\" to the value of this options."
  :package-version '(magit . "2.4.0")
  :group 'magit-commands
  :type '(repeat string))

(defcustom magit-branch-adjust-remote-upstream-alist nil
  "Alist of upstreams to be used when branching from remote branches.

When creating a local branch from an ephemeral branch located
on a remote, e.g. a feature or hotfix branch, then that remote
branch should usually not be used as the upstream branch, since
the push-remote already allows accessing it and having both the
upstream and the push-remote reference the same related branch
would be wasteful.  Instead a branch like \"maint\" or \"master\"
should be used as the upstream.

This option allows specifing the branch that should be used as
the upstream when branching certain remote branches.  The value
is an alist of the form ((UPSTREAM . RULE)...).  The first
matching element is used, the following elements are ignored.

UPSTREAM is the branch to be used as the upstream for branches
specified by RULE.  It can be a local or a remote branch.

RULE can either be a regular expression, matching branches whose
upstream should be the one specified by UPSTREAM.  Or it can be
a list of the only branches that should *not* use UPSTREAM; all
other branches will.  Matching is done after stripping the remote
part of the name of the branch that is being branched from.

If you use a finite set of non-ephemeral branches across all your
repositories, then you might use something like:

  ((\"origin/master\" \"master\" \"next\" \"maint\"))

Or if the names of all your ephemeral branches contain a slash,
at least in some repositories, then a good value could be:

  ((\"origin/master\" . \"/\"))

Of course you can also fine-tune:

  ((\"origin/maint\" . \"\\`hotfix/\")
   (\"origin/master\" . \"\\`feature/\"))"
  :package-version '(magit . "2.9.0")
  :group 'magit-commands
  :type '(repeat (cons (string :tag "Use upstream")
                       (choice :tag "for branches"
                               (regexp :tag "matching")
                               (repeat :tag "except"
                                       (string :tag "branch"))))))

(defcustom magit-branch-popup-show-variables t
  "Whether the `magit-branch-popup' shows Git variables.
This defaults to t to avoid changing key bindings.  When set to
nil, no variables are displayed directly in this popup, instead
the sub-popup `magit-branch-config-popup' has to be used to view
and change branch related variables."
  :package-version '(magit . "2.7.0")
  :group 'magit-commands
  :type 'boolean)

;;; Branch Popup

;;;###autoload (autoload 'magit-branch-popup "magit" nil t)
(magit-define-popup magit-branch-popup
  "Popup console for branch commands."
  :man-page "git-branch"
  :actions '((?b "Checkout"              magit-checkout)
             (?n "Create new branch"     magit-branch)
             (?C "Configure..."          magit-branch-config-popup)
             (?c "Checkout new branch"   magit-branch-and-checkout)
             (?s "Create new spin-off"   magit-branch-spinoff)
             (?m "Rename"                magit-branch-rename)
             (?w "Checkout new worktree" magit-worktree-checkout)
             (?W "Create new worktree"   magit-worktree-branch)
             (?x "Reset"                 magit-branch-reset) nil nil
             (?k "Delete"                magit-branch-delete))
  :default-action 'magit-checkout
  :max-action-columns 3
  :setup-function 'magit-branch-popup-setup)

(defvar magit-branch-config-variables)

(defun magit-branch-popup-setup (val def)
  (magit-popup-default-setup val def)
  (when magit-branch-popup-show-variables
    (magit-popup-put :variables (magit-popup-convert-variables
                                 val magit-branch-config-variables))
    (use-local-map (copy-keymap magit-popup-mode-map))
    (dolist (ev (-filter #'magit-popup-event-p (magit-popup-get :variables)))
      (local-set-key (vector (magit-popup-event-key ev))
                     'magit-invoke-popup-action))))

;;; Branch Commands

;;;###autoload
(defun magit-checkout (revision)
  "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch, then that becomes the current
branch.  If it is something else, then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.
\n(git checkout REVISION)."
  (interactive (list (magit-read-other-branch-or-commit "Checkout")))
  (when (string-match "\\`heads/\\(.+\\)" revision)
    (setq revision (match-string 1 revision)))
  (magit-run-git "checkout" revision))

;;;###autoload
(defun magit-branch (branch start-point &optional args)
  "Create BRANCH at branch or revision START-POINT.
\n(git branch [ARGS] BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create branch"))
  (magit-call-git "branch" args branch start-point)
  (magit-branch-maybe-adjust-upstream branch start-point)
  (magit-refresh))

;;;###autoload
(defun magit-branch-and-checkout (branch start-point &optional args)
  "Create and checkout BRANCH at branch or revision START-POINT.
\n(git checkout [ARGS] -b BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create and checkout branch"))
  (if (string-match-p "^stash@{[0-9]+}$" start-point)
      (magit-run-git "stash" "branch" branch start-point)
    (magit-call-git "checkout" args "-b" branch start-point)
    (magit-branch-maybe-adjust-upstream branch start-point)
    (magit-refresh)))

(defun magit-branch-maybe-adjust-upstream (branch start-point)
  (--when-let
      (or (and (magit-get-upstream-branch branch)
               (magit-get-indirect-upstream-branch start-point))
          (and (magit-remote-branch-p start-point)
               (let ((name (cdr (magit-split-branch-name start-point))))
                 (car (--first (if (listp (cdr it))
                                   (not (member name (cdr it)))
                                 (string-match-p (cdr it) name))
                               magit-branch-adjust-remote-upstream-alist)))))
    (magit-call-git "branch" (concat "--set-upstream-to=" it) branch)))

;;;###autoload
(defun magit-branch-orphan (branch start-point &optional args)
  "Create and checkout an orphan BRANCH with contents from revision START-POINT.
\n(git checkout --orphan [ARGS] BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create and checkout orphan branch"))
  (magit-run-git "checkout" "--orphan" args branch start-point))

(defun magit-branch-read-args (prompt)
  (let ((args (magit-branch-arguments)))
    (if magit-branch-read-upstream-first
        (let* ((default (and (or (memq this-command magit-no-confirm-default)
                                 (memq magit-current-popup-action
                                       magit-no-confirm-default))
                             (magit--default-starting-point)))
               (choice (or default
                           (magit-read-starting-point prompt))))
          (if (magit-rev-verify choice)
              (list (magit-read-string-ns
                     (if default
                         (format "%s (starting at %s)" prompt choice)
                       "Branch name")
                     (let ((def (mapconcat #'identity
                                           (cdr (split-string choice "/"))
                                           "/")))
                       (and (member choice (magit-list-remote-branch-names))
                            (not (member def (magit-list-local-branch-names)))
                            def)))
                    choice args)
            (if (eq magit-branch-read-upstream-first 'fallback)
                (list choice
                      (magit-read-starting-point (concat prompt " " choice))
                      args)
              (user-error "Not a valid starting-point: %s" choice))))
      (let ((branch (magit-read-string-ns (concat prompt " named"))))
        (list branch
              (magit-read-starting-point (concat prompt " " branch))
              args)))))

;;;###autoload
(defun magit-branch-spinoff (branch &optional from &rest args)
  "Create new branch from the unpushed commits.

Create and checkout a new branch starting at and tracking the
current branch.  That branch in turn is reset to the last commit
it shares with its upstream.  If the current branch has no
upstream or no unpushed commits, then the new branch is created
anyway and the previously current branch is not touched.

This is useful to create a feature branch after work has already
began on the old branch (likely but not necessarily \"master\").

If the current branch is a member of the value of option
`magit-branch-prefer-remote-upstream' (which see), then the
current branch will be used as the starting point as usual, but
the upstream of the starting-point may be used as the upstream
of the new branch, instead of the starting-point itself.

If optional FROM is non-nil, then the source branch is reset
to `FROM~', instead of to the last commit it shares with its
upstream.  Interactively, FROM is only ever non-nil, if the
region selects some commits, and among those commits, FROM is
the commit that is the fewest commits ahead of the source
branch.  (It not yet possible to spin off a single commit,
unless it is the only unpushed commit.  See #2920.)

The commit at the other end of the selection actually does not
matter, all commits between FROM and `HEAD' are moved to the new
branch.  If FROM is not reachable from `HEAD' or is reachable
from the source branch's upstream, then an error is raised."
  (interactive (list (magit-read-string "Spin off branch")
                     (car (last (magit-region-values 'commit)))
                     (magit-branch-arguments)))
  (when (magit-branch-p branch)
    (user-error "Cannot spin off %s.  It already exists" branch))
  (-if-let (current (magit-get-current-branch))
      (let ((tracked (magit-get-upstream-branch current))
            base)
        (when from
          (unless (magit-rev-ancestor-p from current)
            (user-error "Cannot spin off %s.  %s is not reachable from %s"
                        branch from current))
          (when (and tracked
                     (magit-rev-ancestor-p from tracked))
            (user-error "Cannot spin off %s.  %s is ancestor of upstream %s"
                        branch from tracked)))
        (magit-call-git "checkout" args "-b" branch current)
        (--when-let (magit-get-indirect-upstream-branch current)
          (magit-call-git "branch" "--set-upstream-to" it branch))
        (when (and tracked
                   (setq base
                         (if from
                             (concat from "^")
                           (magit-git-string "merge-base" current tracked)))
                   (not (magit-rev-eq base current)))
          (magit-call-git "update-ref" "-m"
                          (format "reset: moving to %s" base)
                          (concat "refs/heads/" current) base))
        (magit-refresh))
    (magit-run-git "checkout" "-b" branch)))

;;;###autoload
(defun magit-branch-reset (branch to &optional args set-upstream)
  "Reset a branch to the tip of another branch or any other commit.

When the branch being reset is the current branch, then do a
hard reset.  If there are any uncommitted changes, then the user
has to confirm the reset because those changes would be lost.

This is useful when you have started work on a feature branch but
realize it's all crap and want to start over.

When resetting to another branch and a prefix argument is used,
then also set the target branch as the upstream of the branch
that is being reset."
  (interactive
   (let* ((atpoint (magit-branch-at-point))
          (branch  (magit-read-local-branch "Reset branch" atpoint)))
     (list branch
           (magit-completing-read (format "Reset %s to" branch)
                                  (delete branch (magit-list-branch-names))
                                  nil nil nil 'magit-revision-history
                                  (or (and (not (equal branch atpoint)) atpoint)
                                      (magit-get-upstream-branch branch)))
           (magit-branch-arguments)
           current-prefix-arg)))
  (unless (member "--force" args)
    (setq args (cons "--force" args)))
  (if (equal branch (magit-get-current-branch))
      (if (and (magit-anything-modified-p)
               (not (yes-or-no-p "Uncommitted changes will be lost.  Proceed?")))
          (user-error "Abort")
        (magit-reset-hard to)
        (when (and set-upstream (magit-branch-p to))
          (magit-set-branch*merge/remote branch to)))
    (magit-branch branch to args)))

;;;###autoload
(defun magit-branch-delete (branches &optional force)
  "Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point."
  ;; One would expect this to be a command as simple as, for example,
  ;; `magit-branch-rename'; but it turns out everyone wants to squeeze
  ;; a bit of extra functionality into this one.  And once it's there,
  ;; you cannot remove it anymore. (I tried, it causes protests.)
  (interactive
   (let ((branches (magit-region-values 'branch))
         (force current-prefix-arg))
     (if (if (> (length branches) 1)
             (magit-confirm t nil "Delete %i branches" branches)
           (setq branches
                 (list (magit-read-branch (if force
                                              "Force delete branch"
                                            "Delete branch")
                                          (magit-get-previous-branch)))))
         (unless force
           (--when-let (-remove #'magit-branch-merged-p branches)
             (if (magit-confirm 'delete-unmerged-branch
                   "Delete unmerged branch %s"
                   "Delete %i unmerged branches" it)
                 (setq force branches)
               (or (setq branches (-difference branches it))
                   (user-error "Abort")))))
       (user-error "Abort"))
     (list branches force)))
  (let* ((refs (-map #'magit-ref-fullname branches))
         (ambiguous (--filter (not it) refs)))
    (when ambiguous
      (user-error
       "%s ambiguous.  Please cleanup using git directly."
       (let ((len (length ambiguous)))
         (cond
          ((= len 1)
           (format "%s is" (-first #'magit-ref-ambiguous-p branches)))
          ((= len (length refs))
           (format "These %s names are" len))
          (t
           (format "%s of these names are" len))))))
    (cond
     ((string-match "^refs/remotes/\\([^/]+\\)" (car refs))
      (let* ((remote (match-string 1 (car refs)))
             (offset (1+ (length remote))))
        ;; Assume the branches actually still exists on the remote.
        (magit-run-git-async
         "push" remote (--map (concat ":" (substring it offset)) branches))
        ;; If that is not the case, then this deletes the tracking branches.
        (set-process-sentinel
         magit-this-process
         (apply-partially 'magit-delete-remote-branch-sentinel refs))))
     ((> (length branches) 1)
      (magit-run-git "branch" (if force "-D" "-d")
                     (delete (magit-get-current-branch) branches)))
     (t ; And now for something completely different.
      (let* ((branch (car branches))
             (prompt (format "Branch %s is checked out.  " branch)))
        (when (equal branch (magit-get-current-branch))
          (pcase (if (or (equal branch "master")
                         (not (magit-rev-verify "master")))
                     (magit-read-char-case prompt nil
                       (?d "[d]etach HEAD & delete" 'detach)
                       (?a "[a]bort"                'abort))
                   (magit-read-char-case prompt nil
                     (?d "[d]etach HEAD & delete"     'detach)
                     (?c "[c]heckout master & delete" 'master)
                     (?a "[a]bort"                    'abort)))
            (`detach (unless (or (equal force '(4))
                                 (member branch force)
                                 (magit-branch-merged-p branch t)
                                 (magit-confirm 'delete-unmerged-branch
                                   "Delete unmerged branch %s" ""
                                   (list branch)))
                       (user-error "Abort"))
                     (magit-call-git "checkout" "--detach"))
            (`master (unless (or (equal force '(4))
                                 (member branch force)
                                 (magit-branch-merged-p branch "master")
                                 (magit-confirm 'delete-unmerged-branch
                                   "Delete unmerged branch %s" ""
                                   (list branch)))
                       (user-error "Abort"))
                     (magit-call-git "checkout" "master"))
            (`abort  (user-error "Abort")))
          (setq force t))
        (magit-run-git "branch" (if force "-D" "-d") branch))))))

(put 'magit-branch-delete 'interactive-only t)

(defun magit-delete-remote-branch-sentinel (refs process event)
  (when (memq (process-status process) '(exit signal))
    (if (= (process-exit-status process) 0)
        (magit-process-sentinel process event)
      (-if-let (rest (-filter #'magit-ref-exists-p refs))
          (progn
            (process-put process 'inhibit-refresh t)
            (magit-process-sentinel process event)
            (setq magit-this-error nil)
            (message "Some remote branches no longer exist.  %s"
                     "Deleting just the local tracking refs instead...")
            (--each rest (magit-call-git "update-ref" "-d" it))
            (magit-refresh)
            (message "Deleting local remote-tracking refs...done"))
        (magit-process-sentinel process event)))))

;;;###autoload
(defun magit-branch-rename (old new &optional force)
  "Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\n(git branch -m|-M OLD NEW)."
  (interactive
   (let ((branch (or (and (memq 'magit-branch-rename magit-no-confirm-default)
                          (or (magit-local-branch-at-point)
                              (magit-get-current-branch)))
                     (magit-read-local-branch "Rename branch"))))
     (list branch
           (magit-read-string-ns (format "Rename branch '%s' to" branch)
                                 nil 'magit-revision-history)
           current-prefix-arg)))
  (unless (string= old new)
    (magit-run-git "branch" (if force "-M" "-m") old new)))

;;; Config Popup

(defvar magit-branch-config-branch nil)

;;;###autoload
(defun magit-branch-config-popup (branch)
  "Popup console for setting branch variables."
  (interactive
   (list (if (or current-prefix-arg
                 (and (eq magit-current-popup 'magit-branch-popup)
                      magit-branch-popup-show-variables))
             (magit-read-local-branch "Configure branch")
           (magit-get-current-branch))))
  (let ((magit-branch-config-branch branch))
    (magit-invoke-popup 'magit-branch-config-popup nil nil)))

(defvar magit-branch-config-variables
  '((lambda ()
      (concat
       (propertize "Configure " 'face 'magit-popup-heading)
       (propertize (magit-branch-config-branch) 'face 'magit-branch-local)))
    (?d "branch.%s.description"
        magit-edit-branch*description
        magit-format-branch*description)
    (?u "branch.%s.merge"
        magit-set-branch*merge/remote
        magit-format-branch*merge/remote)
    (?r "branch.%s.rebase"
        magit-cycle-branch*rebase
        magit-format-branch*rebase)
    (?p "branch.%s.pushRemote"
        magit-cycle-branch*pushRemote
        magit-format-branch*pushRemote)
    "Configure repository defaults"
    (?\M-r "pull.rebase"
           magit-cycle-pull.rebase
           magit-format-pull.rebase)
    (?\M-p "remote.pushDefault"
           magit-cycle-remote.pushDefault
           magit-format-remote.pushDefault)
    "Configure branch creation"
    (?U "branch.autoSetupMerge"
        magit-cycle-branch*autoSetupMerge
        magit-format-branch*autoSetupMerge)
    (?R "branch.autoSetupRebase"
        magit-cycle-branch*autoSetupRebase
        magit-format-branch*autoSetupRebase)))

(defvar magit-branch-config-popup
  `(:man-page "git-branch"
    :variables ,magit-branch-config-variables
    :default-action magit-checkout
    :setup-function magit-branch-config-popup-setup))

(defun magit-branch-config-popup-setup (val def)
  (magit-popup-default-setup val def)
  (setq-local magit-branch-config-branch magit-branch-config-branch)
  (use-local-map (copy-keymap magit-popup-mode-map))
  (dolist (ev (-filter #'magit-popup-event-p (magit-popup-get :variables)))
    (local-set-key (vector (magit-popup-event-key ev))
                   'magit-invoke-popup-action)))

(defun magit-branch-config-branch (&optional prompt)
  (if prompt
      (or (and (not current-prefix-arg)
               (or magit-branch-config-branch
                   (magit-get-current-branch)))
          (magit-read-local-branch prompt))
    (or magit-branch-config-branch
        (magit-get-current-branch)
        "<name>")))

;;; Config Commands and Inserters

;;;###autoload
(defun magit-edit-branch*description (branch)
  "Edit the description of the current branch.
With a prefix argument edit the description of another branch.

The description for the branch named NAME is stored in the Git
variable `branch.<name>.description'."
  (interactive (list (magit-branch-config-branch "Edit branch description")))
  (magit-run-git-with-editor "branch" "--edit-description" branch))

(defun magit-edit-branch*description-check-buffers ()
  (and buffer-file-name
       (string-match-p "/BRANCH_DESCRIPTION\\'" buffer-file-name)
       (add-hook 'with-editor-post-finish-hook
                 (lambda ()
                   (when (derived-mode-p 'magit-popup-mode)
                     (magit-refresh-popup-buffer)))
                 nil t)))

(add-hook 'find-file-hook 'magit-edit-branch*description-check-buffers)

(defun magit-format-branch*description ()
  (let* ((branch (magit-branch-config-branch))
         (width (+ (length branch) 19))
         (var (format "branch.%s.description" branch)))
    (concat var " " (make-string (- width (length var)) ?\s)
            (-if-let (value (magit-get var))
                (propertize value 'face 'magit-popup-option-value)
              (propertize "unset" 'face 'magit-popup-disabled-argument)))))

;;;###autoload
(defun magit-set-branch*merge/remote (branch upstream)
  "Set or unset the upstream of the current branch.
With a prefix argument do so for another branch.

When the branch in question already has an upstream then simply
unsets it.  Invoke this command again to set another upstream.

Together the Git variables `branch.<name>.remote' and
`branch.<name>.merge' define the upstream branch of the local
branch named NAME.  The value of `branch.<name>.remote' is the
name of the upstream remote.  The value of `branch.<name>.merge'
is the full reference of the upstream branch, on the remote.

Non-interactively, when UPSTREAM is non-nil, then always set it
as the new upstream, regardless of whether another upstream was
already set.  When nil, then always unset."
  (interactive
   (let ((branch (magit-branch-config-branch "Change upstream of branch")))
     (list branch (and (not (magit-get-upstream-branch branch))
                       (magit-read-upstream-branch)))))
  (if upstream
      (-let (((remote . merge) (magit-split-branch-name upstream)))
        (setf (magit-get (format "branch.%s.remote" branch)) remote)
        (setf (magit-get (format "branch.%s.merge"  branch))
              (concat "refs/heads/" merge)))
    (magit-call-git "branch" "--unset-upstream" branch))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(defun magit-format-branch*merge/remote ()
  (let* ((branch (magit-branch-config-branch))
         (width (+ (length branch) 20))
         (varM (format "branch.%s.merge" branch))
         (varR (format "branch.%s.remote" branch))
         (face (if (equal (magit-get varR) ".")
                   'magit-branch-local
                 'magit-branch-remote)))
    (concat varM (make-string (- width (length varM)) ?\s)
            (-if-let (value (magit-get varM))
                (propertize value 'face face)
              (propertize "unset" 'face 'magit-popup-disabled-argument))
            "\n   " varR (make-string (- width (length varR)) ?\s)
            (-if-let (value (magit-get varR))
                (propertize value 'face face)
              (propertize "unset" 'face 'magit-popup-disabled-argument)))))

;;;###autoload
(defun magit-cycle-branch*rebase (branch)
  "Cycle the value of `branch.<name>.rebase' for the current branch.
With a prefix argument cycle the value for another branch.

The Git variables `branch.<name>.rebase' controls whether pulling
into the branch named NAME is done by rebasing that branch onto
the fetched branch or by merging that branch.

When `true' then pulling is done by rebasing.
When `false' then pulling is done by merging.

When that variable is undefined then the value of `pull.rebase'
is used instead.  It defaults to `false'."
  (interactive (list (magit-branch-config-branch
                      "Cycle branch.<name>.rebase for")))
  (magit-popup-set-variable (format "branch.%s.rebase" branch)
                            '("true" "false")
                            "false" "pull.rebase"))

(defun magit-format-branch*rebase ()
  (let ((branch (magit-branch-config-branch)))
    (magit-popup-format-variable (format "branch.%s.rebase" branch)
                                 '("true" "false")
                                 "false" "pull.rebase"
                                 (+ (length branch) 20))))

;;;###autoload
(defun magit-cycle-branch*pushRemote (branch)
  "Cycle the value of `branch.<name>.pushRemote' for the current branch.
With a prefix argument cycle the value for another branch.

The Git variable `branch.<name>.pushRemote' specifies the remote
that the branch named NAME is usually pushed to.  The value has
to be the name of an existing remote.

If that variable is undefined, then the value of the Git variable
`remote.pushDefault' is used instead, provided that it is defined,
which by default it is not."
  (interactive (list (magit-branch-config-branch
                      "Cycle branch.<name>.pushRemote for")))
  (magit-popup-set-variable (format "branch.%s.pushRemote" branch)
                            (magit-list-remotes)
                            "remote.pushDefault"))

(defun magit-format-branch*pushRemote ()
  (let ((branch (magit-branch-config-branch)))
    (magit-popup-format-variable (format "branch.%s.pushRemote" branch)
                                 (magit-list-remotes)
                                 nil "remote.pushDefault"
                                 (+ (length branch) 20))))

;;;###autoload
(defun magit-cycle-pull.rebase ()
  "Cycle the repository-local value of `pull.rebase'.

The Git variable `pull.rebase' specifies whether pulling is done
by rebasing or by merging.  It can be overwritten using the Git
variable `branch.<name>.rebase'.

When `true' then pulling is done by rebasing.
When `false' (the default) then pulling is done by merging."
  (interactive)
  (magit-popup-set-variable "pull.rebase" '("true" "false") "false"))

(defun magit-format-pull.rebase ()
  (magit-popup-format-variable "pull.rebase" '("true" "false") "false" nil 19))

;;;###autoload
(defun magit-cycle-remote.pushDefault ()
  "Cycle the repository-local value of `remote.pushDefault'.

The Git variable `remote.pushDefault' specifies the remote that
local branches are usually pushed to.  It can be overwritten
using the Git variable `branch.<name>.pushRemote'."
  (interactive)
  (magit-popup-set-variable "remote.pushDefault" (magit-list-remotes)))

(defun magit-format-remote.pushDefault ()
  (magit-popup-format-variable "remote.pushDefault"
                               (magit-list-remotes) nil nil 19))

;;;###autoload
(defun magit-cycle-branch*autoSetupMerge ()
  "Cycle the repository-local value of `branch.autoSetupMerge'.

The Git variable `branch.autoSetupMerge' under what circumstances
creating a branch (named NAME) should result in the variables
`branch.<name>.merge' and `branch.<name>.remote' being set
according to the starting point used to create the branch.  If
the starting point isn't a branch, then these variables are never
set.

When `always' then the variables are set regardless of whether
the starting point is a local or a remote branch.

When `true' (the default) then the variable are set when the
starting point is a remote branch, but not when it is a local
branch.

When `false' then the variables are never set."
  (interactive)
  (magit-popup-set-variable "branch.autoSetupMerge"
                            '("always" "true" "false") "true"))

(defun magit-format-branch*autoSetupMerge ()
  (magit-popup-format-variable "branch.autoSetupMerge"
                               '("always" "true" "false") "true" nil 23))

;;;###autoload
(defun magit-cycle-branch*autoSetupRebase ()
  "Cycle the repository-local value of `branch.autoSetupRebase'.

The Git variable `branch.autoSetupRebase' specifies whether
creating a branch (named NAME) should result in the variable
`branch.<name>.rebase' being set to `true'.

When `always' then the variable is set regardless of whether the
starting point is a local or a remote branch.

When `local' then the variable are set when the starting point
is a local branch, but not when it is a remote branch.

When `remote' then the variable are set when the starting point
is a remote branch, but not when it is a local branch.

When `never' (the default) then the variable is never set."
  (interactive)
  (magit-popup-set-variable "branch.autoSetupRebase"
                            '("always" "local" "remote" "never") "never"))

(defun magit-format-branch*autoSetupRebase ()
  (magit-popup-format-variable "branch.autoSetupRebase"
                               '("always" "local" "remote" "never")
                               "never" nil 23))

(provide 'magit-branch)
;;; magit-branch.el ends here
