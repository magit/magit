;;; magit-remote.el --- transfer Git commits  -*- lexical-binding: t -*-

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

;; This library implements remote commands.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-remote-add-set-remote.pushDefault 'ask-if-unset
  "Whether to set the value of `remote.pushDefault' after adding a remote.

If `ask', then always ask.  If `ask-if-unset', then ask, but only
if the variable isn't set already.  If nil, then don't ever set.
If the value is a string, then set without asking, provided that
the name of the added remote is equal to that string and the
variable isn't already set."
  :package-version '(magit . "2.4.0")
  :group 'magit-commands
  :type '(choice (const  :tag "ask if unset" ask-if-unset)
                 (const  :tag "always ask" ask)
                 (string :tag "set if named")
                 (const  :tag "don't set")))

(defcustom magit-remote-direct-configure t
  "Whether the command `magit-remote' shows Git variables.
When set to nil, no variables are displayed by this transient
command, instead the sub-transient `magit-remote-configure'
has to be used to view and change remote related variables."
  :package-version '(magit . "2.12.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-remote-set-if-missing t
  "Whether to configure missing remotes before transfering refs.

When nil and the upstream and/or push-remote is not configure,
then the push, fetch, pull and rebase transient commands do not
feature a suffix to act on the missing remote(s).

When non-nil, then these suffix commands are always available,
and if the required configuration is missing, then they do appear
in a way that indicates that this is the case.  These commands
then configure the missing remote before acting on it.

This option also affects whether the argument `--set-upstream' is
available from `magit-push'.  If the value is non-nil, then that
argument is redundant and it is not made available.

If the value is `default', then `remote.pushDefault' is used to
set the push-remote, `branch.<name>.pushRemote' for other non-nil
values."
  :package-version '(magit . "2.91.0")
  :group 'magit-commands
  :type '(choice
          (const :tag "don't set" nil)
          (const :tag "set (using branch.<name>.pushRemote for push-target)" t)
          (const :tag "set (using remote.pushDefault for push-target" default)))

;;; Commands

;;;###autoload (autoload 'magit-remote "magit-remote" nil t)
(define-transient-command magit-remote (remote)
  "Add, configure or remove a remote."
  :man-page "git-remote"
  :value '("-f")
  ["Variables"
   :if (lambda ()
         (and magit-remote-direct-configure
              (oref transient--prefix scope)))
   ("u" magit-remote.<remote>.url)
   ("U" magit-remote.<remote>.fetch)
   ("s" magit-remote.<remote>.pushurl)
   ("S" magit-remote.<remote>.push)
   ("O" magit-remote.<remote>.tagopt)]
  ["Arguments for add"
   ("-f" "Fetch after add" "-f")]
  ["Actions"
   [("a" "Add"                  magit-remote-add)
    ("r" "Rename"               magit-remote-rename)
    ("k" "Remove"               magit-remote-remove)]
   [("C" "Configure..."         magit-remote-configure)
    ("p" "Prune stale branches" magit-remote-prune)
    ("P" "Prune stale refspecs" magit-remote-prune-refspecs)]]
  (interactive (list (magit-get-current-remote)))
  (transient-setup 'magit-remote nil nil :scope remote))

(defun magit-read-url (prompt &optional initial-input)
  (let ((url (magit-read-string-ns prompt initial-input)))
    (if (string-prefix-p "~" url)
        (expand-file-name url)
      url)))

;;;###autoload
(defun magit-remote-add (remote url &optional args)
  "Add a remote named REMOTE and fetch it."
  (interactive (list (magit-read-string-ns "Remote name")
                     (magit-read-url "Remote url")
                     (transient-args 'magit-remote)))
  (if (pcase (list magit-remote-add-set-remote.pushDefault
                   (magit-get "remote.pushDefault"))
        (`(,(pred stringp) ,_) t)
        ((or `(ask ,_) `(ask-if-unset nil))
         (y-or-n-p (format "Set `remote.pushDefault' to \"%s\"? " remote))))
      (progn (magit-call-git "remote" "add" args remote url)
             (setf (magit-get "remote.pushDefault") remote)
             (magit-refresh))
    (magit-run-git-async "remote" "add" args remote url)))

;;;###autoload
(defun magit-remote-rename (old new)
  "Rename the remote named OLD to NEW."
  (interactive
   (let  ((remote (magit-read-remote "Rename remote")))
     (list remote (magit-read-string-ns (format "Rename %s to" remote)))))
  (unless (string= old new)
    (magit-call-git "remote" "rename" old new)
    (magit-remote--cleanup-push-variables old new)
    (magit-refresh)))

;;;###autoload
(defun magit-remote-remove (remote)
  "Delete the remote named REMOTE."
  (interactive (list (magit-read-remote "Delete remote")))
  (magit-call-git "remote" "rm" remote)
  (magit-remote--cleanup-push-variables remote)
  (magit-refresh))

(defun magit-remote--cleanup-push-variables (remote &optional new-name)
  (magit-with-toplevel
    (when (equal (magit-get "remote.pushDefault") remote)
      (magit-set new-name "remote.pushDefault"))
    (dolist (var (magit-git-lines "config" "--name-only"
                                  "--get-regexp" "^branch\.[^.]*\.pushRemote"
                                  (format "^%s$" remote)))
      (magit-call-git "config" (and (not new-name) "--unset") var new-name))))

(defconst magit--refspec-re "\\`\\(\\+\\)?\\([^:]+\\):\\(.*\\)\\'")

;;;###autoload
(defun magit-remote-prune (remote)
  "Remove stale remote-tracking branches for REMOTE."
  (interactive (list (magit-read-remote "Prune stale branches of remote")))
  (magit-run-git-async "remote" "prune" remote))

;;;###autoload
(defun magit-remote-prune-refspecs (remote)
  "Remove stale refspecs for REMOTE.

A refspec is stale if there no longer exists at least one branch
on the remote that would be fetched due to that refspec.  A stale
refspec is problematic because its existence causes Git to refuse
to fetch according to the remaining non-stale refspecs.

If only stale refspecs remain, then offer to either delete the
remote or to replace the stale refspecs with the default refspec.

Also remove the remote-tracking branches that were created due to
the now stale refspecs.  Other stale branches are not removed."
  (interactive (list (magit-read-remote "Prune refspecs of remote")))
  (let* ((tracking-refs (magit-list-remote-branches remote))
         (remote-refs (magit-remote-list-refs remote))
         (variable (format "remote.%s.fetch" remote))
         (refspecs (magit-get-all variable))
         stale)
    (dolist (refspec refspecs)
      (when (string-match magit--refspec-re refspec)
        (let ((theirs (match-string 2 refspec))
              (ours   (match-string 3 refspec)))
          (unless (if (string-match "\\*" theirs)
                      (let ((re (replace-match ".*" t t theirs)))
                        (--some (string-match-p re it) remote-refs))
                    (member theirs remote-refs))
            (push (cons refspec
                        (if (string-match "\\*" ours)
                            (let ((re (replace-match ".*" t t ours)))
                              (--filter (string-match-p re it) tracking-refs))
                          (list (car (member ours tracking-refs)))))
                  stale)))))
    (if (not stale)
        (message "No stale refspecs for remote %S" remote)
      (if (= (length stale)
             (length refspecs))
          (magit-read-char-case
              (format "All of %s's refspecs are stale.  " remote) nil
            (?s "replace with [d]efault refspec"
                (magit-set-all
                 (list (format "+refs/heads/*:refs/remotes/%s/*" remote))
                 variable))
            (?r "[r]emove remote"
                (magit-call-git "remote" "rm" remote))
            (?a "or [a]abort"
                (user-error "Abort")))
        (if (if (= (length stale) 1)
                (pcase-let ((`(,refspec . ,refs) (car stale)))
                  (magit-confirm 'prune-stale-refspecs
                    (format "Prune stale refspec %s and branch %%s" refspec)
                    (format "Prune stale refspec %s and %%i branches" refspec)
                    nil refs))
              (magit-confirm 'prune-stale-refspecs nil
                (format "Prune %%i stale refspecs and %i branches"
                        (length (cl-mapcan (lambda (s) (copy-sequence (cdr s)))
                                           stale)))
                nil
                (mapcar (pcase-lambda (`(,refspec . ,refs))
                          (concat refspec "\n"
                                  (mapconcat (lambda (b) (concat "  " b))
                                             refs "\n")))
                        stale)))
            (pcase-dolist (`(,refspec . ,refs) stale)
              (magit-call-git "config" "--unset" variable
                              (regexp-quote refspec))
              (magit--log-action
               (lambda (refs)
                 (format "Deleting %i branches" (length refs)))
               (lambda (ref)
                 (format "Deleting branch %s (was %s)" ref
                         (magit-rev-parse "--short" ref)))
               refs)
              (dolist (ref refs)
                (magit-call-git "update-ref" "-d" ref)))
          (user-error "Abort")))
      (magit-refresh))))

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

;;; Configure

;;;###autoload (autoload 'magit-remote-configure "magit-remote" nil t)
(define-transient-command magit-remote-configure (remote)
  "Configure a remote."
  :man-page "git-remote"
  [:description
   (lambda ()
     (concat
      (propertize "Configure " 'face 'transient-heading)
      (propertize (oref transient--prefix scope) 'face 'magit-branch-remote)))
   ("u" magit-remote.<remote>.url)
   ("U" magit-remote.<remote>.fetch)
   ("s" magit-remote.<remote>.pushurl)
   ("S" magit-remote.<remote>.push)
   ("O" magit-remote.<remote>.tagopt)]
  (interactive
   (list (or (and (not current-prefix-arg)
                  (not (and magit-remote-direct-configure
                            (eq current-transient-command 'magit-remote)))
                  (magit-get-current-remote))
             (magit--read-remote-scope))))
  (transient-setup 'magit-remote-configure nil nil :scope remote))

(defun magit--read-remote-scope (&optional obj)
  (magit-read-remote
   (if obj
       (format "Set %s for remote"
               (format (oref obj variable) "<name>"))
     "Configure remote")))

(define-infix-command magit-remote.<remote>.url ()
  :class 'magit--git-variable:urls
  :scope 'magit--read-remote-scope
  :variable "remote.%s.url"
  :multi-value t
  :history-key 'magit-remote.<remote>.*url)

(define-infix-command magit-remote.<remote>.fetch ()
  :class 'magit--git-variable
  :scope 'magit--read-remote-scope
  :variable "remote.%s.fetch"
  :multi-value t)

(define-infix-command magit-remote.<remote>.pushurl ()
  :class 'magit--git-variable:urls
  :scope 'magit--read-remote-scope
  :variable "remote.%s.pushurl"
  :multi-value t
  :history-key 'magit-remote.<remote>.*url
  :seturl-arg "--push")

(define-infix-command magit-remote.<remote>.push ()
  :class 'magit--git-variable
  :scope 'magit--read-remote-scope
  :variable "remote.%s.push")

(define-infix-command magit-remote.<remote>.tagopt ()
  :class 'magit--git-variable:choices
  :scope 'magit--read-remote-scope
  :variable "remote.%s.tagOpt"
  :choices '("--no-tags" "--tags"))

;;; Transfer Utilities
;;;; Push-Remote

(defun magit--transfer-pushremote (remote fn)
  (declare (indent defun))
  (if-let ((branch (magit-get-current-branch)))
      (progn
        (when remote
          (setf (magit-get (if (eq magit-remote-set-if-missing 'default)
                               "remote.pushDefault"
                             (format "branch.%s.pushRemote" branch)))
                remote))
        (if-let ((remote (or remote (magit-get-push-remote branch))))
            (if (member remote (magit-list-remotes))
                (funcall fn remote branch (concat remote "/" branch))
              (user-error "Remote `%s' doesn't exist" remote))
          (user-error "No push-remote is configured for %s" branch)))
    (user-error "No branch is checked out")))

(defun magit--transfer-set-pushremote-p (&optional change)
  (and (or change
           (and magit-remote-set-if-missing
                (not (magit-get-push-remote))))
       (magit-get-current-branch)))

(defun magit--transfer-maybe-read-pushremote (action)
  (and (magit--transfer-set-pushremote-p current-prefix-arg)
       (magit-read-remote
        (if (eq magit-remote-set-if-missing 'default)
            (format "Set `remote.pushDefault' and %s there" action)
          (format "Set `branch.%s.pushRemote' and %s there"
                  (magit-get-current-branch)
                  action)))))

(defun magit--pushbranch-suffix-predicate ()
  (or (magit-get-upstream-branch)
      magit-remote-set-if-missing))

(defun magit--pushbranch-suffix-description (&optional pushp)
  (if-let ((branch (magit-get-push-branch)))
      (cond ((magit-rev-verify branch) branch)
            (pushp (concat branch ", creating it"))
            ;; This shouldn't happen often and even if it does, then
            ;; transfering would still succeed iff the branch exists
            ;; on the remote (only the tracking branch is missing).
            (t (concat branch ", which appears to be missing")))
    (and (magit--transfer-set-pushremote-p)
         (concat (propertize
                  (if (eq magit-remote-set-if-missing 'default)
                      "pushDefault"
                    "pushRemote")
                  'face 'bold)
                 ", after setting that"))))

;;;; Upstream

(defun magit--transfer-upstream (upstream fn)
  (declare (indent defun))
  (if-let ((current (magit-get-current-branch)))
      (progn
        (when upstream
          (magit-set-upstream-branch current upstream))
        (if-let ((upstream (or upstream (magit-get-upstream-branch current))))
            (funcall fn current upstream)
          (user-error "No upstream is configured for %s" current)))
    (user-error "No branch is checked out")))

(defun magit--transfer-set-upstream-p (&optional change)
  (and (or change
           (and magit-remote-set-if-missing
                (not (magit-get-upstream-branch))))
       (magit-get-current-branch)))

(defun magit--transfer-maybe-read-upstream (action)
  (and (magit--transfer-set-upstream-p current-prefix-arg)
       (magit-read-upstream-branch
        nil (format "Set upstream and %s there" action))))

(defun magit--upstream-suffix-predicate ()
  (or (magit-get-push-branch)
      magit-remote-set-if-missing))

(defun magit--upstream-suffix-description (&optional pushp)
  (if-let ((branch (magit-get-upstream-branch)))
      (cond ((magit-rev-verify branch) branch)
            (pushp (concat branch ", creating it"))
            ;; This shouldn't happen often and even if it does, then
            ;; transfering would still succeed iff the branch exists
            ;; on the remote (only the tracking branch is missing).
            (t (concat branch ", which appears to be missing")))
    (and (magit--transfer-set-upstream-p)
         (concat (propertize "@{upstream}" 'face 'bold)
                 ", after setting that"))))

;;; _
(provide 'magit-remote)
;;; magit-remote.el ends here
