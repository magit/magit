;;; magit-remote.el --- transfer Git commits  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2018  The Magit Project Contributors
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

(defcustom magit-remote-popup-show-variables t
  "Whether the `magit-remote-popup' shows Git variables.
When set to nil, no variables are displayed directly in this
popup, instead the sub-popup `magit-remote-config-popup' has
to be used to view and change remote related variables."
  :package-version '(magit . "2.12.0")
  :group 'magit-commands
  :type 'boolean)

;;; Commands

(defvar magit-remote-config-variables)

;;;###autoload (autoload 'magit-remote-popup "magit-remote" nil t)
(magit-define-popup magit-remote-popup
  "Popup console for remote commands."
  :man-page "git-remote"
  :default-arguments '("-f")
  :variables (lambda ()
               (and magit-remote-popup-show-variables
                    magit-remote-config-variables))
  :switches '("Switches for add"
              (?f "Fetch after add" "-f"))
  :actions  '((?a "Add"                  magit-remote-add)
              (?C "Configure..."         magit-remote-config-popup)
              (?r "Rename"               magit-remote-rename)
              (?p "Prune stale branches" magit-remote-prune)
              (?k "Remove"               magit-remote-remove)
              (?P "Prune stale refspecs" magit-remote-prune-refspecs))
  :max-action-columns 2)

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
                     (magit-remote-arguments)))
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

(defvar magit-remote-config--remote nil)

;;;###autoload
(defun magit-remote-config-popup (remote)
  "Popup console for setting remote variables."
  (interactive
   (list (if (or current-prefix-arg
                 (and (eq magit-current-popup 'magit-remote-popup)
                      magit-remote-popup-show-variables))
             (magit-read-remote "Configure remote")
           (magit-remote-config--remote-1))))
  (let ((magit-remote-config--remote remote))
    (magit-invoke-popup 'magit-remote-config-popup nil nil)))

(defvar magit-remote-config-variables
  '((lambda ()
      (concat
       (propertize "Configure " 'face 'magit-popup-heading)
       (propertize (magit-remote-config--remote) 'face 'magit-branch-remote)))
    (?u "remote.%s.url"
        magit-set-remote*url
        magit-format-remote*url)
    (?U "remote.%s.fetch"
        magit-set-remote*fetch
        magit-format-remote*fetch)
    (?s "remote.%s.pushurl"
        magit-set-remote*pushurl
        magit-format-remote*pushurl)
    (?S "remote.%s.push"
        magit-set-remote*push
        magit-format-remote*push)
    (?O "remote.%s.tagOpt"
        magit-cycle-remote*tagOpt
        magit-format-remote*tagOpt)))

(defvar magit-remote-config-popup
  `(:man-page "git-remote"
    :variables ,magit-remote-config-variables
    :setup-function magit-remote-config-popup-setup))

(defun magit-remote-config-popup-setup (val def)
  (magit-popup-default-setup val def)
  (setq-local magit-remote-config--remote magit-remote-config--remote))

(defun magit-remote-config--remote (&optional prompt)
  (if prompt
      (or (and (not current-prefix-arg)
               (or magit-remote-config--remote
                   (magit-remote-config--remote-1)))
          (magit-read-remote prompt))
    (or magit-remote-config--remote
        (magit-remote-config--remote-1)
        "<name>")))

(defun magit-remote-config--remote-1 ()
  (let ((remote (magit-get-upstream-remote)))
    (if (or (not remote)
            (equal remote "."))
        (and (magit-remote-p "origin") "origin")
      remote)))

(defun magit-set-remote*url (remote urls)
  "Set the variable `url' for the remote named REMOTE to URLS."
  (interactive (magit-remote-config--read-args "url" "Urls: "))
  (magit-remote-config--set-url remote "url" urls))

(defun magit-set-remote*fetch (remote values)
  "Set the variable `fetch' for the remote named REMOTE to VALUES."
  (interactive (magit-remote-config--read-args "fetch" "Fetch specs: "))
  (magit-set-all values "remote" remote "fetch")
  (magit-refresh))

(defun magit-set-remote*pushurl (remote urls)
  "Set the variable `pushurl' for the remote named REMOTE to URLS."
  (interactive (magit-remote-config--read-args "pushurl" "Urls: "))
  (magit-remote-config--set-url remote "pushurl" urls "--push"))

(defun magit-set-remote*push (remote values)
  "Set the variable `push' for the remote named REMOTE to VALUES."
  (interactive (magit-remote-config--read-args "push" "Push specs: "))
  (magit-set-all values "remote" remote "push")
  (magit-refresh))

(defun magit-cycle-remote*tagOpt (remote)
  (interactive (list (magit-remote-config--remote)))
  (magit--set-popup-variable (format "remote.%s.tagOpt" remote)
                             '("--no-tags" "--tags") nil))

(defun magit-format-remote*url ()
  (magit-remote-config--format-variable "url"))

(defun magit-format-remote*fetch ()
  (magit-remote-config--format-variable "fetch"))

(defun magit-format-remote*pushurl ()
  (magit-remote-config--format-variable "pushurl"))

(defun magit-format-remote*push ()
  (magit-remote-config--format-variable "push"))

(defun magit-format-remote*tagOpt ()
  (let ((remote (magit-remote-config--remote)))
    (magit--format-popup-variable:choices
     (format "remote.%s.tagOpts" remote)
     '("--no-tags" "--tags") nil nil
     (+ (length remote) 16))))

(defun magit-remote-config--read-args (var prompt)
  (let* ((remote (magit-remote-config--remote (format "Set `%s' of remote" var)))
         (value (magit-get-all "remote" remote var)))
    (list remote
          (mapcar (lambda (url)
                    (if (string-prefix-p "~" url)
                        (expand-file-name url)
                      url))
                  (completing-read-multiple
                   prompt nil nil nil
                   (and value (mapconcat #'identity value ",")))))))

(defun magit-remote-config--set-url (remote var values &optional arg)
  (let ((old (magit-get-all "remote" remote var)))
    (dolist (v (-difference values old))
      (magit-call-git "remote" "set-url" arg "--add" remote v))
    (dolist (v (-difference old values))
      (magit-call-git "remote" "set-url" arg "--delete" remote
                      (concat "^" (regexp-quote v) "$"))))
  (magit-refresh))

(defun magit-remote-config--format-variable (variable)
  (magit--format-popup-variable:values
   (format "remote.%s.%s" (magit-remote-config--remote) variable)
   25))

;;; _
(provide 'magit-remote)
;;; magit-remote.el ends here
