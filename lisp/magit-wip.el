;;; magit-wip.el --- Commit snapshots to work-in-progress refs  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library defines global modes which automatically commit
;; snapshots to branch-specific work-in-progress refs before and after
;; making changes, and two commands which can be used to do so on
;; demand.

;;; Code:

(require 'magit-core)
(require 'magit-log)

;;; Options

(defgroup magit-wip nil
  "Automatically commit to work-in-progress refs."
  :link '(info-link "(magit)Wip Modes")
  :group 'magit-modes
  :group 'magit-essentials)

(defcustom magit-wip-mode-lighter " Wip"
  "Lighter for Magit-Wip mode."
  :package-version '(magit . "2.90.0")
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-merge-branch nil
  "Whether to merge the current branch into its wip ref.

If non-nil and the current branch has new commits, then it is
merged into the wip ref before creating a new wip commit.  This
makes it easier to inspect wip history and the wip commits are
never garbage collected.

If nil and the current branch has new commits, then the wip ref
is reset to the tip of the branch before creating a new wip
commit.  With this setting wip commits are eventually garbage
collected.  This is currently the default.

If `immediately', then use `git-commit-post-finish-hook' to
create the merge commit.  This is discouraged because it can
lead to a race condition, e.g., during rebases.

If `githook', then use `magit-common-git-post-commit-hook' to
create the merge commit.  This uses the experimental support for
calling Lisp hooks from Git hooks, which is disabled by default,
Customize `magit-overriding-githook-directory' to enable use of
Git hooks."
  :package-version '(magit . "2.90.0")
  :group 'magit-wip
  :type '(choice
          (const :tag "Yes (safely, just in time)" t)
          (const :tag "Yes (immediately, with race condition)" immediately)
          (const :tag "Yes (using experimental Git hook support)" githook)
          (const :tag "No" nil)))

(defcustom magit-wip-namespace "refs/wip/"
  "Namespace used for work-in-progress refs.
The wip refs are named \"<namespace/>index/<branchref>\"
and \"<namespace/>wtree/<branchref>\".  When snapshots
are created while the `HEAD' is detached then \"HEAD\"
is used as `branch-ref'."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-debug nil
  "Whether to record commands used to update wip refs.
If non-nil, log the used commands and their output in the process
buffer."
  :package-version '(magit . "4.5.0")
  :group 'magit-wip
  :type 'boolean)

;;; Mode

(defvar magit--wip-inhibit-autosave nil)

;;;###autoload
(define-minor-mode magit-wip-mode
  "Automatically save uncommitted changes to work-in-progress refs."
  :package-version '(magit . "2.90.0")
  :lighter magit-wip-mode-lighter
  :global t
  (cond
    (magit-wip-mode
     (add-hook 'after-save-hook #'magit-wip-commit-buffer-file)
     (add-hook 'magit-after-apply-functions #'magit-wip-commit)
     (add-hook 'magit-before-change-functions #'magit-wip-commit)
     (add-hook 'before-save-hook #'magit-wip-commit-initial-backup)
     (add-hook 'magit-common-git-post-commit-functions #'magit-wip-post-commit)
     (add-hook 'git-commit-post-finish-hook #'magit-wip-commit-post-editmsg))
    (t
     (remove-hook 'after-save-hook #'magit-wip-commit-buffer-file)
     (remove-hook 'magit-after-apply-functions #'magit-wip-commit)
     (remove-hook 'magit-before-change-functions #'magit-wip-commit)
     (remove-hook 'before-save-hook #'magit-wip-commit-initial-backup)
     (remove-hook 'magit-common-git-post-commit-functions #'magit-wip-post-commit)
     (remove-hook 'git-commit-post-finish-hook #'magit-wip-commit-post-editmsg))))

(defun magit-wip-commit-buffer-file (&optional msg)
  "Commit visited file to a worktree work-in-progress ref."
  (interactive (list "save %s snapshot"))
  (when (and (not magit--wip-inhibit-autosave)
             buffer-file-name
             (magit-inside-worktree-p t)
             (magit-file-tracked-p buffer-file-name))
    (magit-wip-commit-worktree
     (magit-wip-get-ref)
     (list buffer-file-name)
     (format (or msg "autosave %s after save")
             (magit-file-relative-name buffer-file-name)))))

(defun magit-run-after-apply-functions (files task)
  (run-hook-with-args 'magit-after-apply-functions
                      (ensure-list files)
                      (format " after %s" task)))

(defun magit-run-before-change-functions (files task)
  (run-hook-with-args 'magit-before-change-functions
                      (ensure-list files)
                      (format " before %s" task)))

(defvar-local magit-wip-buffer-backed-up nil)
(put 'magit-wip-buffer-backed-up 'permanent-local t)

(defun magit-wip-commit-initial-backup ()
  (when (and (not magit-wip-buffer-backed-up)
             buffer-file-name
             (magit-inside-worktree-p t)
             (magit-file-tracked-p buffer-file-name))
    (let ((magit-save-repository-buffers nil))
      (magit-wip-commit-buffer-file "autosave %s before save"))
    (setq magit-wip-buffer-backed-up t)))

(defun magit-wip-post-commit (&rest _)
  (when (eq magit-wip-merge-branch 'githook)
    (magit-wip-commit)))

(defun magit-wip-commit-post-editmsg ()
  (when (eq magit-wip-merge-branch 'immediately)
    (magit-wip-commit)))

;;; Core

(defun magit-wip-commit (&optional files msg)
  "Commit all tracked files to the work-in-progress refs.

Interactively, commit all changes to all tracked files using
a generic commit message.  With a prefix-argument the commit
message is read in the minibuffer.

Non-interactively, only commit changes to FILES using MSG as
commit message."
  (interactive (list nil (if current-prefix-arg
                             (magit-read-string "Wip commit message")
                           "wip-save tracked files")))
  (when-let ((ref (magit-wip-get-ref)))
    (magit-wip-commit-index ref files msg)
    (magit-wip-commit-worktree ref files msg)))

(defun magit-wip-commit-index (ref files msg)
  (let* ((wipref (magit--wip-index-ref ref))
         (parent (magit-wip-get-parent ref wipref))
         (tree   (magit-git-string "write-tree")))
    (magit-wip-update-wipref ref wipref tree parent files msg "index")))

(defun magit-wip-commit-worktree (ref files msg)
  (when (or (not files)
            ;; `update-index' will either ignore (before Git v2.32.0)
            ;; or fail when passed directories (relevant for the
            ;; untracked files code paths).
            (setq files (seq-remove #'file-directory-p files)))
    (let* ((wipref (magit--wip-wtree-ref ref))
           (parent (magit-wip-get-parent ref wipref))
           (tree (magit-with-temp-index parent (list "--reset" "-i")
                   (if files
                       ;; Note: `update-index' is used instead of `add'
                       ;; because `add' will fail if a file is already
                       ;; deleted in the temporary index.
                       (magit-wip--git "update-index" "--add" "--remove"
                                       "--ignore-skip-worktree-entries"
                                       "--" files)
                     (magit-with-toplevel
                       (magit-wip--git "add" "-u" ".")))
                   (magit-git-string "write-tree"))))
      (magit-wip-update-wipref ref wipref tree parent files msg "worktree"))))

(defun magit-wip--git (&rest args)
  (if magit-wip-debug
      (let ((default-process-coding-system (magit--process-coding-system)))
        (apply #'magit-call-process
               (magit-git-executable)
               (magit-process-git-arguments args)))
    (apply #'magit-process-file
           (magit-git-executable) nil nil nil
           (magit-process-git-arguments args))))

(defun magit-wip-update-wipref (ref wipref tree parent files msg start-msg)
  (cond
    ((and (not (equal parent wipref))
          (or (not magit-wip-merge-branch)
              (not (magit-rev-verify wipref))))
     (setq start-msg (concat "start autosaving " start-msg))
     (magit-wip--update-ref wipref start-msg
                            (magit-git-string "commit-tree" "--no-gpg-sign"
                                              "-p" parent "-m" start-msg
                                              (concat parent "^{tree}")))
     (setq parent wipref))
    ((and magit-wip-merge-branch
          (or (not (magit-rev-ancestor-p ref wipref))
              (not (magit-rev-ancestor-p
                    (concat (magit-git-string "log" "--format=%H"
                                              "-1" "--merges" wipref)
                            "^2")
                    ref))))
     (setq start-msg (format "merge %s into %s" ref start-msg))
     (magit-wip--update-ref wipref start-msg
                            (magit-git-string "commit-tree" "--no-gpg-sign"
                                              "-p" wipref "-p" ref
                                              "-m" start-msg
                                              (concat ref "^{tree}")))
     (setq parent wipref)))
  (when (magit-git-failure "diff-tree" "--quiet" parent tree "--" files)
    (unless (and msg (not (= (aref msg 0) ?\s)))
      (let ((len (length files)))
        (setq msg (concat
                   (cond ((= len 0) "autosave tracked files")
                         ((> len 1) (format "autosave %s files" len))
                         ((concat "autosave "
                                  (file-relative-name (car files)
                                                      (magit-toplevel)))))
                   msg))))
    (magit-wip--update-ref wipref msg
                           (magit-git-string "commit-tree" "--no-gpg-sign"
                                             "-p" parent "-m" msg tree))))

(defun magit-wip--update-ref (ref message rev)
  (let ((magit--refresh-cache nil))
    (unless (zerop (magit-wip--git "update-ref" "--create-reflog"
                                   "-m" message ref rev))
      (error "Cannot update %s with %s" ref rev))))

(defun magit-wip-get-ref ()
  (let ((ref (or (magit-git-string "symbolic-ref" "HEAD") "HEAD")))
    (and (magit-rev-verify ref)
         ref)))

(defun magit-wip-get-parent (ref wipref)
  (if (and (magit-rev-verify wipref)
           (equal (magit-git-string "merge-base" wipref ref)
                  (magit-rev-verify ref)))
      wipref
    ref))

(defun magit--wip-index-ref (&optional ref)
  (magit--wip-ref "index/" ref))

(defun magit--wip-wtree-ref (&optional ref)
  (magit--wip-ref "wtree/" ref))

(defun magit--wip-ref (namespace &optional ref)
  (concat magit-wip-namespace namespace
          (or (and ref (string-prefix-p "refs/" ref) ref)
              (and-let ((_(not (equal ref "HEAD")))
                        (branch (or ref (magit-get-current-branch))))
                (concat "refs/heads/" branch))
              "HEAD")))

;;; Log

(defun magit-wip-log-index (args files)
  "Show log for the index wip ref of the current branch."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (list (magit--wip-index-ref)) args files))

(defun magit-wip-log-worktree (args files)
  "Show log for the worktree wip ref of the current branch."
  (interactive (magit-log-arguments))
  (magit-log-setup-buffer (list (magit--wip-wtree-ref)) args files))

(defun magit-wip-log-current (branch args files count)
  "Show log for the current branch and its wip refs.
With a negative prefix argument only show the worktree wip ref.
The absolute numeric value of the prefix argument controls how
many \"branches\" of each wip ref are shown."
  (interactive
    (nconc (list (or (magit-get-current-branch) "HEAD"))
           (magit-log-arguments)
           (list (prefix-numeric-value current-prefix-arg))))
  (magit-wip-log branch args files count))

(defun magit-wip-log (branch args files count)
  "Show log for a branch and its wip refs.
With a negative prefix argument only show the worktree wip ref.
The absolute numeric value of the prefix argument controls how
many \"branches\" of each wip ref are shown."
  (interactive
    (nconc (list (magit-completing-read
                  "Log branch and its wip refs"
                  (nconc (magit-list-local-branch-names)
                         (list "HEAD"))
                  nil t nil 'magit-revision-history
                  (or (magit-branch-at-point)
                      (magit-get-current-branch)
                      "HEAD")))
           (magit-log-arguments)
           (list (prefix-numeric-value current-prefix-arg))))
  (magit-log-setup-buffer (nconc (list branch)
                                 (magit-wip-log-get-tips
                                  (magit--wip-wtree-ref branch)
                                  (abs count))
                                 (and (>= count 0)
                                      (magit-wip-log-get-tips
                                       (magit--wip-index-ref branch)
                                       (abs count))))
                          args files))

(defun magit-wip-log-get-tips (wipref count)
  (and-let ((reflog (magit-git-lines "reflog" wipref)))
    (let (tips)
      (while (and reflog (> count 1))
        ;; "start autosaving ..." is the current message, but it used
        ;; to be "restart autosaving ...", and those messages may
        ;; still be around (e.g., if gc.reflogExpire is set to "never").
        (setq reflog (cl-member "^[^ ]+ [^:]+: \\(?:re\\)?start autosaving"
                                reflog :test #'string-match-p))
        (when (and (cadr reflog)
                   (string-match "^[^ ]+ \\([^:]+\\)" (cadr reflog)))
          (push (match-str 1 (cadr reflog)) tips))
        (setq reflog (cddr reflog))
        (cl-decf count))
      (cons wipref (nreverse tips)))))

(defun magit-wip-purge ()
  "Ask to delete all wip-refs that no longer have a corresponding ref."
  (interactive)
  (cond-let
    ([wiprefs (thread-last
                (cl-set-difference (magit-list-refs "refs/wip/")
                                   (magit-list-refs)
                                   :test (##equal (substring %1 15) %2))
                (delete "refs/wip/index/HEAD")
                (delete "refs/wip/wtree/HEAD"))]
     (magit-confirm 'purge-dangling-wiprefs
       "Delete wip-ref %s without corresponding ref"
       "Delete %d wip-refs without corresponding ref"
       nil wiprefs)
     (message "Deleting wip-refs...")
     (dolist (wipref wiprefs)
       (magit-call-git "update-ref" "-d" wipref))
     (message "Deleting wip-refs...done")
     (magit-refresh))
    ((message "All wip-refs have a corresponding ref"))))

;;; _
(provide 'magit-wip)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("and>"         . "cond-let--and>")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; magit-wip.el ends here
