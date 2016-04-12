;;; magit-wip.el --- commit snapshots to work-in-progress refs  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Magit Project Contributors
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

;; This library defines tree global modes which automatically commit
;; snapshots to branch specific work-in-progress refs before and after
;; making changes, and two commands which can be used to do so on
;; demand.

;;; Code:

(require 'magit-core)
(require 'magit-log)

;;; Options

(defgroup magit-wip nil
  "Automatically commit to work-in-progress refs."
  :group 'magit-extensions)

(defcustom magit-wip-after-save-local-mode-lighter " sWip"
  "Lighter for Magit-Wip-After-Save-Local mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-after-apply-mode-lighter " aWip"
  "Lighter for Magit-Wip-After-Apply mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-before-change-mode-lighter " cWip"
  "Lighter for Magit-Wip-Before-Change mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-namespace "refs/wip/"
  "Namespace used for work-in-progress refs.
The wip refs are named \"<namespace/>index/<branchref>\"
and \"<namespace/>wtree/<branchref>\".  When snapshots
are created while the `HEAD' is detached then \"HEAD\"
is used as `branch-ref'."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :type 'string)

;;; Modes

(define-minor-mode magit-wip-after-save-local-mode
  "After saving, also commit to a worktree work-in-progress ref.

After saving the current file-visiting buffer this mode also
commits the changes to the worktree work-in-progress ref for
the current branch.

This mode should be enabled globally by turning on the globalized
variant `magit-wip-after-save-mode'."
  :package-version '(magit . "2.1.0")
  :lighter magit-wip-after-save-local-mode-lighter
  (if magit-wip-after-save-local-mode
      (if (and buffer-file-name (magit-inside-worktree-p))
          (add-hook 'after-save-hook 'magit-wip-commit-buffer-file t t)
        (setq magit-wip-after-save-local-mode nil)
        (user-error "Need a worktree and a file"))
    (remove-hook 'after-save-hook 'magit-wip-commit-buffer-file t)))

(defun magit-wip-after-save-local-mode-turn-on ()
  (and buffer-file-name
       (ignore-errors (magit-inside-worktree-p))
       (magit-file-tracked-p buffer-file-name)
       (magit-wip-after-save-local-mode)))

;;;###autoload
(define-globalized-minor-mode magit-wip-after-save-mode
  magit-wip-after-save-local-mode magit-wip-after-save-local-mode-turn-on
  :package-version '(magit . "2.1.0")
  :group 'magit-wip)

(defun magit-wip-commit-buffer-file ()
  "Commit visited file to a worktree work-in-progress ref.

Also see `magit-wip-after-save-mode' which calls this function
automatically whenever a buffer visiting a tracked file is saved."
  (interactive)
  (--when-let (magit-wip-get-ref)
    (magit-with-toplevel
      (let ((file (file-relative-name buffer-file-name)))
        (magit-wip-commit-worktree
         it (list file) (if (called-interactively-p 'any)
                            (format "wip-save %s after save" file)
                          (format "autosave %s after save" file)))))))

;;;###autoload
(define-minor-mode magit-wip-after-apply-mode
  "Commit to work-in-progress refs.

After applying a change using any \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected files to the current wip refs.  For each branch there
may be two wip refs; one contains snapshots of the files as found
in the worktree and the other contains snapshots of the entries
in the index."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :lighter magit-wip-after-change-mode-lighter
  :global t)

(defun magit-wip-commit-after-apply (&optional files msg)
  (when magit-wip-after-apply-mode
    (magit-wip-commit files msg)))

;;;###autoload
(define-minor-mode magit-wip-before-change-mode
  "Commit to work-in-progress refs before certain destructive changes.

Before invoking a revert command or an \"apply variant\"
command (apply, stage, unstage, discard, and reverse) commit the
affected tracked files to the current wip refs.  For each branch
there may be two wip refs; one contains snapshots of the files
as found in the worktree and the other contains snapshots of the
entries in the index.

Only changes to files which could potentially be affected by the
command which is about to be called are committed."
  :package-version '(magit . "2.1.0")
  :group 'magit-wip
  :lighter magit-wip-before-change-mode-lighter
  :global t)

(defun magit-wip-commit-before-change (&optional files msg)
  (when magit-wip-before-change-mode
    (magit-with-toplevel
      (magit-wip-commit files msg))))

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
  (--when-let (magit-wip-get-ref)
    (magit-wip-commit-index it files msg)
    (magit-wip-commit-worktree it files msg)))

(defun magit-wip-commit-index (ref files msg &optional cached-only)
  (let* ((wipref (concat magit-wip-namespace "index/" ref))
         (parent (magit-wip-get-parent ref wipref)))
    (when (magit-git-failure "diff-index" "--quiet"
                             (and cached-only "--cached")
                             parent "--" files)
      (magit-wip-update-wipref wipref (magit-git-string "write-tree")
                               parent files msg "index"))))

(defun magit-wip-commit-worktree (ref files msg)
  (let* ((wipref (concat magit-wip-namespace "wtree/" ref))
         (parent (magit-wip-get-parent ref wipref))
         (tree (magit-with-temp-index parent "--reset"
                 (if files
                     (magit-call-git "add" "--" files)
                   (magit-with-toplevel
                     (magit-call-git "add" "-u" ".")))
                 (magit-git-string "write-tree"))))
    (when (magit-git-failure "diff-tree" "--quiet" parent tree "--" files)
      (magit-wip-update-wipref wipref tree parent files msg "worktree"))))

(defun magit-wip-update-wipref (wipref tree parent files msg start-msg)
  (let ((len (length files)))
    (unless (and msg (not (= (aref msg 0) ?\s)))
      (setq msg (concat
                 (cond ((= len 0) "autosave tracked files")
                       ((> len 1) (format "autosave %s files" len))
                       (t (concat "autosave "
                                  (file-relative-name (car files)
                                                      (magit-toplevel)))))
                 msg)))
    (unless (equal parent wipref)
      (setq start-msg (concat "restart autosaving " start-msg))
      (magit-update-ref wipref start-msg
                        (magit-git-string "commit-tree" "--no-gpg-sign"
                                          "-p" parent "-m" start-msg
                                          (concat parent "^{tree}")))
      (setq parent wipref))
    (magit-update-ref wipref msg
                      (magit-git-string "commit-tree" "--no-gpg-sign"
                                        "-p" parent "-m" msg tree))))

(defun magit-wip-get-ref ()
  (let ((ref (or (magit-git-string "symbolic-ref" "HEAD") "HEAD")))
    (when (magit-rev-verify ref)
      ref)))

(defun magit-wip-get-parent (ref wipref)
  (if (and (magit-rev-verify wipref)
           (equal (magit-git-string "merge-base" wipref ref)
                  (magit-rev-verify ref)))
      wipref
    ref))

;;; Log

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
                 (-snoc (magit-list-local-branch-names) "HEAD")
                 nil t nil 'magit-revision-history
                 (or (magit-branch-at-point)
                     (magit-get-current-branch)
                     "HEAD")))
          (magit-log-arguments)
          (list (prefix-numeric-value current-prefix-arg))))
  (unless (equal branch "HEAD")
    (setq branch (concat "refs/heads/" branch)))
  (magit-log (nconc (list branch)
                    (magit-wip-log-get-tips
                     (concat magit-wip-namespace "wtree/" branch)
                     (abs count))
                    (and (>= count 0)
                         (magit-wip-log-get-tips
                          (concat magit-wip-namespace "index/" branch)
                          (abs count))))
             args files))

(defun magit-wip-log-get-tips (wipref count)
  (-when-let (reflog (magit-git-lines "reflog" wipref))
    (let (tips)
      (while (and reflog (> count 1))
        (setq reflog (cl-member "^[^ ]+ [^:]+: restart autosaving"
                                reflog :test #'string-match-p))
        (when (and (cadr reflog)
                   (string-match "^[^ ]+ \\([^:]+\\)" (cadr reflog)))
          (push (match-string 1 (cadr reflog)) tips))
        (setq reflog (cddr reflog))
        (cl-decf count))
      (cons wipref (nreverse tips)))))

;;; magit-wip.el ends soon
(provide 'magit-wip)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-wip.el ends here
