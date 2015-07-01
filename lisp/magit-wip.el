;;; magit-wip.el --- commit snapshots to work-in-progress refs

;; Copyright (C) 2010-2015  The Magit Project Contributors
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
(require 'format-spec)

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
    (let* ((default-directory (magit-toplevel))
           (file (file-relative-name buffer-file-name )))
      (magit-wip-commit-worktree it (list file)
                                 (if (called-interactively-p 'any)
                                     (format "wip-save %s after save" file)
                                   (format "autosave %s after save" file))))))

;;;###autoload
(define-minor-mode magit-wip-after-apply-mode
  "Commit to work-in-progress refs"
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
    (magit-wip-commit files msg)))

;;; Core

(defun magit-wip-commit (&optional files msg)
  "Commit all tracked files to the work-in-progress refs.

Non-interactivly, on behalf of `magit-wip-before-change-hook',
only commit changes to FILES using MSG as commit message."
  (interactive (list nil "wip-save tracked files"))
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
                               parent files msg))))

(defun magit-wip-commit-worktree (ref files msg)
  (let* ((wipref (concat magit-wip-namespace "wtree/" ref))
         (parent (magit-wip-get-parent ref wipref))
         (tree (magit-with-temp-index parent
                 (if files
                     (magit-call-git "add" "--" files)
                   (let ((default-directory (magit-toplevel)))
                     (magit-call-git "add" "-u" ".")))
                 (magit-git-string "write-tree"))))
    (when (magit-git-failure "diff-tree" "--quiet" parent tree "--" files)
      (magit-wip-update-wipref wipref tree parent files msg))))

(defun magit-wip-update-wipref (wipref tree parent files msg)
  (let ((len (length files)))
    (unless (and msg (not (= (aref msg 0) ?\s)))
      (setq msg (concat
                 (cond ((= len 0) "autosave tracked files")
                       ((> len 1) (format "autosave %s files" len))
                       (t (concat "autosave "
                                  (file-relative-name (car files)
                                                      (magit-toplevel)))))
                 msg)))
    (magit-reflog-enable wipref)
    (unless (equal parent wipref)
      (magit-call-git "update-ref" wipref "-m" "restart autosaving"
                      (magit-git-string "commit-tree" "-p" parent
                                        "-m" "restart autosaving"
                                        (concat parent "^{tree}")))
      (setq parent wipref))
    (magit-call-git "update-ref" wipref "-m" msg
                    (magit-git-string "commit-tree" tree
                                      "-p" parent "-m" msg))))

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

;;; magit-wip.el ends soon
(provide 'magit-wip)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-wip.el ends here
