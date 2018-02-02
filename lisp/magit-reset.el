;;; magit-reset.el --- reset fuctionality  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
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

;; This library implements reset commands.

;;; Code:

(require 'magit)

;;;###autoload (autoload 'magit-reset-popup "magit" nil t)
(magit-define-popup magit-reset-popup
  "Popup console for reset commands."
  :man-page "git-reset"
  :actions '((?m "reset mixed  (HEAD and index)"         magit-reset-head)
             (?s "reset soft   (HEAD only)"              magit-reset-soft)
             (?h "reset hard   (HEAD, index, and files)" magit-reset-hard)
             (?i "reset index  (index only)"             magit-reset-index) nil
             (?f "reset a file"                          magit-file-checkout))
  :max-action-columns 1)

;;;###autoload
(defun magit-reset-index (commit)
  "Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT refers to the
head this effectively unstages all changes.
\n(git reset COMMIT .)"
  (interactive (list (magit-read-branch-or-commit "Reset index to")))
  (magit-reset-internal nil commit "."))

;;;###autoload
(defun magit-reset (commit &optional hard)
  "Reset the head and index to COMMIT, but not the working tree.
With a prefix argument also reset the working tree.
\n(git reset --mixed|--hard COMMIT)"
  (interactive (list (magit-read-branch-or-commit
                      (if current-prefix-arg
                          "Hard reset to"
                        "Reset head to"))
                     current-prefix-arg))
  (magit-reset-internal (if hard "--hard" "--mixed") commit))

;;;###autoload
(defun magit-reset-head (commit)
  "Reset the head and index to COMMIT, but not the working tree.
\n(git reset --mixed COMMIT)"
  (interactive (list (magit-read-branch-or-commit "Reset head to")))
  (magit-reset-internal "--mixed" commit))

;;;###autoload
(defun magit-reset-soft (commit)
  "Reset the head to COMMIT, but not the index and working tree.
\n(git reset --soft REVISION)"
  (interactive (list (magit-read-branch-or-commit "Soft reset to")))
  (magit-reset-internal "--soft" commit))

;;;###autoload
(defun magit-reset-hard (commit)
  "Reset the head, index, and working tree to COMMIT.
\n(git reset --hard REVISION)"
  (interactive (list (magit-read-branch-or-commit "Hard reset to")))
  (magit-reset-internal "--hard" commit))

(defun magit-reset-internal (arg commit &optional path)
  (when (and (not (member arg '("--hard" nil)))
             (equal (magit-rev-parse commit)
                    (magit-rev-parse "HEAD~")))
    (with-temp-buffer
      (magit-git-insert "show" "-s" "--format=%B" "HEAD")
      (when git-commit-major-mode
        (funcall git-commit-major-mode))
      (git-commit-setup-font-lock)
      (git-commit-save-message)))
  (let ((cmd (if (and (equal commit "HEAD") (not arg)) "unstage" "reset")))
    (magit-wip-commit-before-change nil (concat " before " cmd))
    (magit-run-git "reset" arg commit "--" path)
    (when (equal cmd "unstage")
      (magit-wip-commit-after-apply nil " after unstage"))))

(provide 'magit-reset)
;;; magit-reset.el ends here
