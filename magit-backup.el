;;; magit-backup.el --- automatically create backup stashes

;; Copyright (C) 2008-2015  The Magit Project Developers
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

;;; Commentary:

;; This library implements support for creating stashes that serve
;; as backups.  When `magit-backup-mode' is enabled, then many Magit
;; command that may lose data, try to mitigate the risk by first
;; saving a stash to `magit-backup-ref'.

;; Note that "many" is not the same as "all" and that "Magit" is not
;; the same as "any way in which one can interact with a repository".
;; Generally speaking only changes which can be saved by creating a
;; stash, are being saved by creating a stash - which is exactly the
;; sort of tautology you would in the description of any backup tool.

;; You might also be interested in `magit-wip-save-mode', which is
;; defined elsewhere, and which covers one of the many cases where
;; using `magit-backup-mode' would not have saved you.

;;; Code:

(require 'magit)
(require 'magit-stash)

;;; Options

(defgroup magit-backup nil
  "Automatically create stashes to backup uncommitted changes."
  :group 'magit-extensions)

(defcustom magit-backup-mode-lighter " MBkp"
  "Lighter for Magit-Backup mode."
  :group 'magit-backup
  :type 'string)

(defcustom magit-backup-untracked nil
  "Whether to include untracked files in backup stashes."
  :group 'magit-backup
  :type 'boolean)

(defcustom magit-backup-ref "refs/backup"
  "Reference used to store backup stashes."
  :group 'magit-backup
  :type 'string)

;;; Commands

;;;###autoload
(defun magit-backup ()
  "Create a backup stash."
  (interactive)
  (magit-stash-save (concat "WIP on " (magit-stash-summary))
                    t t magit-backup-untracked
                    t t nil magit-backup-ref))

;;;###autoload
(defun magit-backup-list ()
  "List all backup stashes in a buffer."
  (interactive)
  (magit-mode-setup magit-stashes-buffer-name-format nil
                    #'magit-stashes-mode
                    #'magit-stashes-refresh-buffer
                    magit-backup-ref "Backups:"))


;;; Mode

;;;###autoload
(define-minor-mode magit-backup-mode
  "Automatically create stashes to backup uncommitted changes."
  :lighter magit-backup-mode-lighter
  :global t
  :group 'magit
  :group 'magit-backup)

(defun magit-backup-maybe ()
  "If `magit-backup-mode' is enabled, create a backup stash."
  (when (and magit-backup-mode
             (magit-rev-parse "--verify" "HEAD"))
    (magit-stash-save (concat "WIP on " (magit-stash-summary))
                      (not (magit-anything-unmerged-p))
                      t magit-backup-untracked
                      nil t t magit-backup-ref)))

;;;###autoload
(defun magit-insert-backups ()
  "Insert `stashes' section showing reflog for `magit-backup-ref'."
  (magit-insert-stashes magit-backup-ref "Backups:"))

;;; magit-backup.el ends soon
(provide 'magit-backup)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-backup.el ends here
