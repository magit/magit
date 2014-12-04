;;; magit-backup.el --- automatically create backup stashes

;; Copyright (C) 2008-2014  The Magit Project Developers
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

(defun magit-backup ()
  "Create a backup stash."
  (interactive)
  (magit-stash-save (concat "WIP on " (magit-stash-summary))
                    t t magit-backup-untracked
                    t t nil magit-backup-ref))

(defun magit-backup-list ()
  "List all backup stashes in a buffer."
  (interactive)
  (magit-mode-setup magit-stashes-buffer-name-format nil
                    #'magit-stashes-mode
                    #'magit-stashes-refresh-buffer
                    magit-backup-ref "Backups:"))


;;; Mode

(define-minor-mode magit-backup-mode
  "Automatically create stashes to backup uncommitted changes."
  :lighter magit-backup-mode-lighter
  :init-value t
  :global t
  :group 'magit
  :group 'magit-backup)

(defun magit-maybe-backup ()
  (when (and magit-backup-mode
             (magit-rev-parse "--verify" "HEAD"))
    (magit-stash-save (concat "WIP on " (magit-stash-summary))
                      (not (magit-anything-unmerged-p))
                      t magit-backup-untracked
                      nil t t magit-backup-ref)))

(defun magit-insert-backups ()
  (magit-insert-stashes magit-backup-ref "Backups:"))

;;; magit-backup.el ends soon
(provide 'magit-backup)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-backup.el ends here
