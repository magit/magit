;;; magit-stgit.el --- StGit plug-in for Magit

;; Copyright (C) 2011-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Lluís Vilanova <vilanova@ac.upc.edu>
;; Keywords: vc tools
;; Package: magit-stgit
;; Package-Requires: ((cl-lib "0.3") (magit "1.3.0"))

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
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides very basic support for StGit.

;; When `magit-stgit-mode' is turned on the current patch series is
;; displayed in the status buffer.  Additionally a few Emacs commands
;; are defined that wrap around StGit commands.  These commands are
;; also available as "section actions".

;; If you are looking for full fledged StGit support in Emacs, then
;; have a look at `stgit.el' which is distributed with StGit.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-stgit-executable "stg"
  "The name of the StGit executable."
  :group 'magit
  :type 'string)

;;; Faces

(defface magit-stgit-patch
  '((t :inherit magit-log-sha1))
  "Face for name of a stgit patch."
  :group 'magit-faces)

(defface magit-stgit-current
  '((t :inherit magit-log-sha1))
  "Face for the current stgit patch."
  :group 'magit-faces)

(defface magit-stgit-applied
  '((t :inherit magit-cherry-equivalent))
  "Face for an applied stgit patch."
  :group 'magit-faces)

(defface magit-stgit-unapplied
  '((t :inherit magit-cherry-unmatched))
  "Face for an unapplied stgit patch."
  :group 'magit-faces)

(defface magit-stgit-empty
  '((t :inherit magit-diff-del))
  "Face for an empty stgit patch."
  :group 'magit-faces)

(defface magit-stgit-hidden
  '((t :inherit magit-diff-empty))
  "Face for an hidden stgit patch."
  :group 'magit-faces)

;;; Variables

(defvar magit-stgit-patch-buffer-name "*magit-stgit-patch*"
  "Name of buffer used to display a stgit patch.")

(defvar magit-stgit-patch-history nil
  "Input history for `magit-stgit-read-patch'.")

;;; Utilities

(defun magit-run-stgit (&rest args)
  (apply #'magit-call-process magit-stgit-executable args)
  (magit-refresh))

(defun magit-stgit-lines (&rest args)
  (with-temp-buffer
    (apply 'process-file magit-stgit-executable nil (list t nil) nil args)
    (split-string (buffer-string) "\n" 'omit-nulls)))

(defun magit-stgit-read-patch (prompt &optional require-match)
  (magit-completing-read prompt (magit-stgit-lines "series" "--noprefix")
                         nil require-match
                         nil 'magit-read-rev-history))

;;; Commands

;;;###autoload
(defun magit-stgit-refresh (&optional patch)
  "Refresh a StGit patch."
  (interactive
   (list (magit-stgit-read-patch "Refresh patch (default top)")))
  (if patch
      (magit-run-stgit "refresh" "-p" patch)
    (magit-run-stgit "refresh")))

;;;###autoload
(defun magit-stgit-repair ()
  "Repair StGit metadata if branch was modified with git commands.
In the case of Git commits these will be imported as new patches
into the series."
  (interactive)
  (message "Repairing series...")
  (magit-run-stgit "repair")
  (message "Repairing series...done"))

;;;###autoload
(defun magit-stgit-rebase ()
  "Rebase a StGit patch series."
  (interactive)
  (let ((remote (magit-get-current-remote))
        (branch (magit-get-current-branch)))
    (if (not (and remote branch))
        (error "Branch has no upstream")
      (when (y-or-n-p "Update remote first? ")
        (message "Updating remote...")
        (magit-run-git-async "remote" "update" remote)
        (message "Updating remote...done"))
      (magit-run-stgit "rebase" (format "remotes/%s/%s" remote branch)))))

;;;###autoload
(defun magit-stgit-discard (patch)
  "Discard a StGit patch."
  (interactive (list (magit-stgit-read-patch "Discard patch" t)))
  (magit-run-stgit "delete" patch))

;;;###autoload
(defun magit-stgit-show (patch)
  "Show diff of a StGit patch."
  (interactive (list (magit-stgit-read-patch "Show patch" t)))
  (magit-mode-setup magit-stgit-patch-buffer-name
                    #'pop-to-buffer
                    #'magit-commit-mode
                    #'magit-stgit-refresh-patch-buffer
                    patch))

(defun magit-stgit-refresh-patch-buffer (patch)
  (magit-cmd-insert-section (stgit-patch)
      #'magit-wash-commit
    magit-stgit-executable "show" patch))

;;; Mode

(defvar magit-stgit-mode-lighter " Stg")

;;;###autoload
(define-minor-mode magit-stgit-mode
  "StGit support for Magit"
  :lighter magit-stgit-mode-lighter
  :require 'magit-stgit
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (if magit-stgit-mode
      (magit-add-section-hook 'magit-status-sections-hook
                              'magit-insert-stgit-series
                              'magit-insert-stashes t t)
    (remove-hook 'magit-status-sections-hook 'magit-insert-stgit-series t))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-stgit ()
  "Unconditionally turn on `magit-stgit-mode'."
  (magit-stgit-mode 1))

(magit-add-action-clauses (item info "visit")
  ((stgit-patch)
   (magit-stgit-show info)))

(magit-add-action-clauses (item info "apply")
  ((stgit-patch)
   (magit-run-stgit "goto" info)))

(magit-add-action-clauses (item info "discard")
  ((stgit-patch)
   (when (yes-or-no-p (format "Discard patch `%s'? " info))
     (magit-stgit-discard info))))

(easy-menu-define magit-stgit-extension-menu nil
  "StGit extension menu"
  '("StGit" :visible magit-stgit-mode
    ["Refresh patch" magit-stgit-refresh
     :help "Refresh the contents of a patch in an StGit series"]
    ["Repair" magit-stgit-repair
     :help "Repair StGit metadata if branch was modified with git commands"]
    ["Rebase series" magit-stgit-rebase
     :help "Rebase an StGit patch series"]))

(easy-menu-add-item 'magit-mode-menu '("Extensions")
                    magit-stgit-extension-menu)

;;; Series Section

(defconst magit-stgit-patch-re
  "^\\(.\\)\\([-+>!]\\) \\([^ ]+\\) +# \\(.*\\)$")

(defun magit-insert-stgit-series ()
  (when magit-stgit-mode
    (magit-cmd-insert-section (series "Patch series:")
        (apply-partially 'magit-wash-sequence 'magit-stgit-wash-patch)
      magit-stgit-executable "series" "--all" "--empty" "--description")))

(defun magit-stgit-wash-patch ()
  (looking-at magit-stgit-patch-re)
  (magit-bind-match-strings (empty state patch msg)
    (delete-region (point) (point-at-eol))
    (magit-with-section (section stgit-patch patch)
      (setf (magit-section-info section) patch)
      (insert (propertize state 'face
                          (cond ((equal state ">") 'magit-stgit-current)
                                ((equal state "+") 'magit-stgit-applied)
                                ((equal state "-") 'magit-stgit-unapplied)
                                ((equal state "!") 'magit-stgit-hidden)
                                (t (error "Unknown stgit patch state: %s"
                                          state))))
              (propertize empty 'face 'magit-stgit-empty) " "
              (propertize patch 'face 'magit-stgit-patch) " "
              msg)
      (forward-line))))

(provide 'magit-stgit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-stgit.el ends here
