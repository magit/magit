;;; magit-extras.el --- additional functionality for Magit

;; Copyright (C) 2008-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Package: magit

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

;; Additional functionality for Magit.

;;; Code:

(require 'magit)

(defgroup magit-extras nil
  "Additional functionality for Magit."
  :group 'magit-extensions)

;;; Commit Mark

;; Add this to your init.el file:
;;   (define-key magit-mode-map "." 'magit-mark-item)
;;   (define-key magit-mode-map "." 'magit-diff-with-mark)
;;   (add-hook 'magit-mode-refresh-buffer-hook
;;             'magit-refresh-marked-commits-in-buffer))

(defface magit-item-mark '((t :inherit highlight))
  "Face for highlighting marked item."
  :group 'magit-extras)

(defvar magit-marked-commit nil)

(defvar-local magit-mark-overlay nil)
(put 'magit-mark-overlay 'permanent-local t)

(defun magit-mark-item (&optional unmark)
  "Mark the commit at point.
Some commands act on the marked commit by default or use it as
default when prompting for a commit."
  (interactive "P")
  (if unmark
      (setq magit-marked-commit nil)
    (magit-section-action mark (info)
      (commit (setq magit-marked-commit
                    (if (equal magit-marked-commit info) nil info)))))
  (magit-refresh-marked-commits)
  (run-hooks 'magit-mark-commit-hook))

(defun magit-refresh-marked-commits ()
  (magit-map-magit-buffers #'magit-refresh-marked-commits-in-buffer))

(defun magit-refresh-marked-commits-in-buffer ()
  (unless magit-mark-overlay
    (setq magit-mark-overlay (make-overlay 1 1))
    (overlay-put magit-mark-overlay 'face 'magit-item-mark))
  (delete-overlay magit-mark-overlay)
  (magit-map-sections
   (lambda (section)
     (when (and (eq (magit-section-type section) 'commit)
                (equal (magit-section-info section)
                       magit-marked-commit))
       (move-overlay magit-mark-overlay
                     (magit-section-beginning section)
                     (magit-section-end section)
                     (current-buffer))))
   magit-root-section))

(defun magit-diff-with-mark (range)
  "Show changes between the marked commit and the one at point.
If there is no commit at point, then prompt for one."
  (interactive
   (let* ((marked (or magit-marked-commit (user-error "No commit marked")))
          (current (magit-get-current-branch))
          (is-current (string= (magit-name-rev marked) current))
          (commit (or (magit-guess-branch)
                      (magit-read-rev
                       (format "Diff marked commit %s with" marked)
                       (unless is-current current)
                       current))))
     (list (concat marked ".." commit))))
  (magit-diff range))

(provide 'magit-extras)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-extras.el ends here
