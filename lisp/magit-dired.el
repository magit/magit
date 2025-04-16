;;; magit-dired.el --- Dired support for Magit  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2025 The Magit Project Contributors

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

;; Dired support for Magit.

;;; Code:

(require 'magit)

;; For `magit-do-async-shell-command'.
(declare-function dired-read-shell-command "dired-aux" (prompt arg files))

;;; Open Dired from Magit

;;;###autoload
(defun magit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in another window.  If there
is no file at point, then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window
              (and-let* ((file (magit-file-at-point)))
                (expand-file-name (if (file-directory-p file)
                                      (file-name-as-directory file)
                                    file)))))

;;; Commands for Dired Buffers

;;;###autoload
(defun magit-dired-stage ()
  "In Dired, staged all marked files or the file at point."
  (interactive)
  (magit-stage-files (dired-get-marked-files)))

;;;###autoload
(defun magit-dired-unstage ()
  "In Dired, unstaged all marked files or the file at point."
  (interactive)
  (magit-unstage-files (dired-get-marked-files)))

;;;###autoload
(defun magit-dired-log (&optional follow)
  "In Dired, show log for all marked files or the directory if none are marked."
  (interactive "P")
  (if-let ((topdir (magit-toplevel default-directory)))
      (let ((args (car (magit-log-arguments)))
            (files (or (dired-get-marked-files nil 'marked)
                       (list default-directory))))
        (when (and follow
                   (not (member "--follow" args))
                   (not (cdr files)))
          (push "--follow" args))
        (magit-log-setup-buffer
         (list (or (magit-get-current-branch) "HEAD"))
         args
         (let ((default-directory topdir))
           (mapcar #'file-relative-name files))
         magit-log-buffer-file-locked))
    (magit--not-inside-repository-error)))

;;;###autoload
(defun magit-dired-am-apply-patches (repo &optional arg)
  "In Dired, apply the marked (or next ARG) files as patches.
If inside a repository, then apply in that.  Otherwise prompt
for a repository."
  (interactive (list (or (magit-toplevel)
                         (magit-read-repository t))
                     current-prefix-arg))
  (let ((files (dired-get-marked-files nil arg nil nil t)))
    (magit-status-setup-buffer repo)
    (magit-am-apply-patches files)))

;;; Miscellaneous Commands

;;;###autoload
(defun magit-do-async-shell-command (file)
  "Open FILE with `dired-do-async-shell-command'.
Interactively, open the file at point."
  (interactive (list (or (magit-file-at-point)
                         (magit-read-file "Act on file"))))
  (require 'dired-aux)
  (dired-do-async-shell-command
   (dired-read-shell-command "& on %s: " current-prefix-arg (list file))
   nil (list file)))

;;; _
(provide 'magit-dired)
;;; magit-dired.el ends here
