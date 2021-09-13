;;; magit-diff.el --- inspect Git diffs  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2021  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Mark Dawson <markgdawson@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; This library implements support for looking at Git diffs and
;; commits.

;;; Code:

(require 'magit-core)

(defun magit-smerge--goto-line (line)
  "Goto LINE in current buffer."
  (widen)
  (goto-char (point-min))
  (forward-line (- line 1)))

(defun magit-smerge--funcall-at-buffer-point (fn)
  "Call function FN from buffer location at point and save file.
User will be prompted to save before running FN if the file has modifications."
  (save-window-excursion
    (let ((file (magit-file-at-point t t))
          (line (magit-diff-hunk-line (magit-diff-visit--hunk) nil)))
      (with-current-buffer (find-file-noselect file)
        (if (buffer-modified-p (current-buffer))
            (user-error "Buffer %s is already modified"
                        (buffer-name (current-buffer))))

        (magit-smerge--goto-line line)
        (funcall fn)
        (save-buffer)))))

;;;###autoload
(defun magit-smerge-keep-upper ()
  "Call `smerge-keep-upper` from hunk at point in magit diff buffer.
File modifications will be saved if file is unmodified, otherwise the user will be promopted."
  (interactive)
  (magit-smerge--funcall-at-buffer-point #'smerge-keep-upper)
  (magit-refresh))

;;;###autoload
(defun magit-smerge-keep-lower ()
  "Call `smerge-keep-lower` from hunk at point in magit diff buffer.
File modifications will be saved if file is unmodified, otherwise the user will be promopted."
  (interactive)
  (magit-smerge--funcall-at-buffer-point #'smerge-keep-lower)
  (magit-refresh))

;;;###autoload
(defun magit-smerge-setup ()
  "Setup magit-smerge keybindings."
  (interactive)
  (define-key magit-hunk-section-map (kbd "C-c ^ l") #'magit-smerge-keep-lower)
  (define-key magit-hunk-section-map (kbd "C-c ^ u") #'magit-smerge-keep-upper))

;;; _
(provide 'magit-smerge)
;;; magit-smerge.el ends here
