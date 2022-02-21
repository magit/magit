;;; magit-imenu.el --- Integrate Imenu in magit major modes  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2022  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Damien Cassou <damien@cassou.me>
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

;; Emacs' major modes can facilitate navigation in their buffers by
;; supporting Imenu.  In such major modes, launching Imenu (M-x imenu)
;; makes Emacs display a list of items (e.g., function definitions in
;; a programming major mode).  Selecting an item from this list moves
;; point to this item.

;;; Code:

(require 'magit)
(require 'git-rebase)

;;; Submodule list mode

;;;###autoload
(defun magit-imenu--submodule-prev-index-position-function ()
  "Move point to previous line in magit-submodule-list buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (unless (bobp)
    (forward-line -1)))

;;;###autoload
(defun magit-imenu--submodule-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (elt (tabulated-list-get-entry) 0))

;;; Repolist mode

;;;###autoload
(defun magit-imenu--repolist-prev-index-position-function ()
  "Move point to previous line in magit-repolist buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (unless (bobp)
    (forward-line -1)))

;;;###autoload
(defun magit-imenu--repolist-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (let ((entry (tabulated-list-get-entry)))
    (format "%s (%s)"
            (elt entry 0)
            (elt entry (1- (length entry))))))

;;; Rebase mode

;;;###autoload
(defun magit-imenu--rebase-prev-index-position-function ()
  "Move point to previous commit in git-rebase buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (catch 'found
    (while (not (bobp))
      (git-rebase-backward-line)
      (when (git-rebase-line-p)
        (throw 'found t)))))

;;;###autoload
(defun magit-imenu--rebase-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

;;; _
(provide 'magit-imenu)
;;; magit-imenu.el ends here
