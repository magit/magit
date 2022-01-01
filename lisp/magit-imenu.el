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

;; magit-imenu.el adds Imenu support to every major mode in Magit.

;;; Code:

(require 'magit)
(require 'git-rebase)

;;; Core

(defun magit-imenu--index-function (entry-types menu-types)
  "Return an alist of imenu entries in current buffer.

ENTRY-TYPES is a list of section types to be selected through
`imenu'.

MENU-TYPES is a list of section types containing elements of
ENTRY-TYPES.  Elements of MENU-TYPES are used to categorize
elements of ENTRY-TYPES.

This function is used as a helper for functions set as
`imenu-create-index-function'."
  ;; If `which-function-mode' is active, then the create-index
  ;; function is called at the time the major-mode is being enabled.
  ;; Modes that derive from `magit-mode' have not populated the buffer
  ;; at that time yet, so we have to abort.
  (when-let ((section (magit-current-section))
             (entries (make-hash-table :test 'equal)))
    (goto-char (point-max))
    (unless (oref section parent)
      (forward-line -1))
    (while (magit-section--backward-find
            (lambda ()
              (let* ((section (magit-current-section))
                     (type (oref section type))
                     (parent (oref section parent))
                     (parent-type (oref parent type)))
                (and (memq type entry-types)
                     (memq parent-type menu-types)))))
      (let* ((section (magit-current-section))
             (name (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
             (parent (oref section parent))
             (parent-title (buffer-substring-no-properties
                            (oref parent start)
                            (1- (oref parent content)))))
        (when (string-match " ([0-9]*)\\'" parent-title)
          (setq parent-title (substring parent-title 0 (match-beginning 0))))
        (puthash parent-title
                 (cons (cons name (point))
                       (gethash parent-title entries (list)))
                 entries)))
    (mapcar (lambda (menu-title)
              (cons menu-title (gethash menu-title entries)))
            (hash-table-keys entries))))

;;; Log mode

;;;###autoload
(defun magit-imenu--log-prev-index-position-function ()
  "Move point to previous line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (magit-section--backward-find
   (lambda ()
     (-contains-p '(commit stash)
                  (oref (magit-current-section) type)))))

;;;###autoload
(defun magit-imenu--log-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (save-match-data
    (looking-at "\\([^ ]+\\)[ *|]+\\(.+\\)$")
    (format "%s: %s"
            (match-string-no-properties 1)
            (match-string-no-properties 2))))

;;; Diff mode

;;;###autoload
(defun magit-imenu--diff-prev-index-position-function ()
  "Move point to previous file line in current buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (magit-section--backward-find
   (lambda ()
     (let ((section (magit-current-section)))
       (and (magit-file-section-p section)
            (not (equal (oref (oref section parent) type)
                        'diffstat)))))))

;;;###autoload
(defun magit-imenu--diff-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

;;; Status mode

;;;###autoload
(defun magit-imenu--status-create-index-function ()
  "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'."
  (magit-imenu--index-function
   '(file commit stash pullreq issue)
   '(unpushed unstaged unpulled untracked staged stashes pullreqs issues)))

;;; Refs mode

;;;###autoload
(defun magit-imenu--refs-create-index-function ()
  "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'."
  (magit-imenu--index-function
   '(branch commit tag)
   '(local remote tags)))

;;; Cherry mode

;;;###autoload
(defun magit-imenu--cherry-create-index-function ()
  "Return an alist of all imenu entries in current buffer.
This function is used as a value for
`imenu-create-index-function'."
  (magit-imenu--index-function
   '(commit)
   '(cherries)))

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

;;; Process mode

;;;###autoload
(defun magit-imenu--process-prev-index-position-function ()
  "Move point to previous process in magit-process buffer.
This function is used as a value for
`imenu-prev-index-position-function'."
  (magit-section--backward-find
   (lambda ()
     (eq (oref (magit-current-section) type) 'process))))

;;;###autoload
(defun magit-imenu--process-extract-index-name-function ()
  "Return imenu name for line at point.
This function is used as a value for
`imenu-extract-index-name-function'.  Point should be at the
beginning of the line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

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
