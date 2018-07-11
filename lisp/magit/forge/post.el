;;; magit/forge/post.el --- forge post support    -*- lexical-binding: t -*-

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

;;; Code:

(require 'markdown-mode nil t)

(require 'magit/forge)

;;; Class

(defclass magit-forge-post (magit-forge-object) () :abstract t)

;;; Sections

(defun magit-post-at-point ()
  (magit-section-when (issue pullreq post)))

(defvar magit-post-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'magit-forge-edit-post)
    map))

;;; Mode

(defvar magit-forge-post-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'magit-forge-post-submit)
    (define-key map (kbd "C-c C-k") 'magit-forge-post-cancel)
    map))

(define-minor-mode magit-forge-post-mode
  "Edit a forge issue, pull-request or post.")

(defvar-local magit-forge-buffer-post nil)

;;; Commands

(defun magit-forge-edit-post (post)
  (interactive (list (magit-post-at-point)))
  (let ((file (magit-git-dir (convert-standard-filename
                              (concat "magit/posts/" (oref post id))))))
    (when (file-exists-p file)
      (delete-file file))
    (if-let ((buf (find-buffer-visiting file)))
        (pop-to-buffer buf)
      (make-directory (file-name-directory file) t)
      (find-file-other-window file)
      (erase-buffer) ; TODO offer to use draft
      (save-excursion
        (insert (oref post body)))
      (if (fboundp 'gfm-mode)
          (gfm-mode)
        (text-mode))
      (visual-line-mode))
    (setq magit-forge-buffer-post post)
    (magit-forge-post-mode)))

(defun magit-forge-post-submit ()
  (interactive)
  (cl-typecase magit-forge-buffer-post
    (magit-forge-post (message "post"))))

(defun magit-forge-post-cancel ()
  (interactive)
  (kill-buffer))

;;; _
(provide 'magit/forge/post)
;;; magit/forge/post.el ends here
