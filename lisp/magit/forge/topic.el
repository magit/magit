;;; magit/forge/topic.el --- forge topics support  -*- lexical-binding: t -*-

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
(require 'magit/forge/post)

;;; Options

(defvar magit--topic-limit 20) ; TODO fancier

;;; Faces

(defface magit-topic-unread
  '((t :inherit bold))
  "Face used for title unread topics."
  :group 'magit-faces)

(defface magit-topic-closed
  '((t :inherit magit-dimmed))
  "Face used for title of unread topics."
  :group 'magit-faces)

(defface magit-topic-open
  '((t :inherit default))
  "Face used for title of open topics."
  :group 'magit-faces)

(defface magit-topic-merged
  '((t :inherit magit-dimmed))
  "Face used for number of merged topics."
  :group 'magit-faces)

(defface magit-topic-unmerged
  '((t :inherit magit-dimmed :slant italic))
  "Face used for number of unmerged topics."
  :group 'magit-faces)

;;; Class

(defclass magit-forge-topic (magit-forge-object) () :abstract t)

;;; Core

(cl-defmethod magit-forge-get-project ((topic magit-forge-topic))
  "Return the object for the project that TOPIC belongs to."
  (closql-get (magit-db)
              (oref topic project)
              'magit-forge-project))

;;; Utilities

(cl-defmethod magit-forge--format-url ((topic magit-forge-topic) slot)
  (magit-forge--format-url (magit-forge-get-project topic) slot
                           `((?i . ,(oref topic number)))))

(defun magit-forge--sanitize-string (string)
  (replace-regexp-in-string "\r\n" "\n" string t t))

;;; Sections

(defun magit-forge-highlight-post (section &optional selection)
  (when (eq (oref section type) 'post)
    (dolist (section (or selection (list section)))
      (magit-section-make-overlay
       (oref section start)
       (or (oref section content)
           (oref section end))
       'magit-diff-hunk-heading-highlight))
    nil))

;;; Mode

(define-derived-mode magit-forge-topic-mode magit-mode "View Topic"
  "View a forge issue or pull-request.")

(defun magit-forge-topic-refresh-buffer (topic)
  (magit-set-header-line-format
   (format "#%s: %s"
           (oref topic number)
           (oref topic title)))
  (magit-insert-section (topicbuf)
    (dolist (post (cons topic (oref topic posts)))
      (with-slots (author created body) post
        (magit-insert-section (post post)
          (let ((heading
                 (format
                  "%s %s\n"
                  (propertize author  'face 'bold)
                  (propertize created 'face 'italic))))
            (add-face-text-property 0 (length heading)
                                    'magit-diff-hunk-heading t heading)
            (magit-insert-heading heading))
          (insert (magit-forge--fontify-markdown body) "\n\n"))))))

(defun magit-forge-topic-buffer-name (_mode topic)
  (with-slots (owner name)
      (magit-forge-get-project topic)
    (format "*%s/%s #%d*" owner name (oref topic number))))

(defun magit-forge--fontify-markdown (text)
  (with-temp-buffer
    (delay-mode-hooks
      (if (fboundp 'gfm-mode)
          (gfm-mode)
        (text-mode)))
    (insert text)
    (font-lock-ensure)
    (fill-region (point-min) (point-max))
    (buffer-string)))

;;; _
(provide 'magit/forge/topic)
;;; magit/forge/topic.el ends here
