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
  "Face used for unread topics."
  :group 'magit-faces)

(defface magit-topic-closed
  '((t :inherit magit-dimmed))
  "Face used for unread topics."
  :group 'magit-faces)

(defface magit-topic-open
  '((t :inherit default))
  "Face used for open topics."
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

;;; _
(provide 'magit/forge/topic)
;;; magit/forge/topic.el ends here
