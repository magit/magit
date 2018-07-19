;;; magit/forge/notify.el --- forge notify support  -*- lexical-binding: t -*-

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

(require 'magit/forge)

;;; Class

(defclass magit-forge-notification (magit-forge-object)
  ((closql-class-prefix       :initform "magit-")
   (closql-table              :initform notification)
   (closql-primary-key        :initform id)
   (closql-order-by           :initform [(desc id)])
   (id                        :initarg :id)
   (project                   :initarg :project)
   (forge                     :initarg :forge)
   (reason                    :initarg :reason)
   (unread-p                  :initarg :unread-p)
   (last-read                 :initarg :last-read)
   (updated                   :initarg :updated)
   (title                     :initarg :title)
   (type                      :initarg :type)
   (topic                     :initarg :topic)
   (url                       :initarg :url)))

;;; Core

(cl-defmethod magit-forge-get-project ((notify magit-forge-notification))
  "Return the object for the project that NOTIFY belongs to."
  (when-let ((id (oref notify project)))
    (closql-get (magit-db) id 'magit-forge-project)))

;;; Utilities

(defun magit-forge--list-notifications-all ()
  (closql-query (magit-db) nil nil 'magit-forge-notification))

(defun magit-forge--list-notifications-unread ()
  (mapcar (lambda (row)
            (closql--remake-instance 'magit-forge-notification (magit-db) row))
          (magit-sql [:select * :from notification
                      :where (notnull unread-p)
                      :order-by [(desc id)]])))

;;; Sections

(defun magit-insert-notifications ()
  (when-let ((ns (magit-forge--list-notifications-all)))
    (magit-insert-section (notifications)
      (magit-insert-heading "Notifications:")
      (pcase-dolist (`(,_ . ,ns) (--group-by (oref it project) ns))
        (let ((prj (magit-forge-get-project (car ns))))
          (magit-insert-section (project prj)
            (magit-insert-heading
              (propertize (format "%s/%s:" (oref prj owner) (oref prj name))
                          'face 'bold))
            (dolist (notify ns)
              (with-slots (type topic title url unread-p) notify
                (pcase type
                  ('issue
                   (magit-insert-issue (magit-forge-get-issue prj topic)))
                  ('pullreq
                   (magit-insert-pullreq (magit-forge-get-pullreq prj topic)))
                  ('commit
                   (magit-insert-section (ncommit nil) ; !commit
                     (string-match "[^/]*\\'" url)
                     (insert
                      (format "%s %s\n"
                              (propertize (substring (match-string 0 url)
                                                     0 (magit-abbrev-length))
                                          'face 'magit-hash)
                              (magit-log-propertize-keywords
                               nil (propertize title 'face
                                               (if unread-p
                                                   'magit-topic-unread
                                                 'magit-topic-open)))))))
                  (_
                   ;; The documentation does not mention what "types"
                   ;; exist.  Make it obvious that this is something
                   ;; we do not know how to handle properly yet.
                   (magit-insert-section (notification notify)
                     (insert (propertize (format "(%s) %s\n" type title)
                                         'face 'error)))))))
            (insert ?\n)))))))

;;; Mode

(defvar magit-forge-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-forge-notifications-mode'.")

(define-derived-mode magit-forge-notifications-mode magit-mode "Magit Dashboard"
  ""
  :group 'magit-refs
  (hack-dir-local-variables-non-file-buffer))

(defun magit-forge-notifications-refresh-buffer ()
  (magit-insert-notifications))

;;;###autoload
(defun magit-forge-notifications ()
  (interactive)
  (magit-mode-setup #'magit-forge-notifications-mode))

;;; _
(provide 'magit/forge/notify)
;;; magit/forge/notify.el ends here
