;;; magit/forge/issue.el --- forge issue support  -*- lexical-binding: t -*-

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
(require 'magit/forge/post)
(require 'magit/forge/topic)

;;; Classes

(defclass magit-forge-issue (magit-forge-topic)
  ((closql-table         :initform issue)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(desc number)])
   (closql-foreign-key   :initform project)
   (closql-foreign-table :initform project)
   (closql-class-prefix  :initform "magit-forge-")
   (id                   :initarg :id)
   (project              :initarg :project)
   (number               :initarg :number)
   (state                :initarg :state)
   (author               :initarg :author)
   (title                :initarg :title)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (closed               :initarg :closed)
   (unread-p             :initarg :unread-p :initform nil)
   (locked-p             :initarg :locked-p)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees)
   (cards) ; projectsCards
   (edits) ; userContentEdits
   (labels)
   (participants)
   (posts                :closql-class magit-forge-issue-post)
   (reactions)
   (timeline)
   ))

(defclass magit-forge-issue-post (magit-forge-post)
  ((closql-table         :initform issue-post)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform issue)
   (closql-foreign-table :initform issue)
   (closql-class-prefix  :initform "magit-forge-issue-")
   (id                   :initarg :id)
   (issue                :initarg :issue)
   (number               :initarg :number)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   ))

;;; Query

(cl-defmethod magit-forge--issue-id ((prj magit-forge-project) number)
  (format "%s:i%s" (oref prj id) number))

(cl-defmethod magit-forge-get-issue ((prj magit-forge-project) number
                                     &optional demand)
  (or (closql-get (magit-db)
                  (magit-forge--issue-id prj number)
                  'magit-forge-issue)
      (and demand
           (progn (magit-forge--pull-issues prj number)
                  (magit-forge-get-issue prj number)))))

(cl-defmethod magit-forge-get-issue ((number integer))
  (when-let ((prj (magit-forge-get-project nil)))
    (magit-forge-get-issue prj number)))

(cl-defmethod magit-forge-list-issues ((prj magit-forge-project) &optional limit)
  (mapcar (lambda (row)
            (closql--remake-instance 'magit-forge-issue (magit-db) row))
          (if limit
              (magit-sql [:select * :from issue
                          :where (= project $s1)
                          :order-by [(desc number)]
                          :limit $s2]
                         (oref prj id) limit)
            (magit-sql [:select * :from issue
                        :where (= project $s1)
                        :order-by [(desc number)]]
                       (oref prj id)))))

;;; Utilities

(defun magit-read-issue (prompt)
  (let* ((default (magit-issue-at-point))
         (prj     (magit-forge-get-project (or default t)))
         (format  (lambda (topic)
                    (format "%s  %s"
                            (oref topic number)
                            (oref topic title))))
         (choices (oref prj issues))
         (choice  (magit-completing-read
                   prompt
                   (mapcar format choices)
                   nil nil nil nil
                   (and default (funcall format default))))
         (number  (and (string-match "\\([0-9]+\\)" choice)
                       (string-to-number (match-string 1 choice)))))
    (and number
         (magit-forge-get-issue prj number))))

(defun magit-issue-at-point ()
  (magit-section-when issue))

;;; _
(provide 'magit/forge/issue)
;;; magit/forge/issue.el ends here
