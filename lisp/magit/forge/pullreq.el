;;; magit/forge/pullreq.el --- forge pullreq support  -*- lexical-binding: t -*-

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

(defclass magit-forge-pullreq (magit-forge-topic)
  ((closql-table         :initform pullreq)
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
   (merged               :initarg :merged)
   (unread-p             :initarg :unread-p :initform nil)
   (locked-p             :initarg :locked-p)
   (editable-p           :initarg :editable-p)
   (cross-repo-p         :initarg :cross-repo-p)
   (base-ref             :initarg :base-ref)
   (base-repo            :initarg :base-repo)
   (head-ref             :initarg :head-ref)
   (head-user            :initarg :head-user)
   (head-repo            :initarg :head-repo)
   (milestone            :initarg :milestone)
   (body                 :initarg :body)
   (assignees)
   (cards) ; projectsCards
   (commits)
   (edits) ; userContentEdits
   (labels)
   (participants)
   (posts                :closql-class magit-forge-pullreq-post)
   (reactions)
   (review-requests)
   (reviews)
   (timeline)
   ;; We don't use these fields:
   ;; includesCreatedEdit (huh?),
   ;; lastEditedAt (same as updatedAt?),
   ;; publishedAt (same as createdAt?),
   ;; activeLockReason, additions, authorAssociation, (baseRefName), baseRefOid,
   ;; bodyHTML, bodyText, canBeRebased, changedFiles, closed, createdViaEmail,
   ;; databaseId, deletions, editor, (headRefName), headRefOid, mergeCommit,
   ;; mergeStateStatus, mergeable, merged, mergedBy, permalink,
   ;; potentialMergeCommit,, reactionGroups, resourcePath, revertResourcePath,
   ;; revertUrl, url, viewer{*}
   ))

(defclass magit-forge-pullreq-post (magit-forge-post)
  ((closql-table         :initform pullreq-post)
   (closql-primary-key   :initform id)
   (closql-order-by      :initform [(asc number)])
   (closql-foreign-key   :initform pullreq)
   (closql-foreign-table :initform pullreq)
   (closql-class-prefix  :initform "magit-forge-pullreq-")
   (id                   :initarg :id)
   (pullreq              :initarg :pullreq)
   (number               :initarg :number)
   (author               :initarg :author)
   (created              :initarg :created)
   (updated              :initarg :updated)
   (body                 :initarg :body)
   (edits)
   (reactions)
   ;; We don't use these fields:
   ;; includesCreatedEdit (huh?),
   ;; lastEditedAt (same as updatedAt?),
   ;; publishedAt (same as createdAt?),
   ;; pullRequest (same as issue),
   ;; repository (use .pullreq.project),
   ;; authorAssociation, bodyHTML, bodyText, createdViaEmail,
   ;; editor, id, reactionGroups, resourcePath, url, viewer{*}
   ))

;;; Query

(cl-defmethod magit-forge--pullreq-id ((prj magit-forge-project) number)
  (format "%s:p%s" (oref prj id) number))

(cl-defmethod magit-forge-get-pullreq ((prj magit-forge-project) number
                                     &optional demand)
  (or (closql-get (magit-db)
                  (magit-forge--pullreq-id prj number)
                  'magit-forge-pullreq)
      (and demand
           (progn (magit-forge--pull-pullreqs prj number)
                  (magit-forge-get-pullreq prj number)))))

(cl-defmethod magit-forge-get-pullreq ((number integer))
  (when-let ((prj (magit-forge-get-project nil)))
    (magit-forge-get-pullreq prj number)))

(cl-defmethod magit-forge-list-pullreqs ((prj magit-forge-project) &optional limit)
  (mapcar (lambda (row)
            (closql--remake-instance 'magit-forge-pullreq (magit-db) row))
          (if limit
              (magit-sql [:select * :from pullreq
                          :where (= project $s1)
                          :order-by [(desc number)]
                          :limit $s2]
                         (oref prj id) limit)
            (magit-sql [:select * :from pullreq
                        :where (= project $s1)
                        :order-by [(desc number)]]
                       (oref prj id)))))

;;; Commands

;;;###autoload
(defun magit-pullreq-browse (pullreq)
  "Visit the url corresponding to PULLREQ using a browser."
  (interactive (list (magit-read-pullreq "Browse pull-request")))
  (browse-url (magit-forge--format-url pullreq 'pullreq-url-format))
  (oset pullreq unread-p nil))

(defalias 'magit-pullreq-visit 'magit-pullreq-browse)

;;;###autoload
(defun magit-pullreq-create ()
  "Create a new pull-request for this project using a browser."
  (interactive)
  (browse-url (magit-forge--format-url (magit-forge-get-project t)
                                       'create-pullreq-url-format)))

;;; Utilities

(defun magit-read-pullreq (prompt)
  (let* ((default (magit-pullreq-at-point))
         (prj     (magit-forge-get-project (or default t)))
         (format  (lambda (topic)
                    (format "%s  %s"
                            (oref topic number)
                            (oref topic title))))
         (choices (oref prj pullreqs))
         (choice  (magit-completing-read prompt
                                         (mapcar format choices)
                                         nil nil nil nil
                                         (and default
                                              (funcall format default))))
         (number  (and (string-match "\\([0-9]+\\)" choice)
                       (string-to-number (match-string 1 choice)))))
    (and number
         (magit-forge-get-pullreq prj number))))

;;; Sections

(defun magit-pullreq-at-point ()
  (magit-section-when pullreq))

(defvar magit-pullreq-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-browse-thing] 'magit-pullreq-browse)
    (define-key map [remap magit-visit-thing]  'magit-pullreq-visit)
    map))

(defun magit-insert-pullreqs ()
  (when-let* ((prj (magit-forge-get-project nil))
              (- (not (oref prj sparse-p)))
              (pullreqs (magit-forge-list-pullreqs prj magit--topic-limit)))
    (magit-insert-section (pullreqs nil t)
      (magit-insert-heading "Pull requests:")
      (let ((width (length (number-to-string (oref (car pullreqs) number)))))
        (dolist (pullreq pullreqs)
          (magit-insert-pullreq pullreq width)))
      (insert ?\n))))

(defun magit-insert-pullreq (pullreq &optional width)
  (with-slots (number title unread-p closed) pullreq
    (magit-insert-section (pullreq pullreq)
      (insert
       (format (if width
                   (format "%%-%is %%s\n" (1+ width))
                 "%s %s\n")
               (propertize (format "#%s" number) 'face 'magit-dimmed)
               (magit-log-propertize-keywords
                nil (propertize title 'face
                                (cond (unread-p 'magit-topic-unread)
                                      (closed   'magit-topic-closed)
                                      (t        'magit-topic-open)))))))))

;;; _
(provide 'magit/forge/pullreq)
;;; magit/forge/pullreq.el ends here
