;;; magit/forge/gitlab.el --- gitlab support      -*- lexical-binding: t -*-

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

;; NOTE Do not trust the API documentation, it is not accurate.

(require 'glab)
(require 'magit/forge)
(require 'magit/forge/issue)
(require 'magit/forge/pullreq)

;;; Variables

(defvar magit-gitlab-token-scopes '(api)
  "The Gitlab API scopes needed by Magit.

`api' is the only required scope.  It gives read and write access
to everything.  The Gitlab API provides more fine-grained scopes
for read-only access, but when any write access is required, then
it is all or nothing.")

;;; Projects

(defclass magit-gitlab-project (magit-forge-project)
  ())

(cl-defmethod magit-forge--object-id
  ((_class (subclass magit-gitlab-project)) forge host owner name)
  "Return the id of the specified project.
This method has to make an API request."
  (format "%s:%s" forge
          (cdr (assq 'id (glab-get (format "/projects/%s%%2F%s" owner name) nil
                                   :host host :auth 'magit)))))

;;; Issues

(cl-defmethod magit-forge--pull-issues ((prj magit-gitlab-project))
  (emacsql-with-transaction (magit-db)
    (dolist (i (magit-forge--fetch-issues prj))
      (let-alist i
        (let* ((issue-id (magit-forge--issue-id prj .iid))
               (issue
                (magit-forge-issue
                 :id        issue-id
                 :project   (oref prj id)
                 :number    .iid
                 :state     (intern .state)
                 :author    .author.username
                 :title     .title
                 :created   .created_at
                 :updated   .updated_at
                 :closed    .closed_at
                 :locked-p  .discussion_locked
                 :milestone .milestone.iid
                 :body      (magit-forge--sanitize-string .description))))
          (closql-insert (magit-db) issue)
          (dolist (c .notes)
            (let-alist c
              (let ((post
                     (magit-forge-issue-post
                      :id      (format "%s:%s" issue-id .id)
                      :issue   issue-id
                      :number  .id
                      :author  .author.username
                      :created .created_at
                      :updated .updated_at
                      :body    (magit-forge--sanitize-string .body))))
                (closql-insert (magit-db) post)))))))))

(cl-defmethod magit-forge--fetch-issues ((prj magit-gitlab-project))
  (with-slots (owner name) prj
    (mapcar
     (lambda (issue)
       (let-alist issue
         (magit-msg "Pulling issue %s/%s#%s..." owner name .iid)
         ;; NOTE Gitlab has no decent endpoint to get posts on
         ;; an issue.  The list we get here includes all kinds
         ;; of events.  There is a `type' field, but its value
         ;; is always nil.  For now we just display all events.
         ;; We probably will have to reverse engineer and come
         ;; up with some heuristics; I am not holding my breath:
         ;; https://gitlab.com/gitlab-org/gitlab-ce/issues/20901
         (setf (alist-get 'notes issue)
               (magit--glab-get
                prj (format "/projects/%s/issues/%s/notes" .project_id .iid)
                '((per_page . 100)) :unpaginate t)))
       issue)
     (magit--glab-get prj
                      (format "/projects/%s%%2F%s/issues" owner name)
                      '((per_page . 100)) :unpaginate nil))))

;;; Pullreqs

(cl-defmethod magit-forge--pull-pullreqs ((prj magit-gitlab-project))
  (emacsql-with-transaction (magit-db)
    (dolist (p (magit-forge--fetch-pullreqs prj))
      (let-alist p
        (let* ((pullreq-id (magit-forge--pullreq-id prj .iid))
               (pullreq
                (magit-forge-pullreq
                 :id           pullreq-id
                 :project      (oref prj id)
                 :number       .iid
                 :state        (intern .state)
                 :author       .author.username
                 :title        .title
                 :created      .created_at
                 :updated      .updated_at
                 ;; NOTE .merged_at and .closed_at may both be nil
                 ;; even though the pullreq is merged or otherwise
                 ;; closed.  In such cases use 1, so that these
                 ;; variables at least can serve as booleans.
                 :closed       (or .closed_at
                                   (and (member .state '("closed" "merged")) 1))
                 :merged       (or .merged_at
                                   (and (equal .state "merged") 1))
                 :locked-p     .discussion_locked
                 :editable-p   .allow_maintainer_to_push
                 :cross-repo-p (not (equal .source_project_id
                                           .target_project_id))
                 :base-ref     .target_branch
                 :base-repo    .target_project.path_with_namespace
                 :head-ref     .source_branch
                 :head-user    .source_project.owner.username
                 :head-repo    .source_project.path_with_namespace
                 :milestone    .milestone.iid
                 :body         (magit-forge--sanitize-string .description))))
          (closql-insert (magit-db) pullreq)
          (dolist (c .notes)
            (let-alist c
              (let ((post
                     (magit-forge-pullreq-post
                      :id      (format "%s:%s" pullreq-id .id)
                      :pullreq pullreq-id
                      :number  .id
                      :author  .author.username
                      :created .created_at
                      :updated .updated_at
                      :body    (magit-forge--sanitize-string .body))))
                (closql-insert (magit-db) post)))))))))

(cl-defmethod magit-forge--fetch-pullreqs ((prj magit-gitlab-project))
  (with-slots (owner name) prj
    (let (target-project)
      (mapcar
       (lambda (pullreq)
         ;; NOTE When fetching multiple pullreqs at once, then the
         ;; pullreqs lack some data, so we have to fetch each one
         ;; individually.
         (let ((number (cdr (assq 'iid pullreq))))
           (magit-msg "Pulling pullreq %s/%s#%s..." owner name number)
           (setq pullreq
                 (magit--glab-get
                  prj (format "/projects/%s%%2F%s/merge_requests/%s"
                              owner name number))))
         (let-alist pullreq
           (when .source_project_id
             ;; If the fork no longer exists, then this is nil.
             ;; This will lead to difficulties later on but there
             ;; is nothing we can do about it.
             (setf (alist-get 'source_project pullreq)
                   (magit--glab-get
                    prj (format "/projects/%s" .source_project_id))))
           (setf (alist-get 'target_project pullreq)
                 (or target-project
                     (setq target-project
                           (magit--glab-get
                            prj (format "/projects/%s" .target_project_id)))))
           (setf (alist-get 'notes pullreq)
                 (magit--glab-get
                  prj (format "/projects/%s/merge_requests/%s/notes"
                              .target_project_id .iid)
                  '((per_page . 100)) :unpaginate t)))
         pullreq)
       (magit--glab-get prj (format "/projects/%s%%2F%s/merge_requests"
                                    owner name)
                        '((per_page . 100)) :unpaginate t)))))

;;; Utilities

(cl-defun magit--glab-get (prj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               callback errorback)
  (glab-get resource params
            :host (oref prj apihost)
            :auth 'magit
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

;;; _
(provide 'magit/forge/gitlab)
;;; magit/forge/gitlab.el ends here
