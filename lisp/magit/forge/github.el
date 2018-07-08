;;; magit/forge/github.el --- github support      -*- lexical-binding: t -*-

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

(require 'ghub)
(require 'treepy)

(require 'magit/forge)
(require 'magit/forge/issue)
(require 'magit/forge/pullreq)

;;; Variables

(defvar magit-github-token-scopes '(repo)
  "The Github API scopes needed by Magit.

`repo' is the only required scope.  Without this scope none of
Magit's features that use the API work.  Instead of this scope
you could use `public_repo' if you are only interested in public
repositories.

`repo' Grants read/write access to code, commit statuses,
  invitations, collaborators, adding team memberships, and
  deployment statuses for public and private repositories
  and organizations.

`public_repo' Grants read/write access to code, commit statuses,
  collaborators, and deployment statuses for public repositories
  and organizations. Also required for starring public
  repositories.")

;;; Projects

(defclass magit-github-project (magit-forge-project)
  ((issue-url-format          :initform "https://%h/%o/%n/issues/%i")
   (pullreq-url-format        :initform "https://%h/%o/%n/pull/%i")
   (commit-url-format         :initform "https://%h/%o/%n/commit/%r")
   (branch-url-format         :initform "https://%h/%o/%n/commits/%r")
   (remote-url-format         :initform "https://%h/%o/%n")
   (create-issue-url-format   :initform "https://%h/%o/%n/issues/new")
   (create-pullreq-url-format :initform "https://%h/%o/%n/compare")
   (pullreq-refspec           :initform "+refs/pull/*/head:refs/pullreqs/*")))

(cl-defmethod magit-forge--object-id
  ((_class (subclass magit-github-project)) forge host owner name)
  "Return the id of the specified project.
This method has to make an API request."
  (let-alist (ghub-graphql "\
query ($owner:String!, $name:String!) {
  repository(owner:$owner, name:$name) {
    id }}" `((owner . ,owner)
             (name  . ,name))
    :host host :auth 'magit)
    (format "%s:%s" forge .data.repository.id)))

;;; Issues

(cl-defmethod magit-forge--pull-issues ((prj magit-github-project)
                                        &optional number)
  (emacsql-with-transaction (magit-db)
    (let ((until (and (not number)
                      (caar (magit-sql [:select [updated] :from issue
                                        :where (= project $s1)
		                        :order-by [(desc updated)]
                                        :limit 1]
                                       (oref prj id))))))
      (dolist (i (if number
                     (list (magit-forge--fetch-issue prj number))
                   (magit-forge--fetch-issues prj nil until)))
        (let-alist i
          (when (or (not until) (string> .updatedAt until)) ; if
            (let* ((issue-id (magit-forge--issue-id prj .number))
                   (issue
                    (magit-forge-issue
                     :id        issue-id
                     :project   (oref prj id)
                     :number    .number
                     :state     (intern (downcase .state))
                     :author    .author
                     :title     .title
                     :created   .createdAt
                     :updated   .updatedAt
                     :closed    .closedAt
                     :locked-p  .locked
                     :milestone .milestone
                     :body      (magit-forge--sanitize-string .body))))
              ;; (message "+i+ %s    %s    %s" .number until .updatedAt)
              (closql-insert (magit-db) issue)
              (dolist (c .comments)
                (let-alist c
                  (let ((post
                         (magit-forge-issue-post
                          :id      (format "%s:%s" issue-id .databaseId)
                          :issue   issue-id
                          :number  .databaseId
                          :author  .author
                          :created .createdAt
                          :updated .updatedAt
                          :body    (magit-forge--sanitize-string .body))))
                    (closql-insert (magit-db) post)))))
            ;; (message "-i- %s    %s    %s" .number until .updatedAt)
            ))))))

(cl-defmethod magit-forge--fetch-issues ((prj magit-github-project)
                                         &optional after until)
  ;; (message "-I- %s %s ---" after until)
  (magit--ghub-topic-fetch prj "\
    query ($owner:String!, $name:String!, $after:String) {
      repository(owner:$owner, name:$name) {
        issues(first:100,
               after:$after,
               orderBy: { field:UPDATED_AT, direction:DESC } ) {
          pageInfo { endCursor hasNextPage }
          edges {
            node {
              number
              state
              author { login }
              title
              createdAt
              updatedAt
              closedAt
              locked
              milestone { id }
              body
              comments (first:100) {
                pageInfo { endCursor hasNextPage }
                edges {
                  node {
                    databaseId
                    author { login }
                    createdAt
                    updatedAt
                    body }}}}}}}}"
    `((after    . ,after))
    `((issues   . ,(lambda (prj _loc node)
                     (let-alist (cdr node)
                       (let ((issues (nth 2 node)))
                         (if (and .pageInfo.hasNextPage
                                  (or (not until)
                                      (string<
                                       until
                                       (cdr (assq 'updatedAt
                                                  (car (last issues)))))))
                             (nconc issues
                                    (magit-forge--fetch-issues
                                     prj .pageInfo.endCursor until))
                           issues)))))
      (comments . magit-forge--fetch-issue-comments-1))))

(cl-defmethod magit-forge--fetch-issue ((prj magit-github-project) number)
  (magit--ghub-topic-fetch prj "\
    query ($owner:String!, $name:String!, $number:Int!) {
      repository(owner:$owner, name:$name) {
        issue(number:$number) {
          number
          state
          author { login }
          title
          createdAt
          updatedAt
          closedAt
          locked
          milestone { id }
          body
          comments (first:100) {
            pageInfo { endCursor hasNextPage }
            edges {
              node {
                databaseId
                author { login }
                createdAt
                updatedAt
                body }}}}}}"
    `((number   . ,number))
    `((issue    . ,(lambda (_prj _loc node) (cdr node)))
      (comments . magit-forge--fetch-issue-comments-1))))

(cl-defmethod magit-forge--fetch-issue-comments ((prj magit-github-project)
                                                 number &optional after)
  (magit--ghub-topic-fetch prj "\
    query ($owner:String!, $name:String!, $number:Int!, $after:String) {
      repository(owner:$owner, name:$name) {
        issue(number:$number) {
          comments(first:100, after:$after) {
            pageInfo { endCursor hasNextPage }
            edges {
              node {
                databaseId
                author { login }
                createdAt
                updatedAt
                body }}}}}}"
    `((number   . ,number)
      (after    . ,after))
    `((issue    . ,(lambda (_prj _loc node) (cadr node)))
      (comments . ,(lambda (prj _loc node)
                     (let-alist (cdr node)
                       (if .pageInfo.hasNextPage
                           (nconc (nth 2 node)
                                  (magit-forge--fetch-issue-comments
                                   prj number .pageInfo.endCursor))
                         (nth 2 node))))))))

(cl-defmethod magit-forge--fetch-issue-comments-1 ((prj magit-github-project)
                                                   loc node)
  (cons 'comments
        (let-alist (cdr node)
          (if .pageInfo.hasNextPage
              (nconc (nth 2 node)
                     (magit-forge--fetch-issue-comments
                      prj
                      (cdr (assq 'number (treepy-node (treepy-up loc))))
                      .pageInfo.endCursor))
            (nth 2 node)))))

;;; Pullreqs

(cl-defmethod magit-forge--pull-pullreqs ((prj magit-github-project)
                                          &optional number)
  (emacsql-with-transaction (magit-db)
    (let ((until (and (not number)
                      (caar (magit-sql [:select [updated] :from pullreq
                                        :where (= project $s1)
		                        :order-by [(desc updated)]
                                        :limit 1]
                                       (oref prj id))))))
      (dolist (p (if number
                     (list (magit-forge--fetch-pullreq prj number))
                   (magit-forge--fetch-pullreqs prj nil until)))
        (let-alist p
          (when (or (not until) (string> .updatedAt until)) ; if
            (let* ((pullreq-id (magit-forge--pullreq-id prj .number))
                   (pullreq
                    (magit-forge-pullreq
                     :id           pullreq-id
                     :project      (oref prj id)
                     :number       .number
                     :state        (intern (downcase .state))
                     :author       .author
                     :title        .title
                     :created      .createdAt
                     :updated      .updatedAt
                     :closed       .closedAt
                     :merged       .mergedAt
                     :locked-p     .locked
                     :editable-p   .maintainerCanModify
                     :cross-repo-p .isCrossRepository
                     :base-ref     .baseRef.name
                     :base-repo    .baseRef.repository.nameWithOwner
                     :head-ref     .headRef.name
                     :head-user    .headRef.repository.owner
                     :head-repo    .headRef.repository.nameWithOwner
                     :milestone    .milestone
                     :body         (magit-forge--sanitize-string .body))))
              ;; (message "+p+ %s    %s    %s" .number until .updatedAt)
              (closql-insert (magit-db) pullreq)
              (dolist (p .comments)
                (let-alist p
                  (let ((post
                         (magit-forge-pullreq-post
                          :id      (format "%s:%s" pullreq-id .databaseId)
                          :pullreq pullreq-id
                          :number  .databaseId
                          :author  .author
                          :created .createdAt
                          :updated .updatedAt
                          :body    (magit-forge--sanitize-string .body))))
                    (closql-insert (magit-db) post)))))
            ;; (message "-p- %s    %s    %s" .number until .updatedAt)
            ))))))

(cl-defmethod magit-forge--fetch-pullreqs ((prj magit-github-project)
                                           &optional after until)
  ;; (message "-P- %s %s ---" after until)
  (magit--ghub-topic-fetch prj "\
    query ($owner:String!, $name:String!, $after:String) {
      repository(owner:$owner, name:$name) {
        pullRequests(first:100,
                     after:$after,
                     orderBy: { field:UPDATED_AT, direction:DESC } ) {
          pageInfo { endCursor hasNextPage }
          edges {
            node {
              number
              state
              author { login }
              title
              createdAt
              updatedAt
              closedAt
              mergedAt
              locked
              maintainerCanModify
              isCrossRepository
              milestone { id }
              body
              baseRef {
                name
                repository { nameWithOwner }}
              headRef {
                name
                repository {
                  owner { login }
                  nameWithOwner }}
              comments(first:100) {
                pageInfo { endCursor hasNextPage }
                edges {
                  node {
                    databaseId
                    author { login }
                    createdAt
                    updatedAt
                    body }}}}}}}}"
    `((after . ,after))
    `((pullRequests . ,(lambda (prj _loc node)
                         (let-alist (cdr node)
                           (let ((pullreqs (nth 2 node)))
                             (if (and .pageInfo.hasNextPage
                                      (or (not until)
                                          (string<
                                           until
                                           (cdr (assq 'updatedAt
                                                      (car (last pullreqs)))))))
                                 (nconc pullreqs
                                        (magit-forge--fetch-pullreqs
                                         prj .pageInfo.endCursor until))
                               pullreqs)))))
      (comments     . magit-forge--fetch-pullreq-comments-1))))

(cl-defmethod magit-forge--fetch-pullreq ((prj magit-github-project) number)
  (magit--ghub-topic-fetch prj "\
    query ($owner:String!, $name:String!, $number:Int!) {
      repository(owner:$owner, name:$name) {
        pullRequest(number:$number) {
          number
          state
          author { login }
          title
          createdAt
          updatedAt
          closedAt
          mergedAt
          locked
          maintainerCanModify
          isCrossRepository
          milestone { id }
          body
          baseRef {
            name
            repository {
              nameWithOwner }}
          headRef {
            name
            repository {
              owner {
                login }
              nameWithOwner }}
          comments(first:100) {
            pageInfo { endCursor hasNextPage }
            edges {
              node {
                databaseId
                author { login }
                createdAt
                updatedAt
                body }}}}}}"
    `((number . ,number))
    `((pullRequest . ,(lambda (_prj _loc node) (cdr node)))
      (comments    . magit-forge--fetch-pullreq-comments-1))))

(cl-defmethod magit-forge--fetch-pullreq-comments ((prj magit-github-project)
                                                   number &optional after)
  (magit--ghub-topic-fetch prj "\
    query ($owner:String!, $name:String!) {
      repository(owner:$owner, name:$name) {
        pullRequest(number:$number) {
          comments(first:100) {
            pageInfo { endCursor hasNextPage }
            edges {
              node {
                databaseId
                author { login }
                createdAt
                updatedAt
                body }}}}}}"
    `((number . ,number)
      (after  . ,after))
    `((pullRequest . ,(lambda (_prj _loc node) (cadr node)))
      (comments    . ,(lambda (prj _loc node)
                        (let-alist (cdr node)
                          (if .pageInfo.hasNextPage
                              (nconc (nth 2 node)
                                     (magit-forge--fetch-issue-comments
                                      prj number .pageInfo.endCursor))
                            (nth 2 node))))))))

(cl-defmethod magit-forge--fetch-pullreq-comments-1 ((prj magit-github-project)
                                                     loc node)
  (cons 'comments
        (let-alist (cdr node)
          (if .pageInfo.hasNextPage
              (nconc (nth 2 node)
                     (magit-forge--fetch-pullreq-comments
                      prj
                      (cdr (assq 'number (treepy-node (treepy-up loc))))
                      .pageInfo.endCursor))
          (nth 2 node)))))

;;; Utilities

(cl-defun magit--ghub-get (prj resource
                               &optional params
                               &key query payload headers
                               silent unpaginate noerror reader
                               host callback errorback)
  (ghub-get resource params
            :host (or host (oref prj apihost))
            :auth 'magit
            :query query :payload payload :headers headers
            :silent silent :unpaginate unpaginate
            :noerror noerror :reader reader
            :callback callback :errorback errorback))

(cl-defun magit--ghub-graphql (prj graphql &optional variables
                                   &key silent callback errorback)
  (ghub-graphql graphql variables
                :host      (oref prj apihost)
                :auth      'magit
                :silent    silent
                :callback  callback
                :errorback errorback))

(defun magit--ghub-graphql-zipper (prj graphql variables)
  (treepy-leftmost-descendant
   (treepy-zipper (lambda (x) (and (listp x) (listp (cdr x))))
                  #'identity
                  (lambda (_ cs) cs)
                  (magit--ghub-graphql prj graphql variables))))

(defun magit--ghub-topic-fetch (prj graphql variables filters)
  (declare (indent 2))
  (cl-do ((loc (magit--ghub-graphql-zipper prj graphql
                                           `((owner . ,(oref prj owner))
                                             (name  . ,(oref prj name))
                                             ,@variables))
               (treepy-next loc :postorder)))
      ((treepy-end-p loc)
       (car (treepy-root loc)))
    (let* ((node  (treepy-node loc))
           (key   (car-safe node))
           (node* (pcase key
                    ('errors (error "%s" node))
                    ((or 'author 'owner 'milestone)
                     (cons (car node) (cdr (cadr node))))
                    ('data (cadr node))
                    ('edges (mapcar #'car (cdr node)))
                    ('node (cdr node))
                    ('repository
                     (if (eq (car (treepy-node (treepy-up loc))) 'data)
                         (cadr node)
                       node))
                    (_
                     (if-let ((fn (cdr (assq key filters))))
                         (funcall fn prj loc node)
                       node)))))
      (unless (eq node* node)
        (setq loc (treepy-replace loc node*))))))

;;; _
(provide 'magit/forge/github)
;;; magit/forge/github.el ends here
