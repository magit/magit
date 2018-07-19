;;; magit/forge/db.el --- forge database          -*- lexical-binding: t -*-

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

(require 'closql)
(require 'eieio)
(require 'emacsql)
(require 'emacsql-sqlite)

(declare-function magit-forge-reset-database "magit/forge")

;;; Options

(defcustom magit-forge-database-file "~/.emacs.d/magit-forge-database.sqlite"
  "The file used to store the forge database."
  :package-version '(magit . "2.90.0")
  :group 'magit-forge
  :type 'file)

;;; Core

(defclass magit-database (closql-database)
  ((object-class :initform magit-forge-project)))

;; FIXME Instead add closql-update and/or closql-replace to closql.el
(cl-defmethod closql-insert ((db magit-database) obj)
  (closql--oset obj 'closql-database db)
  (let (alist)
    (dolist (slot (eieio-class-slots (eieio--object-class obj)))
      (setq  slot (cl--slot-descriptor-name slot))
      (let ((columns (closql--slot-get obj slot :closql-columns)))
        (when columns
          (push (cons slot (closql-oref obj slot)) alist)
          (closql--oset obj slot eieio-unbound))))
    (emacsql-with-transaction db
      (emacsql db [:insert-or-replace-into $i1 :values $v2]
               (oref-default obj closql-table)
               (pcase-let ((`(,class ,_db . ,values)
                            (closql--intern-unbound
                             (closql--coerce obj 'list))))
                 (vconcat (cons (closql--abbrev-class
                                 (if (fboundp 'record)
                                     (eieio--class-name class)
                                   class))
                                values))))
      (pcase-dolist (`(,slot . ,value) alist)
        (closql--dset db obj slot value))))
  obj)

(defconst magit--db-version 1)

(defvar magit--db-disabled nil)

(defvar magit--db-connection nil
  "The EmacSQL database connection.")

(defun magit-db ()
  (setq magit--db-disabled nil)
  (unless (and magit--db-connection (emacsql-live-p magit--db-connection))
    (make-directory (file-name-directory magit-forge-database-file) t)
    (closql-db 'magit-database 'magit--db-connection
               magit-forge-database-file t)
    (let ((version (caar (emacsql magit--db-connection "PRAGMA user_version"))))
      (cond
       ((> version magit--db-version)
        (emacsql-close magit--db-connection)
        (user-error "BUG: magit-db-version is to low"))
       ((< version magit--db-version)
        (emacsql-close magit--db-connection)
        (if (yes-or-no-p "The database scheme changed. Reset database now? ")
            (magit-forge-reset-database)
          (setq magit--db-disabled t)
          (user-error "Aborted.  Database disabled until update"))))))
  magit--db-connection)

;;; Api

(defun magit-sql (sql &rest args)
  (if (stringp sql)
      (emacsql (magit-db) (apply #'format sql args))
    (apply #'emacsql (magit-db) sql args)))

;;; Schemata

(defconst magit--db-table-schemata
  '((project
     [(class :not-null)
      (id :not-null :primary-key)
      forge
      owner
      name
      apihost
      githost
      remote
      sparse-p
      (issues   :default eieio-unbound)
      (pullreqs :default eieio-unbound)])

    (issue
     [(class :not-null)
      (id :not-null :primary-key)
      project
      number
      state
      author
      title
      created
      updated
      closed
      unread-p
      locked-p
      milestone
      body
      (assignees :default eieio-unbound)
      (cards :default eieio-unbound)
      (edits :default eieio-unbound)
      (labels :default eieio-unbound)
      (participants :default eieio-unbound)
      (posts :default eieio-unbound)
      (reactions :default eieio-unbound)
      (timeline :default eieio-unbound)]
     (:foreign-key
      [project] :references project [id]
      :on-delete :cascade))

    (issue-post
     [(class :not-null)
      (id :not-null :primary-key)
      issue
      number
      author
      created
      updated
      body
      (edits :default eieio-unbound)
      (reactions :default eieio-unbound)]
     (:foreign-key
      [issue] :references issue [id]
      :on-delete :cascade))

    (pullreq
     [(class :not-null)
      (id :not-null :primary-key)
      project
      number
      state
      author
      title
      created
      updated
      closed
      merged
      unread-p
      locked-p
      editable-p
      cross-repo-p
      base-ref
      base-repo
      head-ref
      head-user
      head-repo
      milestone
      body
      (assignees :default eieio-unbound)
      (cards :default eieio-unbound)
      (commits :default eieio-unbound)
      (edits :default eieio-unbound)
      (labels :default eieio-unbound)
      (participants :default eieio-unbound)
      (posts :default eieio-unbound)
      (reactions :default eieio-unbound)
      (review-requests :default eieio-unbound)
      (reviews :default eieio-unbound)
      (timeline :default eieio-unbound)]
     (:foreign-key
      [project] :references project [id]
      :on-delete :cascade))

    (pullreq-post
     [(class :not-null)
      (id :not-null :primary-key)
      pullreq
      number
      author
      created
      updated
      body
      (edits :default eieio-unbound)
      (reactions :default eieio-unbound)]
     (:foreign-key
      [pullreq] :references pullreq [id]
      :on-delete :cascade))

    (notification
     [(class :not-null)
      (id :not-null :primary-key)
      project
      forge
      reason
      unread-p
      last-read
      updated
      title
      type
      topic
      url]
     (:foreign-key
      [project] :references project [id]
      :on-delete :cascade))))

(cl-defmethod closql--db-init ((db magit-database))
  (emacsql-with-transaction db
    (pcase-dolist (`(,table . ,schema) magit--db-table-schemata)
      (emacsql db [:create-table $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" magit--db-version))))

;;; _
(provide 'magit/forge/db)
;;; magit/forge/db.el ends here
