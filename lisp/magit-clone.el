;;; magit-clone.el --- clone a repository  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2018  The Magit Project Contributors
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

;;; Commentary:

;; This library implements clone commands.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-clone-set-remote-head nil
  "Whether cloning creates the symbolic-ref `<remote>/HEAD'."
  :package-version '(magit . "2.4.2")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-clone-set-remote.pushDefault 'ask
  "Whether to set the value of `remote.pushDefault' after cloning.

If t, then set without asking.  If nil, then don't set.  If
`ask', then ask."
  :package-version '(magit . "2.4.0")
  :group 'magit-commands
  :type '(choice (const :tag "set" t)
                 (const :tag "ask" ask)
                 (const :tag "don't set" nil)))

(defcustom magit-clone-default-directory nil
  "Default directory to use when `magit-clone' reads destination.
If nil (the default), then use the value of `default-directory'.
If a directory, then use that.  If a function, then call that
with the remote url as only argument and use the returned value."
  :package-version '(magit . "2.90.0")
  :group 'magit-commands
  :type '(choice (const     :tag "value of default-directory")
                 (directory :tag "constant directory")
                 (function  :tag "function's value")))

;;; Commands

;;;###autoload
(defun magit-clone (repository directory)
  "Clone the REPOSITORY to DIRECTORY.
Then show the status buffer for the new repository."
  (interactive
   (let  ((url (magit-read-string-ns "Clone repository")))
     (list url (read-directory-name
                "Clone to: "
                (if (functionp magit-clone-default-directory)
                    (funcall magit-clone-default-directory url)
                  magit-clone-default-directory)
                nil nil
                (and (string-match "\\([^/:]+?\\)\\(/?\\.git\\)?$" url)
                     (match-string 1 url))))))
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (magit-run-git-async "clone" repository
                       (magit-convert-filename-for-git directory))
  ;; Don't refresh the buffer we're calling from.
  (process-put magit-this-process 'inhibit-refresh t)
  (set-process-sentinel
   magit-this-process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (let ((magit-process-raise-error t))
         (magit-process-sentinel process event)))
     (when (and (eq (process-status process) 'exit)
                (= (process-exit-status process) 0))
       (let ((default-directory directory))
         (when (or (eq  magit-clone-set-remote.pushDefault t)
                   (and magit-clone-set-remote.pushDefault
                        (y-or-n-p "Set `remote.pushDefault' to \"origin\"? ")))
           (setf (magit-get "remote.pushDefault") "origin"))
         (unless magit-clone-set-remote-head
           (magit-remote-unset-head "origin")))
       (with-current-buffer (process-get process 'command-buf)
         (magit-status-internal directory))))))

;;; _
(provide 'magit-clone)
;;; magit-clone.el ends here
