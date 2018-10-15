;;; magit-libgit.el --- Libgit functionality       -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'dash)
(require 'subr-x)

;; `magit-libgit' and `magit-git' depend on one another for now.
;; Don't spew warnings.  Sort alphabetically.
(defvar magit--refresh-cache)
(defvar magit-inhibit-libgit)
(declare-function magit--assert-default-directory "magit-git")
(declare-function magit--libgit-available-p "magit-git")
(declare-function magit-gitdir "magit-git")

(if (and (not magit-inhibit-libgit)
         (magit--libgit-available-p))
    (condition-case err
        (require 'libgit)
      (error
       (setq magit-inhibit-libgit t)
       (message "Error while loading `magit-libgit': %S" err)))
  ;; If `libegit2' isn't available, then this file is compiled
  ;; anyway.  Don't spew warnings.  Sort alphabetically.
  (declare-function libgit-repository-bare-p "libegit2")
  (declare-function libgit-repository-open "libegit2")
  )

;; Identical to `magit--with-refresh-cache', which we cannot use here.
(defmacro magit-libgit--with-refresh-cache (key &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((k (cl-gensym)))
    `(if magit--refresh-cache
         (let ((,k ,key))
           (--if-let (assoc ,k (cdr magit--refresh-cache))
               (progn (cl-incf (caar magit--refresh-cache))
                      (cdr it))
             (cl-incf (cdar magit--refresh-cache))
             (let ((value ,(macroexp-progn body)))
               (push (cons ,k value)
                     (cdr magit--refresh-cache))
               value)))
       ,@body)))

(defun magit-libgit-repo (&optional directory)
  (and-let* ((default-directory
               (let ((magit-inhibit-libgit t))
                 (magit-gitdir directory))))
    (magit-libgit--with-refresh-cache
        (cons default-directory 'magit-libgit-repo)
      (libgit-repository-open default-directory))))

(defun magit-libgit-bare-repo-p (&optional noerror)
  (and (magit--assert-default-directory noerror)
       (if-let ((repo (magit-libgit-repo)))
           (libgit-repository-bare-p repo)
         (unless noerror
           (signal 'magit-outside-git-repo default-directory)))))

;;; _
(provide 'magit-libgit)
;;; magit-libgit.el ends here

