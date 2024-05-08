;;; magit-libgit.el --- (POC) Teach Magit to use Libgit2  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;; You should have received a copy of the AUTHORS.md file, which
;; lists all contributors.  If not, see https://magit.vc/authors.

;;; Commentary:

;; This library is not used by default and it is highly experimental.
;; It only implements a single method.  Do not use this.

;; This library teaches Magit to use functions provided by the
;; `libegit2' module to perform certain tasks.  That module used the
;; Libgit2 implementation of the Git core methods and is implemented
;; in the `libgit' package.

;; The hope is that using a C module instead of calling out to `git'
;; all the time increases performance; especially on Windows where
;; starting a process is unreasonably slow.

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'eieio)
(require 'subr-x)

(when (and (featurep 'seq)
           (not (fboundp 'seq-keep)))
  (unload-feature 'seq 'force))
(require 'seq)

(require 'magit-git)

(require 'libgit)

;;; Utilities

(defun magit-libgit-repo (&optional directory)
  "Return an object for the repository in DIRECTORY.
If optional DIRECTORY is nil, then use `default-directory'."
  (and-let* ((default-directory
              (let ((magit-inhibit-libgit t))
                (magit-gitdir directory))))
    (magit--with-refresh-cache
        (cons default-directory 'magit-libgit-repo)
      (libgit-repository-open default-directory))))

;;; Methods

(cl-defmethod magit-bare-repo-p
  (&context ((magit-gitimpl) (eql libgit)) &optional noerror)
  (and (magit--assert-default-directory noerror)
       (if-let ((repo (magit-libgit-repo)))
           (libgit-repository-bare-p repo)
         (unless noerror
           (signal 'magit-outside-git-repo default-directory)))))

;;; _
(provide 'magit-libgit)
;;; magit-libgit.el ends here
