;;; magit-bookmark.el --- Bookmarks for Magit buffers  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2025 The Magit Project Contributors

;; Inspired by an earlier implementation by Yuri Khan.

;; Author: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.magit@jonas.bernoulli.dev>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Support for bookmarks for most Magit buffers.

;;; Code:

(require 'magit)

(require 'bookmark)

;;; Common

(cl-defmethod magit-bookmark-get-filename (&context (major-mode magit-mode))
  (magit-toplevel))

(cl-defmethod magit-bookmark-get-value
  (bookmark &context (major-mode magit-mode))
  (dolist (var (get major-mode 'magit-bookmark-variables))
    (bookmark-prop-set bookmark var (symbol-value var))))

(cl-defmethod magit-bookmark-get-buffer-create
  (bookmark (mode (derived-mode magit-mode)))
  (let ((default-directory (bookmark-get-filename bookmark))
        (magit-display-buffer-function #'identity)
        (magit-display-buffer-noselect t))
    (apply (intern (format "%s-setup-buffer"
                           (substring (symbol-name mode) 0 -5)))
           (mapcar (##bookmark-prop-get bookmark %)
                   (get mode 'magit-bookmark-variables)))))

;;; Diff
;;;; Diff

(put 'magit-diff-mode 'magit-bookmark-variables
     '(magit-buffer-range-hashed
       magit-buffer-typearg
       magit-buffer-diff-args
       magit-buffer-diff-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-diff-mode))
  (format "magit-diff(%s%s)"
          (pcase (magit-diff-type)
            ('staged "staged")
            ('unstaged "unstaged")
            ('committed magit-buffer-range)
            ('undefined
             (delq nil (list magit-buffer-typearg magit-buffer-range-hashed))))
          (if magit-buffer-diff-files
              (concat " -- " (string-join magit-buffer-diff-files " "))
            "")))

;;;; Revision

(put 'magit-revision-mode 'magit-bookmark-variables
     '(magit-buffer-revision-hash
       magit-buffer-diff-args
       magit-buffer-diff-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-revision-mode))
  (format "magit-revision(%s %s)"
          (magit-rev-abbrev magit-buffer-revision)
          (if magit-buffer-diff-files
              (string-join magit-buffer-diff-files " ")
            (magit-rev-format "%s" magit-buffer-revision))))

;;;; Stash

(put 'magit-stash-mode 'magit-bookmark-variables
     '(magit-buffer-revision-hash
       magit-buffer-diff-args
       magit-buffer-diff-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-stash-mode))
  (format "magit-stash(%s %s)"
          (magit-rev-abbrev magit-buffer-revision)
          (if magit-buffer-diff-files
              (string-join magit-buffer-diff-files " ")
            (magit-rev-format "%s" magit-buffer-revision))))

(cl-defmethod magit-bookmark--get-child-value
  (section &context (major-mode magit-stash-mode))
  (string-replace magit-buffer-revision
                  magit-buffer-revision-hash
                  (oref section value)))

;;; Log
;;;; Log

(put 'magit-log-mode 'magit-bookmark-variables
     '(magit-buffer-revisions
       magit-buffer-log-args
       magit-buffer-log-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-log-mode))
  (format "magit-log(%s%s)"
          (string-join magit-buffer-revisions " ")
          (if magit-buffer-log-files
              (concat " -- " (string-join magit-buffer-log-files " "))
            "")))

;;;; Cherry

(put 'magit-cherry-mode 'magit-bookmark-variables
     '(magit-buffer-refname
       magit-buffer-upstream))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-cherry-mode))
  (format "magit-cherry(%s > %s)"
          magit-buffer-refname
          magit-buffer-upstream))

;;;; Reflog

(put 'magit-reflog-mode 'magit-bookmark-variables
     '(magit-buffer-refname))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-reflog-mode))
  (format "magit-reflog(%s)" magit-buffer-refname))

;;; Misc

(put 'magit-status-mode 'magit-bookmark-variables nil)

(put 'magit-refs-mode 'magit-bookmark-variables
     '(magit-buffer-upstream
       magit-buffer-arguments))

(put 'magit-stashes-mode 'magit-bookmark-variables nil)

(cl-defmethod magit-bookmark-name (&context (major-mode magit-stashes-mode))
  (format "magit-states(%s)" magit-buffer-refname))

;;; _
(provide 'magit-bookmark)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("and>"         . "cond-let--and>")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; magit-bookmark.el ends here
