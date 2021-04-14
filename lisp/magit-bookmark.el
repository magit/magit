;;; magit-bookmark.el --- bookmark support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2021  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Inspired by an earlier implementation by Yuri Khan.

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

;; Support for bookmarks for most Magit buffers.

;;; Code:

(require 'magit)
(require 'bookmark)

;;; Core

(defun magit--make-bookmark ()
  "Create a bookmark for the current Magit buffer.
Input values are the major-mode's `magit-bookmark-name' method,
and the buffer-local values of the variables referenced in its
`magit-bookmark-variables' property."
  (if (plist-member (symbol-plist major-mode) 'magit-bookmark-variables)
      ;; `bookmark-make-record-default's return value does not match
      ;; (NAME . ALIST), even though it is used as the default value
      ;; of `bookmark-make-record-function', which states that such
      ;; functions must do that.  See #4356.
      (let ((bookmark (cons nil (bookmark-make-record-default 'no-file))))
        (bookmark-prop-set bookmark 'handler  'magit--handle-bookmark)
        (bookmark-prop-set bookmark 'mode     major-mode)
        (bookmark-prop-set bookmark 'filename (magit-toplevel))
        (bookmark-prop-set bookmark 'defaults (list (magit-bookmark-name)))
        (dolist (var (get major-mode 'magit-bookmark-variables))
          (bookmark-prop-set bookmark var (symbol-value var)))
        (bookmark-prop-set
         bookmark 'magit-hidden-sections
         (--keep (and (oref it hidden)
                      (cons (oref it type)
                            (if (derived-mode-p 'magit-stash-mode)
                                (replace-regexp-in-string
                                 (regexp-quote magit-buffer-revision)
                                 magit-buffer-revision-hash
                                 (oref it value))
                              (oref it value))))
                 (oref magit-root-section children)))
        bookmark)
    (user-error "Bookmarking is not implemented for %s buffers" major-mode)))

;;;###autoload
(defun magit--handle-bookmark (bookmark)
  "Open a bookmark created by `magit--make-bookmark'.
Call the `magit-*-setup-buffer' function of the the major-mode
with the variables' values as arguments, which were recorded by
`magit--make-bookmark'.  Ignore `magit-display-buffer-function'."
  (let ((buffer (let ((default-directory (bookmark-get-filename bookmark))
                      (mode (bookmark-prop-get bookmark 'mode))
                      (magit-display-buffer-function #'identity)
                      (magit-display-buffer-noselect t))
                  (apply (intern (format "%s-setup-buffer"
                                         (substring (symbol-name mode) 0 -5)))
                         (--map (bookmark-prop-get bookmark it)
                                (get mode 'magit-bookmark-variables))))))
    (set-buffer buffer) ; That is the interface we have to adhere to.
    (when-let ((hidden (bookmark-prop-get bookmark 'magit-hidden-sections)))
      (with-current-buffer buffer
        (dolist (child (oref magit-root-section children))
          (if (member (cons (oref child type)
                            (oref child value))
                      hidden)
              (magit-section-hide child)
            (magit-section-show child)))))
    ;; Compatibility with `bookmark+' package.  See #4356.
    (when (bound-and-true-p bmkp-jump-display-function)
      (funcall bmkp-jump-display-function (current-buffer)))
    nil))

(cl-defgeneric magit-bookmark-name ()
  "Return name for bookmark to current buffer."
  (format "%s%s"
          (substring (symbol-name major-mode) 0 -5)
          (if-let ((vars (get major-mode 'magit-bookmark-variables)))
              (cl-mapcan (lambda (var)
                           (let ((val (symbol-value var)))
                             (if (and val (atom val))
                                 (list val)
                               val)))
                         vars)
            "")))

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
            (`staged "staged")
            (`unstaged "unstaged")
            (`committed magit-buffer-range)
            (`undefined
             (delq nil (list magit-buffer-typearg magit-buffer-range-hashed))))
          (if magit-buffer-diff-files
              (concat " -- " (mapconcat #'identity magit-buffer-diff-files " "))
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
              (mapconcat #'identity magit-buffer-diff-files " ")
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
              (mapconcat #'identity magit-buffer-diff-files " ")
            (magit-rev-format "%s" magit-buffer-revision))))

;;; Log
;;;; Log

(put 'magit-log-mode 'magit-bookmark-variables
     '(magit-buffer-revisions
       magit-buffer-log-args
       magit-buffer-log-files))

(cl-defmethod magit-bookmark-name (&context (major-mode magit-log-mode))
  (format "magit-log(%s%s)"
          (mapconcat #'identity magit-buffer-revisions " ")
          (if magit-buffer-log-files
              (concat " -- " (mapconcat #'identity magit-buffer-log-files " "))
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
;;; magit-bookmark.el ends here
