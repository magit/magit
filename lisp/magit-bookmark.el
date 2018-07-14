;;; magit-bookmark.el --- bookmark support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Yuri Khan <yuri.v.khan@gmail.com>
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

;; Support for bookmarks for most Magit buffers.

;;; Code:

(require 'magit)
(require 'bookmark)

;;; Supporting primitives

(defun magit-bookmark--jump (bookmark fn &rest args)
  "Handle a Magit BOOKMARK.

This function will:

1. Bind `default-directory' to the repository root directory
   stored in the `filename' bookmark property.
2. Invoke the function FN with ARGS as arguments.  This needs to
   restore the buffer.
3. Restore the expanded/collapsed status of top level sections
   and the point position."
  (declare (indent 2))
  (let* ((default-directory (bookmark-get-filename bookmark)))
    (if default-directory
        (apply fn args)
      (signal 'bookmark-error-no-filename (list 'stringp default-directory)))
    (when (derived-mode-p 'magit-mode)
      (when-let ((hidden-sections (bookmark-prop-get bookmark
                                                     'magit-hidden-sections)))
        (dolist (child (oref magit-root-section children))
          (if (member (cons (oref child type)
                            (oref child value))
                      hidden-sections)
              (magit-section-hide child)
            (magit-section-show child)))))
    (--when-let (bookmark-get-position bookmark)
      (goto-char it))
    (--when-let (bookmark-get-front-context-string bookmark)
      (when (search-forward it (point-max) t)
        (goto-char (match-beginning 0))))
    (--when-let (bookmark-get-rear-context-string bookmark)
      (when (search-backward it (point-min) t)
        (goto-char (match-end 0))))
    nil))

(defun magit-bookmark--make-record (mode handler &optional make-props)
  "Create a Magit bookmark.

MODE specifies the expected major mode of current buffer.

HANDLER should be a function that will be used to restore this
buffer.

MAKE-PROPS should be either nil or a function that will be called
with `magit-refresh-args' as the argument list, and may return an
alist whose every element has the form (PROP . VALUE) and
specifies additional properties to store in the bookmark."
  (declare (indent 1))
  (unless (eq major-mode mode)
    (user-error "Not in a %s buffer" mode))
  (let ((bookmark (bookmark-make-record-default 'no-file)))
    (bookmark-prop-set bookmark 'handler handler)
    (bookmark-set-filename bookmark (magit-toplevel))
    (when (derived-mode-p 'magit-mode)
      (bookmark-prop-set
       bookmark 'magit-hidden-sections
       (--map (cons (oref it type)
                    (oref it value))
              (--filter (oref it hidden)
                        (oref magit-root-section children)))))
    (when make-props
      (pcase-dolist (`(,prop . ,value) (apply make-props magit-refresh-args))
        (bookmark-prop-set bookmark prop value)))
    bookmark))

;;; Status

;;;###autoload
(defun magit-bookmark--status-jump (bookmark)
  "Handle a Magit status BOOKMARK."
  (magit-bookmark--jump bookmark
      (lambda () (magit-status-internal default-directory))))

;;;###autoload
(defun magit-bookmark--status-make-record ()
  "Create a Magit status bookmark."
  (magit-bookmark--make-record 'magit-status-mode
    #'magit-bookmark--status-jump))

;;; Refs

;;;###autoload
(defun magit-bookmark--refs-jump (bookmark)
  "Handle a Magit refs BOOKMARK."
  (magit-bookmark--jump bookmark #'magit-show-refs
    (bookmark-prop-get bookmark 'magit-refs)
    (bookmark-prop-get bookmark 'magit-args)))

;;;###autoload
(defun magit-bookmark--refs-make-record ()
  "Create a Magit refs bookmark."
  (magit-bookmark--make-record 'magit-refs-mode
    #'magit-bookmark--refs-jump
    (lambda (refs args)
      `((magit-refs . ,refs)
        (magit-args . ,args)))))

;;; Log

;;;###autoload
(defun magit-bookmark--log-jump (bookmark)
  "Handle a Magit log BOOKMARK."
  (magit-bookmark--jump bookmark #'magit-log
    (bookmark-prop-get bookmark 'magit-revs)
    (bookmark-prop-get bookmark 'magit-args)
    (bookmark-prop-get bookmark 'magit-files)))

(defun magit-bookmark--log-make-name (buffer-name revs _args files)
  "Generate the default name for a log bookmark."
  (concat
   buffer-name " " (mapconcat #'identity revs " ")
   (and files
        (concat " touching " (mapconcat #'identity files " ")))))

;;;###autoload
(defun magit-bookmark--log-make-record ()
  "Create a Magit log bookmark."
  (magit-bookmark--make-record 'magit-log-mode
    #'magit-bookmark--log-jump
    (lambda (revs args files)
      `((defaults    . (,(magit-bookmark--log-make-name
                          (buffer-name) revs args files)))
        (magit-revs  . ,revs)
        (magit-args  . ,args)
        (magit-files . ,files)))))

;;; Reflog

;;;###autoload
(defun magit-bookmark--reflog-jump (bookmark)
  "Handle a Magit reflog BOOKMARK."
  (magit-bookmark--jump bookmark
      (lambda ()
        (let ((magit-reflog-arguments (bookmark-prop-get bookmark 'magit-args)))
          (magit-reflog (bookmark-prop-get bookmark 'magit-ref))))))

(defun magit-bookmark--reflog-make-name (buffer-name ref)
  "Generate the default name for a reflog bookmark."
  (concat buffer-name " " ref))

;;;###autoload
(defun magit-bookmark--reflog-make-record ()
  "Create a Magit reflog bookmark."
  (magit-bookmark--make-record 'magit-reflog-mode
    #'magit-bookmark--reflog-jump
    (lambda (ref args)
      `((defaults   . (,(magit-bookmark--reflog-make-name (buffer-name) ref)))
        (magit-ref  . ,ref)
        (magit-args . ,args)))))

;;; Stashes

;;;###autoload
(defun magit-bookmark--stashes-jump (bookmark)
  "Handle a Magit stash list BOOKMARK."
  (magit-bookmark--jump bookmark #'magit-stash-list))

;;;###autoload
(defun magit-bookmark--stashes-make-record ()
  "Create a Magit stash list bookmark."
  (magit-bookmark--make-record 'magit-stashes-mode
    #'magit-bookmark--stashes-jump))

;;; Cherry

;;;###autoload
(defun magit-bookmark--cherry-jump (bookmark)
  "Handle a Magit cherry BOOKMARK."
  (magit-bookmark--jump bookmark #'magit-cherry
    (bookmark-prop-get bookmark 'magit-head)
    (bookmark-prop-get bookmark 'magit-upstream)))

(defun magit-bookmark--cherry-make-name (buffer-name head upstream)
  "Generate the default name for a cherry bookmark."
  (concat buffer-name " " head " upstream " upstream))

;;;###autoload
(defun magit-bookmark--cherry-make-record ()
  "Create a Magit cherry bookmark."
  (magit-bookmark--make-record 'magit-cherry-mode
    #'magit-bookmark--cherry-jump
    (lambda (upstream head)
      `((defaults       . (,(magit-bookmark--cherry-make-name
                             (buffer-name) head upstream)))
        (magit-head     . ,head)
        (magit-upstream . ,upstream)))))

;;; Diff

;;;###autoload
(defun magit-bookmark--diff-jump (bookmark)
  "Handle a Magit diff BOOKMARK."
  (magit-bookmark--jump bookmark #'magit-diff-setup
    (bookmark-prop-get bookmark 'magit-rev-or-range)
    (bookmark-prop-get bookmark 'magit-const)
    (bookmark-prop-get bookmark 'magit-args)
    (bookmark-prop-get bookmark 'magit-files)))

(defun magit-bookmark--resolve (rev-or-range)
  "Return REV-OR-RANGE with ref names resolved to commit hashes."
  (pcase (magit-git-lines "rev-parse" rev-or-range)
    (`(,rev)
     (magit-rev-abbrev rev))
    ((and `(,rev1 ,rev2)
          (guard (/= ?^ (aref rev1 0)))
          (guard (=  ?^ (aref rev2 0))))
     (concat (magit-rev-abbrev (substring rev2 1))
             ".."
             (magit-rev-abbrev rev1)))
    ((and `(,rev1 ,rev2 ,rev3)
          (guard (/= ?^ (aref rev1 0)))
          (guard (/= ?^ (aref rev2 0)))
          (guard (=  ?^ (aref rev3 0))))
     (ignore rev3)
     (concat (magit-rev-abbrev rev1)
             "..."
             (magit-rev-abbrev rev2)))
    (_
     rev-or-range)))

(defun magit-bookmark--diff-make-name
    (buffer-name rev-or-range const _args files)
  "Generate a default name for a diff bookmark."
  (if (member "--no-index" const)
      (apply #'format "*magit-diff %s %s" files)
    (concat buffer-name " "
            (cond (rev-or-range)
                  ((member "--cached" const) "staged")
                  (t                       "unstaged"))
            (when files
              (concat " in " (mapconcat #'identity files ", "))))))

;;;###autoload
(defun magit-bookmark--diff-make-record ()
  "Create a Magit diff bookmark."
  (magit-bookmark--make-record 'magit-diff-mode
    #'magit-bookmark--diff-jump
    (lambda (rev-or-range const args files)
      (let ((resolved (magit-bookmark--resolve rev-or-range)))
        `((defaults           . (,(magit-bookmark--diff-make-name
                                   (buffer-name) resolved const args files)))
          (magit-rev-or-range . ,resolved)
          (magit-const        . ,const)
          (magit-args         . ,args)
          (magit-files        . ,files))))))

;;; Revision

;;;###autoload
(defun magit-bookmark--revision-jump (bookmark)
  "Handle a Magit revision BOOKMARK."
  (magit-bookmark--jump bookmark #'magit-show-commit
    (bookmark-prop-get bookmark 'magit-rev)
    (bookmark-prop-get bookmark 'args)
    (bookmark-prop-get bookmark 'files)))

(defun magit-bookmark--revision-make-name (buffer-name rev _args files)
  "Generate a default name for a revision bookmark."
  (let ((subject (magit-rev-format "%s" rev)))
    (concat buffer-name " "
            (magit-rev-abbrev rev)
            (cond (files   (concat " " (mapconcat #'identity files " ")))
                  (subject (concat " " subject))))))

;;;###autoload
(defun magit-bookmark--revision-make-record ()
  "Create a Magit revision bookmark."
  ;; magit-refresh-args stores the revision in relative form.
  ;; For bookmarks, the exact hash is more appropriate.
  (magit-bookmark--make-record 'magit-revision-mode
    #'magit-bookmark--revision-jump
    (lambda (_rev _ args files)
      `((defaults    . (,(magit-bookmark--revision-make-name
                          (buffer-name) magit-buffer-revision-hash
                          args files)))
        (magit-rev   . ,magit-buffer-revision-hash)
        (magit-args  . ,args)
        (magit-files . ,files)))))

;;; Stash

;;;###autoload
(defun magit-bookmark--stash-jump (bookmark)
  "Handle a Magit stash BOOKMARK."
  (magit-bookmark--jump bookmark #'magit-stash-show
    (bookmark-prop-get bookmark 'magit-stash)
    (bookmark-prop-get bookmark 'magit-args)
    (bookmark-prop-get bookmark 'magit-files)))

(defun magit-bookmark--stash-make-name (buffer-name stash _args files)
  "Generate the default name for a stash bookmark."
  (concat buffer-name " " stash " "
          (if files
              (mapconcat #'identity files " ")
            (magit-rev-format "%s" stash))))

;;;###autoload
(defun magit-bookmark--stash-make-record ()
  "Create a Magit stash bookmark."
  (magit-bookmark--make-record 'magit-stash-mode
    #'magit-bookmark--stash-jump
    (lambda (stash _ args files)
      `((defaults    . (,(magit-bookmark--stash-make-name
                          (buffer-name)
                          (magit-rev-abbrev magit-buffer-revision-hash)
                          args files)))
        (magit-stash . ,magit-buffer-revision-hash)
        (magit-args  . ,args)
        (magit-files . ,files)
        (magit-hidden-sections
         . ,(--map `(,(oref it type)
                     . ,(replace-regexp-in-string (regexp-quote stash)
                                                  magit-buffer-revision-hash
                                                  (oref it value)))
                   (--filter (oref it hidden)
                             (oref magit-root-section children))))))))

;;; Submodules

;;;###autoload
(defun magit-bookmark--submodules-jump (bookmark)
  "Handle a Magit submodule list BOOKMARK."
  (magit-bookmark--jump bookmark #'magit-list-submodules))

;;;###autoload
(defun magit-bookmark--submodules-make-record ()
  "Create a Magit submodule list bookmark."
  (magit-bookmark--make-record 'magit-submodule-list-mode
    #'magit-bookmark--submodules-jump))

(provide 'magit-bookmark)
;;; magit-bookmark.el ends here
