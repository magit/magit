;;; magit-revert.el --- the old buffer revert implementation  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Magit Project Contributors
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

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'magit-git)

;; For `magit-revert-buffers'
(declare-function magit-blame-mode 'magit-blame)
(defvar magit-blame-mode)

(defvar magit-revert-buffers-timer nil)

(defun magit-revert-buffers-set-timer ()
  (when (timerp magit-revert-buffers-timer)
    (cancel-timer magit-revert-buffers-timer))
  (setq magit-revert-buffers-timer
        (and (boundp 'magit-revert-buffers)
             (numberp magit-revert-buffers)
             (run-with-timer 0 (abs magit-revert-buffers)
                             'magit-revert-buffers-async))))

(defcustom magit-revert-buffers 'usage
  "How file-visiting buffers in the current repository are reverted.

After running certain commands, after refreshing the current
Magit buffer, unmodified buffers visiting files belonging to
the current repository may optionally be reverted.

`nil'     Don't revert any buffers.

`ask'     List the buffers which might potentially have to be
          reverted and ask the user whether she wants to revert
          them.  If so, then do it synchronously.

`t'       Revert the buffers synchronously, mentioning each one
          as it is being reverted and then also show a summary.

`usage'   Like `t' but include usage information in the summary.
          This is the default so that users come here and pick
          what is right for them.

`silent'  Revert the buffers synchronously and be quiet about it.
          This (or a negative number) is the recommended setting,
          because for the other values the revert messages might
          prevent you from seeing other, more important, messages
          in the echo area.

NUMBER    An integer or float.  Revert the buffers asynchronously.
          If NUMBER is positive, then mention each buffer as it is
          being reverted.  If it is negative, then be quiet about
          it.  If user input arrives, then stop reverting.  After
          (the absolute value of) NUMBER seconds resume reverting.

Also see option `magit-revert-buffers-only-for-tracked-files'."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type '(choice
          (const :tag "Don't revert" nil)
          (const :tag "Ask whether to revert" ask)
          (const :tag "Revert synchronously" t)
          (const :tag "Revert synchronously but in silence" silent)
          (const :tag "Revert synchronously with usage information" usage)
          (integer :tag "Revert asynchronously (interval in seconds)"))
  :set (lambda (var val)
         (set-default var val)
         (magit-revert-buffers-set-timer)))

(defcustom magit-revert-buffers-only-for-tracked-files t
  "Whether to revert only buffers that visit tracked files.

If non-nil, then only tracked files may be reverted.  If nil,
then all files in the current repository may potentially be
reverted.  Reverting untracked files should be safe and limiting
to only tracked files has the potential of causing very noticable
delays.

Also see option `magit-revert-buffers'."
  :package-version '(magit . "2.4.0")
  :group 'magit
  :type 'boolean)

(defcustom magit-revert-buffers-use-cache t
  "Whether to cache whether a given buffer is tracked or not.

Not caching this information drastically reduces performance
when many buffers are open and/or in large repositories.

When `magit-revert-buffers-only-for-tracked-files' is nil,
then this option is irrelevant."
  :package-version '(magit . "2.4.0")
  :group 'magit
  :type 'boolean)

(defcustom magit-after-revert-hook nil
  "Normal hook for `magit-revert-buffer' to run after reverting.

This hook is only run for buffers that were actually reverted.
For other buffers `magit-not-reverted-hook' is run instead."
  :package-version '(magit . "2.4.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-refresh-vc-mode-line))

(defcustom magit-not-reverted-hook nil
  "Normal hook for `magit-revert-buffer' to run instead of reverting.

This hook is only run for buffers which might have been reverted
but were not actually reverted, because that was not necessary.
For other buffers `magit-after-revert-hook' is run instead."
  :package-version '(magit . "2.4.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-refresh-vc-mode-line))

(defvar inhibit-magit-revert nil)
(defvar magit-revert-buffers-backlog nil)

(defun magit-revert-buffers (&optional force)
  "Revert unmodified file-visiting buffers of the current repository.

If either `magit-revert-buffers' is non-nil and `inhibit-magit-revert'
is nil, or if optional FORCE is non-nil; then revert all unmodified
buffers that visit files being tracked in the current repository.

When called interactively then the revert is forced."
  (interactive (list t))
  (-when-let* ((- (or force
                      (and magit-revert-buffers
                           (not inhibit-magit-revert))))
               (topdir (magit-toplevel))
               (buffers (magit-revert-list-modified-buffers topdir))
               (- (or force
                      (not (eq magit-revert-buffers 'ask))
                      (magit-confirm 'revert-buffer
                        "Revert %s from visited file"
                        "Revert %i buffers from visited files"
                        (mapcar #'buffer-name buffers)))))
    (if (numberp magit-revert-buffers)
        (magit-revert-buffers-async buffers)
      (magit-revert-buffers-sync buffers force))))

(add-hook 'git-commit-setup-hook #'magit-revert-buffers)

(defun magit-revert-buffers-sync (buffers force)
  (if (eq magit-revert-buffers 'silent)
      (mapc #'magit-revert-buffer buffers)
    (let ((cnt (length buffers)))
      (message "Reverting (up to) %s file-visiting buffer(s)..." cnt)
      (setq cnt (length (-non-nil (mapcar #'magit-revert-buffer buffers))))
      (if (> cnt 0)
          (let ((s (if (> cnt 1) "s" "")))
            (pcase magit-revert-buffers
              (`t
               (message "Reverting %s file-visiting buffer%s...done" cnt s))
              (`usage
               (message "Reverting %s file-visiting buffer%s...done%s%s%s" cnt s
                        (substitute-command-keys
                         "\n  This can be undone using `\\[undo]' in the ")
                        "affected buffers\n  Customize behavior using `M-x "
                        "customize-option RET magit-revert-buffers RET'"))
              ((or `nil `ask)
               (message "Reverting %s file-visiting buffer%s...done%s"
                        cnt s (if force " (forced)" "")))))
        (message "(No buffers need to be reverted)")))))

(defun magit-revert-buffers-async (&optional buffers)
  (setq buffers (nconc buffers (--filter (not (memq it buffers))
                                         magit-revert-buffers-backlog)))
  (while (and buffers (not (input-pending-p)))
    (let ((buf (pop buffers)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (magit-revert-buffer buf)))))
  (setq magit-revert-buffers-backlog buffers))

(defun magit-revert-buffer (&optional buffer)
  "Revert the current buffer.
If optional BUFFER is non-nil, then revert that instead.
The buffer is expected to visit a file.  Return t if the
buffer had to be reverted, nil otherwise."
  (with-current-buffer (or buffer (current-buffer))
    (if (and (file-readable-p buffer-file-name)
             (not (verify-visited-file-modtime (current-buffer))))
        (if magit-blame-mode
            (progn (message "Reverting %s inhibited due to magit-blame-mode"
                            buffer-file-name)
                   (run-hooks 'magit-not-reverted-hook)
                   nil)
          (if (or (eq magit-revert-buffers 'silent)
                  (and (numberp magit-revert-buffers)
                       (< magit-revert-buffers 0)))
              (revert-buffer 'ignore-auto t t)
            (message "Reverting buffer `%s'..." (buffer-name))
            (revert-buffer 'ignore-auto t t)
            (message "Reverting buffer `%s'...done" (buffer-name)))
          (run-hooks 'magit-after-revert-hook)
          t)
      (run-hooks 'magit-not-reverted-hook)
      nil)))

(defvar-local magit-revert-buffer-p nil)

(defun magit-revert-list-modified-buffers (topdir)
  (let ((tracked (and magit-revert-buffers-only-for-tracked-files
                      (not magit-revert-buffers-use-cache)
                      (magit-revision-files "HEAD"))))
    (--filter (let ((file (buffer-file-name it)))
                (and file
                     (equal (file-remote-p file)
                            (file-remote-p topdir))
                     (file-in-directory-p file topdir)
                     (cond (magit-revert-buffers-use-cache
                            (with-current-buffer it magit-revert-buffer-p))
                           (magit-revert-buffers-only-for-tracked-files
                            (member (file-relative-name file topdir) tracked))
                           (t t))))
              (buffer-list))))

(defun magit-revert-buffer-cache-files (files)
  (dolist (file files)
    (--when-let (find-buffer-visiting file)
      (with-current-buffer it
        (setq-local magit-revert-buffer-p t)))))

(defun magit-revert-buffer-cache-buffer ()
  (when (and magit-revert-buffers-use-cache
             buffer-file-name
             (magit-file-tracked-p buffer-file-name))
    (setq magit-revert-buffer-p t)))

(add-hook 'find-file-hook 'magit-revert-buffer-cache-buffer t t)

(defun magit-refresh-vc-mode-line ()
  "Update `vc-mode' which is displayed in the mode-line.
Like `vc-mode-line' but simpler, more efficient, and less buggy."
  (setq vc-mode
        (if vc-display-status
            (magit-with-toplevel
              (let* ((rev (or (magit-get-current-branch)
                              (magit-rev-parse "--short" "HEAD")))
                     (msg (cl-letf (((symbol-function #'vc-working-revision)
                                     (lambda (&rest _) rev)))
                            (vc-default-mode-line-string
                             'Git buffer-file-name))))
                (propertize
                 (concat " " msg)
                 'mouse-face 'mode-line-highlight
                 'help-echo (concat (get-text-property 0 'help-echo msg)
                                    "\nCurrent revision: " rev
                                    "\nmouse-1: Version Control menu")
                 'local-map vc-mode-line-map)))
          " Git"))
  (force-mode-line-update))

;;; magit-revert.el ends soon
(provide 'magit-revert)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-revert.el ends here
