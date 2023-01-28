;;; magit-autorevert.el --- Revert buffers when files in repository change  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2023 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

;;; Code:

(require 'magit-git)

(require 'autorevert)

;;; Options

(defgroup magit-auto-revert nil
  "Revert buffers when files in repository change."
  :link '(custom-group-link auto-revert)
  :link '(info-link "(magit)Automatic Reverting of File-Visiting Buffers")
  :group 'auto-revert
  :group 'magit-essentials
  :group 'magit-modes)

(defcustom auto-revert-buffer-list-filter nil
  "Filter that determines which buffers `auto-revert-buffers' reverts.

This option is provided by Magit, which also advises
`auto-revert-buffers' to respect it.  Magit users who do not turn
on the local mode `auto-revert-mode' themselves, are best served
by setting the value to `magit-auto-revert-repository-buffer-p'.

However the default is nil, so as not to disturb users who do use
the local mode directly.  If you experience delays when running
Magit commands, then you should consider using one of the
predicates provided by Magit - especially if you also use Tramp.

Users who do turn on `auto-revert-mode' in buffers in which Magit
doesn't do that for them, should likely not use any filter.
Users who turn on `global-auto-revert-mode', do not have to worry
about this option, because it is disregarded if the global mode
is enabled."
  :package-version '(magit . "2.4.2")
  :group 'auto-revert
  :group 'magit-auto-revert
  :group 'magit-related
  :type '(radio (const :tag "No filter" nil)
                (function-item magit-auto-revert-buffer-p)
                (function-item magit-auto-revert-repository-buffer-p)
                function))

(defcustom magit-auto-revert-tracked-only t
  "Obsolesced.  `magit-auto-revert-mode' applies only to magit tracked files.
To auto-revert all files, use `global-auto-revert-mode'."
  :package-version '(magit . "2.4.0")
  :group 'magit-auto-revert
  :type 'boolean)
(make-obsolete-variable 'magit-auto-revert-tracked-only
                        'global-auto-revert-mode
                        "Magit 3.3.1")

(defcustom magit-auto-revert-immediately t
  "Whether Magit reverts buffers immediately.

If this is non-nil and either `global-auto-revert-mode' or
`magit-auto-revert-mode' is enabled, then Magit immediately
reverts buffers by explicitly calling `auto-revert-buffers'
after running Git for side-effects.

If `auto-revert-use-notify' is non-nil (and file notifications
are actually supported), then `magit-auto-revert-immediately'
does not have to be non-nil, because the reverts happen
immediately anyway.

If `magit-auto-revert-immediately' and `auto-revert-use-notify'
are both nil, then reverts happen after `auto-revert-interval'
seconds of user inactivity.  That is not desirable."
  :package-version '(magit . "2.4.0")
  :group 'magit-auto-revert
  :type 'boolean)

(defcustom magit-auto-revert-mode (not noninteractive)
  "Turns on `auto-revert-mode' for magit tracked files."
  :package-version '(magit . "2.4.0")
  :group 'magit-auto-revert
  :type 'boolean
  :set (lambda (variable value)
	 (if (featurep 'magit-autorevert)
	     (magit-auto-revert-mode (unless value 0))
           (set-default variable value))))

;;; Mode

(defun magit-turn-on-auto-revert-mode-if-desired (&optional file)
  (if file
      (--when-let (find-buffer-visiting file)
        (with-current-buffer it
          (magit-turn-on-auto-revert-mode-if-desired)))
    (when (and (not auto-revert-mode)        ; see #3014
               (not global-auto-revert-mode) ; see #3460
               buffer-file-name
               (file-readable-p buffer-file-name)
               (compat-call executable-find (magit-git-executable) t)
               (magit-toplevel)
               (magit-file-tracked-p buffer-file-name))
      (auto-revert-mode 1))))

;;;###autoload
(defun magit-auto-revert-mode (&optional arg)
  "ARG follows same rules as those in any minor mode."
  (interactive (list 'toggle))
  (set-default 'magit-auto-revert-mode
               (if (eq arg 'toggle)
                   (not magit-auto-revert-mode)
                 (or (not (numberp arg)) (>= arg 1))))
  (if magit-auto-revert-mode
      (add-hook 'after-change-major-mode-hook
                #'magit-turn-on-auto-revert-mode-if-desired
                ;; depth 1 sequences this after global-auto-revert-mode
                1)
    (remove-hook 'after-change-major-mode-hook
                 #'magit-turn-on-auto-revert-mode-if-desired))
  (run-hooks 'magit-auto-revert-mode-hook)
  (if magit-auto-revert-mode
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (magit-turn-on-auto-revert-mode-if-desired)))
    ;; Cannot turn off auto-revert-mode since...
    (ignore "...provenance of auto-revert-mode unknown."))
  (when (eq arg 'toggle)
    (magit-message "magit-auto-revert-mode %s"
                   (if magit-auto-revert-mode "enabled" "disabled"))))

(put 'magit-auto-revert-mode 'function-documentation
     "Toggle Magit Auto Revert mode.
If called interactively, enable Magit Auto Revert mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Magit Auto Revert mode reverts buffers associated with a file
that is located inside a Git repository when the file changes on
disk.  Use `auto-revert-mode' to revert a particular buffer.  Or
use `global-auto-revert-mode' to revert all file-visiting
buffers, not just those that visit a file located inside a Git
repository.

This function calls the hook `magit-auto-revert-mode-hook'.")

(defun magit-auto-revert-buffers ()
  (when (and magit-auto-revert-immediately
             (or global-auto-revert-mode
                 (and magit-auto-revert-mode auto-revert-buffer-list)))
    (let ((auto-revert-buffer-list-filter
           (or auto-revert-buffer-list-filter
               #'magit-auto-revert-repository-buffer-p)))
      (auto-revert-buffers))))

(defvar magit-auto-revert-toplevel nil)

(defvar magit-auto-revert-counter 1
  "Incremented each time `auto-revert-buffers' is called.")

(defun magit-auto-revert-buffer-p (buffer)
  "Return non-nil if BUFFER visits a file inside the current repository.
The current repository is the one containing `default-directory'.
If there is no current repository, then return t for any BUFFER."
  (magit-auto-revert-repository-buffer-p buffer t))

(defun magit-auto-revert-repository-buffer-p (buffer &optional fallback)
  "Return non-nil if BUFFER visits a file inside the current repository.
The current repository is the one containing `default-directory'.
If there is no current repository, then return FALLBACK (which
defaults to nil) for any BUFFER."
  ;; Call `magit-toplevel' just once per cycle.
  (unless (and magit-auto-revert-toplevel
               (= (cdr magit-auto-revert-toplevel)
                  magit-auto-revert-counter))
    (setq magit-auto-revert-toplevel
          (cons (or (magit-toplevel) 'no-repo)
                magit-auto-revert-counter)))
  (let ((top (car magit-auto-revert-toplevel)))
    (if (eq top 'no-repo)
        fallback
      (let ((dir (buffer-local-value 'default-directory buffer)))
        (and (equal (file-remote-p dir)
                    (file-remote-p top))
             ;; ^ `tramp-handle-file-in-directory-p' lacks this optimization.
             (file-in-directory-p dir top))))))

(defun auto-revert-buffers--buffer-list-filter (fn)
  (cl-incf magit-auto-revert-counter)
  (if (or global-auto-revert-mode
          (not auto-revert-buffer-list)
          (not auto-revert-buffer-list-filter))
      (funcall fn)
    (let ((auto-revert-buffer-list
           (-filter auto-revert-buffer-list-filter
                    auto-revert-buffer-list)))
      (funcall fn))
    (unless auto-revert-timer
      (auto-revert-set-timer))))

(advice-add 'auto-revert-buffers :around
            #'auto-revert-buffers--buffer-list-filter)

(magit-auto-revert-mode (unless magit-auto-revert-mode 0))

;;; _
(provide 'magit-autorevert)
;;; magit-autorevert.el ends here
