;;; magit-autorevert.el --- Revert buffers when files in repository change  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2025 The Magit Project Contributors

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

;; This library implements support for automatically reverting buffers
;; when visited files in the repository change.

;; See (info "(magit)Automatic Reverting of File-Visiting Buffers").

;;; Code:

(require 'autorevert)

(declare-function magit-file-tracked-p "magit-git" (file))
(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-git-executable "magit-git" ())

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
  :type `(radio (const :tag "No filter" nil)
                (function-item ,#'magit-auto-revert-buffer-p)
                (function-item ,#'magit-auto-revert-repository-buffer-p)
                function))

(defcustom magit-auto-revert-tracked-only t
  "Whether `magit-auto-revert-mode' only reverts tracked files."
  :package-version '(magit . "2.4.0")
  :group 'magit-auto-revert
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (when (and (bound-and-true-p magit-auto-revert-mode)
                    (featurep 'magit-autorevert))
           (magit-auto-revert-mode -1)
           (magit-auto-revert-mode))))

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

;;; Mode

;;;###autoload
(progn ; magit-auto-revert-mode--initialize
  (defun magit-auto-revert-mode--initialize (symbol value)
    (internal--define-uninitialized-variable symbol)
    (if (not load-file-name)
        (custom-initialize-set symbol value)
      ;; Bugs in package managers prevent the use of lexical
      ;; bindings in autoloaded code.  See #5476 and #5485.
      (defalias 'magit-auto-revert-mode--after-load
        (apply-partially
         (lambda (symbol value mode-file file)
           (when (equal file mode-file)
             (remove-hook 'after-load-functions
                          'magit-auto-revert-mode--after-load)
             (fmakunbound 'magit-auto-revert-mode--after-load)
             (if after-init-time
                 (custom-initialize-set symbol value)
               ;; Delay activation in case the user disables the mode
               ;; after loading this library but still during startup.
               (defalias 'magit-auto-revert-mode--after-init
                 (apply-partially
                  (lambda (symbol value)
                    (remove-hook 'after-init-hook
                                 'magit-auto-revert-mode--after-init)
                    (fmakunbound 'magit-auto-revert-mode--after-init)
                    (custom-initialize-set symbol value))
                  symbol value))
               (add-hook 'after-init-hook 'magit-auto-revert-mode--after-init))))
         symbol value load-file-name))
      (add-hook 'after-load-functions 'magit-auto-revert-mode--after-load))))

(defun magit-turn-on-auto-revert-mode-if-desired (&optional file)
  (cond (file
         (let ((buffer (find-buffer-visiting file)))
           (when buffer
             (with-current-buffer buffer
               (magit-turn-on-auto-revert-mode-if-desired)))))
        ((and (not auto-revert-mode)        ; see #3014
              (not global-auto-revert-mode) ; see #3460
              buffer-file-name
              (or auto-revert-remote-files  ; see #5422
                  (not (file-remote-p buffer-file-name)))
              (file-readable-p buffer-file-name)
              (require 'magit-process)
              (executable-find (magit-git-executable) t)
              (magit-toplevel)
              (or (not magit-auto-revert-tracked-only)
                  (magit-file-tracked-p buffer-file-name)))
         (auto-revert-mode 1))))

;;;###autoload
(define-globalized-minor-mode magit-auto-revert-mode auto-revert-mode
  magit-turn-on-auto-revert-mode-if-desired
  :package-version '(magit . "2.4.0")
  :link '(info-link "(magit)Automatic Reverting of File-Visiting Buffers")
  :group 'magit-auto-revert
  :group 'magit-essentials
  :init-value (not (or global-auto-revert-mode noninteractive))
  :initialize #'magit-auto-revert-mode--initialize)

(defun magit-auto-revert-mode--disable ()
  "When enabling `global-auto-revert-mode', disable `magit-auto-revert-mode'."
  (when (and global-auto-revert-mode
             (bound-and-true-p magit-auto-revert-mode))
    (magit-auto-revert-mode -1)))

(add-hook 'global-auto-revert-mode-hook #'magit-auto-revert-mode--disable)

(put 'magit-auto-revert-mode 'function-documentation
     "Toggle Magit Auto Revert mode.

If called interactively, enable Magit Auto Revert mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

Magit Auto Revert mode is a global minor mode that reverts
buffers associated with a file that is located inside a Git
repository when the file changes on disk.  Use `auto-revert-mode'
to revert a particular buffer.  Or use `global-auto-revert-mode'
to revert all file-visiting buffers, not just those that visit
a file located inside a Git repository.

This global mode works by turning on the buffer-local mode
`auto-revert-mode' at the time a buffer is first created.  The
local mode is turned on if the visited file is being tracked in
a Git repository at the time when the buffer is created.

If `magit-auto-revert-tracked-only' is non-nil (the default),
then only tracked files are reverted.  But if you stage a
previously untracked file using `magit-stage', then this mode
notices that.

Unlike `global-auto-revert-mode', this mode never reverts any
buffers that are not visiting files.

The behavior of this mode can be customized using the options
in the `autorevert' and `magit-autorevert' groups.

This function calls the hook `magit-auto-revert-mode-hook'.

Like nearly every mode, this mode should be enabled or disabled
by calling the respective mode function, the reason being that
changing the state of a mode involves more than merely toggling
a single switch, so setting the mode variable is not enough.
Also, you should not use `after-init-hook' to disable this mode.

\(fn &optional ARG)")

;;;###autoload
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
    (require 'magit-process)
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

(define-advice auto-revert-buffers (:around (fn) buffer-list-filter)
  (cl-incf magit-auto-revert-counter)
  (if (or global-auto-revert-mode
          (not auto-revert-buffer-list)
          (not auto-revert-buffer-list-filter))
      (funcall fn)
    (let ((auto-revert-buffer-list
           (seq-filter auto-revert-buffer-list-filter
                       auto-revert-buffer-list)))
      (funcall fn))
    (unless auto-revert-timer
      (auto-revert-set-timer))))

;;; _
(provide 'magit-autorevert)
;; `cond-let' intentionally not used.
;;; magit-autorevert.el ends here
