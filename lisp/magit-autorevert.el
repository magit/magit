;;; magit-autorevert.el --- revert buffers when files in repository change  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
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

This option is provided by `magit', which also redefines
`auto-revert-buffers' to respect it.  Magit users who do not turn
on the local mode `auto-revert-mode' themselves, are best served
by setting the value to `magit-auto-revert-repository-buffers-p'.

However the default is nil, to not disturb users who do use the
local mode directly.  If you experience delays when running Magit
commands, then you should consider using one of the predicates
provided by Magit - especially if you also use Tramp.

Users who do turn on `auto-revert-mode' in buffers in which Magit
doesn't do that for them, should likely not use any filter.
Users who turn on `global-auto-revert-mode', do not have to worry
about this option, because it is disregarded if the global mode
is enabled."
  :package-version '(magit . "2.4.2")
  :group 'auto-revert
  :group 'magit-auto-revert
  :group 'magit-related
  :type '(radio (const :tag "no filter" nil)
                (function-item magit-auto-revert-buffer-p)
                (function-item magit-auto-revert-repository-buffer-p)
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
after running git for side-effects.

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

(defun magit-turn-on-auto-revert-mode-if-desired (&optional file)
  (if file
      (--when-let (find-buffer-visiting file)
        (with-current-buffer it
          (magit-turn-on-auto-revert-mode-if-desired)))
    (when (and buffer-file-name
               (file-readable-p buffer-file-name)
               (magit-toplevel)
               (or (not magit-auto-revert-tracked-only)
                   (magit-file-tracked-p buffer-file-name)))
      (auto-revert-mode))))

;;;###autoload
(defvar magit-revert-buffers t) ; obsolete

;;;###autoload
(define-globalized-minor-mode magit-auto-revert-mode auto-revert-mode
  magit-turn-on-auto-revert-mode-if-desired
  :package-version '(magit . "2.4.0")
  :link '(info-link "(magit)Automatic Reverting of File-Visiting Buffers")
  :group 'magit-auto-revert
  :group 'magit-essentials
  ;; When `global-auto-revert-mode' is enabled, then this mode is
  ;; redundant.  When `magit-revert-buffers' is nil, then the user has
  ;; opted out of the automatic reverts while the old implementation
  ;; was still in use.  In all other cases enable the mode because if
  ;; buffers are not automatically reverted that would make many very
  ;; common tasks much more cumbersome.
  :init-value (and (with-no-warnings magit-revert-buffers)
                   (not global-auto-revert-mode)
                   (not noninteractive)))
;; - Unfortunately `:init-value t' only sets the value of the mode
;;   variable but does not cause the mode function to be called.
;; - I don't think it works like this on purpose, but since one usually
;;   should not enable global modes by default, it is understandable.
;; - If the user has set the variable `magit-auto-revert-mode' to nil
;;   after loading magit (instead of doing so before loading magit or
;;   by using the function), then we should still respect that setting.
;; - If the user has set the obsolete variable `magit-revert-buffers'
;;   to nil before or after loading magit, then we should still respect
;;   that setting.
;; - If the user sets one of these variables after loading magit and
;;   after `after-init-hook' has run, then that won't have an effect
;;   and there is nothing we can do about it.
(defun magit-auto-revert-mode--init-kludge ()
  "This is an internal kludge to be used on `after-init-hook'.
Do not use this function elsewhere, and don't remove it from
the `after-init-hook'.  For more information see the comments
and code surrounding the definition of this function."
  ;; `magit-revert-buffers' may have been set to nil before the alias
  ;; had been established, so consult the value of both variables.
  (if (and magit-auto-revert-mode (with-no-warnings magit-revert-buffers))
      (let ((start (current-time)))
        (magit-message "Turning on magit-auto-revert-mode...")
        (magit-auto-revert-mode 1)
        (magit-message
         "Turning on magit-auto-revert-mode...done%s"
         (let ((elapsed (float-time (time-subtract (current-time) start))))
           (if (> elapsed 0.2)
               (format " (%.3fs, %s buffers checked)" elapsed
                       (length (buffer-list)))
             ""))))
    (magit-auto-revert-mode -1)))
(if after-init-time
    ;; Since `after-init-hook' has already been
    ;; run, turn the mode on or off right now.
    (magit-auto-revert-mode--init-kludge)
  ;; By the time the init file has been fully loaded the
  ;; values of the relevant variables might have changed.
  (add-hook 'after-init-hook #'magit-auto-revert-mode--init-kludge t))

(put 'magit-auto-revert-mode 'function-documentation
     "Toggle Magit Auto Revert mode.
With a prefix argument ARG, enable Magit Auto Revert mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

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

This function calls the hook `magit-auto-revert-mode-hook'.")

(defun magit-auto-revert-buffers ()
  (when (and magit-auto-revert-immediately
             (or global-auto-revert-mode
                 (and magit-auto-revert-mode auto-revert-buffer-list)))
    (let ((auto-revert-buffer-list-filter
           (or auto-revert-buffer-list-filter
               'magit-auto-revert-repository-buffer-p)))
      (auto-revert-buffers))))

(defvar magit-auto-revert-toplevel nil)

(when (< emacs-major-version 25)
  (defvar auto-revert-buffers-counter 1
    "Incremented each time `auto-revert-buffers' is called"))

(defun magit-auto-revert-buffer-p (buffer)
  "Return t if BUFFER visits a file inside the current repository.
The current repository is the one in which `default-directory' is
located.  If there is no current repository, then return t for
any BUFFER."
  (magit-auto-revert-repository-buffer-p buffer t))

(defun magit-auto-revert-repository-buffer-p (buffer &optional fallback)
  "Return t if BUFFER visits a file inside the current repository.
The current repository is the one in which `default-directory' is
located.  If there is no current repository, then return FALLBACK
\(which defaults to nil) for any BUFFER."
  ;; Call `magit-toplevel' just once per cycle.
  (unless (and magit-auto-revert-toplevel
               (= (cdr magit-auto-revert-toplevel)
                  auto-revert-buffers-counter))
    (setq magit-auto-revert-toplevel
          (cons (or (magit-toplevel) 'no-repo)
                auto-revert-buffers-counter)))
  (let ((top (car magit-auto-revert-toplevel)))
    (if (eq top 'no-repo)
        fallback
      (let ((dir (with-current-buffer buffer default-directory)))
        (and (equal (file-remote-p dir)
                    (file-remote-p top))
             ;; ^ `tramp-handle-file-in-directory-p' lacks this optimization.
             (file-in-directory-p dir top))))))

(defun auto-revert-buffers--buffer-list-filter ()
  (when (< emacs-major-version 25)
    (cl-incf auto-revert-buffers-counter))
  (when auto-revert-buffer-list-filter
    (setq auto-revert-buffer-list
          (--filter auto-revert-buffer-list-filter
                    auto-revert-buffer-list))))

(advice-add 'auto-revert-buffers :before
            'auto-revert-buffers--buffer-list-filter)

(provide 'magit-autorevert)
;;; magit-autorevert.el ends here
