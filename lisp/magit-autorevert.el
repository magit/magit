;;; magit-autorevert.el --- Revert buffers when files in repository change  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

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

(defcustom magit-auto-revert-explicitly t
  "Whether Magit reverts file-visiting buffers explicitly.

If this is non-nil and either `global-auto-revert-mode' or
`magit-auto-revert-mode' is enabled, then Magit immediately
reverts buffers by explicitly calling `auto-revert-buffers'
after running Git for side-effects."
  :package-version '(magit . "2.4.0")
  :group 'magit-auto-revert
  :type 'boolean)

;;; Mode

(defun magit-turn-on-auto-revert-mode-if-desired (&optional file)
  (if file
      (when-let ((buffer (find-buffer-visiting file)))
        (with-current-buffer buffer
          (magit-turn-on-auto-revert-mode-if-desired)))
    (when (and (not auto-revert-mode)        ; see #3014
               (not global-auto-revert-mode) ; see #3460
               buffer-file-name
               (file-readable-p buffer-file-name)
               (compat-call executable-find (magit-git-executable) t)
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
  ;; - When `global-auto-revert-mode' is enabled, then this mode is
  ;;   redundant.
  ;; - In all other cases enable the mode because if buffers are not
  ;;   automatically reverted that would make many very common tasks
  ;;   much more cumbersome.
  :init-value (not (or global-auto-revert-mode
                       noninteractive)))
;; - Unfortunately `:init-value t' only sets the value of the mode
;;   variable but does not cause the mode function to be called.
;; - I don't think it works like this on purpose, but since one usually
;;   should not enable global modes by default, it is understandable.
;; - If the user has set the variable `magit-auto-revert-mode' to nil
;;   after loading magit (instead of doing so before loading magit or
;;   by using the function), then we should still respect that setting.
;; - If the user enables `global-auto-revert-mode' after loading magit
;;   and after `after-init-hook' has run, then `magit-auto-revert-mode'
;;   remains enabled; and there is nothing we can do about it.
;; - However if the init file causes `magit-autorevert' to be loaded
;;   and only later it enables `global-auto-revert-mode', then we can
;;   and should leave `magit-auto-revert-mode' disabled.
(defun magit-auto-revert-mode--init-kludge ()
  "This is an internal kludge to be used on `after-init-hook'.
Do not use this function elsewhere, and don't remove it from
the `after-init-hook'.  For more information see the comments
and code surrounding the definition of this function."
  (if (or (not magit-auto-revert-mode)
          (and global-auto-revert-mode (not after-init-time)))
      (magit-auto-revert-mode -1)
    (let ((start (current-time)))
      (magit-message "Turning on magit-auto-revert-mode...")
      (magit-auto-revert-mode 1)
      (magit-message
       "Turning on magit-auto-revert-mode...done%s"
       (let ((elapsed (float-time (time-subtract nil start))))
         (if (> elapsed 0.2)
             (format " (%.3fs, %s buffers checked)" elapsed
                     (length (buffer-list)))
           ""))))))
(if after-init-time
    ;; Since `after-init-hook' has already been
    ;; run, turn the mode on or off right now.
    (magit-auto-revert-mode--init-kludge)
  ;; By the time the init file has been fully loaded the
  ;; values of the relevant variables might have changed.
  (add-hook 'after-init-hook #'magit-auto-revert-mode--init-kludge t))

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
Also, you should not use `after-init-hook' to disable this mode.")

;;; Implementation

(require 'vc-git)
(require 'diff-hl nil t)

(defvar magit-update-vc-state t)

(defvar magit-update-diff-hl t)

(defvar magit-auto-revert-suspend 0.2)

(defvar magit--pending-vc-git-states nil)

(defvar magit--pre-run-modified-files nil)

(defun magit-auto-revert-buffers ()
  (interactive)
  (when (magit--auto-revert-explicitly)
    (save-match-data
      (setq magit--pending-vc-git-states (magit--vc-git-states))
      (let* ((buffers (delete-dups
                       (nconc magit--pre-run-modified-files
                              (mapcar #'car magit--pending-vc-git-states))))
             (remaining auto-revert-remaining-buffers))
        (setq auto-revert-remaining-buffers nil)
        (dolist (buffer buffers)
          (cl-delete buffer remaining :count 1 :test #'eq))
        (unwind-protect
            (if (numberp magit-auto-revert-suspend)
                (with-timeout (magit-auto-revert-suspend
                               (message "-- timeout"))
                  (while buffers
                    (auto-revert-buffer (pop buffers))))
              (while (and buffers
		          (not (and (if (eq magit-auto-revert-suspend 'default)
                                        auto-revert-stop-on-user-input
                                      magit-auto-revert-suspend)
		                    (input-pending-p))))
                (auto-revert-buffer (pop buffers))))
          (setq magit--pending-vc-git-states nil)
          (setq auto-revert-remaining-buffers (nconc buffers remaining)))))))

(defun magit--vc-git-states ()
  (mapcar (lambda (line)
            (cons (substring line 2)
                  (vc-git--git-status-to-vc-state
                   (list (substring line 0 2)))))
          (magit--auto-revert-ls-files)))

(defun magit--auto-revert-ls-files ()
  (mapcar #'expand-file-name
          (magit-git-items "status" "-z" "--porcelain" "--ignore-submodules"
                           (and (not magit-auto-revert-tracked-only)
                                (list "--untracked" "--ignored")))))

(defun magit--auto-revert-explicitly ()
  (and magit-auto-revert-explicitly
       (or global-auto-revert-mode
           (and magit-auto-revert-mode auto-revert-buffer-list))))

(defun auto-revert-handler--magit-refresh-vc (fn)
  (cond
   ((and magit-update-vc-state
         magit--pending-vc-git-states
         buffer-file-name
         (eq (vc-backend buffer-file-name) 'Git))
    (cl-letf
        (((symbol-function #'vc-refresh-state)
          (lambda ()
            (let ((file buffer-file-name))
              (if-let ((state (cdr (assoc file magit--pending-vc-git-states))))
                  (vc-file-setprop file 'vc-state state)
                (vc-state-refresh file 'Git))
              (when magit-update-diff-hl
                (diff-hl-update))))))
      (let ((auto-revert-check-vc-info t))
        (funcall fn))))
   (t (funcall fn))))

(advice-add 'auto-revert-handler :around
            'auto-revert-handler--magit-refresh-vc)

(defun magit--cache-pre-modified-files ()
  (when (magit--auto-revert-explicitly)
    (setq magit--pre-run-modified-files (magit--auto-revert-ls-files))))

(add-hook 'magit-pre-call-git-hook  #'magit--cache-pre-modified-files)
(add-hook 'magit-pre-start-git-hook #'magit--cache-pre-modified-files)

;;; _
(provide 'magit-autorevert)
;;; magit-autorevert.el ends here
