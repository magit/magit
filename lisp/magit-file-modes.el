;;; magit-file-modes.el --- modes for files and blobs  -*- lexical-binding: t -*-

;; Copyright (C) 2020  The Magit Project Contributors
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

;; This library defines modes useful in buffers visiting files and
;; blobs.

;;; Code:

(require 'cl-lib)

(declare-function magit-inside-worktree-p "magit-git")

;;; File Mode

(defvar magit-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-xg"    'magit-status)
    (define-key map "\C-x\M-g" 'magit-dispatch)
    (define-key map "\C-c\M-g" 'magit-file-dispatch)
    map)
  "Keymap for `magit-file-mode'.")

(defvar magit-file-mode-lighter "")

(define-minor-mode magit-file-mode
  "Enable some Magit features in a file-visiting buffer.

Currently this only adds the following key bindings.
\n\\{magit-file-mode-map}"
  :package-version '(magit . "2.2.0")
  :lighter magit-file-mode-lighter
  :keymap  magit-file-mode-map)

(defun magit-file-mode-turn-on ()
  (and buffer-file-name
       (magit-inside-worktree-p t)
       (magit-file-mode)))

;;;###autoload
(define-globalized-minor-mode global-magit-file-mode
  magit-file-mode magit-file-mode-turn-on
  :package-version '(magit . "2.13.0")
  :link '(info-link "(magit)Minor Mode for Buffers Visiting Files")
  :group 'magit-essentials
  :group 'magit-modes
  :init-value t)
;; Unfortunately `:init-value t' only sets the value of the mode
;; variable but does not cause the mode function to be called, and we
;; cannot use `:initialize' to call that explicitly because the option
;; is defined before the functions, so we have to do it here.
(cl-eval-when (load eval)
  (when global-magit-file-mode
    (global-magit-file-mode 1)))

;;; Blob Mode

(defvar magit-blob-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-file-mode-map)
    (define-key map "p" 'magit-blob-previous)
    (define-key map "n" 'magit-blob-next)
    (define-key map "b" 'magit-blame-addition)
    (define-key map "r" 'magit-blame-removal)
    (define-key map "f" 'magit-blame-reverse)
    (define-key map "q" 'magit-kill-this-buffer)
    map)
  "Keymap for `magit-blob-mode'.")

(define-minor-mode magit-blob-mode
  "Enable some Magit features in blob-visiting buffers.

Currently this only adds the following key bindings.
\n\\{magit-blob-mode-map}"
  :package-version '(magit . "2.3.0"))

;;; _
(provide 'magit-file-modes)
;;; magit-file-modes.el ends here
