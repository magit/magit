;;; with-editor.el --- use the Emacsclient as $EDITOR -*- lexical-binding: t -*-

;; Copyright (C) 2014  The Magit Project Developers

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/git-modes

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Use the Emacsclient as $EDITOR.

;;; Code:

(require 'cl-lib)
(require 'server)

;;; Options

(defgroup with-editor nil
  "Use the Emacsclient as $EDITOR."
  :group 'external
  :group 'server)

(defcustom with-editor-emacsclient-executable
  (ignore-errors
    (shell-quote-argument
     (let ((version
            (format "%s.%s" emacs-major-version emacs-minor-version)))
       (or (and (eq system-type 'darwin)
                (let ((exec-path
                       (list (expand-file-name "bin" invocation-directory))))
                  (executable-find "emacsclient")))
           (executable-find (format "emacsclient%s"   version))
           (executable-find (format "emacsclient-%s"   version))
           (executable-find (format "emacsclient%s.exe" version))
           (executable-find (format "emacsclient-%s.exe" version))
           (executable-find (format "emacsclient%s"   emacs-major-version))
           (executable-find (format "emacsclient-%s"   emacs-major-version))
           (executable-find (format "emacsclient%s.exe" emacs-major-version))
           (executable-find (format "emacsclient-%s.exe" emacs-major-version))
           (executable-find "emacsclient")
           (executable-find "emacsclient.exe")))))
  "The Emacsclient executable used by the `with-editor' macro."
  :group 'with-editor
  :type '(choice (string :tag "Executable")
                 (const  :tag "Don't use Emacsclient" nil)))

(defcustom with-editor-finish-query-functions nil
  "List of functions called to query before finishing session.

The buffer in question is current while the functions are called.
If any of them returns nil, then the session is not finished and
the buffer is not killed.  The user should then fix the issue and
try again.  The functions are called with one argument.  If it is
non-nil then that indicates that the user used a prefix argument
to force finishing the session despite issues.  Functions should
usually honor that and return non-nil."
  :group 'with-editor
  :type 'hook)
(put 'with-editor-finish-query-functions 'permanent-local t)

(defcustom with-editor-cancel-query-functions nil
  "List of functions called to query before canceling session.

The buffer in question is current while the functions are called.
If any of them returns nil, then the session is not canceled and
the buffer is not killed.  The user should then fix the issue and
try again.  The functions are called with one argument.  If it is
non-nil then that indicates that the user used a prefix argument
to force canceling the session despite issues.  Functions should
usually honor that and return non-nil."
  :group 'with-editor
  :type 'hook)
(put 'with-editor-cancel-query-functions 'permanent-local t)

(defcustom with-editor-mode-lighter " WE"
  "The mode-line lighter of the With-Editor mode."
  :group 'with-editor
  :type '(choice (const :tag "No lighter" "") string))

;;; Commands

(defvar with-editor-finish-noclient-hook nil)
(defvar with-editor-pre-finish-hook nil)
(defvar with-editor-pre-cancel-hook nil)
(put 'with-editor-finish-noclient-hook 'permanent-local t)
(put 'with-editor-pre-finish-hook 'permanent-local t)
(put 'with-editor-pre-cancel-hook 'permanent-local t)

(defvar with-editor-show-usage t)
(defvar with-editor-cancel-message nil)
(defvar with-editor-previous-winconf nil)
(make-variable-buffer-local 'with-editor-show-usage)
(make-variable-buffer-local 'with-editor-cancel-message)
(make-variable-buffer-local 'with-editor-previous-winconf)
(put 'with-editor-cancel-message 'permanent-local t)
(put 'with-editor-previous-winconf 'permanent-local t)

(defun with-editor-finish (force)
  "Finish the current edit session."
  (interactive "P")
  (when (run-hook-with-args-until-failure
         'with-editor-finish-query-functions force)
    (run-hooks 'with-editor-pre-finish-hook)
    (with-editor-return nil)))

(defun with-editor-cancel (force)
  "Cancel the current edit session."
  (interactive "P")
  (when (run-hook-with-args-until-failure
         'with-editor-cancel-query-functions force)
    (let ((message with-editor-cancel-message))
      (when (functionp message)
        (setq message (funcall message)))
      (run-hooks 'with-editor-pre-cancel-hook)
      (with-editor-return t)
      (accept-process-output nil 0.1)
      (message (or message "Canceled by user")))))

(defun with-editor-return (cancel)
  (let ((winconf with-editor-previous-winconf)
        (clients server-buffer-clients))
    (remove-hook 'kill-buffer-query-functions
                 'with-editor-kill-buffer-noop t)
    (cond (cancel
           (when (< emacs-major-version 24)
             (erase-buffer))
           (save-buffer)
           (if clients
               (dolist (client clients)
                 (ignore-errors
                   (server-send-string client "-error Canceled by user"))
                 (delete-process client))
             (kill-buffer)))
          (t
           (save-buffer)
           (if clients
               (server-edit)
             (run-hooks 'with-editor-finish-noclient-hook)
             (kill-buffer))))
    (when (and winconf (eq (window-configuration-frame winconf)
                           (selected-frame)))
      (set-window-configuration winconf))))

;;; Mode

(defvar with-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c"                   'with-editor-finish)
    (define-key map [remap server-edit]          'with-editor-finish)
    (define-key map "\C-c\C-k"                   'with-editor-cancel)
    (define-key map [remap kill-buffer]          'with-editor-cancel)
    (define-key map [remap ido-kill-buffer]      'with-editor-cancel)
    (define-key map [remap iswitchb-kill-buffer] 'with-editor-cancel)
    map))

(define-minor-mode with-editor-mode
  "Edit a file as the $EDITOR of an external process."
  :lighter with-editor-mode-lighter
  ;; Protect the user from killing the buffer without using
  ;; either `with-editor-finish' or `with-editor-cancel',
  ;; and from removing the key bindings for these commands.
  (unless with-editor-mode
    (error "With-Editor mode cannot be turned off"))
  (add-hook 'kill-buffer-query-functions
            'with-editor-kill-buffer-noop nil t)
  ;; `server-excecute' displays a message which is not
  ;; correct when using this mode.
  (when with-editor-show-usage
    (with-editor-usage-message)))

(put 'with-editor-mode 'permanent-local t)

(defun with-editor-kill-buffer-noop ()
  (message (substitute-command-keys "\
Don't kill this buffer.  Instead cancel using \\[with-editor-cancel]")))

(defun with-editor-usage-message ()
  ;; Run after `server-execute', which is run using
  ;; a timer which starts immediately.
  (run-with-timer
   0.01 nil `(lambda ()
               (with-current-buffer ,(current-buffer)
                 (message (substitute-command-keys "\
Type \\[with-editor-finish] to finish, \
or \\[with-editor-cancel] to cancel"))))))

;;; Wrappers

(defmacro with-editor (&rest body)
  "Use the Emacsclient as $EDITOR while evaluating BODY.
Modify the `process-environment' for processes started in BODY,
instructing them to use the Emacsclient as $EDITOR.  If optional
ENVVAR is provided then bind that environment variable instead.
\n(fn [ENVVAR] &rest BODY)"
  (declare (indent defun))
  (let ((envvar (if (stringp (car body)) (pop body) "EDITOR")))
    `(if (tramp-tramp-file-p default-directory)
         (error "Implementation does not handle Tramp yet")
       (let ((process-environment process-environment))
         ;; Make sure server-use-tcp's value is valid.
         (unless (featurep 'make-network-process '(:family local))
           (setq server-use-tcp t))
         ;; Make sure the server is running.
         (unless server-process
           (when (server-running-p server-name)
             (setq server-name (format "server%s" (emacs-pid)))
             (when (server-running-p server-name)
               (server-force-delete server-name)))
           (server-start))
         ;; Tell Git to use the Emacsclient.
         (setenv ,envvar
                 (concat with-editor-emacsclient-executable
         ;; Tell the process where the server file is.
                         (and (not server-use-tcp)
                              (concat " --socket-name="
                                      (expand-file-name server-name
                                                        server-socket-dir)))))
         (when server-use-tcp
           (setenv "EMACS_SERVER_FILE"
                   (expand-file-name server-name server-auth-dir)))
         ;; As last resort fallback to a new Emacs instance.
         (setenv "ALTERNATE_EDITOR"
                 (expand-file-name invocation-name invocation-directory))
         ,@body))))

(defadvice server-switch-buffer (around with-editor activate)
  "If the buffer being switched to has a buffer-local value for
`server-window' then use that instead of the default value, and
finally delete the local value.  To use this, add a function to
`server-visit-hook' which possibly sets the local value in the
current buffer (which is the one requested by the client)."
  (let ((server-window (with-current-buffer
                           (or next-buffer (current-buffer))
                         server-window)))
    ad-do-it
    (when next-buffer
      (with-current-buffer next-buffer
        (kill-local-variable 'server-window)))))

;;; with-editor.el ends soon

(defconst with-editor-font-lock-keywords
  '(("(\\(with-\\(?:git-\\)?editor\\)\\_>" . 1)))
(font-lock-add-keywords 'emacs-lisp-mode with-editor-font-lock-keywords)

(provide 'with-editor)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; with-editor.el ends here
