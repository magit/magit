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

;;; Options

(defgroup with-editor nil
  "Use the Emacsclient as $EDITOR."
  :group 'external
  :group 'server)

(defcustom with-editor-emacsclient-executable
  (ignore-errors
    (shell-quote-argument
     (let ((version (format "%s.%s"
                            emacs-major-version
                            emacs-minor-version)))
       (or (and (eq system-type 'darwin)
                (let ((exec-path
                       (list (expand-file-name "bin" invocation-directory))))
                  (executable-find "emacsclient")))
           (executable-find (format "emacsclient%s"   version))
           (executable-find (format "emacsclient-%s"   version))
           (executable-find (format "emacsclient%s.exe" version))
           (executable-find (format "emacsclient-%s.exe" version))
           (executable-find "emacsclient")
           (executable-find "emacsclient.exe")))))
  "The Emacsclient executable."
  :group 'with-process
  :type '(choice (string :tag "Executable")
                 (const :tag "Don't use Emacsclient" nil)))

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

(defmacro with-git-editor (&rest body)
  "Use the Emacsclient as $GIT_EDITOR while evaluating BODY.
Modify the `process-environment' for processes started in BODY,
instructing them to use the Emacsclient as $GIT_EDITOR.  If
optional ENVVAR is provided then bind that environment variable
instead.
\n(fn [ENVVAR] &rest BODY)"
  (declare (indent defun))
  `(with-editor ,(if (stringp (car body)) (pop body) "GIT_EDITOR")
     (let ((magit-process-popup-time -1))
       ,@body)))

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
  '(("(\\(with-\\(?:git-\\)?editor\\)\\>" . 1)))
(font-lock-add-keywords 'emacs-lisp-mode with-editor-font-lock-keywords)

(provide 'with-editor)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; with-editor.el ends here
