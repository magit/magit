;;; ssh-w32.el --- use ssh-agent on windows-nt -*- lexical-binding: t -*-

;; Copyright (C) 2015  The Magit Project Contributors
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Package-Requires: ((emacs "24.4") (cl-lib "0.5") (dash "2.10.0") (magit "2.0.50"))
;; Homepage: https://github.com/magit/magit

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

;; This file provides functions to startup ssh-agent, prompt for
;; passphrases from the Windows console, and set the needed
;; environment variables in Emacs, so that pushes and pulls from magit
;; will not require entering any passphrase.

;;; Code:

(require 'dash)
(require 'magit-git) ; for git.exe location

;;; Options

(defgroup ssh-w32 ()
  "Using ssh-agent on `windows-nt'.")

(defcustom ssh-w32-add-executable
  (concat (file-name-directory magit-git-executable) "ssh-add.exe")
  "Location of ssh-add.exe"
  :group 'ssh-w32
  :type 'string)

(defcustom ssh-w32-agent-executable
  (concat (file-name-directory magit-git-executable) "ssh-agent.exe")
  "Location of ssh-agent.exe"
  :group 'ssh-w32
  :type 'string)

;;; Functions

(defun ssh-w32-add-keys ()
  "Add keys to ssh-agent."
  (pcase (call-process ssh-w32-add-executable nil nil nil "-l")
    (0 t)                ; ssh-agent running, has keys. Nothing to do.
    (1 (call-process-shell-command ; ssh-agent running, no keys. So add them.
        ;; Passphrase can only be entered in console, so use cmd.exe's `start' to get one.
        (concat "start \"ssh-add\" " (shell-quote-argument ssh-w32-add-executable))
        nil '(t t) t))
    (_ nil)))                           ; ssh-agent not running

(defun ssh-w32-start-agent (&rest _)
  "Start ssh-agent and add keys, as needed."
  (or (ssh-w32-add-keys)
      (let* ((pid
              (--first (-let (((&alist 'comm comm 'user user) (process-attributes it)))
                         (and (string= comm "ssh-agent.exe")
                              (string= user user-login-name)))
                       (list-system-processes)))
             (sock
              (when pid
                (catch 'ssh-sock
                  (dolist (sock-dir (directory-files temporary-file-directory t "\\`ssh-" t))
                    (dolist (sock-file (directory-files sock-dir t "\\`agent\.[0-9]+\\'" t))
                      ;; Follow the lead of msysgit's start-ssh-agent.cmd: replace %TEMP% with "/tmp".
                      (setq sock-file (replace-regexp-in-string (regexp-quote temporary-file-directory)
                                                                "/tmp/" sock-file))
                      (let ((process-environment (cons (concat "SSH_AUTH_SOCK=" sock-file)
                                                       process-environment)))
                        (when (ssh-w32-add-keys)
                          (throw 'ssh-sock sock-file)))))))))
        (if (not (and pid sock))
            (with-temp-buffer ; no running agent, so start one.
              (call-process ssh-w32-agent-executable nil '(t t))
              (goto-char 1)
              (while (re-search-forward "^\\(SSH_[^=]+\\)=\\([^;]+\\)" nil t)
                (setenv (match-string 1) (match-string 2)))
              (ssh-w32-add-keys))
          (setenv "SSH_AGENT_PID" (number-to-string pid))
          (setenv "SSH_AUTH_SOCK" sock)))))

;;; Hooking into magit

(advice-add 'magit-push :before #'ssh-w32-start-agent)

(provide 'ssh-w32)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; ssh-w32.el ends here
