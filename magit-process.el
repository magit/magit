;;; magit-process.el --- process functionality

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

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

(require 'with-editor)
(require 'magit-utils)
(require 'magit-section)
(require 'magit-git)
(require 'magit-mode)

(require 'autorevert)

(eval-when-compile (require 'dired))
(declare-function dired-uncache 'dired)

;;; Options

(defcustom magit-process-buffer-name-format "*magit-process: %a*"
  "Name format for buffers where output of processes is put.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'string)

(defcustom magit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the Git process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables Magit to prompt for passphrases when needed."
  :group 'magit-process
  :type '(choice (const :tag "pipe" nil)
                 (const :tag "pty" t)))

(defcustom magit-process-popup-time -1
  "Popup the process buffer if a command takes longer than this many seconds."
  :group 'magit-process
  :type '(choice (const :tag "Never" -1)
                 (const :tag "Immediately" 0)
                 (integer :tag "After this many seconds")))

(defcustom magit-process-log-max 32
  "Maximum number of sections to keep in a process log buffer.
When adding a new section would go beyond the limit set here,
then the older half of the sections are remove.  Sections that
belong to processes that are still running are never removed."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'integer)

(defcustom magit-process-yes-or-no-prompt-regexp
  " [\[(]\\([Yy]\\(?:es\\)?\\)[/|]\\([Nn]o?\\)[\])] ?[?:] ?$"
  "Regexp matching Yes-or-No prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'regexp)

(defcustom magit-process-password-prompt-regexps
  '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
    "^\\(Enter \\)?[Pp]assword\\( for '.*'\\)?: ?$"
    "^.*'s password: ?$"
    "^Yubikey for .*: ?$")
  "List of regexps matching password prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(repeat (regexp)))

(defcustom magit-process-username-prompt-regexps
  '("^Username for '.*': ?$")
  "List of regexps matching username prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(repeat (regexp)))

(defface magit-process-ok
  '((t :inherit magit-section-heading :foreground "green"))
  "Face for zero exit-status."
  :group 'magit-faces)

(defface magit-process-ng
  '((t :inherit magit-section-heading :foreground "red"))
  "Face for non-zero exit-status."
  :group 'magit-faces)

;;; Process Mode

(defvar magit-process-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-process-mode'.")

(define-derived-mode magit-process-mode magit-mode "Magit Process"
  "Mode for looking at Git process output."
  :group 'magit-process)

(defun magit-process-buffer (&optional topdir)
  (or (magit-mode-get-buffer magit-process-buffer-name-format
                             'magit-process-mode topdir)
      (with-current-buffer (magit-mode-get-buffer-create
                            magit-process-buffer-name-format
                            'magit-process-mode topdir)
        (magit-process-mode)
        (let ((inhibit-read-only t))
          (make-local-variable 'text-property-default-nonsticky)
          (magit-insert-section (processbuf)
            (insert "\n")))
        (current-buffer))))

(defun magit-process ()
  "Display Magit process buffer."
  (interactive)
  (let ((buf (magit-process-buffer)))
    (if (buffer-live-p buf)
        (pop-to-buffer buf)
      (user-error "Process buffer doesn't exist"))))

(defun magit-process-kill ()
  "Kill the process at point."
  (interactive)
  (magit-section-when process
    (let ((process (magit-section-value it)))
      (if (eq (process-status process) 'run)
          (when (magit-confirm 'kill-process)
            (kill-process process))
        (user-error "Process isn't running")))))

;;; Synchronous Processes

(defun magit-run-git (&rest args)
  "Call Git synchronously in a separate process, and refresh.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-standard-options' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.
Unmodified buffers visiting files that are tracked in the current
repository are reverted if `magit-auto-revert-mode' is active.

Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name-format'."
  (magit-call-git args)
  (magit-refresh))

(defun magit-call-git (&rest args)
  "Call Git synchronously in a separate process.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-standard-options' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name-format'."
  (run-hooks 'magit-pre-call-git-hook)
  (let ((process-environment process-environment))
    (setenv "GIT_LITERAL_PATHSPECS" "1")
    (apply #'magit-call-process magit-git-executable
           (magit-process-git-arguments args))))

(defun magit-call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.
Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name-format'."
  (cl-destructuring-bind (process-buf . section)
      (magit-process-setup program args)
    (magit-process-finish
     (let ((inhibit-read-only t))
       (apply #'process-file program nil process-buf nil args))
     process-buf (current-buffer) default-directory section)))

(defun magit-run-git-with-input (input &rest args)
  "Call Git in a separate process.
ARGS is flattened and then used as arguments to Git.

The first argument, INPUT, should be a buffer or the name of
an existing buffer.  The content of that buffer is used as the
process' standard input.  It may also be nil in which case the
current buffer is used.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-standard-options' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.
Unmodified buffers visiting files that are tracked in the current
repository are reverted if `magit-auto-revert-mode' is active.
When INPUT is nil then do not refresh any buffers.

This function actually starts a asynchronous process, but it then
waits for that process to return."
  (declare (indent 1))
  (magit-start-git (or input (current-buffer)) args)
  (magit-process-wait)
  (when input (magit-refresh)))

(defvar magit-this-process nil)

(defun magit-run-git-with-logfile (file &rest args)
  "Call Git in a separate process and log its output to FILE.
See `magit-run-git' for more information.
This function might have a short halflive."
  (magit-start-git nil args)
  (process-put magit-this-process 'logfile file)
  (set-process-filter magit-this-process 'magit-process-logfile-filter)
  (magit-process-wait)
  (magit-refresh))

;;; Asynchronous Processes

(defun magit-run-git-async (&rest args)
  "Start Git, prepare for refresh, and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when `magit-start-process' was called (if it is a Magit
buffer and still alive), as well as the respective Magit status
buffer.  Unmodified buffers visiting files that are tracked in
the current repository are reverted if `magit-auto-revert-mode'
is active.

See `magit-start-process' for more information."
  (message "Running %s %s" magit-git-executable
           (mapconcat 'identity (-flatten args) " "))
  (magit-start-git nil args))

(defun magit-start-git (input &rest args)
  "Start Git, prepare for refresh, and return the process object.

If INPUT is non-nil, it has to be a buffer or the name of an
existing buffer.  The buffer content becomes the processes
standard input.

Option `magit-git-executable' specifies the Git executable and
option `magit-git-standard-options' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns some buffers are refreshed: the buffer that was
current when `magit-start-process' was called (if it is a Magit
buffer and still alive), as well as the respective Magit status
buffer.  Unmodified buffers visiting files that are tracked in
the current repository are reverted if `magit-auto-revert-mode'
is active.

See `magit-start-process' for more information."
  (run-hooks 'magit-pre-start-git-hook)
  (let ((process-environment process-environment))
    (setenv "GIT_LITERAL_PATHSPECS")
    (apply #'magit-start-process magit-git-executable input
           (magit-process-git-arguments args))))

(defun magit-start-process (program &optional input &rest args)
  "Start PROGRAM, prepare for refresh, and return the process object.

If optional argument INPUT is non-nil, it has to be a buffer or
the name of an existing buffer.  The buffer content becomes the
processes standard input.

The process is started using `start-file-process' and then setup
to use the sentinel `magit-process-sentinel' and the filter
`magit-process-filter'.  Information required by these functions
is stored in the process object.  When this function returns the
process has not started to run yet so it is possible to override
the sentinel and filter.

After the process returns, `magit-process-sentinel' refreshes the
buffer that was current when `magit-start-process' was called (if
it is a Magit buffer and still alive), as well as the respective
Magit status buffer.  Unmodified buffers visiting files that are
tracked in the current repository are reverted if
`magit-auto-revert-mode' is active."
  (cl-destructuring-bind (process-buf . section)
      (magit-process-setup program args)
    (let* ((process-connection-type
            ;; Don't use a pty, because it would set icrnl
            ;; which would modify the input (issue #20).
            (and (not input) magit-process-connection-type))
           (process (apply #'start-file-process
                           (file-name-nondirectory program)
                           process-buf program args)))
      (with-editor-set-process-filter process #'magit-process-filter)
      (set-process-sentinel process #'magit-process-sentinel)
      (set-process-buffer   process process-buf)
      (process-put process 'section section)
      (process-put process 'command-buf (current-buffer))
      (process-put process 'default-dir default-directory)
      (when inhibit-magit-refresh
        (process-put process 'inhibit-refresh t))
      (setf (magit-section-process section) process)
      (with-current-buffer process-buf
        (set-marker (process-mark process) (point)))
      (when input
        (with-current-buffer input
          (process-send-region process (point-min) (point-max))
          (process-send-eof    process)))
      (setq magit-this-process process)
      (setf (magit-section-value section) process)
      (magit-process-display-buffer process)
      process)))

;;; Editor Processes

(defun magit-run-git-with-editor (&rest args)
  (with-editor "GIT_EDITOR"
    (let ((magit-process-popup-time -1))
      (magit-run-git-async args))))

;;; Process Internals

(defun magit-process-setup (program args)
  (magit-process-set-mode-line program args)
  (let ((buf (magit-process-buffer)))
    (if  buf
        (magit-process-truncate-log buf)
      (setq buf (magit-process-buffer)))
    (with-current-buffer buf
      (goto-char (1- (point-max)))
      (let* ((inhibit-read-only t)
             (magit-insert-section--parent magit-root-section)
             (elide (if (and args (equal program magit-git-executable))
                        (length magit-git-standard-options)
                      0))
             (section (magit-insert-section (process)
                        (magit-insert-heading "run " program " "
                          (mapconcat 'identity
                                     (if (> elide 0)
                                         (cons (char-to-string magit-ellipsis)
                                               (cadr (-split-at elide args)))
                                       args)
                                     " "))
                        (insert "\n"))))
        (backward-char 1)
        (cons (current-buffer) section)))))

(defun magit-process-truncate-log (buffer)
  (with-current-buffer buffer
    (let* ((head nil)
           (tail (magit-section-children magit-root-section))
           (count (length tail)))
      (when (> (1+ count) magit-process-log-max)
        (while (and (cdr tail)
                    (> count (/ magit-process-log-max 2)))
          (let* ((inhibit-read-only t)
                 (section (car tail))
                 (process (magit-section-process section)))
            (cond ((not process))
                  ((memq (process-status process) '(exit signal))
                   (delete-region (magit-section-start section)
                                  (1+ (magit-section-end section)))
                   (cl-decf count))
                  (t
                   (push section head))))
          (pop tail))
        (setf (magit-section-children magit-root-section)
              (nconc (reverse head) tail))))))

(defun magit-process-sentinel (process event)
  "Default sentinel used by `magit-start-process'."
  (let ((debug-on-error t)) ; temporary
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (magit-process-unset-mode-line)
    (when (string-match "^finished" event)
      (message (concat (capitalize (process-name process)) " finished")))
    (magit-process-finish process)
    (when (eq process magit-this-process)
      (setq magit-this-process nil))
    (--when-let (and (not (process-get process 'inhibit-refresh))
                     (process-get process 'command-buf))
      (when (buffer-live-p it)
        (with-current-buffer it
          (magit-refresh)))))))

(defun magit-process-filter (proc string)
  "Default filter used by `magit-start-process'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (magit-process-yes-or-no-prompt proc string)
      (magit-process-username-prompt  proc string)
      (magit-process-password-prompt  proc string)
      (goto-char (process-mark proc))
      (setq string (propertize string 'magit-section
                               (process-get proc 'section)))
      ;; Find last ^M in string.  If one was found, ignore
      ;; everything before it and delete the current line.
      (let ((ret-pos (length string)))
        (while (and (>= (cl-decf ret-pos) 0)
                    (/= ?\r (aref string ret-pos))))
        (if (< ret-pos 0)
            (insert string)
          (delete-region (line-beginning-position) (point))
          (insert (substring string (1+ ret-pos)))))
      (set-marker (process-mark proc) (point)))))

(defun magit-process-logfile-filter (process string)
  "Special filter used by `magit-run-git-with-logfile'."
  (magit-process-filter process string)
  (let ((file (process-get process 'logfile)))
    (with-temp-file file
      (when (file-exists-p file)
        (insert-file-contents file)
        (goto-char (point-max)))
      (insert string)
      (write-region (point-min) (point-max) file))))

(defun magit-process-yes-or-no-prompt (proc string)
  "Forward Yes-or-No prompts to the user."
  (-when-let (beg (string-match magit-process-yes-or-no-prompt-regexp string))
    (let ((max-mini-window-height 30))
      (process-send-string
       proc
       (downcase
        (concat
         (match-string
          (if (save-match-data (yes-or-no-p (substring string 0 beg))) 1 2)
          string)
         "\n"))))))

(defun magit-process-password-prompt (proc string)
  "Forward password prompts to the user."
  (--when-let (magit-process-match-prompt
               magit-process-password-prompt-regexps string)
    (process-send-string proc (concat (read-passwd it) "\n"))))

(defun magit-process-username-prompt (proc string)
  "Forward username prompts to the user."
  (--when-let (magit-process-match-prompt
               magit-process-username-prompt-regexps string)
    (process-send-string
     proc (concat (read-string it nil nil (user-login-name)) "\n"))))

(defun magit-process-match-prompt (prompts string)
  (when (--any? (string-match it string) prompts)
    (let ((prompt (match-string 0 string)))
      (cond ((string-match ": $" prompt) prompt)
            ((string-match ":$"  prompt) (concat prompt " "))
            (t                           (concat prompt ": "))))))

(defun magit-process-wait ()
  (while (and magit-this-process
              (eq (process-status magit-this-process) 'run))
    (sit-for 0.1 t)))

(defun magit-process-set-mode-line (program args)
  (when (equal program magit-git-executable)
    (setq args (nthcdr (1+ (length magit-git-standard-options)) args)))
  (let ((str (concat " " program (and args (concat " " (car args))))))
    (dolist (buf (magit-mode-get-buffers))
      (with-current-buffer buf (setq mode-line-process str)))))

(defun magit-process-unset-mode-line ()
  (dolist (buf (magit-mode-get-buffers))
    (with-current-buffer buf (setq mode-line-process nil))))

(defvar magit-process-error-message-re
  (concat "^\\(?:error\\|fatal\\|git\\): \\(.*\\)" paragraph-separate))

(defun magit-process-finish (arg &optional process-buf command-buf
                                 default-dir section)
  (unless (integerp arg)
    (setq process-buf (process-buffer arg)
          command-buf (process-get arg 'command-buf)
          default-dir (process-get arg 'default-dir)
          section     (process-get arg 'section)
          arg         (process-exit-status arg)))
  (when (featurep 'dired)
    (dired-uncache default-dir))
  (when (buffer-live-p process-buf)
    (with-current-buffer process-buf
      (let ((inhibit-read-only t)
            (marker (magit-section-start section)))
        (goto-char marker)
        (save-excursion
          (delete-char 3)
          (set-marker-insertion-type marker nil)
          (insert (propertize (format "%3s" arg) 'magit-section section))
          (set-marker-insertion-type marker t)
          (magit-put-face-property (- (point) 3) (point)
                                   (if (= arg 0)
                                       'magit-process-ok
                                     'magit-process-ng)))
        (if (= (magit-section-end section)
               (+ (line-end-position) 2))
            (save-excursion
              (goto-char (1+ (line-end-position)))
              (delete-char -1)
              (setf (magit-section-content section) nil))
          (when (= arg 0)
            (magit-section-hide section))))))
  (unless (= arg 0)
    (message ; `error' would prevent refresh
     "%s ... [%s buffer %s for details]"
     (or (and (buffer-live-p process-buf)
              (with-current-buffer process-buf
                (save-excursion
                  (goto-char (magit-section-end section))
                  (--when-let (magit-section-content section)
                    (when (re-search-backward
                           magit-process-error-message-re it t)
                      (match-string 1))))))
         "Git failed")
     (-if-let (key (and (buffer-live-p command-buf)
                        (with-current-buffer command-buf
                          (car (where-is-internal
                                'magit-process-display-buffer)))))
         (format "Hit %s to see" (key-description key))
       "See")
     (buffer-name process-buf)))
  arg)

(defun magit-process-display-buffer (process)
  (when (process-live-p process)
    (let ((buf (process-buffer process)))
      (cond ((not (buffer-live-p buf)))
            ((= magit-process-popup-time 0)
             (pop-to-buffer buf))
            ((> magit-process-popup-time 0)
             (run-with-timer magit-process-popup-time nil
                             (lambda (p)
                               (when (eq (process-status p) 'run)
                                 (let ((buf (process-buffer p)))
                                   (when (buffer-live-p buf)
                                     (pop-to-buffer buf)))))
                             process))))))

;;; magit-process.el ends soon
(provide 'magit-process)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-process.el ends here
