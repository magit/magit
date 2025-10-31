;;; magit-process.el --- Process functionality  -*- lexical-binding:t -*-

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

;; This library implements the tools used to run Git for side-effects.

;; Note that the functions used to run Git and then consume its
;; output, are defined in `magit-git.el'.  There's a bit of overlap
;; though.

;;; Code:

(require 'magit-base)
(require 'magit-git)
(require 'magit-mode)

(require 'ansi-color)
(require 'auth-source)
(require 'with-editor)

(defvar messages-buffer-name)
(defvar y-or-n-p-map)

(define-obsolete-variable-alias 'magit-process-finish-apply-ansi-colors
  'magit-process-apply-ansi-colors "Magit-Section 4.3.2")

(defclass magit-process-section (magit-section)
  ((process :initform nil)))

;;; Options

(defcustom magit-process-connection-type (not (eq system-type 'cygwin))
  "Connection type used for the Git process.

If nil, use pipes: this is usually more efficient, and works on Cygwin.
If t, use ptys: this enables Magit to prompt for passphrases when needed."
  :group 'magit-process
  :type '(choice (const :tag "Pipe" nil)
                 (const :tag "Pty" t)))

(defcustom magit-need-cygwin-noglob
  (and (eq system-type 'windows-nt)
       (with-temp-buffer
         (let ((process-environment
                (append magit-git-environment process-environment)))
           (condition-case e
               (process-file magit-git-executable
                             nil (current-buffer) nil
                             "-c" "alias.echo=!echo" "echo" "x{0}")
             (file-error
              (lwarn 'magit-process :warning
                     "Could not run Git: %S" e))))
         (equal "x0\n" (buffer-string))))
  "Whether to use a workaround for Cygwin's globbing behavior.

If non-nil, add environment variables to `process-environment' to
prevent the git.exe distributed by Cygwin and MSYS2 from
attempting to perform glob expansion when called from a native
Windows build of Emacs.  See #2246."
  :package-version '(magit . "2.3.0")
  :group 'magit-process
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)))

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
belong to processes that are still running are never removed.
When this is nil, no sections are ever removed."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(choice (const :tag "Never remove old sections" nil) integer))

(defcustom magit-process-error-tooltip-max-lines 20
  "The number of lines for `magit-process-error-lines' to return.

These are displayed in a tooltip for `mode-line-process' errors.

If `magit-process-error-tooltip-max-lines' is nil, the tooltip
displays the text of `magit-process-error-summary' instead."
  :package-version '(magit . "2.12.0")
  :group 'magit-process
  :type '(choice (const :tag "Use summary line" nil)
                 integer))

(defcustom magit-credential-cache-daemon-socket
  (seq-some (lambda (line)
              (pcase-let ((`(,prog . ,args) (split-string line)))
                (and prog
                     (string-match-p
                      "\\`\\(?:\\(?:/.*/\\)?git-credential-\\)?cache\\'" prog)
                     (or (cadr (member "--socket" args))
                         (expand-file-name "~/.git-credential-cache/socket")))))
            ;; Note: `magit-process-file' is not yet defined when
            ;; evaluating this form, so we use `process-lines'.
            (ignore-errors
              (let ((process-environment
                     (append magit-git-environment process-environment)))
                (process-lines magit-git-executable
                               "config" "--get-all" "credential.helper"))))
  "If non-nil, start a credential cache daemon using this socket.

When using Git's cache credential helper in the normal way, Emacs
sends a SIGHUP to the credential daemon after the git subprocess
has exited, causing the daemon to also quit.  This can be avoided
by starting the `git-credential-cache--daemon' process directly
from Emacs.

The function `magit-maybe-start-credential-cache-daemon' takes
care of starting the daemon if necessary, using the value of this
option as the socket.  If this option is nil, then it does not
start any daemon.  Likewise if another daemon is already running,
then it starts no new daemon.  This function has to be a member
of the hook variable `magit-credential-hook' for this to work.
If an error occurs while starting the daemon, most likely because
the necessary executable is missing, then the function removes
itself from the hook, to avoid further futile attempts."
  :package-version '(magit . "2.3.0")
  :group 'magit-process
  :type '(choice (file  :tag "Socket")
                 (const :tag "Don't start a cache daemon" nil)))

(defcustom magit-process-yes-or-no-prompt-regexp
  (eval-when-compile
    (concat " [([]"
            "\\([Yy]\\(?:es\\)?\\)"
            "[/|]"
            "\\([Nn]o?\\)"
            ;; OpenSSH v8 prints this.  See #3969.
            "\\(?:/\\[fingerprint\\]\\)?"
            "[])] ?[?:]? ?$"))
  "Regexp matching Yes-or-No prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type 'regexp)

(defcustom magit-process-password-prompt-regexps
  ;; See also history in test `magit-process:password-prompt-regexps'.
  '(;; * CLI-prompt for passphrase for key:
    "^\\(\\(Please e\\|E\\)nter \\(the \\)?p\\|P\\)assphrase.*: ?$"
    ;; * Password for something other than a host:
    "^\\(\\(Please e\\|E\\)nter \\(the \\)?p\\|P\\)assword: ?$"
    ;; * Password for [user@]host (which we put in match group 99):
    "^\\(\\(Please e\\|E\\)nter \\(the \\)?p\\|P\\)assword for \
[\"']?\\(https?://\\)?\\(?99:[^\"']+\\)[\"']?: ?$"
    "^(\\(?1:[^) ]+\\)) Password for \\(?99:\\1\\): ?$" ;#4992
    "^\\(?99:[^']+\\)\\('s\\)? password: ?$"
    ;; * Token for git-credential-manager-core (#4318):
    "^Token: ?$"
    ;; * Secret for card:
    "^Yubikey for .*: ?$"
    "^Enter PIN for .*: ?$"
    ;; * Unanchored TUI-prompt for passphrase for key:
    "Please enter the passphrase for the ssh key"
    "Please enter the passphrase to unlock the OpenPGP secret key")
  "List of regexps matching password prompts of Git and its subprocesses.
Also see `magit-process-find-password-functions'."
  :package-version '(magit . "4.3.0")
  :group 'magit-process
  :type '(repeat (regexp)))

(defcustom magit-process-find-password-functions nil
  "List of functions to try in sequence to get a password.

These functions may be called when git asks for a password, which
is detected using `magit-process-password-prompt-regexps'.  They
are called if and only if matching the prompt resulted in the
value of the 99th submatch to be non-nil.  Therefore users can
control for which prompts these functions should be called by
putting the host name in the 99th submatch, or not.

If the functions are called, then they are called in the order
given, with the host name as only argument, until one of them
returns non-nil.  If they are not called or none of them returns
non-nil, then the password is read from the user instead."
  :package-version '(magit . "2.3.0")
  :group 'magit-process
  :type 'hook
  :options (list #'magit-process-password-auth-source))

(defcustom magit-process-username-prompt-regexps
  '("^Username for '.*': ?$")
  "List of regexps matching username prompts of Git and its subprocesses."
  :package-version '(magit . "2.1.0")
  :group 'magit-process
  :type '(repeat (regexp)))

(defcustom magit-process-prompt-functions nil
  "List of functions used to forward arbitrary questions to the user.

Magit has dedicated support for forwarding username and password
prompts and Yes-or-No questions asked by Git and its subprocesses
to the user.  This can be customized using other options in the
`magit-process' customization group.

If you encounter a new question that isn't handled by default,
then those options should be used instead of this hook.

However subprocesses may also ask questions that differ too much
from what the code related to the above options assume, and this
hook allows users to deal with such questions explicitly.

Each function is called with the process and the output string
as arguments until one of the functions returns non-nil.  The
function is responsible for asking the user the appropriate
question using, e.g., `read-char-choice' and then forwarding the
answer to the process using `process-send-string'.

While functions such as `magit-process-yes-or-no-prompt' may not
be sufficient to handle some prompt, it may still be of benefit
to look at the implementations to gain some insights on how to
implement such functions."
  :package-version '(magit . "3.0.0")
  :group 'magit-process
  :type 'hook)

(defcustom magit-process-ensure-unix-line-ending t
  "Whether Magit should ensure a unix coding system when talking to Git."
  :package-version '(magit . "2.6.0")
  :group 'magit-process
  :type 'boolean)

(defcustom magit-process-display-mode-line-error t
  "Whether Magit should retain and highlight process errors in the mode line.

See `magit-show-process-buffer-hint' for another way to display the
complete output on demand."
  :package-version '(magit . "2.12.0")
  :group 'magit-process
  :type 'boolean)

(defcustom magit-show-process-buffer-hint t
  "Whether to append hint about process buffer to Git error messages.

When Magit runs Git for side-effects, the output is always logged to
a per-repository process buffer.  If Git exits with a non-zero status,
then a single line of its error output is shown in the repositories
status buffer and in the echo area.

When a user want to learn more about the error, they can switch to that
process buffer, to see the complete output, but initially users are not
aware of this, so Magit appends a usage hint to the error message in
both of these places.

Once you are aware of this, you probably won't need the reminder and can
set this option to nil.

See `magit-process-display-mode-line-error' for another way to display
the complete output on demand."
  :package-version '(magit . "4.3.7")
  :group 'magit-process
  :type 'boolean)

(defcustom magit-process-apply-ansi-colors nil
  "Whether and when to apply color escapes in the process buffer.

Magit instructs Git to not colorize its output, but third-party Git
hooks may do so anyway.  We recommend you figure out how to prevent
such hooks from colorizing their output instead of customizing this
option.

If `nil' (the default), do not apply color escape sequences.  If `t',
apply them once the subprocess has finished.  If `filter', apply them
as input arrives (which is more expensive and potentially fragile).
This is a footgun; starter-kits should leave this option untouched."
  :package-version '(magit . "4.3.2")
  :group 'magit-process
  :type '(choice (const :tag "Do not apply" nil)
                 (const :tag "Apply when subprocess has finished" t)
                 (const :tag "Apply using process filter" filter)))

(defcustom magit-process-timestamp-format nil
  "Format of timestamp for each process in the process buffer.
If non-nil, pass this to `format-time-string' when creating a
process section in the process buffer, and insert the returned
string in the heading of its section."
  :package-version '(magit . "4.0.0")
  :group 'magit-process
  :type '(choice (const :tag "None" nil) string))

(defvar tramp-pipe-stty-settings)
(defvar magit-tramp-pipe-stty-settings ""
  "Override `tramp-pipe-stty-settings' in `magit-start-process'.

The default for that Tramp variable is \"-icanon min 1 time 0\",
which causes staging of individual hunks to hang.  Using \"\"
prevents that, but apparently has other issues, which is why it
isn't the default.

This variable defaults to \"\" and is used to override the Tramp
variable in `magit-start-process'.  This only has an effect when
using Tramp 2.6.2 or greater.  This can also be set to `pty', in
which case a pty is used instead of a pipe.  That also prevents
the hanging, but doesn't work for files with DOS line endings
\(see #20).

For connections that have `tramp-direct-async-process' enabled,
staging hunks hangs, unless this variable is set to `pty' (see
#5220).

To fall back to the value of `tramp-pipe-stty-settings', set this
variable to nil.

Also see https://github.com/magit/magit/issues/4720
and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=62093.")

(defface magit-process-ok
  '((t :inherit magit-section-heading :foreground "green"))
  "Face for zero exit-status."
  :group 'magit-faces)

(defface magit-process-ng
  '((t :inherit magit-section-heading :foreground "red"))
  "Face for non-zero exit-status."
  :group 'magit-faces)

(defface magit-mode-line-process
  '((t :inherit mode-line-emphasis))
  "Face for `mode-line-process' status when Git is running for side-effects."
  :group 'magit-faces)

(defface magit-mode-line-process-error
  '((t :inherit error))
  "Face for `mode-line-process' error status.

Used when `magit-process-display-mode-line-error' is non-nil."
  :group 'magit-faces)

;;; Process Mode

(defvar-keymap magit-process-mode-map
  :doc "Keymap for `magit-process-mode'."
  :parent magit-mode-map
  "<remap> <magit-refresh>"      #'undefined
  "<remap> <magit-delete-thing>" #'magit-process-kill)

(define-derived-mode magit-process-mode magit-mode "Magit Process"
  "Mode for looking at Git process output."
  :interactive nil
  :group 'magit-process
  (magit-hack-dir-local-variables)
  (setq magit--imenu-item-types 'process))

(defun magit-process-buffer (&optional nodisplay)
  "Display the current repository's process buffer.

If that buffer doesn't exist yet, then create it.
Non-interactively return the buffer and unless
optional NODISPLAY is non-nil also display it."
  (interactive)
  (let ((topdir (magit-toplevel)))
    (unless topdir
      (magit--with-safe-default-directory nil
        (setq topdir default-directory)
        (let (prev)
          (while (not (equal topdir prev))
            (setq prev topdir)
            (setq topdir (file-name-directory (directory-file-name topdir)))))))
    (let ((buffer (or (seq-find (##with-current-buffer %
                                  (and (eq major-mode 'magit-process-mode)
                                       (equal default-directory topdir)))
                                (buffer-list))
                      (magit-generate-new-buffer 'magit-process-mode
                                                 nil topdir))))
      (with-current-buffer buffer
        (if magit-root-section
            (when magit-process-log-max
              (magit-process-truncate-log))
          (magit-process-mode)
          (let ((inhibit-read-only t)
                (magit-insert-section--parent  nil)
                (magit-insert-section--oldroot nil))
            (make-local-variable 'text-property-default-nonsticky)
            (magit-insert-section (processbuf)
              (insert "\n")))))
      (unless nodisplay
        (magit-display-buffer buffer))
      buffer)))

(defun magit-process-kill ()
  "Kill the process at point."
  (interactive)
  (when-let ((process (magit-section-value-if 'process)))
    (unless (eq (process-status process) 'run)
      (user-error "Process isn't running"))
    (magit-confirm 'kill-process)
    (kill-process process)))

;;; Synchronous Processes

(defvar magit-process-raise-error nil)

(defvar magit-process-record-invocations nil)
(defvar magit-process-record-buffer-name " *magit-process-file record*")
(defvar magit-process-record-entry-format "%T %%d $ %%a")

(defun magit-toggle-subprocess-record ()
  "Toggle whether subprocess invocations are recorded.

When enabled, all subprocesses started by `magit-process-file' are
logged into the buffer specified by `magit-process-record-buffer-name'
using the format `magit-process-record-entry-format'.  This is for
debugging purposes.

This is in addition to and distinct from the default logging done by
default, and additional logging enabled with ~magit-toggle-git-debug~.

For alternatives, see info node `(magit)Debugging Tools'."
  (interactive)
  (setq magit-process-record-invocations (not magit-process-record-invocations))
  (message "Recording of subprocess invocations %s"
           (if magit-process-record-invocations "enabled" "disabled")))

(defun magit-git (&rest args)
  "Call Git synchronously in a separate process, for side-effects.

Option `magit-git-executable' specifies the Git executable.
The arguments ARGS specify arguments to Git, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`magit-process-buffer'.  If Git exits with a non-zero status,
then raise an error."
  (let ((magit-process-raise-error t))
    (magit-call-git args)))

(defun magit-run-git (&rest args)
  "Call Git synchronously in a separate process, and refresh.

Function `magit-git-executable' specifies the Git executable and
option `magit-git-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.

Process output goes into a new section in the buffer returned by
`magit-process-buffer'.  Return the exit code."
  (let ((magit--refresh-cache (list (cons 0 0))))
    (prog1 (magit-call-git args)
      (when (member (car args) '("init" "clone"))
        ;; Creating a new repository invalidates the cache.
        (setq magit--refresh-cache nil))
      (magit-refresh))))

(defvar magit-pre-call-git-hook (list #'magit-maybe-save-repository-buffers))

(defun magit-call-git (&rest args)
  "Call Git synchronously in a separate process.

Function `magit-git-executable' specifies the Git executable and
option `magit-git-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

Process output goes into a new section in the buffer returned by
`magit-process-buffer'.  Return the exit code."
  (run-hooks 'magit-pre-call-git-hook)
  (let ((default-process-coding-system (magit--process-coding-system)))
    (apply #'magit-call-process
           (magit-git-executable)
           (magit-process-git-arguments args))))

(defun magit-call-process (program &rest args)
  "Call PROGRAM synchronously in a separate process.
Process output goes into a new section in the buffer returned by
`magit-process-buffer'.  Return the exit code."
  (pcase-let ((`(,process-buf . ,section)
               (magit-process-setup program args)))
    (magit-process-finish
     (let ((inhibit-read-only t))
       (apply #'magit-process-file program nil process-buf nil args))
     process-buf (current-buffer) default-directory section)))

(defun magit-process-git (destination &rest args)
  "Call Git synchronously in a separate process, returning its exit code.
DESTINATION specifies how to handle the output, like for
`call-process', except that file handlers are supported.
Enable Cygwin's \"noglob\" option during the call and
ensure unix eol conversion."
  (apply #'magit-process-file
         (magit-git-executable)
         nil destination nil
         (magit-process-git-arguments args)))

(defun magit-process-file (process &optional infile buffer display &rest args)
  "Process files synchronously in a separate process.
Return the exit code.  Similar to `process-file' but temporarily
enable Cygwin's \"noglob\" option during the call and ensure unix
eol conversion."
  (when magit-process-record-invocations
    (let ((messages-buffer-name magit-process-record-buffer-name)
          (inhibit-message t))
      (message "%s"
               (format-spec
                (format-time-string magit-process-record-entry-format)
                `((?d . ,(abbreviate-file-name default-directory))
                  (?a . ,(magit-process--format-arguments process args)))))))
  (let ((process-environment (magit-process-environment))
        (default-process-coding-system (magit--process-coding-system)))
    (apply #'process-file process infile buffer display args)))

(defvar magit--shadowed-githook-directory nil)
(defun magit--shadowed-githook-directory ()
  (or magit--shadowed-githook-directory
      (setq magit--shadowed-githook-directory
            (let ((magit-git-global-arguments nil))
              (cl-letf (((symbol-function 'magit-process-environment)
                         (lambda () process-environment)))
                (or (magit-get "core.hooksPath")
                    (expand-file-name "hooks" (magit-gitdir))))))))

(defun magit-process-environment ()
  ;; The various w32 hacks are only applicable when running on the local
  ;; machine.  A local binding of process-environment different from the
  ;; top-level value affects the environment used by Tramp.
  (let ((local (not (file-remote-p default-directory))))
    (append magit-git-environment
            (list (concat "ORIG_HOOKS_PATH="
                          (magit--shadowed-githook-directory)))
            (and local
                 (cdr (assoc magit-git-executable magit-git-w32-path-hack)))
            (and local magit-need-cygwin-noglob
                 (mapcar (lambda (var)
                           (concat var "=" (if-let ((val (getenv var)))
                                               (concat val " noglob")
                                             "noglob")))
                         '("CYGWIN" "MSYS")))
            process-environment)))

(defvar magit-this-process nil)

(defun magit-run-git-with-input (&rest args)
  "Call Git in a separate process.
ARGS is flattened and then used as arguments to Git.

The current buffer's content is used as the process's standard
input.  The buffer is assumed to be temporary and thus OK to
modify.  Return the exit code.

Function `magit-git-executable' specifies the Git executable and
option `magit-git-global-arguments' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use."
  (when (eq system-type 'windows-nt)
    ;; On w32, git expects UTF-8 encoded input, ignore any user
    ;; configuration telling us otherwise (see #3250).
    (encode-coding-region (point-min) (point-max) 'utf-8-unix))
  (if (file-remote-p default-directory)
      ;; We lack `process-file-region', so fall back to asynch +
      ;; waiting in remote case.
      (progn
        (magit-start-git (current-buffer) args)
        (while (and magit-this-process
                    (eq (process-status magit-this-process) 'run))
          (sleep-for 0.005)))
    (run-hooks 'magit-pre-call-git-hook)
    (pcase-let* ((process-environment (magit-process-environment))
                 (default-process-coding-system (magit--process-coding-system))
                 (flat-args (magit-process-git-arguments args))
                 (`(,process-buf . ,section)
                  (magit-process-setup (magit-git-executable) flat-args))
                 (inhibit-read-only t))
      (magit-process-finish
       (apply #'call-process-region (point-min) (point-max)
              (magit-git-executable) nil process-buf nil flat-args)
       process-buf nil default-directory section))))

;;; Asynchronous Processes

(defun magit-run-git-async (&rest args)
  "Start Git, prepare for refresh, and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Magit buffer
and still alive), as well as the respective Magit status buffer.

See `magit-start-process' for more information."
  (magit-msg "Running %s %s" (magit-git-executable)
             (let ((m (string-join (flatten-tree args) " ")))
               (remove-list-of-text-properties 0 (length m) '(face) m)
               m))
  (magit-start-git nil args))

(defun magit-run-git-with-editor (&rest args)
  "Export GIT_EDITOR and start Git.
Also prepare for refresh and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Magit buffer
and still alive), as well as the respective Magit status buffer.

See `magit-start-process' and `with-editor' for more information."
  (magit--record-separated-gitdir)
  (magit-with-editor (magit-run-git-async args)))

(defun magit-run-git-sequencer (&rest args)
  "Export GIT_EDITOR and start Git.
Also prepare for refresh and return the process object.
ARGS is flattened and then used as arguments to Git.

Display the command line arguments in the echo area.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Magit buffer
and still alive), as well as the respective Magit status buffer.
If the sequence stops at a commit, make the section representing
that commit the current section by moving `point' there.

See `magit-start-process' and `with-editor' for more information."
  (apply #'magit-run-git-with-editor args)
  (set-process-sentinel magit-this-process #'magit-sequencer-process-sentinel)
  magit-this-process)

(defvar magit-pre-start-git-hook (list #'magit-maybe-save-repository-buffers))

(defun magit-start-git (input &rest args)
  "Start Git, prepare for refresh, and return the process object.

If INPUT is non-nil, it has to be a buffer or the name of an
existing buffer.  The buffer content becomes the processes
standard input.

Function `magit-git-executable' specifies the Git executable and
option `magit-git-global-arguments' specifies constant arguments.
The remaining arguments ARGS specify arguments to Git, they are
flattened before use.

After Git returns some buffers are refreshed: the buffer that was
current when this function was called (if it is a Magit buffer
and still alive), as well as the respective Magit status buffer.

See `magit-start-process' for more information."
  (run-hooks 'magit-pre-start-git-hook)
  (let ((default-process-coding-system (magit--process-coding-system)))
    (apply #'magit-start-process (magit-git-executable) input
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
Magit status buffer."
  (pcase-let*
      ((`(,process-buf . ,section)
        (magit-process-setup program args))
       (process
        (let ((process-connection-type ;t=pty nil=pipe
               (or
                ;; With Tramp, maybe force use a pty.  #4720
                (and (file-remote-p default-directory)
                     (eq magit-tramp-pipe-stty-settings 'pty))
                ;; Without input, don't use a pty, because it would
                ;; set icrnl, which would modify the input.  #20
                (and (not input) magit-process-connection-type)))
              (tramp-pipe-stty-settings
               (or (and (not (eq magit-tramp-pipe-stty-settings 'pty))
                        ;; Defaults to "", to allow staging hunks over
                        ;; Tramp again.  #4720
                        magit-tramp-pipe-stty-settings)
                   (bound-and-true-p tramp-pipe-stty-settings)))
              (process-environment (magit-process-environment))
              (default-process-coding-system (magit--process-coding-system)))
          (apply #'start-file-process
                 (file-name-nondirectory program)
                 process-buf program args))))
    (with-editor-set-process-filter process #'magit-process-filter)
    (set-process-sentinel process #'magit-process-sentinel)
    (set-process-buffer   process process-buf)
    (when (eq system-type 'windows-nt)
      ;; On w32, git expects UTF-8 encoded input, ignore any user
      ;; configuration telling us otherwise.
      (set-process-coding-system process nil 'utf-8-unix))
    (process-put process 'section section)
    (process-put process 'command-buf (current-buffer))
    (process-put process 'default-dir default-directory)
    (when magit-inhibit-refresh
      (process-put process 'inhibit-refresh t))
    (oset section process process)
    (with-current-buffer process-buf
      (set-marker (process-mark process) (point)))
    (when input
      (with-current-buffer input
        (process-send-region process (point-min) (point-max))
        ;; `process-send-eof' appears to be broken over
        ;;  Tramp from Windows. See #3624 and bug#43226.
        (if (and (eq system-type 'windows-nt)
                 (file-remote-p (process-get process 'default-dir) nil t))
            (process-send-string process "")
          (process-send-eof process))))
    (setq magit-this-process process)
    (oset section value process)
    (magit-process-display-buffer process)
    process))

(defun magit-parse-git-async (&rest args)
  (setq args (magit-process-git-arguments args))
  (let ((command-buf (current-buffer))
        (stdout-buf (generate-new-buffer " *git-stdout*"))
        (stderr-buf (generate-new-buffer " *git-stderr*"))
        (toplevel (magit-toplevel)))
    (with-current-buffer stdout-buf
      (setq default-directory toplevel)
      (let ((process
             (let ((process-environment (magit-process-environment)))
               (make-process :name "git"
                             :buffer stdout-buf
                             :stderr stderr-buf
                             :command (cons (magit-git-executable) args)
                             :coding (magit--process-coding-system)
                             :file-handler t))))
        (process-put process 'command-buf command-buf)
        (process-put process 'stderr-buf stderr-buf)
        (process-put process 'parsed (point))
        (setq magit-this-process process)
        process))))

;;; Process Internals

(setf (alist-get 'process magit--section-type-alist) 'magit-process-section)

(defun magit-process-setup (program args)
  (magit-process-set-mode-line program args)
  (let ((pwd default-directory)
        (buf (magit-process-buffer t)))
    (cons buf (with-current-buffer buf
                (prog1 (magit-process-insert-section pwd program args nil nil)
                  (backward-char 1))))))

(defun magit-process-insert-section
    (pwd program args &optional errcode errlog face)
  (let ((inhibit-read-only t)
        (magit-insert-section--current nil)
        (magit-insert-section--parent magit-root-section)
        (magit-insert-section--oldroot nil))
    (goto-char (1- (point-max)))
    (magit-insert-section (process)
      (insert (if errcode
                  (format "%3s " (propertize (number-to-string errcode)
                                             'font-lock-face 'magit-process-ng))
                "run "))
      (when magit-process-timestamp-format
        (insert (format-time-string magit-process-timestamp-format) " "))
      (let ((cmd (concat
                  (and (not (equal
                             (file-name-as-directory (expand-file-name pwd))
                             (file-name-as-directory (expand-file-name
                                                      default-directory))))
                       (concat (file-relative-name pwd default-directory) " "))
                  (magit-process--format-arguments program args))))
        (magit-insert-heading
          (if face (magit--propertize-face cmd face) cmd)))
      (when errlog
        (if (bufferp errlog)
            (insert (with-current-buffer errlog
                      (buffer-substring-no-properties (point-min) (point-max))))
          (insert-file-contents errlog)
          (goto-char (1- (point-max)))))
      (insert "\n"))))

(defun magit-process--format-arguments (program args)
  (cond
   ((and args (equal program (magit-git-executable)))
    (let ((global (length magit-git-global-arguments)))
      (concat
       (propertize (file-name-nondirectory program)
                   'font-lock-face 'magit-section-heading)
       " "
       (propertize (magit--ellipsis)
                   'font-lock-face 'magit-section-heading
                   'help-echo (string-join (seq-take args global) " "))
       " "
       (propertize (mapconcat #'shell-quote-argument (seq-drop args global) " ")
                   'font-lock-face 'magit-section-heading))))
   ((and args (equal program shell-file-name))
    (propertize (cadr args)
                'font-lock-face 'magit-section-heading))
   ((concat (propertize (file-name-nondirectory program)
                        'font-lock-face 'magit-section-heading)
            " "
            (propertize (mapconcat #'shell-quote-argument args " ")
                        'font-lock-face 'magit-section-heading)))))

(defun magit-process-truncate-log ()
  (let* ((head nil)
         (tail (oref magit-root-section children))
         (count (length tail)))
    (when (> (1+ count) magit-process-log-max)
      (while (and (cdr tail)
                  (> count (/ magit-process-log-max 2)))
        (let* ((inhibit-read-only t)
               (section (car tail))
               (process (oref section process)))
          (cond ((not process))
                ((memq (process-status process) '(exit signal))
                 (delete-region (oref section start)
                                (1+ (oref section end)))
                 (cl-decf count))
                ((push section head))))
        (pop tail))
      (oset magit-root-section children
            (nconc (reverse head) tail)))))

(defun magit-process-sentinel (process event)
  "Default sentinel used by `magit-start-process'."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (message (concat (capitalize (process-name process)) " finished")))
    (magit-process-finish process)
    (when (eq process magit-this-process)
      (setq magit-this-process nil))
    (unless (process-get process 'inhibit-refresh)
      (let ((command-buf (process-get process 'command-buf)))
        (if (buffer-live-p command-buf)
            (with-current-buffer command-buf
              (magit-refresh))
          (with-temp-buffer
            (setq default-directory (process-get process 'default-dir))
            (magit-refresh)))))))

(defun magit-sequencer-process-sentinel (process event)
  "Special sentinel used by `magit-run-git-sequencer'."
  (when (memq (process-status process) '(exit signal))
    (magit-process-sentinel process event)
    (when-let* ((process-buf (process-buffer process))
                (_(buffer-live-p process-buf))
                (status-buf (with-current-buffer process-buf
                              (magit-get-mode-buffer 'magit-status-mode))))
      (with-current-buffer status-buf
        (when-let ((section
                    (magit-get-section
                     `((commit . ,(magit-rev-parse "HEAD"))
                       (,(pcase (car (seq-drop
                                      (process-command process)
                                      (1+ (length magit-git-global-arguments))))
                           ((or "rebase" "am") 'rebase-sequence)
                           ((or "cherry-pick" "revert") 'sequence)))
                       (status)))))
          (goto-char (oref section start))
          (magit-section-update-highlight))))))

(defun magit-process-filter (proc string)
  "Default filter used by `magit-start-process'."
  (with-current-buffer (process-buffer proc)
    (let ((inhibit-read-only t))
      (goto-char (process-mark proc))
      ;; Find last ^M in string.  If one was found, ignore
      ;; everything before it and delete the current line.
      (when-let ((ret-pos (cl-position ?\r string :from-end t)))
        (setq string (substring string (1+ ret-pos)))
        (delete-region (line-beginning-position) (point)))
      (setq string (magit-process-remove-bogus-errors string))
      (when (eq magit-process-apply-ansi-colors 'filter)
        (setq string (ansi-color-apply string)))
      (insert (propertize string 'magit-section
                          (process-get proc 'section)))
      (set-marker (process-mark proc) (point))
      ;; Make sure prompts are matched after removing ^M.
      (magit-process-yes-or-no-prompt proc string)
      (magit-process-username-prompt  proc string)
      (magit-process-password-prompt  proc string)
      (run-hook-with-args-until-success 'magit-process-prompt-functions
                                        proc string))))

(defun magit-process-make-keymap (process parent)
  "Remap `abort-minibuffers' to a command that also kills PROCESS.
PARENT is used as the parent of the returned keymap."
  (let ((cmd (lambda ()
               (interactive)
               (ignore-errors (kill-process process))
               (if (fboundp 'abort-minibuffers)
                   (abort-minibuffers)
                 (abort-recursive-edit)))))
    (define-keymap :parent parent
      "C-g" cmd
      "<remap> <abort-minibuffers>" cmd
      "<remap> <abort-recursive-edit>" cmd)))

(defmacro magit-process-kill-on-abort (process &rest body)
  (declare (indent 1)
           (debug (form body))
           (obsolete magit-process-make-keymap "Magit 4.0.0"))
  `(let ((minibuffer-local-map
          (magit-process-make-keymap ,process minibuffer-local-map)))
     ,@body))

(defun magit-process-remove-bogus-errors (str)
  (save-match-data
    (when (string-match "^\\(\\*ERROR\\*: \\)Canceled by user" str)
      (setq str (replace-match "" nil nil str 1)))
    (when (string-match "^error: There was a problem with the editor.*\n" str)
      (setq str (replace-match "" nil nil str)))
    (when (string-match
           "^Please supply the message using either -m or -F option\\.\n" str)
      (setq str (replace-match "" nil nil str))))
  str)

(defun magit-process-yes-or-no-prompt (process string)
  "Forward Yes-or-No prompts to the user."
  (when-let ((beg (string-match magit-process-yes-or-no-prompt-regexp string)))
    (process-send-string
     process
     (if (save-match-data
           (let ((max-mini-window-height 30)
                 (minibuffer-local-map
                  (magit-process-make-keymap process minibuffer-local-map))
                 ;; In case yes-or-no-p is fset to that, but does
                 ;; not cover use-dialog-box-p and y-or-n-p-read-key.
                 (y-or-n-p-map
                  (magit-process-make-keymap process y-or-n-p-map)))
             (yes-or-no-p (substring string 0 beg))))
         (concat (downcase (match-str 1 string)) "\n")
       (concat (downcase (match-str 2 string)) "\n")))))

(defun magit-process-password-auth-source (key)
  "Use `auth-source-search' to get a password.
If found, return the password.  Otherwise, return nil.

KEY typically derives from a prompt such as:
  Password for \\='https://yourname@github.com\\='
in which case it would be the string
  yourname@github.com
which matches the ~/.authinfo.gpg entry
  machine github.com login yourname password 12345
or iff that is undefined, for backward compatibility
  machine yourname@github.com password 12345

On github.com you should not use your password but a
personal access token, see [1].  For information about
the peculiarities of other forges, please consult the
respective documentation.

After manually editing ~/.authinfo.gpg you must reset
the cache using
  \\`M-x' `auth-source-forget-all-cached' \\`RET'

The above will save you from having to repeatedly type
your token or password, but you might still repeatedly
be asked for your username.  To prevent that, change an
URL like
  https://github.com/foo/bar.git
to
  https://yourname@github.com/foo/bar.git

Instead of changing all such URLs manually, they can
be translated on the fly by doing this once
  git config --global \
    url.https://yourname@github.com.insteadOf \
    https://github.com

[1]: https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token."
  (require 'auth-source)
  (and (fboundp 'auth-source-search)
       (string-match "\\`\\(.+\\)@\\([^@]+\\)\\'" key)
       (let* ((user (match-str 1 key))
              (host (match-str 2 key))
              (secret
               (plist-get
                (car (or (auth-source-search :max 1 :host host :user user)
                         (auth-source-search :max 1 :host key)))
                :secret)))
         (if (functionp secret)
             (funcall secret)
           secret))))

(defun magit-process-git-credential-manager-core (process string)
  "Authenticate using `git-credential-manager-core'.

To use this function add it to the appropriate hook
  (add-hook \\='magit-process-prompt-functions
            \\='magit-process-git-credential-manager-core)"
  (and (string-match "^option (enter for default): $" string)
       (progn
         (magit-process-buffer)
         (let ((option (format "%c\n"
                               (read-char-choice "Option: " '(?\r ?\j ?1 ?2)))))
           (insert-before-markers-and-inherit option)
           (process-send-string process option)))))

(defun magit-process-password-prompt (process string)
  "Find a password based on prompt STRING and send it to git.
Use `magit-process-password-prompt-regexps' to find a known
prompt.  If and only if one is found, then call functions in
`magit-process-find-password-functions' until one of them returns
the password.  If all functions return nil, then read the password
from the user."
  (when-let ((prompt (magit-process-match-prompt
                      magit-process-password-prompt-regexps string)))
    (process-send-string
     process
     (concat (or (and$ (match-str 99 string)
                       (run-hook-with-args-until-success
                        'magit-process-find-password-functions $))
                 (let ((read-passwd-map
                        (magit-process-make-keymap process read-passwd-map)))
                   (read-passwd prompt)))
             "\n"))))

(defun magit-process-username-prompt (process string)
  "Forward username prompts to the user."
  (when-let ((prompt (magit-process-match-prompt
                      magit-process-username-prompt-regexps string)))
    (process-send-string
     process
     (let ((minibuffer-local-map
            (magit-process-make-keymap process minibuffer-local-map)))
       (concat (read-string prompt nil nil (user-login-name)) "\n")))))

(defun magit-process-match-prompt (prompts string)
  "Match STRING against PROMPTS and set match data.
Return the matched string, appending \": \" if needed."
  (when (seq-some (##string-match % string) prompts)
    (let ((prompt (match-str 0 string)))
      (cond ((string-suffix-p ": " prompt) prompt)
            ((string-suffix-p ":"  prompt) (concat prompt " "))
            (t                             (concat prompt ": "))))))

(defun magit--process-coding-system ()
  (let ((fro (or magit-git-output-coding-system
                 (car default-process-coding-system)))
        (to (cdr default-process-coding-system)))
    (if magit-process-ensure-unix-line-ending
        (cons (coding-system-change-eol-conversion fro 'unix)
              (coding-system-change-eol-conversion to 'unix))
      (cons fro to))))

(defvar magit-credential-hook nil
  "Hook run before Git needs credentials.")

(defvar magit-credential-cache-daemon-process nil)

(defun magit-maybe-start-credential-cache-daemon ()
  "Maybe start a `git-credential-cache--daemon' process.

If such a process is already running or if the value of option
`magit-credential-cache-daemon-socket' is nil, then do nothing.
Otherwise start the process passing the value of that options
as argument."
  (unless (or (not magit-credential-cache-daemon-socket)
              (process-live-p magit-credential-cache-daemon-process)
              (memq magit-credential-cache-daemon-process
                    (list-system-processes)))
    (setq magit-credential-cache-daemon-process
          (or (seq-find (lambda (process)
                          (let* ((attr (process-attributes process))
                                 (comm (cdr (assq 'comm attr)))
                                 (user (cdr (assq 'user attr))))
                            (and (string= comm "git-credential-cache--daemon")
                                 (string= user user-login-name))))
                        (list-system-processes))
              (condition-case nil
                  (start-process "git-credential-cache--daemon"
                                 " *git-credential-cache--daemon*"
                                 (magit-git-executable)
                                 "credential-cache--daemon"
                                 magit-credential-cache-daemon-socket)
                ;; Some Git implementations (e.g., Windows) won't have
                ;; this program; if we fail the first time, stop trying.
                ((debug error)
                 (remove-hook 'magit-credential-hook
                              #'magit-maybe-start-credential-cache-daemon)))))))

(add-hook 'magit-credential-hook #'magit-maybe-start-credential-cache-daemon)

(defvar-keymap magit-mode-line-process-map
  :doc "Keymap for `mode-line-process'."
  "<mode-line> <mouse-1>" 'magit-process-buffer)

(defun magit-process-set-mode-line (program args)
  "Display the git command (sans arguments) in the mode line."
  (when (equal program (magit-git-executable))
    (setq args (nthcdr (length magit-git-global-arguments) args)))
  (let ((str (concat " " (propertize
                          (concat (file-name-nondirectory program)
                                  (and args (concat " " (car args))))
                          'mouse-face 'highlight
                          'keymap magit-mode-line-process-map
                          'help-echo "mouse-1: Show process buffer"
                          'font-lock-face 'magit-mode-line-process))))
    (magit-repository-local-set 'mode-line-process str)
    (dolist (buf (magit-mode-get-buffers))
      (with-current-buffer buf
        (setq mode-line-process str)))
    (force-mode-line-update t)))

(defun magit-process-set-mode-line-error-status (&optional error str)
  "Apply an error face to the string set by `magit-process-set-mode-line'.

If ERROR is supplied, include it in the `mode-line-process' tooltip.

If STR is supplied, it replaces the `mode-line-process' text."
  (setq str (or str (magit-repository-local-get 'mode-line-process)))
  (when str
    (setq error (format "%smouse-1: Show process buffer"
                        (if (stringp error)
                            (concat error "\n\n")
                          "")))
    (setq str (concat " " (propertize
                           (substring-no-properties str 1)
                           'mouse-face 'highlight
                           'keymap magit-mode-line-process-map
                           'help-echo error
                           'font-lock-face 'magit-mode-line-process-error)))
    (magit-repository-local-set 'mode-line-process str)
    (dolist (buf (magit-mode-get-buffers))
      (with-current-buffer buf
        (setq mode-line-process str)))
    (force-mode-line-update t)
    ;; We remove any error status from the mode line when a magit
    ;; buffer is refreshed (see `magit-refresh-buffer'), but we must
    ;; ensure that we ignore any refreshes during the remainder of the
    ;; current command -- otherwise a newly-set error status would be
    ;; removed before it was seen.  We set a flag which prevents the
    ;; status from being removed prior to the next command, so that
    ;; the error status is guaranteed to remain visible until then.
    (let ((repokey (magit-repository-local-repository)))
      ;; The following closure captures the repokey value, and is
      ;; added to `pre-command-hook'.
      (cl-labels ((enable-magit-process-unset-mode-line ()
                    ;; Remove ourself from the hook variable, so
                    ;; that we only run once.
                    (remove-hook 'pre-command-hook
                                 #'enable-magit-process-unset-mode-line)
                    ;; Clear the inhibit flag for the repository in
                    ;; which we set it.
                    (magit-repository-local-set
                     'inhibit-magit-process-unset-mode-line nil repokey)))
        ;; Set the inhibit flag until the next command is invoked.
        (magit-repository-local-set
         'inhibit-magit-process-unset-mode-line t repokey)
        (add-hook 'pre-command-hook
                  #'enable-magit-process-unset-mode-line)))))

(defun magit-process-unset-mode-line-error-status ()
  "Remove any current error status from the mode line."
  (let ((status (or mode-line-process
                    (magit-repository-local-get 'mode-line-process))))
    (when (and status
               (eq (get-text-property 1 'font-lock-face status)
                   'magit-mode-line-process-error))
      (magit-process-unset-mode-line))))

(add-hook 'magit-refresh-buffer-hook
          #'magit-process-unset-mode-line-error-status)

(defun magit-process-unset-mode-line (&optional directory)
  "Remove the git command from the mode line."
  (let ((default-directory (or directory default-directory)))
    (unless (magit-repository-local-get 'inhibit-magit-process-unset-mode-line)
      (magit-repository-local-set 'mode-line-process nil)
      (dolist (buf (magit-mode-get-buffers))
        (with-current-buffer buf (setq mode-line-process nil)))
      (force-mode-line-update t))))

(defvar magit-process-error-message-regexps
  (list "^\\*ERROR\\*: Canceled by user$"
        "^\\(?:error\\|fatal\\|git\\): \\(.*\\)$"
        "^\\(Cannot rebase:.*\\)$"))

(define-error 'magit-git-error "Git error")

(defun magit-process-error-summary (process-buf section)
  "A one-line error summary from the given SECTION."
  (and (buffer-live-p process-buf)
       (with-current-buffer process-buf
         (and (oref section content)
              (save-excursion
                (goto-char (oref section end))
                (run-hook-wrapped
                 'magit-process-error-message-regexps
                 (lambda (re)
                   (save-excursion
                     (and (re-search-backward re (oref section start) t)
                          (match-str 1))))))))))

(defun magit-process-error-tooltip (process-buf section)
  "Returns the text from SECTION of the PROCESS-BUF buffer.

Limited by `magit-process-error-tooltip-max-lines'."
  (and (integerp magit-process-error-tooltip-max-lines)
       (> magit-process-error-tooltip-max-lines 0)
       (buffer-live-p process-buf)
       (with-current-buffer process-buf
         (save-excursion
           (goto-char (or (oref section content)
                          (oref section start)))
           (buffer-substring-no-properties
            (point)
            (save-excursion
              (forward-line magit-process-error-tooltip-max-lines)
              (goto-char
               (if (> (point) (oref section end))
                   (oref section end)
                 (point)))
              ;; Remove any trailing whitespace.
              (when (re-search-backward "[^[:space:]\n]"
                                        (oref section start) t)
                (forward-char 1))
              (point)))))))

(defvar-local magit-this-error nil)

(defun magit-process-finish (arg &optional process-buf _command-buf
                                 default-dir section)
  (unless (integerp arg)
    (setq process-buf (process-buffer arg))
    (setq default-dir (process-get arg 'default-dir))
    (setq section     (process-get arg 'section))
    (setq arg         (process-exit-status arg)))
  (when (fboundp 'dired-uncache)
    (dired-uncache default-dir))
  (when (buffer-live-p process-buf)
    (with-current-buffer process-buf
      (magit-process-finish-section section arg)))
  (if (= arg 0)
      (magit-process-unset-mode-line default-dir)
    (let ((msg (magit-process-error-summary process-buf section)))
      (if magit-process-display-mode-line-error
          (magit-process-set-mode-line-error-status
           (or (magit-process-error-tooltip process-buf section) msg))
        (magit-process-unset-mode-line default-dir))
      (when (buffer-live-p process-buf)
        (with-current-buffer process-buf
          (when-let ((status-buf (magit-get-mode-buffer 'magit-status-mode)))
            (with-current-buffer status-buf
              (setq magit-this-error msg)))))
      (let ((usage
             (and magit-show-process-buffer-hint
                  (if-let ((keys (where-is-internal 'magit-process-buffer)))
                      (format "Type %s to see %S for details"
                              (key-description (car keys)) process-buf)
                    (format "See %S for details" process-buf)))))
        (if magit-process-raise-error
            (signal 'magit-git-error
                    (list msg (or usage (list 'in default-dir))))
          (message "Git error: %s"
                   (concat msg (and usage (format " [%s]" usage))))))))
  arg)

(defun magit-process-finish-section (section exit-code)
  (let ((inhibit-read-only t)
        (buffer (current-buffer))
        (marker (oref section start)))
    (goto-char marker)
    (save-excursion
      (delete-char 3)
      (set-marker-insertion-type marker nil)
      (insert (propertize (format "%3s" exit-code)
                          'magit-section section
                          'font-lock-face (if (= exit-code 0)
                                              'magit-process-ok
                                            'magit-process-ng)))
      (set-marker-insertion-type marker t))
    (when (eq magit-process-apply-ansi-colors t)
      (ansi-color-apply-on-region (oref section content)
                                  (oref section end)))
    (cond ((= (oref section end)
              (+ (line-end-position) 2))
           (save-excursion
             (goto-char (1+ (line-end-position)))
             (delete-char -1)
             (oset section content nil)))
          ((and (= exit-code 0)
                (not (seq-some (##eq (window-buffer %) buffer)
                               (window-list))))
           (magit-section-hide section)))))

(defun magit-process-display-buffer (process)
  (when (process-live-p process)
    (let ((buf (process-buffer process)))
      (cond ((not (buffer-live-p buf)))
            ((= magit-process-popup-time 0)
             (if (minibufferp)
                 (switch-to-buffer-other-window buf)
               (pop-to-buffer buf)))
            ((> magit-process-popup-time 0)
             (run-with-timer magit-process-popup-time nil
                             (lambda (p)
                               (when-let* ((_(eq (process-status p) 'run))
                                           (buf (process-buffer p))
                                           (_(buffer-live-p buf)))
                                 (if (minibufferp)
                                     (switch-to-buffer-other-window buf)
                                   (pop-to-buffer buf))))
                             process))))))

(defun magit--log-action (summary line list)
  (let (heading lines)
    (cond ((cdr list)
           (setq lines (mapcar line list))
           (setq heading (funcall summary list)))
          ((setq heading (funcall line (car list)))))
    (with-current-buffer (magit-process-buffer t)
      (goto-char (1- (point-max)))
      (let ((inhibit-read-only t))
        (magit-insert-section (message)
          (magit-insert-heading (concat "  * " heading))
          (when lines
            (dolist (line lines)
              (insert line "\n"))
            (insert "\n"))))
      (let ((inhibit-message t))
        (when heading
          (setq lines (cons heading lines)))
        (message (string-join lines "\n"))))))

;;; _
(provide 'magit-process)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("and$"         . "cond-let--and$")
;;   ("and>"         . "cond-let--and>")
;;   ("and-let"      . "cond-let--and-let")
;;   ("if-let"       . "cond-let--if-let")
;;   ("when-let"     . "cond-let--when-let")
;;   ("while-let"    . "cond-let--while-let")
;;   ("match-string" . "match-string")
;;   ("match-str"    . "match-string-no-properties"))
;; End:
;;; magit-process.el ends here
