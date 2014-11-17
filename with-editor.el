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

;; Use the Emacsclient as $EDITOR of child processes, making sure
;; they know how to call home.  For remote processes a substitute is
;; provided, which communicates with Emacs on stdout instead of using
;; a socket as the Emacsclient does.

;; Additionally `with-editor-mode' provides some utilities that make
;; it nicer to specialize edit sessions.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'server)
(require 'tramp)
(require 'tramp-sh nil t)

;;; Options

(defgroup with-editor nil
  "Use the Emacsclient as $EDITOR."
  :group 'external
  :group 'server)

(defun with-editor-locate-emacsclient ()
  "Search for a suitable Emacsclient executable."
  (let ((path exec-path))
    (when invocation-directory
      (push (directory-file-name invocation-directory) path)
      (when (eq system-type 'darwin)
        (let ((dir (expand-file-name "bin" invocation-directory)))
          (when (file-directory-p dir)
            (push dir path)))
        (when (string-match-p "Cellar" invocation-directory)
          (let ((dir (expand-file-name "../../../bin" invocation-directory)))
            (when (file-directory-p dir)
              (push dir path))))))
    (--if-let (with-editor-locate-emacsclient-1
               (cl-remove-duplicates path :test 'equal) 3)
        (shell-quote-argument it)
      (display-warning 'with-editor (format "\
Cannot determine a suitable Emacsclient

Determining an Emacsclient executable suitable for the
current Emacs instance failed.  For more information
please see https://github.com/magit/magit/wiki/Emacsclient."))
      nil)))

(defun with-editor-locate-emacsclient-1 (path depth)
  (let* ((version-lst (-take depth (split-string emacs-version "\\.")))
         (version-reg (concat "^" (mapconcat #'identity version-lst "\\."))))
    (or (locate-file-internal
         "emacsclient" path
         (cl-mapcan
          (lambda (v) (cl-mapcar (lambda (e) (concat v e)) exec-suffixes))
          (nconc (cl-mapcon (lambda (v)
                              (setq v (mapconcat #'identity (reverse v) "."))
                              (list v (concat "-" v)))
                            (reverse version-lst))
                 (list "")))
         (lambda (exec)
           (and (ignore-errors
                  (string-match-p
                   version-reg
                   (cadr (split-string
                          (car (process-lines exec "--version"))))))
                (file-executable-p exec))))
        (and (> depth 1)
             (with-editor-locate-emacsclient-1 path (1- depth))))))

(defcustom with-editor-emacsclient-executable (with-editor-locate-emacsclient)
  "The Emacsclient executable used by the `with-editor' macro."
  :group 'with-editor
  :type '(choice (string :tag "Executable")
                 (const  :tag "Don't use Emacsclient" nil)))

(defcustom with-editor-looping-editor "\
sh -c '\
echo \"WITH-EDITOR: $$ OPEN $0\"; \
trap \"exit 0\" USR1; \
trap \"exit 1\" USR2; \
while true; do %s; done'"
  "The looping editor, used when the Emacsclient cannot be used.

This fallback is used for asynchronous process started inside the
macro `with-editor', when the process runs on a remote machine or
for local processes when `with-editor-emacsclient-executable' is
nil (i.e. when no suitable Emacsclient was found, or the user
decided not to use it).

Where the latter uses a socket to communicate with Emacs' server,
this substitute prints edit requests to its standard output on
which a process filter listens for such requests.  As such it is
not a complete substitute for a proper Emacsclient, it can only
be used as $EDITOR of child process of the current Emacs instance.

If the value of this variable contains %s, then that is replaced
with the value of `with-editor-looping-sleep'."
  :group 'with-editor
  :type 'string)

(defcustom with-editor-looping-sleep "sleep 1"
  "How the looping editor sleeps.

The executable that sleeps and the argument that controls how
long it shall nap.  Unfortunately not all implementations support
floats so the defaults is \"sleep 1\", which leads to a noticable
delay.  If you only ever connect to machines that have GNU sleep
installed change this to \"sleep 0.1\" or so.  If you only ever
use BSDs then consider using \"nanosleep 0.1\" instead."
  :group 'with-editor
  :type 'string)

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

(defvar with-editor-server-window-alist nil
  "Alist of filename patterns vs corresponding `server-window'.

Each element looks like (REGEXP . FUNCTION).  Files matching
REGEXP are selected using FUNCTION instead of the default in
`server-window'.

Note that when a package adds an entry here then it probably
has a reason to disrespect `server-window' and it likely is
not a good idea to change such entries.  The `git-commit' and
`git-rebase' packages do no add entries themselves but loading
`magit' does add entries for the files handled by these packages.
Don't change these, or Magit will get confused.")

;;; Commands

(defvar with-editor-pre-finish-hook nil)
(defvar with-editor-pre-cancel-hook nil)
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

(defvar with-editor--pid nil "For internal use.")
(put 'with-editor--pid 'permanent-local t)

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
        (clients server-buffer-clients)
        (dir default-directory)
        (pid with-editor--pid))
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
           (if clients (server-edit) (kill-buffer))))
    (when pid
      (let ((default-directory dir))
        (process-file "kill" nil nil nil
                      "-s" (if cancel "USR2" "USR1") pid)))
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

(defvar with-editor--envvar nil "For internal use.")

(defmacro with-editor (&rest body)
  "Use the Emacsclient as $EDITOR while evaluating BODY.
Modify the `process-environment' for processes started in BODY,
instructing them to use the Emacsclient as $EDITOR.  If optional
ENVVAR is provided then bind that environment variable instead.
\n(fn [ENVVAR] BODY...)"
  (declare (indent defun))
  `(let ((with-editor--envvar ,(if (stringp (car body)) (pop body) "EDITOR"))
         (process-environment process-environment))
     (if (not with-editor-emacsclient-executable)
         (setenv with-editor--envvar (with-editor-looping-editor))
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
       (setenv with-editor--envvar
               (concat with-editor-emacsclient-executable
       ;; Tell the process where the server file is.
                       (and (not server-use-tcp)
                            (concat " --socket-name="
                                    (expand-file-name server-name
                                                      server-socket-dir)))))
       (when server-use-tcp
         (setenv "EMACS_SERVER_FILE"
                 (expand-file-name server-name server-auth-dir)))
       ;; As last resort fallback to the looping editor.
       (setenv "ALTERNATE_EDITOR" (with-editor-looping-editor)))
     ,@body))

(defun with-editor-server-window ()
  (or (and buffer-file-name
           (cdr (--first (string-match-p (car it) buffer-file-name)
                         with-editor-server-window-alist)))
      server-window))

(defadvice server-switch-buffer (around with-editor activate)
  "Honor `with-editor-server-window-alist' (which see)."
  (let ((server-window (with-current-buffer
                           (or next-buffer (current-buffer))
                         (with-editor-server-window))))
    ad-do-it))

(defun with-editor-looping-editor ()
  "Return the looping editor appropriate for `default-directory'.
Also see documentation for option `with-editor-looping-editor'."
  (format with-editor-looping-editor
          with-editor-looping-sleep))

(defadvice start-file-process (around with-editor activate)
  "When called inside a `with-editor' form and the Emacsclient
cannot be used, then give the process the filter function
`with-editor-process-filter'.  To avoid overriding the filter
being added here you should use `with-editor-set-process-filter'
instead of `set-process-filter' inside `with-editor' forms.

When the `default-directory' is located on a remote machine,
then also manipulate PROGRAM and PROGRAM-ARGS in order to set
the appropriate editor environment variable."
  ;; (fn NAME BUFFER PROGRAM &rest PROGRAM-ARGS)
  (if (not with-editor--envvar)
      ad-do-it
    (when (file-remote-p default-directory)
      (let ((prog (ad-get-arg  2))
            (args (ad-get-args 3)))
        (unless (equal program "env")
          (push prog args)
          (setq prog "env"))
        (push (concat with-editor--envvar "="
                      (with-editor-looping-editor)) args)
        (ad-set-arg  2 prog)
        (ad-set-args 3 args)))
    (let ((process ad-do-it))
      (set-process-filter process 'with-editor-process-filter)
      (process-put process 'default-dir default-directory)
      process)))

(defun with-editor-set-process-filter (process filter)
  "Like `set-process-filter' but keep `with-editor-process-filter'.
Give PROCESS the new FILTER but keep `with-editor-process-filter'
if that was added earlier by the adviced `start-file-process'.

Do so by wrapping the two filter functions using a lambda, which
becomes the actual filter.  It calls `with-editor-process-filter'
first, passing t as NO-STANDARD-FILTER.  Then it calls FILTER,
which may or may not insert the text into the PROCESS' buffer."
  (set-process-filter
   process
   (if (eq (process-filter process) 'with-editor-process-filter)
       `(lambda (proc str)
          (with-editor-process-filter proc str t)
          (,filter proc str))
     filter)))

(defun with-editor-process-filter
    (process string &optional no-default-filter)
  "Listen for edit requests by child processes."
  (when (string-match "^WITH-EDITOR: \\([0-9]+\\) OPEN \\(.+\\)$" string)
    (let ((pid  (match-string 1 string))
          (file (match-string 2 string))
          (dir  (process-get process 'default-dir)))
      (with-current-buffer
          (find-file-noselect
           (if (file-name-absolute-p file)
               (if (tramp-tramp-file-p dir)
                   (with-parsed-tramp-file-name dir nil
                     (tramp-make-tramp-file-name method user host file hop))
                 file)
             (expand-file-name dir file)))
        (with-editor-mode 1)
        (setq with-editor--pid pid)
        (run-hooks 'with-editor-filter-visit-hook)
        (funcall (or (with-editor-server-window) 'switch-to-buffer)
                 (current-buffer))
        (kill-local-variable 'server-window))))
  (unless no-default-filter
    (internal-default-process-filter process string)))

(unless (fboundp 'internal-default-process-filter)
  ;; Added in Emacs 24.4 (488ac8e).
  (defun internal-default-process-filter (process string)
    (let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let* ((mark (process-mark process))
                 (move (= (point) mark))
                 (inhibit-read-only t))
            (save-excursion
              (goto-char mark)
              (insert string)
              (setq mark (set-marker mark (point))))
            (when move
              (goto-char mark))))))))

;;; with-editor.el ends soon

(defconst with-editor-font-lock-keywords
  '(("(\\(with-\\(?:git-\\)?editor\\)\\_>" . 1)))
(font-lock-add-keywords 'emacs-lisp-mode with-editor-font-lock-keywords)

(provide 'with-editor)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; with-editor.el ends here
