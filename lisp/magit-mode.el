;;; magit-mode.el --- create and refresh Magit buffers

;; Copyright (C) 2010-2015  The Magit Project Contributors
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

;; This library implements the abstract major-mode `magit-mode' from
;; which almost all other Magit major-modes derive.  The code in here
;; is mostly concerned with creating and refreshing Magit buffers.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'magit-section)
(require 'magit-git)

;; For `magit-xref-insert-buttons' from `magit'
(defvar magit-diff-show-xref-buttons)
(defvar magit-revision-show-xref-buttons)
;; For `magit-refresh' from `magit'
(defvar magit-status-buffer-name-format)
;; For `magit-revert-buffers'
(declare-function magit-blame-mode 'magit-blame)
(defvar magit-blame-mode)

(require 'format-spec)
(require 'help-mode)

;;; Options

(defcustom magit-mode-hook '(magit-load-config-extensions)
  "Hook run when entering a mode derived from Magit mode."
  :group 'magit-modes
  :type 'hook
  :options '(magit-load-config-extensions bug-reference-mode))

(defcustom magit-region-highlight-hook
  '(magit-section-update-region magit-diff-update-hunk-region)
  "Functions used to highlight the region.
Each function is run with the current section as only argument
until one of them returns non-nil.  When multiple sections are
selected, then this hook does not run and the region is not
displayed.  Otherwise fall back to regular region highlighting."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-section-update-region magit-diff-update-hunk-region))

(defcustom magit-restore-window-configuration t
  "Whether quitting a Magit buffer restores previous window configuration.

Function `magit-mode-display-buffer' is used to display and
select Magit buffers.  Unless the buffer was already displayed in
a window of the selected frame it also stores the previous window
configuration.  If this option is non-nil that configuration will
later be restored by `magit-mode-bury-buffer', provided the
buffer has not since been displayed in another frame.

This works best when only two windows are usually displayed in a
frame.  If this isn't the case setting then the default value
might lead to undesirable behaviour.  Also quitting a Magit
buffer while another Magit buffer that was created earlier is
still displayed will cause that buffer to be hidden, which might
or might not be what you want.

Note that if this was previously disabled, then setting it to t
does not effect Magit buffers that already exist, because the
previous window configurations are only stored if and only if
this option is non-nil."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'boolean)

(defcustom magit-refresh-verbose nil
  "Whether to revert Magit buffers verbosely."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'boolean)

(defcustom magit-refresh-buffer-hook nil
  "Normal hook for `magit-revert-buffer' to run after refreshing."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook)

(defvar magit-revert-buffers-timer nil)

(defun magit-revert-buffers-set-timer ()
  (when (timerp magit-revert-buffers-timer)
    (cancel-timer magit-revert-buffers-timer))
  (setq magit-revert-buffers-timer
        (and (boundp 'magit-revert-buffers)
             (numberp magit-revert-buffers)
             (run-with-timer 0 magit-revert-buffers
                             'magit-revert-buffers-async))))

(defcustom magit-revert-buffers 'usage
  "How file-visiting buffers in the current repository are reverted.

After running certain commands, after refreshing the current
Magit buffer, unmodified buffers visiting files belonging to
the current repository may optionally be reverted.

`nil'     Don't revert any buffers.

`ask'     List the buffers which might potentially have to be
          reverted and ask the user whether she wants to revert
          them.  If so, then do it synchronously.

`t'       Revert the buffers synchronously, mentioning each one
          as it is being reverted and then also show a summary.

`usage'   Like `t' but include usage information in the summary.
          This is the default so that users come here and pick
          what is right for them.

`silent'  Revert the buffers synchronously and be quiet about it.

NUMBER    An integer or float.  Revert the buffers asynchronously,
          mentioning each one as it is being reverted.  If user
          input arrives, then stop reverting.  After NUMBER
          seconds resume reverting."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type '(choice
          (const :tag "Don't revert" nil)
          (const :tag "Ask whether to revert" ask)
          (const :tag "Revert synchronously" t)
          (const :tag "Revert synchronously but in silence" silent)
          (const :tag "Revert synchronously with usage information" usage)
          (integer :tag "Revert asynchronously (interval in seconds)"))
  :set (lambda (var val)
         (set-default var val)
         (magit-revert-buffers-set-timer)))

(defcustom magit-after-revert-hook '(magit-refresh-vc-mode-line)
  "Normal hook for `magit-revert-buffer' to run after reverting."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-refresh-vc-mode-line))

(defcustom magit-not-reverted-hook '(magit-refresh-vc-mode-line)
  "Normal hook for `magit-revert-buffer' to run instead of reverting.
Run if the visited file has not changed on disk and the buffer
therefore does not have to be reverted."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-refresh-vc-mode-line))

(defcustom magit-save-repository-buffers t
  "Whether to save file-visiting buffers when appropriate.

If this is non-nil then all modified file-visiting buffers
belonging to the current repository may be saved before running
commands, before creating new Magit buffers, and before
explicitly refreshing such buffers.  If this is `dontask' then
this is done without user intervention, if it is t then the user
has to confirm each save."
  :group 'magit
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Save without asking" dontask)))

;;; Magit Mode

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "\t"    'magit-section-toggle)
    (define-key map [C-tab] 'magit-section-cycle)
    (define-key map [M-tab] 'magit-section-cycle-diffs)
    (define-key map [s-tab] 'magit-section-cycle-global)
    (define-key map "^"    'magit-section-up)
    (define-key map "n"    'magit-section-forward)
    (define-key map "p"    'magit-section-backward)
    (define-key map "\M-n" 'magit-section-forward-sibling)
    (define-key map "\M-p" 'magit-section-backward-sibling)
    (define-key map "+"    'magit-diff-more-context)
    (define-key map "-"    'magit-diff-less-context)
    (define-key map "0"    'magit-diff-default-context)
    (define-key map "1"    'magit-section-show-level-1)
    (define-key map "2"    'magit-section-show-level-2)
    (define-key map "3"    'magit-section-show-level-3)
    (define-key map "4"    'magit-section-show-level-4)
    (define-key map "\M-1" 'magit-section-show-level-1-all)
    (define-key map "\M-2" 'magit-section-show-level-2-all)
    (define-key map "\M-3" 'magit-section-show-level-3-all)
    (define-key map "\M-4" 'magit-section-show-level-4-all)
    (define-key map "g" 'magit-refresh)
    (define-key map "G" 'magit-refresh-all)
    (define-key map "q" 'magit-mode-bury-buffer)
    (define-key map "$" 'magit-process)
    (define-key map "a" 'magit-cherry-apply)
    (define-key map "A" 'magit-cherry-pick-popup)
    (define-key map "b" 'magit-branch-popup)
    (define-key map "B" 'magit-bisect-popup)
    (define-key map "c" 'magit-commit-popup)
    (define-key map "d" 'magit-diff-popup)
    (define-key map "D" 'magit-diff-refresh-popup)
    (define-key map "h" 'magit-dispatch-popup)
    (define-key map "?" 'magit-dispatch-popup)
    (define-key map "\C-c\C-c" 'magit-dispatch-popup)
    (define-key map "\C-c\C-e" 'magit-dispatch-popup)
    (define-key map "e" 'magit-ediff-dwim)
    (define-key map "E" 'magit-ediff-popup)
    (define-key map "f" 'magit-fetch-popup)
    (define-key map "F" 'magit-pull-popup)
    (define-key map "i" 'magit-gitignore)
    (define-key map "I" 'magit-gitignore-locally)
    (define-key map "k" 'magit-delete-thing)
    (define-key map "K" 'magit-file-untrack)
    (define-key map "l" 'magit-log-popup)
    (define-key map "L" 'magit-log-refresh-popup)
    (define-key map "m" 'magit-merge-popup)
    (define-key map "M" 'magit-remote-popup)
    (define-key map "o" 'magit-submodule-popup)
    (define-key map "P" 'magit-push-popup)
    (define-key map "r" 'magit-rebase-popup)
    (define-key map "R" 'magit-file-rename)
    (define-key map "t" 'magit-tag-popup)
    (define-key map "T" 'magit-notes-popup)
    (define-key map "\r"       'magit-visit-thing)
    (define-key map [C-return] 'magit-visit-thing)
    (define-key map [M-return] 'magit-dired-jump)
    (define-key map "\s"       'magit-diff-show-or-scroll-up)
    (define-key map "\d"       'magit-diff-show-or-scroll-down)
    (define-key map "s" 'magit-stage-file)
    (define-key map "S" 'magit-stage-modified)
    (define-key map "u" 'magit-unstage-file)
    (define-key map "U" 'magit-reset-index)
    (define-key map "v" 'magit-revert-no-commit)
    (define-key map "V" 'magit-revert-popup)
    (define-key map "w" 'magit-am-popup)
    (define-key map "W" 'magit-patch-popup)
    (define-key map "x" 'magit-reset)
    (define-key map "y" 'magit-show-refs-popup)
    (define-key map "Y" 'magit-cherry)
    (define-key map "z" 'magit-stash-popup)
    (define-key map "Z" 'magit-stash-popup)
    (define-key map ":" 'magit-git-command)
    (define-key map "!" 'magit-run-popup)
    (define-key map "\C-xa"  'magit-add-change-log-entry)
    (define-key map "\C-x4a" 'magit-add-change-log-entry-other-window)
    (define-key map "\C-w"   'magit-copy-as-kill)
    (define-key map "\M-w"   'magit-copy-buffer-thing-as-kill)
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line] 'evil-next-visual-line)
    map)
  "Parent keymap for all keymaps of modes derived from `magit-mode'.")

(defun magit-delete-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which deletes the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be deleted"))

(defun magit-visit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point."
  (interactive)
  (user-error "There is no thing at point that could be visited"))

(easy-menu-define magit-mode-menu magit-mode-map
  "Magit menu"
  '("Magit"
    ["Refresh" magit-refresh t]
    ["Refresh all" magit-refresh-all t]
    "---"
    ["Stage" magit-stage t]
    ["Stage modified" magit-stage-modified t]
    ["Unstage" magit-unstage t]
    ["Reset index" magit-reset-index t]
    ["Commit" magit-commit-popup t]
    ["Add log entry" magit-commit-add-log t]
    ["Tag" magit-tag t]
    "---"
    ["Diff working tree" magit-diff-working-tree t]
    ["Diff" magit-diff t]
    ("Log"
     ["Oneline Log" magit-log t]
     ["Verbose Log" magit-log-verbose t]
     ["Reflog" magit-reflog t]
     ["Extended..." magit-log-popup t])
    "---"
    ["Cherry pick" magit-cherry-pick t]
    ["Revert commit" magit-revert-popup t]
    "---"
    ["Ignore" magit-gitignore t]
    ["Ignore locally" magit-gitignore-locally t]
    ["Discard" magit-discard t]
    ["Reset head" magit-reset-head t]
    ["Stash" magit-stash t]
    ["Snapshot" magit-snapshot t]
    "---"
    ["Branch..." magit-checkout t]
    ["Merge" magit-merge t]
    ["Ediff resolve" magit-ediff-resolve t]
    ["Rebase..." magit-rebase-popup t]
    "---"
    ["Push" magit-push t]
    ["Pull" magit-pull t]
    ["Remote update" magit-fetch-all t]
    ("Submodule"
     ["Submodule update" magit-submodule-update t]
     ["Submodule update and init" magit-submodule-setup t]
     ["Submodule init" magit-submodule-init t]
     ["Submodule sync" magit-submodule-sync t])
    "---"
    ("Extensions")
    "---"
    ["Display Git output" magit-process t]
    ["Quit Magit" magit-mode-bury-buffer t]))

(defun magit-load-config-extensions ()
  "Load Magit extensions that are defined at the Git config layer."
  (dolist (ext (magit-get-all "magit.extension"))
    (let ((sym (intern (format "magit-%s-mode" ext))))
      (when (fboundp sym)
        (funcall sym 1)))))

(define-derived-mode magit-mode special-mode "Magit"
  "Parent major mode from which Magit major modes inherit.

Magit is documented in info node `(magit)'."
  :group 'magit-modes
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t) ; see #1771
  (setq show-trailing-whitespace nil)
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function 'magit-highlight-region)
  (setq-local redisplay-unhighlight-region-function 'magit-unhighlight-region)
  (when (fboundp 'linum-mode)
    (linum-mode -1)))

(defvar-local magit-region-overlays nil)

(defun magit-highlight-region (start end window rol)
  (mapc #'delete-overlay magit-region-overlays)
  (if (run-hook-with-args-until-success 'magit-region-highlight-hook
                                        (magit-current-section))
      (funcall (default-value 'redisplay-unhighlight-region-function) rol)
    (funcall (default-value 'redisplay-highlight-region-function)
             start end window rol)))

(defun magit-unhighlight-region (rol)
  (setq magit-section-highlighted-section nil)
  (mapc #'delete-overlay magit-region-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

(defvar-local magit-refresh-function nil
  "The function used to refresh the current buffer.
This is called with `magit-refresh-args' as arguments.
The value is usually set using `magit-mode-setup'.")
(put 'magit-refresh-function 'permanent-local t)

(defvar-local magit-refresh-args nil
  "The arguments used to refresh the current buffer.
`magit-refresh-function' is called with these arguments.
The value is usually set using `magit-mode-setup'.")
(put 'magit-refresh-args 'permanent-local t)

(defvar magit-mode-setup-hook nil)

;; Kludge.  We use this instead of adding a new, optional argument to
;; `magit-setup-mode' in order to avoid breaking third-party packages.
;; See #2054 and #2060.
(defvar magit-mode-setup--topdir nil)

(defmacro magit-mode-setup
  (buffer switch-func mode refresh-func &rest refresh-args)
  "Display and select BUFFER, turn on MODE, and refresh a first time.
Display BUFFER using `magit-mode-display-buffer', then turn on
MODE in BUFFER, set the local value of `magit-refresh-function'
to REFRESH-FUNC and that of `magit-refresh-args' to REFRESH-ARGS
and finally \"refresh\" a first time.  All arguments are evaluated
before switching to BUFFER."
  (declare (debug (form form form form &rest form)))
  (let ((smode (cl-gensym "mode"))
        (sroot (cl-gensym "root"))
        (sfunc (cl-gensym "func"))
        (sargs (cl-gensym "args"))
        (sbuf  (cl-gensym "buffer")))
    `(let* ((,smode ,mode)
            (,sroot (let ((default-directory (or magit-mode-setup--topdir
                                                 default-directory)))
                      (magit-toplevel)))
            (,sfunc ,refresh-func)
            (,sargs (list ,@refresh-args))
            (,sbuf  (magit-mode-display-buffer
                     ,buffer ,smode ,switch-func ,sroot)))
       (when find-file-visit-truename
         (setq ,sroot (file-truename ,sroot)))
       (if ,sroot
           (with-current-buffer ,sbuf
             (setq default-directory ,sroot
                   magit-refresh-function ,sfunc
                   magit-refresh-args ,sargs)
             (run-hooks 'magit-mode-setup-hook)
             (pcase ,smode
               ((or `magit-log-mode `magit-reflog-mode)
                (magit-xref-setup ,sargs))
               ((or `magit-diff-mode `magit-revision-mode)
                (magit-xref-setup ,sargs)
                (goto-char (point-min))))
             (funcall ,smode)
             (magit-refresh-buffer))
         (user-error "Not inside a Git repository")))))

(defvar-local magit-previous-section nil)
(put 'magit-previous-section 'permanent-local t)

(defun magit-mode-display-buffer (buffer mode &optional switch-function topdir)
  "Display BUFFER in some window and select it.
BUFFER may be a buffer or a string, the name of a buffer.  Return
the buffer.

Unless BUFFER is already displayed in the selected frame store the
previous window configuration as a buffer local value, so that it
can later be restored by `magit-mode-bury-buffer'.

Then display and select BUFFER using SWITCH-FUNCTION.  If that is
nil either use `pop-to-buffer' if the current buffer's major mode
derives from Magit mode; or else use `switch-to-buffer'.

When optional TOPDIR is specified, then it has to be the top-level
directory of the repository.  Otherwise that is determined using
the function `magit-toplevel'."
  (cond ((stringp buffer)
         (setq buffer (magit-mode-get-buffer-create buffer mode topdir)))
        ((not (bufferp buffer))
         (signal 'wrong-type-argument (list 'bufferp nil))))
  (let ((section (magit-current-section)))
    (with-current-buffer buffer
      (setq magit-previous-section section)
      (when magit-restore-window-configuration
        (magit-save-window-configuration))))
  (funcall (or switch-function
               (if (derived-mode-p 'magit-mode)
                   'switch-to-buffer
                 'pop-to-buffer))
           buffer)
  buffer)

(defun magit-mode-get-buffers ()
  (let ((topdir (magit-toplevel)))
    (--filter (with-current-buffer it
                (and (derived-mode-p 'magit-mode)
                     (equal default-directory topdir)))
              (buffer-list))))

(defun magit-mode-get-buffer (format mode &optional pwd create)
  (unless format
    (setq format (symbol-value
                  (intern (format "%s-buffer-name-format"
                                  (substring (symbol-name mode) 0 -5))))))
  (setq pwd (expand-file-name (or pwd default-directory)))
  (let* ((topdir (let ((default-directory pwd))
                   (magit-toplevel)))
         (name (format-spec
                format (if topdir
                           `((?a . ,(abbreviate-file-name topdir))
                             (?b . ,(file-name-nondirectory
                                     (directory-file-name topdir))))
                         '((?a . "-") (?b . "-"))))))
    (or (--first (with-current-buffer it
                   (and (equal (buffer-name) name)
                        (or (not topdir)
                            (equal (expand-file-name default-directory)
                                   topdir))))
                 (buffer-list))
        (and create
             (let ((default-directory (or topdir pwd)))
               (generate-new-buffer name))))))

(defun magit-mode-get-buffer-create (format mode &optional directory)
  (magit-mode-get-buffer format mode directory t))

(defun magit-mode-bury-buffer (&optional kill-buffer)
  "Bury the current buffer.
With a prefix argument, kill the buffer instead.

If `magit-restore-window-configuration' is non-nil and the last
configuration stored by `magit-mode-display-buffer' originates
from the selected frame then restore it after burying/killing
the buffer."
  (interactive "P")
  (if magit-restore-window-configuration
      (magit-restore-window-configuration kill-buffer)
    (quit-window kill-buffer)))

(defun magit-rename-buffer (&optional newname)
  "Rename the current buffer, so that Magit won't reuse it.

By default there is only one buffer with a certain Magit mode
per repository.  Displaying e.g. some diff will reuse the buffer
previously used to display another diff.  If you want to have
two buffers displaying different diffs belonging to the same
repository, then you have to create a buffer whose name differs
from the default name.

The easiest way to do that is to use this command.  It appends
\"<N>\" to the name of the current buffer, where N is the lowest
available number, starting with 2, which is still available.

With a prefix argument, the user can pick an arbitrary name."
  (interactive
   (list (and current-prefix-arg
              (read-buffer "Rename buffer to: " (current-buffer)))))
  (unless newname
    (setq newname (buffer-name)))
  (when (string-match "<[0-9]+>\\'" newname)
    (setq newname (substring newname 0 (match-beginning 0))))
  (rename-buffer (generate-new-buffer-name newname)))

;;; Refresh Machinery

(defvar inhibit-magit-refresh nil)

(defvar magit-pre-refresh-hook nil)

(defun magit-refresh ()
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`magit-mode', and refresh the corresponding status buffer.

If option `magit-revert-buffers' call for it, then also revert
all unmodified buffers that visit files being tracked in the
current repository."
  (interactive)
  (unless inhibit-magit-refresh
    (when (derived-mode-p 'magit-mode)
      (run-hooks 'magit-pre-refresh-hook)
      (magit-refresh-buffer)
      (unless (derived-mode-p 'magit-status-mode)
        (--when-let (magit-mode-get-buffer
                     magit-status-buffer-name-format
                     'magit-status-mode)
          (with-current-buffer it
            (magit-refresh-buffer)))))
    (magit-revert-buffers)))

(defun magit-refresh-all ()
  "Refresh all buffers belonging to the current repository.

Refresh all Magit buffers belonging to the current repository.

Also always revert all unmodified buffers that visit files being
tracked in the current repository."
  (interactive)
  (dolist (buffer (magit-mode-get-buffers))
    (with-current-buffer buffer (magit-refresh-buffer)))
  (magit-revert-buffers t))

(defvar-local magit-refresh-start-time nil)

(defun magit-refresh-buffer ()
  "Refresh the current Magit buffer.
Uses the buffer-local `magit-refresh-function'."
  (setq magit-refresh-start-time (current-time))
  (when magit-refresh-function
    (when magit-refresh-verbose
      (message "Refreshing buffer `%s'..." (buffer-name)))
    (let* ((buffer (current-buffer))
           (windows
            (--mapcat (with-selected-window it
                        (with-current-buffer buffer
                          (-when-let (section (magit-current-section))
                            (list
                             (list it section
                                   (count-lines (magit-section-start section)
                                                (point))
                                   (- (point) (line-beginning-position)))))))
                      (or (get-buffer-window-list buffer nil t)
                          (list (selected-window))))))
      (deactivate-mark)
      (setq magit-section-highlight-overlays nil
            magit-section-highlighted-section nil
            magit-section-highlighted-sections nil
            magit-section-unhighlight-sections nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (save-excursion
          (apply magit-refresh-function
                 magit-refresh-args)))
      (dolist (window windows)
        (with-selected-window (car window)
          (with-current-buffer buffer
            (apply #'magit-section-goto-successor (cdr window)))))
      (run-hooks 'magit-refresh-buffer-hook)
      (magit-section-update-highlight)
      (set-buffer-modified-p nil))
    (when magit-refresh-verbose
      (message "Refreshing buffer `%s'...done (%.3fs)" (buffer-name)
               (float-time (time-subtract (current-time)
                                          magit-refresh-start-time))))))

(defvar inhibit-magit-revert nil)
(defvar magit-revert-buffers-backlog nil)

(defun magit-revert-buffers (&optional force)
  "Revert unmodified file-visiting buffers of the current repository.

If either `magit-revert-buffers' is non-nil and `inhibit-magit-revert'
is nil, or if optional FORCE is non-nil; then revert all unmodified
buffers that visit files being tracked in the current repository.

When called interactively then the revert is forced."
  (interactive (list t))
  (when (or force (and magit-revert-buffers (not inhibit-magit-revert)))
    (-when-let (topdir (magit-toplevel))
      (let* ((tracked (magit-revision-files "HEAD"))
             (buffers
              (if (> (length tracked)
                     (length (buffer-list)))
                  (--filter
                   (let ((file (buffer-file-name it)))
                     (and file
                          (file-in-directory-p file topdir)
                          (member (file-relative-name file topdir) tracked)))
                   (buffer-list))
                (--mapcat
                 (--when-let (find-buffer-visiting (expand-file-name it topdir))
                   (list it))
                 tracked))))
        (when (and buffers
                   (or force
                       (not (eq magit-revert-buffers 'ask))
                       (magit-confirm 'revert-buffer
                         "Revert %s from visited file"
                         "Revert %i buffers from visited files"
                         (mapcar #'buffer-name buffers))))
          (cond
           ((numberp magit-revert-buffers)
            (magit-revert-buffers-async buffers))
           ((eq magit-revert-buffers 'silent)
            (mapc #'magit-revert-buffer buffers))
           (t
            (let ((cnt (length buffers)))
              (when (> cnt 0)
                (message "Reverting (up to) %s file-visiting buffer(s)..." cnt)
                (setq cnt (length (-non-nil (mapcar #'magit-revert-buffer
                                                    buffers))))
                (if (> cnt 0)
                    (pcase magit-revert-buffers
                      (`t
                       (message "Reverting %s file-visiting buffer(s)...done" cnt))
                      (`usage
                       (message
                        "Reverting %s file-visiting buffer(s)...done%s%s%s" cnt
                        (substitute-command-keys
                         "\n  This can be undone using `\\[undo]' in the ")
                        "affected buffers\n  Customize behavior using `M-x "
                        "customize-option RET magit-revert-buffers RET'"))
                      ((or `nil `ask)
                       (message "Reverting %s file-visiting buffer(s)...done%s"
                                cnt (if force " (forced)" ""))))
                  (message "(No buffers need to be reverted)")))))))))))

(defun magit-revert-buffers-async (&optional buffers)
  (setq buffers (nconc buffers (--filter (not (memq it buffers))
                                         magit-revert-buffers-backlog)))
  (while (and buffers (not (input-pending-p)))
    (let ((buf (pop buffers)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (magit-revert-buffer buf)))))
  (setq magit-revert-buffers-backlog buffers))

(defun magit-revert-buffer (&optional buffer)
  "Revert the current file-visiting buffer."
  (let (ret)
    (with-current-buffer (or buffer (current-buffer))
      (if (and (file-readable-p buffer-file-name)
               (not (verify-visited-file-modtime (current-buffer))))
          (if magit-blame-mode
              (progn (message "Reverting %s inhibited due to magit-blame-mode"
                              buffer-file-name)
                     (run-hooks 'magit-not-reverted-hook))
            (if (eq magit-revert-buffers 'silent)
                (revert-buffer 'ignore-auto t t)
              (message "Reverting buffer `%s'..." (buffer-name))
              (revert-buffer 'ignore-auto t t)
              (message "Reverting buffer `%s'...done" (buffer-name)))
            (run-hooks 'magit-after-revert-hook)
            (setq ret t))
        (run-hooks 'magit-not-reverted-hook)))
    ret))

(add-hook 'git-commit-setup-hook #'magit-revert-buffers)

(defun magit-refresh-vc-mode-line ()
  "Update `vc-mode' which is displayed in the mode-line.
Like `vc-mode-line' but simpler, more efficient, and less buggy."
  (setq vc-mode
        (if vc-display-status
            (magit-with-toplevel
              (let* ((rev (or (magit-get-current-branch)
                              (magit-rev-parse "--short" "HEAD")))
                     (msg (cl-letf (((symbol-function #'vc-working-revision)
                                     (lambda (&rest _) rev)))
                            (vc-default-mode-line-string
                             'Git buffer-file-name))))
                (propertize
                 (concat " " msg)
                 'mouse-face 'mode-line-highlight
                 'help-echo (concat (get-text-property 0 'help-echo msg)
                                    "\nCurrent revision: " rev
                                    "\nmouse-1: Version Control menu")
                 'local-map vc-mode-line-map)))
          " Git"))
  (force-mode-line-update))

(defvar disable-magit-save-buffers nil)

(defun magit-pre-command-hook ()
  (setq disable-magit-save-buffers nil))
(add-hook 'pre-command-hook #'magit-pre-command-hook)

(defun magit-maybe-save-repository-buffers ()
  (when (and magit-save-repository-buffers
             (not disable-magit-save-buffers))
    (setq disable-magit-save-buffers t)
    (let ((msg (current-message)))
      (magit-save-repository-buffers
       (eq magit-save-repository-buffers 'dontask))
      (when msg (message "%s" msg)))))

(add-hook 'magit-mode-setup-hook #'magit-maybe-save-repository-buffers)
(add-hook 'magit-pre-refresh-hook #'magit-maybe-save-repository-buffers)
(add-hook 'magit-pre-call-git-hook #'magit-maybe-save-repository-buffers)
(add-hook 'magit-pre-start-git-hook #'magit-maybe-save-repository-buffers)

(defun magit-save-repository-buffers (&optional arg)
  "Save file-visiting buffers belonging to the current repository.
After any buffer where `buffer-save-without-query' is non-nil
is saved without asking, the user is asked about each modified
buffer which visits a file in the current repository.  Optional
argument (the prefix) non-nil means save all with no questions."
  (interactive "P")
  (-when-let (topdir (magit-toplevel))
    (save-some-buffers
     arg (-partial (lambda (topdir)
                     (and buffer-file-name
                          ;; Avoid needlessly connecting to unrelated remotes.
                          (string-prefix-p topdir buffer-file-name)
                          (equal (ignore-errors (magit-toplevel nil t)) topdir)))
                   topdir))))

;;; Restore Window Configuration

(defvar magit-inhibit-save-previous-winconf nil)

(defvar-local magit-previous-window-configuration nil)
(put 'magit-previous-window-configuration 'permanent-local t)

(defun magit-save-window-configuration ()
  (if magit-inhibit-save-previous-winconf
      (when (eq magit-inhibit-save-previous-winconf 'unset)
        (setq magit-previous-window-configuration nil))
    (unless (get-buffer-window (current-buffer) (selected-frame))
      (setq magit-previous-window-configuration
            (current-window-configuration)))))

(defun magit-restore-window-configuration (&optional kill-buffer)
  (let ((winconf magit-previous-window-configuration)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when (and winconf (equal frame (window-configuration-frame winconf)))
      (set-window-configuration winconf)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (setq magit-previous-window-configuration nil))))))

;;; Buffer History

(defun magit-go-backward ()
  "Move backward in current buffer's history."
  (interactive)
  (if help-xref-stack
      (help-xref-go-back (current-buffer))
    (user-error "No previous entry in buffer's history")))

(defun magit-go-forward ()
  "Move forward in current buffer's history."
  (interactive)
  (if help-xref-forward-stack
      (help-xref-go-forward (current-buffer))
    (user-error "No next entry in buffer's history")))

(defun magit-xref-insert-buttons ()
  (when (and (or (and magit-revision-show-xref-buttons
                      (derived-mode-p 'magit-revision-mode))
                 (and magit-diff-show-xref-buttons
                      (derived-mode-p 'magit-diff-mode)))
             (or help-xref-stack help-xref-forward-stack))
    (insert "\n")
    (when help-xref-stack
      (magit-xref-insert-button help-back-label 'magit-xref-backward))
    (when help-xref-forward-stack
      (when help-xref-stack
        (insert " "))
      (magit-xref-insert-button help-forward-label 'magit-xref-forward))))

(defun magit-xref-insert-button (label type)
  (magit-insert-section (button label)
    (insert-text-button label 'type type
                        'help-args (list (current-buffer)))))

(define-button-type 'magit-xref-backward
  :supertype 'help-back
  'mouse-face 'magit-section-highlight
  'help-echo (purecopy "mouse-2, RET: go back to previous history entry"))

(define-button-type 'magit-xref-forward
  :supertype 'help-forward
  'mouse-face 'magit-section-highlight
  'help-echo (purecopy "mouse-2, RET: go back to next history entry"))

(defun magit-xref-setup (refresh-args)
  (when help-xref-stack-item
    (push (cons (point) help-xref-stack-item) help-xref-stack)
    (setq help-xref-forward-stack nil))
  (when (called-interactively-p 'interactive)
    (--when-let (nthcdr 10 help-xref-stack)
      (setcdr it nil)))
  (setq help-xref-stack-item
        `(magit-xref-restore ,default-directory ,@refresh-args)))

(defun magit-xref-restore (&rest args)
  (magit-xref-setup magit-refresh-args)
  (setq default-directory  (car args))
  (setq magit-refresh-args (cdr args))
  (magit-refresh-buffer))

;;; magit-mode.el ends soon
(provide 'magit-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-mode.el ends here
