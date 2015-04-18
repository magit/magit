;;; magit-mode.el --- create and refresh Magit buffers

;; Copyright (C) 2010-2015  The Magit Project Developers
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

(require 'autorevert)
(require 'format-spec)
(require 'help-mode)

;;; Options

(defcustom magit-mode-hook '(magit-load-config-extensions)
  "Hook run when entering a mode derived from Magit mode."
  :group 'magit-modes
  :type 'hook
  :options '(magit-load-config-extensions bug-reference-mode))

(defcustom magit-auto-revert-mode-lighter " MRev"
  "String to display when Magit-Auto-Revert mode is active."
  :group 'magit-modes
  :type 'string)

(define-minor-mode magit-auto-revert-mode
  "Toggle global Magit-Auto-Revert mode.
With prefix ARG, enable Magit-Auto-Revert mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Magit-Auto-Revert mode is a global minor mode that, after Magit
has run a Git command, reverts buffers associated with files that
have changed on disk and are tracked in the current Git repository."
  :group 'magit
  :lighter magit-auto-revert-mode-lighter
  :global t
  :init-value t)

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
or might not be what you want."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'boolean)

(defcustom magit-save-repository-buffers t
  "Whether to save modified buffers when approriate.

If this is non-nil then modified buffers belonging to the current
repository may be saved when the status buffer is being refreshed
and before a checkout is performed.  When the value is `dontask'
then this is done without user intervention, when it is t then
the user has to confirm each save."
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
    (define-key map "l" 'magit-log-popup)
    (define-key map "L" 'magit-toggle-margin)
    (define-key map "m" 'magit-merge-popup)
    (define-key map "M" 'magit-remote-popup)
    (define-key map "o" 'magit-submodule-popup)
    (define-key map "P" 'magit-push-popup)
    (define-key map "r" 'magit-rebase-popup)
    (define-key map "t" 'magit-tag-popup)
    (define-key map "T" 'magit-notes-popup)
    (define-key map [M-return] 'magit-dired-jump)
    (define-key map "\s"       'magit-diff-show-or-scroll-up)
    (define-key map "\d"       'magit-diff-show-or-scroll-down)
    (define-key map "s" 'magit-stage-file)
    (define-key map "S" 'magit-stage-modified)
    (define-key map "u" 'magit-unstage-file)
    (define-key map "U" 'magit-reset-index)
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
    map)
  "Parent keymap for all keymaps of modes derived from `magit-mode'.")

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
    ["Snapshot" magit-stash-snapshot t]
    "---"
    ["Branch..." magit-checkout t]
    ["Merge" magit-merge t]
    ["Ediff resolve" magit-ediff-resolve t]
    ["Rebase..." magit-rebase-popup t]
    "---"
    ["Push" magit-push t]
    ["Pull" magit-pull t]
    ["Remote update" magit-remote-update t]
    ("Submodule"
     ["Submodule update" magit-submodule-update t]
     ["Submodule update and init" magit-submodule-update-init t]
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
      (when (and (fboundp sym)
                 (not (eq sym 'magit-wip-save-mode)))
        (funcall sym 1)))))

(define-derived-mode magit-mode special-mode "Magit"
  "Parent major mode from which Magit major modes inherit.
Magit is documented in info node `(magit)'."
  :group 'magit-modes
  (buffer-disable-undo)
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq show-trailing-whitespace nil)
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (push (cons 'invisible t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-post-command-adjust-point t t)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t))

(defun magit-post-command-adjust-point ()
  (when (and (get-text-property (point) 'invisible)
             (not (if (fboundp 'get-pos-property) ; since 24.4, see #1671
                      (get-pos-property (point) 'invisible)
                    (get-text-property (1+ (point)) 'invisible))))
    (goto-char (next-single-char-property-change (point) 'invisible))))

(defvar-local magit-refresh-function nil)
(put 'magit-refresh-function 'permanent-local t)

(defvar-local magit-refresh-args nil)
(put 'magit-refresh-args 'permanent-local t)

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
            (,sroot (magit-get-top-dir))
            (,sfunc ,refresh-func)
            (,sargs (list ,@refresh-args))
            (,sbuf  (magit-mode-display-buffer ,buffer ,smode ,switch-func)))
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

(defvar magit-inhibit-save-previous-winconf nil)

(defvar-local magit-previous-window-configuration nil)
(put 'magit-previous-window-configuration 'permanent-local t)

(defvar-local magit-previous-section nil)
(put 'magit-previous-section 'permanent-local t)

(defun magit-mode-display-buffer (buffer mode &optional switch-function)
  "Display BUFFER in some window and select it.
BUFFER may be a buffer or a string, the name of a buffer.  Return
the buffer.

Unless BUFFER is already displayed in the selected frame store the
previous window configuration as a buffer local value, so that it
can later be restored by `magit-mode-bury-buffer'.

Then display and select BUFFER using SWITCH-FUNCTION.  If that is
nil either use `pop-to-buffer' if the current buffer's major mode
derives from Magit mode; or else use `switch-to-buffer'."
  (cond ((stringp buffer)
         (setq buffer (magit-mode-get-buffer-create buffer mode)))
        ((not (bufferp buffer))
         (signal 'wrong-type-argument (list 'bufferp nil))))
  (let ((section (magit-current-section)))
    (with-current-buffer (get-buffer-create buffer)
      (setq magit-previous-section section)
      (if magit-inhibit-save-previous-winconf
          (when (eq magit-inhibit-save-previous-winconf 'unset)
            (setq magit-previous-window-configuration nil))
        (unless (get-buffer-window buffer (selected-frame))
          (setq magit-previous-window-configuration
                (current-window-configuration))))))
  (funcall (or switch-function
               (if (derived-mode-p 'magit-mode)
                   'switch-to-buffer
                 'pop-to-buffer))
           buffer)
  buffer)

(defun magit-mode-get-buffers (&optional topdir)
  (unless topdir
    (setq topdir (magit-get-top-dir)))
  (--filter (with-current-buffer it
              (and (derived-mode-p 'magit-mode)
                   (equal default-directory topdir)))
            (buffer-list)))

(defun magit-mode-get-buffer (format mode &optional topdir create)
  (if (not (string-match-p "%[ab]" format))
      (funcall (if create #'get-buffer-create #'get-buffer) format)
    (unless topdir
      (setq topdir (magit-get-top-dir)))
    (let ((name (format-spec format
                             `((?a . ,(abbreviate-file-name (or topdir "-")))
                               (?b . ,(if topdir
                                          (file-name-nondirectory
                                           (directory-file-name topdir))
                                        "-"))))))
      (or (--first (with-current-buffer it
                     (and (or (not topdir)
                              (equal (expand-file-name default-directory)
                                     topdir))
                          (string-match-p (format "^%s\\(?:<[0-9]+>\\)?$"
                                                  (regexp-quote name))
                                          (buffer-name))))
                   (buffer-list))
          (and create (generate-new-buffer name))))))

(defun magit-mode-get-buffer-create (format mode &optional topdir)
  (magit-mode-get-buffer format mode topdir t))

(defun magit-mode-bury-buffer (&optional kill-buffer)
  "Bury the current buffer.
With a prefix argument, kill the buffer instead.

If `magit-restore-window-configuration' is non-nil and the last
configuration stored by `magit-mode-display-buffer' originates
from the selected frame then restore it after burying/killing
the buffer.  Finally reset the window configuration to nil."
  (interactive "P")
  (let ((winconf magit-previous-window-configuration)
        (buffer (current-buffer))
        (frame (selected-frame)))
    (quit-window kill-buffer (selected-window))
    (when winconf
      (when (and magit-restore-window-configuration
                 (equal frame (window-configuration-frame winconf)))
        (set-window-configuration winconf)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq magit-previous-window-configuration nil)))))
    (run-hook-with-args 'magit-mode-bury-buffer-hook buffer)))

;;; Refresh Machinery

(defvar inhibit-magit-refresh nil)

(defun magit-refresh ()
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`magit-mode', and refresh the corresponding status buffer.
If the global `magit-auto-revert-mode' is turned on, then
also revert all unmodified buffers that visit files being
tracked in the current repository."
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

(defvar magit-refresh-buffer-hook nil
  "Hook run after refreshing a file-visiting buffer.")

(defun magit-refresh-buffer ()
  "Refresh the current Magit buffer.
Uses the buffer-local `magit-refresh-function'."
  (when magit-refresh-function
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
            magit-section-highlighted-sections nil)
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
      (set-buffer-modified-p nil))))

(defun magit-revert-buffers (&optional force)
  "Revert unmodified file-visiting buffers of the current repository.

If, and only if, the global `magit-auto-revert-mode' is turned
on, or if optional FORCE is non-nil, revert all unmodified
buffers that visit files being tracked in the current
repository."
  (when (or force magit-auto-revert-mode)
    (-when-let (topdir (magit-toplevel-safe))
      (let ((tracked (magit-revision-files "HEAD"))
            (buffers (buffer-list)))
        (if (> (length tracked)
               (length buffers))
            (dolist (buffer buffers)
              (with-current-buffer buffer
                (let ((file buffer-file-truename))
                  (and file
                       (file-in-directory-p file topdir)
                       (member (file-relative-name file topdir) tracked)
                       (magit-revert-buffer)))))
          (dolist (file (--map (expand-file-name it topdir) tracked))
            (-when-let (buffer (find-buffer-visiting file))
              (with-current-buffer buffer
                (magit-revert-buffer)))))))))

(defvar magit-after-revert-hook nil
  "Normal hook for `magit-revert-buffer' to run after reverting.")

(defvar magit-not-reverted-hook nil
  "Normal hook for `magit-revert-buffer' to run instead of reverting.
Run if the visited file has not changed on disk and the buffer
therefor does not have to be reverted.  While Magit does not need
to do anything in that case, some third-party extensions do.")

(defun magit-revert-buffer ()
  "Refresh the current file-visiting buffer."
  (if (and (file-readable-p buffer-file-name)
           (not (verify-visited-file-modtime (current-buffer))))
      (if magit-blame-mode
          (progn (message "Reverting %s inhibited due to magit-blame-mode"
                          buffer-file-name)
                 (run-hooks 'magit-not-reverted-hook))
        (revert-buffer 'ignore-auto 'dont-ask 'preserve-modes)
        (run-hooks 'magit-after-revert-hook))
    (run-hooks 'magit-not-reverted-hook))
  (vc-find-file-hook))

(add-hook 'git-commit-setup-hook 'magit-revert-buffers)

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
  (save-some-buffers
   arg `(lambda ()
          (and buffer-file-name
               (-when-let (topdir ,(magit-get-top-dir default-directory))
                 (and (string-prefix-p topdir buffer-file-name)
                      ;; ^ Avoid needlessly connecting to unrelated remotes.
                      (equal (ignore-errors
                               (magit-get-top-dir default-directory))
                             topdir)
                      (magit-inside-worktree-p)))))))

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
