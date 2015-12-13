;;; magit-mode.el --- create and refresh Magit buffers  -*- lexical-binding: t -*-

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
;; For `magit-revert-buffers'
(declare-function magit-blame-mode 'magit-blame)
(defvar magit-blame-mode)

(require 'format-spec)
(require 'help-mode)

;;; Options

(defcustom magit-mode-hook
  '(magit-load-config-extensions
    magit-xref-setup)
  "Hook run when entering a mode derived from Magit mode."
  :group 'magit-modes
  :type 'hook
  :options '(magit-load-config-extensions
             magit-xref-setup
             bug-reference-mode))

(defcustom magit-mode-setup-hook
  '(magit-maybe-save-repository-buffers
    magit-maybe-show-margin)
  "Hook run by `magit-mode-setup'."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-maybe-save-repository-buffers
             magit-maybe-show-margin))

(defcustom magit-display-buffer-function 'magit-display-buffer-traditional
  "The function used display a Magit buffer.

All Magit buffers (buffers whose major-modes derive from
`magit-mode') are displayed using `magit-display-buffer',
which in turn uses the function specified here."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type '(radio (function-item magit-display-buffer-traditional)
                (function-item display-buffer)
                (function :tag "Function")))

(unless (find-lisp-object-file-name 'magit-pre-display-buffer-hook 'defvar)
  (add-hook 'magit-pre-display-buffer-hook 'magit-save-window-configuration))
(defcustom magit-pre-display-buffer-hook '(magit-save-window-configuration)
  "Hook run by `magit-display-buffer' before displaying the buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-save-window-configuration))

(unless (find-lisp-object-file-name 'magit-post-display-buffer-hook 'defvar)
  (add-hook 'magit-post-display-buffer-hook 'magit-maybe-set-dedicated))
(defcustom magit-post-display-buffer-hook '(magit-maybe-set-dedicated)
  "Hook run by `magit-display-buffer' after displaying the buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-maybe-set-dedicated))

(defcustom magit-generate-buffer-name-function
  'magit-generate-buffer-name-default-function
  "The function used to generate the name for a Magit buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type '(radio (function-item magit-generate-buffer-name-default-function)
                (function :tag "Function")))

(defcustom magit-buffer-name-format "*%M%v: %t"
  "The format string used to name Magit buffers.

The following %-sequences are supported:

`%m' The name of the major-mode, but with the `-mode' suffix
     removed.

`%M' Like \"%m\" but abbreviate `magit-status-mode' as `magit'.

`%v' The value the buffer is locked to, in parentheses, or an empty
     string if the buffer is not locked to a value.

`%V' Like \"%v\", but the string is prefixed with a space, unless it
     is an empty string.

`%t' The top-level directory of the working tree of the
     repository, or if `magit-uniquify-buffer-names' is non-nil
     an abbreviation of that.

The value should always contain either \"%m\" or \"%M\" as well as
\"%t\".  If `magit-uniquify-buffer-names' is non-nil, then the
value must end with \"%t\".

This is used by `magit-generate-buffer-name-default-function'.
If another `magit-generate-buffer-name-function' is used, then
it may not respect this option, or on the contrary it may
support additional %-sequences."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'string)

(defcustom magit-uniquify-buffer-names t
  "Whether to uniquify the names of Magit buffers."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'boolean)

(defcustom magit-bury-buffer-function 'magit-restore-window-configuration
  "The function used to bury or kill the current Magit buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type '(radio (function-item quit-window)
                (function-item magit-mode-quit-window)
                (function-item magit-restore-window-configuration)
                (function :tag "Function")))

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

(defcustom magit-refresh-verbose nil
  "Whether to revert Magit buffers verbosely."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'boolean)

(defcustom magit-refresh-buffer-hook nil
  "Normal hook for `magit-refresh-buffer' to run after refreshing."
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
             (run-with-timer 0 (abs magit-revert-buffers)
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
          This (or a negative number) is the recommended setting,
          because for the other values the revert messages might
          prevent you from seeing other, more important, messages
          in the echo area.

NUMBER    An integer or float.  Revert the buffers asynchronously.
          If NUMBER is positive, then mention each buffer as it is
          being reverted.  If it is negative, then be quiet about
          it.  If user input arrives, then stop reverting.  After
          (the absolute value of) NUMBER seconds resume reverting."
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
  "Normal hook for `magit-revert-buffer' to run after reverting.

This hook is only run for buffers that were actually reverted.
For other buffers `magit-not-reverted-hook' is run instead."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-refresh-vc-mode-line))

(defcustom magit-not-reverted-hook '(magit-refresh-vc-mode-line)
  "Normal hook for `magit-revert-buffer' to run instead of reverting.

This hook is only run for buffers which might have been reverted
but were not actually reverted, because that was not necessary.
For other buffers `magit-after-revert-hook' is run instead."
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
has to confirm each save.  `dontask' is the recommended setting."
  :group 'magit
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Save without asking" dontask)))

(defcustom magit-keep-region-overlay nil
  "Whether to keep the region overlay when there is a valid selection.

By default Magit removes the regular region overlay if, and only
if, that region constitutes a valid selection as understood by
Magit commands.  Otherwise it does not remove that overlay, and
the region looks like it would in other buffers.

There are two types of such valid selections: hunk-internal
regions and regions that select two or more sibling sections.
In such cases Magit removes the region overlay and instead
highlights a slightly larger range.  All text (for hunk-internal
regions) or the headings of all sections (for sibling selections)
that are inside that range (not just inside the region) are acted
on by commands such as the staging command.  This buffer range
begins at the beginning of the line on which the region begins
and ends at the end of the line on which the region ends.

Because Magit acts on this larger range and not the region, it is
actually quite important to visualize that larger range.  If we
don't do that, then one might think that these commands act on
the region instead.  If you want to *also* visualize the region,
then set this option to t.  But please note that when the region
does *not* constitute a valid selection, then the region is
*always* visualized as usual, and that it is usually under such
circumstances that you want to use a non-magit command to act on
the region.

Besides keeping the region overlay, setting this option to t also
causes all face properties, except for `:foreground', to be
ignored for the faces used to highlight headings of selected
sections.  This avoids the worst conflicts that result from
displaying the region and the selection overlays at the same
time.  We are not interested in dealing with other conflicts.
In fact we *already* provide a way to avoid all of these
conflicts: *not* changing the value of this option.

It should be clear by now that we consider it a mistake to set
this to display the region when the Magit selection is also
visualized, but since it has been requested a few times and
because it doesn't cost much to offer this option we do so.
However that might change.  If the existence of this option
starts complicating other things, then it will be removed."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'boolean)

;;; Magit Mode

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "\t"    'magit-section-toggle)
    (define-key map [C-tab] 'magit-section-cycle)
    (define-key map [M-tab] 'magit-section-cycle-diffs)
    (define-key map [s-tab] 'magit-section-cycle-global)
    (define-key map [backtab] 'magit-section-cycle-global)
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
    (define-key map "$" 'magit-process-buffer)
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
    (define-key map "U" 'magit-unstage-all)
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
    (define-key map "\C-w"   'magit-copy-section-value)
    (define-key map "\M-w"   'magit-copy-buffer-revision)
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
     ["Log" magit-log t]
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
    ["Display Git output" magit-process-buffer t]
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
  (setq list-buffers-directory default-directory)
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
  (if (and (run-hook-with-args-until-success 'magit-region-highlight-hook
                                             (magit-current-section))
           (not magit-keep-region-overlay))
      (funcall (default-value 'redisplay-unhighlight-region-function) rol)
    (funcall (default-value 'redisplay-highlight-region-function)
             start end window rol)))

(defun magit-unhighlight-region (rol)
  (setq magit-section-highlighted-section nil)
  (mapc #'delete-overlay magit-region-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

(defvar-local magit-refresh-args nil
  "The arguments used to refresh the current buffer.")
(put 'magit-refresh-args 'permanent-local t)

(defvar-local magit-previous-section nil)
(put 'magit-previous-section 'permanent-local t)

(defun magit-mode-setup (mode &rest args)
  "Setup up a MODE buffer using ARGS to generate its content."
  (let ((buffer (magit-mode-get-buffer mode t))
        (section (magit-current-section)))
    (with-current-buffer buffer
      (setq magit-previous-section section)
      (setq magit-refresh-args args)
      (funcall mode))
    (magit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'magit-mode-setup-hook)
      (magit-refresh-buffer))))

(defvar magit-display-buffer-noselect nil
  "If non-nil, then `magit-display-buffer' doesn't call `select-window'.")

(defun magit-display-buffer (buffer)
  "Display BUFFER in some window and maybe select it.

Display the buffer using `magit-display-buffer-function' and
then, unless `magit-display-buffer-noselect' is non-nil, select
the window which was used to display the buffer.

Also run the hooks `magit-pre-display-buffer-hook'
and `magit-post-display-buffer-hook'."
  (with-current-buffer buffer
    (run-hooks 'magit-pre-display-buffer-hook))
  (let ((window (funcall magit-display-buffer-function buffer)))
    (unless magit-display-buffer-noselect
      (select-window window)))
  (with-current-buffer buffer
    (run-hooks 'magit-post-display-buffer-hook)))

(defun magit-display-buffer-traditional (buffer)
  "Display BUFFER the way this has traditionally been done."
  (display-buffer
   buffer (if (and (derived-mode-p 'magit-mode)
                   (not (memq (with-current-buffer buffer major-mode)
                              '(magit-process-mode
                                magit-revision-mode
                                magit-diff-mode
                                magit-stash-mode
                                magit-status-mode))))
              '(display-buffer-same-window)
            nil))) ; display in another window

(defun magit-maybe-set-dedicated ()
  "Mark the selected window as dedicated if appropriate.

If a new window was created to display the buffer, then remember
that fact.  That information is used by `magit-mode-quit-window',
to determine whether the window should be deleted when its last
Magit buffer is buried."
  (let ((window (get-buffer-window (current-buffer))))
    (when (and (window-live-p window)
               (not (window-prev-buffers window)))
      (set-window-parameter window 'magit-dedicated t))))

(defvar-local magit--default-directory nil
  "Value of `default-directory' when buffer is generated.
This exists to prevent a let-bound `default-directory' from
tricking `magit-mode-get-buffer' or `magit-mode-get-buffers' into
thinking a buffer belongs to a repo that it doesn't.")
(put 'magit--default-directory 'permanent-local t)

(defun magit-mode-get-buffers ()
  (let ((topdir (magit-toplevel)))
    (--filter (with-current-buffer it
                (and (derived-mode-p 'magit-mode)
                     (equal magit--default-directory topdir)))
              (buffer-list))))

(defvar-local magit-buffer-locked-p nil)
(put 'magit-buffer-locked-p 'permanent-local t)

(defun magit-mode-get-buffer (mode &optional create frame)
  (-if-let (topdir (magit-toplevel))
      (or (--first (with-current-buffer it
                     (and (eq major-mode mode)
                          (equal magit--default-directory topdir)
                          (not magit-buffer-locked-p)))
                   (if frame
                       (-map #'window-buffer
                             (window-list (unless (eq frame t) frame)))
                     (buffer-list)))
          (and create
               (let ((default-directory topdir))
                 (magit-generate-new-buffer mode))))
    (user-error "Not inside a Git repository")))

(defun magit-generate-new-buffer (mode)
  (let* ((name (funcall magit-generate-buffer-name-function mode))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq magit--default-directory default-directory))
    (when magit-uniquify-buffer-names
      (add-to-list 'uniquify-list-buffers-directory-modes mode)
      (with-current-buffer buffer
        (setq list-buffers-directory default-directory))
      (let ((uniquify-buffer-name-style
             (if (eq uniquify-buffer-name-style 'forward)
                 'post-forward-angle-brackets
               uniquify-buffer-name-style)))
        (uniquify-rationalize-file-buffer-names
         name (file-name-directory (directory-file-name default-directory))
         buffer)))
    buffer))

(defun magit-generate-buffer-name-default-function (mode &optional value)
  (let ((m (substring (symbol-name mode) 0 -5))
        (v (and value (format "%s" (if (listp value) value (list value))))))
    (format-spec
     magit-buffer-name-format
     `((?m . ,m)
       (?M . ,(if (eq mode 'magit-status-mode) "magit" m))
       (?v . ,(or v ""))
       (?V . ,(if v (concat " " v) ""))
       (?t . ,(if magit-uniquify-buffer-names
                  (file-name-nondirectory
                   (directory-file-name default-directory))
                default-directory))))))

(defun magit-toggle-buffer-lock ()
  "Lock the current buffer to its value or unlock it.

Locking a buffer to its value, prevents it from being reused to
display another value.  The name of a locked buffer contains its
value, which allows telling it apart from other locked buffers
and the unlocked buffer.

Not all Magit buffers can be locked to their values, for example
it wouldn't make sense to lock a status buffer.

There can only be a single unlocked buffer using a certain
major-mode per repository.  So when a buffer is being unlocked
and another unlocked buffer already exists for that mode and
repository, then the former buffer is instead deleted and the
latter is displayed in its place."
  (interactive)
  (if magit-buffer-locked-p
      (-if-let (unlocked (magit-mode-get-buffer major-mode))
          (let ((locked (current-buffer)))
            (set-buffer unlocked)
            (kill-buffer locked))
        (setq magit-buffer-locked-p nil)
        (rename-buffer (funcall magit-generate-buffer-name-function
                                major-mode)))
    (setq magit-buffer-locked-p
          (cond ((memq major-mode '(magit-cherry-mode
                                    magit-log-mode
                                    magit-reflog-mode
                                    magit-refs-mode
                                    magit-revision-mode
                                    magit-stash-mode
                                    magit-stashes-mode))
                 (car magit-refresh-args))
                ((eq major-mode 'magit-diff-mode)
                 (let ((rev  (nth 0 magit-refresh-args))
                       (args (nth 1 magit-refresh-args)))
                   (cond
                    ((member "--no-index" args)
                     (nth 3 magit-refresh-args))
                    (rev (if args (cons rev args) rev))
                    (t   (if (member "--cached" args) "staged" "unstaged")))))))
    (if magit-buffer-locked-p
        (rename-buffer (funcall magit-generate-buffer-name-function
                                major-mode magit-buffer-locked-p))
      (user-error "Buffer has no value it could be locked to"))))

(defun magit-mode-bury-buffer (&optional kill-buffer)
  "Bury the current buffer.
With a prefix argument, kill the buffer instead.
This is done using `magit-bury-buffer-function'."
  (interactive "P")
  (funcall magit-bury-buffer-function kill-buffer))

(defun magit-mode-quit-window (kill-buffer)
  "Quit the selected window and bury its buffer.

This behaves similar to `quit-window', but when the window
was originally created to display a Magit buffer and the
current buffer is the last remaining Magit buffer that was
ever displayed in the selected window, then delete that
window."
  (if (or (one-window-p)
          (--first (let ((buffer (car it)))
                     (and (not (eq buffer (current-buffer)))
                          (buffer-live-p buffer)
                          (or (not (window-parameter nil 'magit-dedicated))
                              (with-current-buffer buffer
                                (derived-mode-p 'magit-mode
                                                'magit-process-mode)))))
                   (window-prev-buffers)))
      (quit-window kill-buffer)
    (let ((window (selected-window)))
      (quit-window kill-buffer)
      (when (window-live-p window)
        (delete-window window)))))

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
    (run-hooks 'magit-pre-refresh-hook)
    (unless (when (derived-mode-p 'magit-mode)
              (magit-refresh-buffer)
              (derived-mode-p 'magit-status-mode))
      (--when-let (magit-mode-get-buffer 'magit-status-mode)
        (with-current-buffer it
          (magit-refresh-buffer))))
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
  "Refresh the current Magit buffer."
  (setq magit-refresh-start-time (current-time))
  (let ((refresh (intern (format "%s-refresh-buffer"
                                 (substring (symbol-name major-mode) 0 -5)))))
    (when (functionp refresh)
      (when magit-refresh-verbose
        (message "Refreshing buffer `%s'..." (buffer-name)))
      (let* ((buffer (current-buffer))
             (windows
              (--mapcat (with-selected-window it
                          (with-current-buffer buffer
                            (-when-let (section (magit-current-section))
                              (list
                               (nconc (list it section)
                                      (magit-refresh-get-relative-position))))))
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
            (apply refresh magit-refresh-args)))
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
                                            magit-refresh-start-time)))))))

(defun magit-refresh-get-relative-position ()
  (-when-let (section (magit-current-section))
    (let ((start (magit-section-start section)))
      (list (count-lines start (point))
            (- (point) (line-beginning-position))
            (and (eq (magit-section-type section) 'hunk)
                 (region-active-p)
                 (progn (goto-char (line-beginning-position))
                        (when  (looking-at "^[-+]") (forward-line))
                        (while (looking-at "^[ @]") (forward-line))
                        (let ((beg (point)))
                          (cond ((looking-at "^[-+]")
                                 (forward-line)
                                 (while (looking-at "^[-+]") (forward-line))
                                 (while (looking-at "^ ")    (forward-line))
                                 (forward-line -1)
                                 (regexp-quote (buffer-substring-no-properties
                                                beg (line-end-position))))
                                (t t)))))))))


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
                          (equal (file-remote-p file)
                                 (file-remote-p topdir))
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
            (if (or (eq magit-revert-buffers 'silent)
                    (and (numberp magit-revert-buffers)
                         (< magit-revert-buffers 0)))
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

(defvar magit-after-save-refresh-buffers nil)

(defun magit-after-save-refresh-buffers ()
  (dolist (buffer magit-after-save-refresh-buffers)
    (with-current-buffer buffer
      (magit-refresh-buffer)))
  (setq magit-after-save-refresh-buffers nil)
  (remove-hook 'post-command-hook 'magit-after-save-refresh-buffers))

(defun magit-after-save-refresh-status ()
  "Refresh the status buffer of the current repository.

This function is intended to be added to `after-save-hook'.

If the status buffer does not exist or the file being visited in
the current buffer isn't inside a repository, then do nothing.

Note that refreshing a Magit buffer is done by re-creating its
contents from scratch, which can be slow in large repositories.
If you are not satisfied with Magit's performance, then you
should obviously not add this function to that hook."
  (unless disable-magit-save-buffers
    (--when-let (ignore-errors (magit-mode-get-buffer 'magit-status-mode))
      (add-to-list 'magit-after-save-refresh-buffers it)
      (add-hook 'post-command-hook 'magit-after-save-refresh-buffers))))

(defun magit-maybe-save-repository-buffers ()
  "Maybe save file-visiting buffers belonging to the current repository.
Do so if `magit-save-repository-buffers' is non-nil."
  (when (and magit-save-repository-buffers
             (not disable-magit-save-buffers))
    (setq disable-magit-save-buffers t)
    (let ((msg (current-message)))
      (magit-save-repository-buffers
       (eq magit-save-repository-buffers 'dontask))
      (when (and msg (not (equal msg (current-message))))
        (message "%s" msg)))))

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
  (-when-let (topdir (magit-rev-parse-safe "--show-toplevel"))
    (save-some-buffers
     arg (-partial (lambda (topdir)
                     (and buffer-file-name
                          ;; Avoid needlessly connecting to unrelated remotes.
                          (string-prefix-p topdir buffer-file-name)
                          (equal (magit-rev-parse-safe "--show-toplevel")
                                 topdir)))
                   topdir))))

;;; Restore Window Configuration

(defvar magit-inhibit-save-previous-winconf nil)

(defvar-local magit-previous-window-configuration nil)
(put 'magit-previous-window-configuration 'permanent-local t)

(defun magit-save-window-configuration ()
  "Save the current window configuration.

Later, when the buffer is buried, it may be restored by
`magit-restore-window-configuration'."
  (if magit-inhibit-save-previous-winconf
      (when (eq magit-inhibit-save-previous-winconf 'unset)
        (setq magit-previous-window-configuration nil))
    (unless (get-buffer-window (current-buffer) (selected-frame))
      (setq magit-previous-window-configuration
            (current-window-configuration)))))

(defun magit-restore-window-configuration (&optional kill-buffer)
  "Bury or kill the current buffer and restore previous window configuration."
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

(defun magit-insert-xref-buttons (&optional _)
  "Insert xref buttons."
  (when (or help-xref-stack help-xref-forward-stack)
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

(defun magit-xref-setup ()
  "Insert backward/forward buttons if the major-mode supports it.
Currently `magit-log-mode', `magit-reflog-mode',
`magit-diff-mode', and `magit-revision-mode' support it"
  (when (memq major-mode '(magit-log-mode
                           magit-reflog-mode
                           magit-diff-mode
                           magit-revision-mode))
    (when help-xref-stack-item
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (setq help-xref-forward-stack nil))
    (when (called-interactively-p 'interactive)
      (--when-let (nthcdr 10 help-xref-stack)
        (setcdr it nil)))
    (setq help-xref-stack-item
          `(magit-xref-restore ,default-directory ,@magit-refresh-args))))

(defun magit-xref-restore (&rest args)
  (magit-xref-setup)
  (setq default-directory  (car args))
  (setq magit-refresh-args (cdr args))
  (magit-refresh-buffer))

;;; magit-mode.el ends soon
(provide 'magit-mode)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-mode.el ends here
