;;; magit-mode.el --- create and refresh Magit buffers  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2019  The Magit Project Contributors
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

(eval-when-compile
  (require 'subr-x))

(require 'magit-section)
(require 'magit-git)

;; For `magit-display-buffer-fullcolumn-most-v1' from `git-commit'
(defvar git-commit-mode)
;; For `magit-xref-insert-buttons' from `magit'
(defvar magit-diff-show-xref-buttons)
(defvar magit-revision-show-xref-buttons)
;; For `magit-refresh'
(defvar magit-post-commit-hook-commands)
(defvar magit-post-stage-hook-commands)
(defvar magit-post-unstage-hook-commands)
;; For `magit-refresh' and `magit-refresh-all'
(declare-function magit-auto-revert-buffers "magit-autorevert" ())
;; For `magit-refresh-buffer'
(declare-function magit-process-unset-mode-line-error-status "magit-process" ())
;; For `magit-mode-setup-internal'
(declare-function magit-status-goto-initial-section "magit-status" ())

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
    magit-set-buffer-margin)
  "Hook run by `magit-mode-setup'.

This is run right after displaying the buffer and right before
generating or updating its content.  `magit-mode-hook' and other,
more specific, `magit-mode-*-hook's on the other hand are run
right before displaying the buffer.  Usually one of these hooks
should be used instead of this one."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'hook
  :options '(magit-maybe-save-repository-buffers
             magit-set-buffer-margin))

(defcustom magit-pre-refresh-hook '(magit-maybe-save-repository-buffers)
  "Hook run before refreshing in `magit-refresh'.

This hook, or `magit-post-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`magit-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(magit . "2.4.0")
  :group 'magit-refresh
  :type 'hook
  :options '(magit-maybe-save-repository-buffers))

(defcustom magit-post-refresh-hook nil
  "Hook run after refreshing in `magit-refresh'.

This hook, or `magit-pre-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`magit-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(magit . "2.4.0")
  :group 'magit-refresh
  :type 'hook)

(defcustom magit-display-buffer-function 'magit-display-buffer-traditional
  "The function used display a Magit buffer.

All Magit buffers (buffers whose major-modes derive from
`magit-mode') are displayed using `magit-display-buffer',
which in turn uses the function specified here."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type '(radio (function-item magit-display-buffer-traditional)
                (function-item magit-display-buffer-same-window-except-diff-v1)
                (function-item magit-display-buffer-fullframe-status-v1)
                (function-item magit-display-buffer-fullframe-status-topleft-v1)
                (function-item magit-display-buffer-fullcolumn-most-v1)
                (function-item display-buffer)
                (function :tag "Function")))

(defcustom magit-pre-display-buffer-hook '(magit-save-window-configuration)
  "Hook run by `magit-display-buffer' before displaying the buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type 'hook
  :get 'magit-hook-custom-get
  :options '(magit-save-window-configuration))

(defcustom magit-post-display-buffer-hook '(magit-maybe-set-dedicated)
  "Hook run by `magit-display-buffer' after displaying the buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type 'hook
  :get 'magit-hook-custom-get
  :options '(magit-maybe-set-dedicated))

(defcustom magit-generate-buffer-name-function
  'magit-generate-buffer-name-default-function
  "The function used to generate the name for a Magit buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type '(radio (function-item magit-generate-buffer-name-default-function)
                (function :tag "Function")))

(defcustom magit-buffer-name-format "%x%M%v: %t%x"
  "The format string used to name Magit buffers.

The following %-sequences are supported:

`%m' The name of the major-mode, but with the `-mode' suffix
     removed.

`%M' Like \"%m\" but abbreviate `magit-status-mode' as `magit'.

`%v' The value the buffer is locked to, in parentheses, or an
     empty string if the buffer is not locked to a value.

`%V' Like \"%v\", but the string is prefixed with a space, unless
     it is an empty string.

`%t' The top-level directory of the working tree of the
     repository, or if `magit-uniquify-buffer-names' is non-nil
     an abbreviation of that.

`%x' If `magit-uniquify-buffer-names' is nil \"*\", otherwise the
     empty string.  Due to limitations of the `uniquify' package,
     buffer names must end with the path.

`%T' Obsolete, use \"%t%x\" instead.  Like \"%t\", but append an
     asterisk if and only if `magit-uniquify-buffer-names' is nil.

The value should always contain \"%m\" or \"%M\", \"%v\" or
\"%V\", and \"%t\" (or the obsolete \"%T\").

If `magit-uniquify-buffer-names' is non-nil, then the value must
end with \"%t\" or \"%t%x\" (or the obsolete \"%T\").  See issue
#2841.

This is used by `magit-generate-buffer-name-default-function'.
If another `magit-generate-buffer-name-function' is used, then
it may not respect this option, or on the contrary it may
support additional %-sequences."
  :package-version '(magit . "2.12.0")
  :group 'magit-buffers
  :type 'string)

(defcustom magit-uniquify-buffer-names t
  "Whether to uniquify the names of Magit buffers."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type 'boolean)

(defcustom magit-bury-buffer-function 'magit-restore-window-configuration
  "The function used to bury or kill the current Magit buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type '(radio (function-item quit-window)
                (function-item magit-mode-quit-window)
                (function-item magit-restore-window-configuration)
                (function :tag "Function")))

(defcustom magit-use-sticky-arguments t
  "How to reuse arguments from existing diff and log buffers.

nil       Always use the default value of the variable
          `magit-log-arguments' for log commands.  Likewise,
          always use the default value of the variable
          `magit-diff-arguments' for diff command calls.

current   If the mode of the current buffer is derived from
          `magit-log-mode' or `magit-diff-mode', reuse the
          arguments from that buffer instead of those given by
          the variable `magit-log-arguments' or
          `magit-diff-arguments', respectively.

t         Like `current', but if the mode of the current buffer
          is not derived from `magit-log-mode' or
          `magit-diff-mode', use the arguments from the current
          repository's active (i.e. non-locked) `magit-log-mode'
          or `magit-diff-mode' buffer, respectively, if it
          exists.

          Note that commands that generate a
          `magit-revision-mode' or `magit-stash-mode' buffer will
          also collect their diff arguments from the active
          `magit-diff-mode' buffer.

In general, there is a separation between the \"sticky\"
arguments for log and diff buffers, but there is one special
case: if the current buffer is a log buffer,
`magit-show-commit' (considered a diff command) uses the file
filter from the log buffer."
  :package-version '(magit . "2.11.0")
  :group 'magit-buffers
  :type '(choice (const :tag "disabled" nil)
                 (const :tag "sticky for current" current)
                 (const :tag "sticky" t)))

(defcustom magit-region-highlight-hook
  '(magit-section-update-region magit-diff-update-hunk-region)
  "Functions used to highlight the region.

Each function is run with the current section as only argument
until one of them returns non-nil.  If all functions return nil,
then fall back to regular region highlighting."
  :package-version '(magit . "2.1.0")
  :group 'magit-refresh
  :type 'hook
  :options '(magit-section-update-region magit-diff-update-hunk-region))

(defcustom magit-create-buffer-hook nil
  "Normal hook run after creating a new `magit-mode' buffer."
  :package-version '(magit . "2.90.0")
  :group 'magit-refresh
  :type 'hook)

(defcustom magit-refresh-buffer-hook nil
  "Normal hook for `magit-refresh-buffer' to run after refreshing."
  :package-version '(magit . "2.1.0")
  :group 'magit-refresh
  :type 'hook)

(defcustom magit-refresh-status-buffer t
  "Whether the status buffer is refreshed after running git.

When this is non-nil, then the status buffer is automatically
refreshed after running git for side-effects, in addition to the
current Magit buffer, which is always refreshed automatically.

Only set this to nil after exhausting all other options to
improve performance."
  :package-version '(magit . "2.4.0")
  :group 'magit-refresh
  :group 'magit-status
  :type 'boolean)

(defcustom magit-refresh-verbose nil
  "Whether to revert Magit buffers verbosely."
  :package-version '(magit . "2.1.0")
  :group 'magit-refresh
  :type 'boolean)

(defcustom magit-save-repository-buffers t
  "Whether to save file-visiting buffers when appropriate.

If non-nil, then all modified file-visiting buffers belonging
to the current repository may be saved before running Magit
commands and before creating or refreshing Magit buffers.
If `dontask', then this is done without user intervention, for
any other non-nil value the user has to confirm each save.

The default is t to avoid surprises, but `dontask' is the
recommended value."
  :group 'magit-essentials
  :group 'magit-buffers
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
  :group 'magit-miscellaneous
  :type 'boolean)

;;; Magit Mode

(defvar magit-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (cond ((featurep 'jkl)
           (define-key map   [return]  'magit-visit-thing)
           (define-key map [C-return]  'magit-dired-jump)
           (define-key map   [tab]     'magit-section-toggle)
           (define-key map [C-tab]     'magit-section-cycle)
           (define-key map [M-tab]     'magit-section-cycle-diffs)
           (define-key map [S-tab]     'magit-section-cycle-global)
           (define-key map (kbd "M-o") 'magit-section-up)
           (define-key map (kbd   "i") 'magit-section-backward)
           (define-key map (kbd   "k") 'magit-section-forward)
           (define-key map (kbd "M-i") 'magit-section-backward-sibling)
           (define-key map (kbd "M-k") 'magit-section-forward-sibling)
           (define-key map (kbd   "p") 'magit-push-popup)
           (define-key map (kbd   ",") 'magit-delete-thing)
           (define-key map (kbd   ";") 'magit-file-untrack)
           (define-key map (kbd "C-c C-i") 'magit-gitignore-popup))
          (t
           (define-key map [C-return]  'magit-visit-thing)
           (define-key map (kbd "C-m") 'magit-visit-thing)
           (define-key map (kbd "C-M-i") 'magit-dired-jump)
           (define-key map (kbd "C-i") 'magit-section-toggle)
           (define-key map [C-tab]     'magit-section-cycle)
           (define-key map [M-tab]     'magit-section-cycle-diffs)
           ;; [backtab] is the most portable binding for Shift+Tab.
           (define-key map [backtab]   'magit-section-cycle-global)
           (define-key map (kbd   "^") 'magit-section-up)
           (define-key map (kbd   "p") 'magit-section-backward)
           (define-key map (kbd   "n") 'magit-section-forward)
           (define-key map (kbd "M-p") 'magit-section-backward-sibling)
           (define-key map (kbd "M-n") 'magit-section-forward-sibling)
           (define-key map (kbd   "P") 'magit-push-popup)
           (define-key map (kbd   "k") 'magit-delete-thing)
           (define-key map (kbd   "K") 'magit-file-untrack)
           (define-key map (kbd   "i") 'magit-gitignore-popup)
           (define-key map (kbd   "I") 'magit-gitignore-popup)))
    (define-key map (kbd "SPC") 'magit-diff-show-or-scroll-up)
    (define-key map (kbd "DEL") 'magit-diff-show-or-scroll-down)
    (define-key map "+"         'magit-diff-more-context)
    (define-key map "-"         'magit-diff-less-context)
    (define-key map "0"         'magit-diff-default-context)
    (define-key map "1"         'magit-section-show-level-1)
    (define-key map "2"         'magit-section-show-level-2)
    (define-key map "3"         'magit-section-show-level-3)
    (define-key map "4"         'magit-section-show-level-4)
    (define-key map (kbd "M-1") 'magit-section-show-level-1-all)
    (define-key map (kbd "M-2") 'magit-section-show-level-2-all)
    (define-key map (kbd "M-3") 'magit-section-show-level-3-all)
    (define-key map (kbd "M-4") 'magit-section-show-level-4-all)
    (define-key map "$" 'magit-process-buffer)
    (define-key map "%" 'magit-worktree-popup)
    (define-key map "a" 'magit-cherry-apply)
    (define-key map "A" 'magit-cherry-pick-popup)
    (define-key map "b" 'magit-branch-popup)
    (define-key map "B" 'magit-bisect-popup)
    (define-key map "c" 'magit-commit-popup)
    (define-key map "d" 'magit-diff-popup)
    (define-key map "D" 'magit-diff-refresh-popup)
    (define-key map "e" 'magit-ediff-dwim)
    (define-key map "E" 'magit-ediff-popup)
    (define-key map "f" 'magit-fetch-popup)
    (define-key map "F" 'magit-pull-popup)
    (define-key map "g" 'magit-refresh)
    (define-key map "G" 'magit-refresh-all)
    (define-key map "h" 'magit-dispatch-popup)
    (define-key map "?" 'magit-dispatch-popup)
    (define-key map "l" 'magit-log-popup)
    (define-key map "L" 'magit-log-refresh-popup)
    (define-key map "m" 'magit-merge-popup)
    (define-key map "M" 'magit-remote-popup)
    (define-key map "o" 'magit-submodule-popup)
    (define-key map "O" 'magit-subtree-popup)
    (define-key map "q" 'magit-mode-bury-buffer)
    (define-key map "r" 'magit-rebase-popup)
    (define-key map "R" 'magit-file-rename)
    (define-key map "t" 'magit-tag-popup)
    (define-key map "T" 'magit-notes-popup)
    (define-key map "s" 'magit-stage-file)
    (define-key map "S" 'magit-stage-modified)
    (define-key map "u" 'magit-unstage-file)
    (define-key map "U" 'magit-unstage-all)
    (define-key map "v" 'magit-revert-no-commit)
    (define-key map "V" 'magit-revert-popup)
    (define-key map "w" 'magit-am-popup)
    (define-key map "W" 'magit-patch-popup)
    (define-key map "x" 'magit-reset-quickly)
    (define-key map "X" 'magit-reset-popup)
    (define-key map "y" 'magit-show-refs-popup)
    (define-key map "Y" 'magit-cherry)
    (define-key map "z" 'magit-stash-popup)
    (define-key map "Z" 'magit-stash-popup)
    (define-key map ":" 'magit-git-command)
    (define-key map "!" 'magit-run-popup)
    (define-key map (kbd "C-c C-c") 'magit-dispatch-popup)
    (define-key map (kbd "C-c C-e") 'magit-edit-thing)
    (define-key map (kbd "C-c C-o") 'magit-browse-thing)
    (define-key map (kbd "C-c C-w") 'magit-browse-thing)
    (define-key map (kbd "C-x a")   'magit-add-change-log-entry)
    (define-key map (kbd "C-x 4 a") 'magit-add-change-log-entry-other-window)
    (define-key map (kbd "C-w")     'magit-copy-section-value)
    (define-key map (kbd "M-w")     'magit-copy-buffer-revision)
    (define-key map [remap previous-line]      'magit-previous-line)
    (define-key map [remap next-line]          'magit-next-line)
    (define-key map [remap evil-previous-line] 'evil-previous-visual-line)
    (define-key map [remap evil-next-line]     'evil-next-visual-line)
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
  (if (eq magit-current-popup 'magit-dispatch-popup)
      (progn (setq magit-current-popup nil)
             (call-interactively (key-binding (this-command-keys))))
    (user-error "There is no thing at point that could be visited")))

(defun magit-edit-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which lets you edit the thing at point, likely in another buffer."
  (interactive)
  (if (eq magit-current-popup 'magit-dispatch-popup)
      (progn (setq magit-current-popup nil)
             (call-interactively (key-binding (this-command-keys))))
    (user-error "There is no thing at point that could be edited")))

(defun magit-browse-thing ()
  "This is a placeholder command.
Where applicable, section-specific keymaps bind another command
which visits the thing at point using `browse-url'."
  (interactive)
  (user-error "There is no thing at point that could be browsed"))

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
    ["Tag" magit-tag-create t]
    "---"
    ["Diff working tree" magit-diff-working-tree t]
    ["Diff" magit-diff t]
    ("Log"
     ["Log" magit-log-other t]
     ["Reflog" magit-reflog-other t]
     ["Extended..." magit-log-popup t])
    "---"
    ["Cherry pick" magit-cherry-pick t]
    ["Revert commit" magit-revert-popup t]
    "---"
    ["Ignore globally" magit-gitignore-globally t]
    ["Ignore locally" magit-gitignore-locally t]
    ["Discard" magit-discard t]
    ["Reset head and index" magit-reset-mixed t]
    ["Stash" magit-stash-both t]
    ["Snapshot" magit-snapshot-both t]
    "---"
    ["Branch..." magit-checkout t]
    ["Merge" magit-merge t]
    ["Ediff resolve" magit-ediff-resolve t]
    ["Rebase..." magit-rebase-popup t]
    "---"
    ["Push" magit-push t]
    ["Pull" magit-pull-branch t]
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
  (setq list-buffers-directory (abbreviate-file-name default-directory))
  (hack-dir-local-variables-non-file-buffer)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (add-hook 'post-command-hook #'magit-section-update-highlight t t)
  (add-hook 'deactivate-mark-hook #'magit-section-update-highlight t t)
  (setq-local redisplay-highlight-region-function 'magit-highlight-region)
  (setq-local redisplay-unhighlight-region-function 'magit-unhighlight-region)
  (setq mode-line-process (magit-repository-local-get 'mode-line-process))
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))
  (add-hook 'kill-buffer-hook 'magit-preserve-section-visibility-cache))

(defvar-local magit-region-overlays nil)

(defun magit-delete-region-overlays ()
  (mapc #'delete-overlay magit-region-overlays)
  (setq magit-region-overlays nil))

(defun magit-highlight-region (start end window rol)
  (magit-delete-region-overlays)
  (if (and (run-hook-with-args-until-success 'magit-region-highlight-hook
                                             (magit-current-section))
           (not magit-keep-region-overlay)
           (not (= (line-number-at-pos start)
                   (line-number-at-pos end)))
           ;; (not (eq (car-safe last-command-event) 'mouse-movement))
           )
      (funcall (default-value 'redisplay-unhighlight-region-function) rol)
    (funcall (default-value 'redisplay-highlight-region-function)
             start end window rol)))

(defun magit-unhighlight-region (rol)
  (setq magit-section-highlighted-section nil)
  (magit-delete-region-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

(defvar-local magit-refresh-args nil
  "The arguments used to refresh the current buffer.")
(put 'magit-refresh-args 'permanent-local t)

(defvar-local magit-previous-section nil)
(put 'magit-previous-section 'permanent-local t)

(defun magit-mode-setup (mode &rest args)
  "Setup up a MODE buffer using ARGS to generate its content."
  (magit-mode-setup-internal mode args))

(defun magit-mode-setup-internal (mode args &optional locked)
  "Setup up a MODE buffer using ARGS to generate its content.
When optional LOCKED is non-nil, then create a buffer that is
locked to its value, which is derived from MODE and ARGS."
  (let* ((value   (and locked (magit-buffer-lock-value mode args)))
         (buffer  (magit-mode-get-buffer mode nil nil value))
         (section (and buffer (magit-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (magit-with-toplevel
                     (magit-generate-new-buffer mode value))))
    (with-current-buffer buffer
      (setq magit-previous-section section)
      (setq magit-refresh-args args)
      (funcall mode)
      (when created
        (magit-status-goto-initial-section)
        (run-hooks 'magit-create-buffer-hook)))
    (magit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'magit-mode-setup-hook)
      (magit-refresh-buffer))))

(defvar magit-display-buffer-noselect nil
  "If non-nil, then `magit-display-buffer' doesn't call `select-window'.")

(defun magit-display-buffer (buffer &optional display-function)
  "Display BUFFER in some window and maybe select it.

If optional DISPLAY-FUNCTION is non-nil, then use that to display
the buffer.  Otherwise use `magit-display-buffer-function', which
is the normal case.

Then, unless `magit-display-buffer-noselect' is non-nil, select
the window which was used to display the buffer.

Also run the hooks `magit-pre-display-buffer-hook'
and `magit-post-display-buffer-hook'."
  (with-current-buffer buffer
    (run-hooks 'magit-pre-display-buffer-hook))
  (let ((window (funcall (or display-function magit-display-buffer-function)
                         buffer)))
    (unless magit-display-buffer-noselect
      (let* ((old-frame (selected-frame))
             (new-frame (window-frame window)))
        (select-window window)
        (unless (eq old-frame new-frame)
          (select-frame-set-input-focus new-frame)))))
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

(defun magit-display-buffer-same-window-except-diff-v1 (buffer)
  "Display BUFFER in the selected window except for some modes.
If a buffer's `major-mode' derives from `magit-diff-mode' or
`magit-process-mode', display it in another window.  Display all
other buffers in the selected window."
  (display-buffer
   buffer (if (with-current-buffer buffer
                (derived-mode-p 'magit-diff-mode 'magit-process-mode))
              nil  ; display in another window
            '(display-buffer-same-window))))

(defun magit--display-buffer-fullframe (buffer alist)
  (when-let ((window (or (display-buffer-reuse-window buffer alist)
                         (display-buffer-same-window buffer alist)
                         (display-buffer-pop-up-window buffer alist)
                         (display-buffer-use-some-window buffer alist))))
    (delete-other-windows window)
    window))

(defun magit-display-buffer-fullframe-status-v1 (buffer)
  "Display BUFFER, filling entire frame if BUFFER is a status buffer.
Otherwise, behave like `magit-display-buffer-traditional'."
  (if (eq (with-current-buffer buffer major-mode)
          'magit-status-mode)
      (display-buffer buffer '(magit--display-buffer-fullframe))
    (magit-display-buffer-traditional buffer)))

(defun magit--display-buffer-topleft (buffer alist)
  (or (display-buffer-reuse-window buffer alist)
      (when-let ((window2 (display-buffer-pop-up-window buffer alist)))
        (let ((window1 (get-buffer-window))
              (buffer1 (current-buffer))
              (buffer2 (window-buffer window2))
              (w2-quit-restore (window-parameter window2 'quit-restore)))
          (set-window-buffer window1 buffer2)
          (set-window-buffer window2 buffer1)
          (select-window window2)
          ;; Swap some window state that `magit-mode-quit-window' and
          ;; `quit-restore-window' inspect.
          (set-window-prev-buffers window2 (cdr (window-prev-buffers window1)))
          (set-window-prev-buffers window1 nil)
          (set-window-parameter window2 'magit-dedicated
                                (window-parameter window1 'magit-dedicated))
          (set-window-parameter window1 'magit-dedicated t)
          (set-window-parameter window1 'quit-restore
                                (list 'window 'window
                                      (nth 2 w2-quit-restore)
                                      (nth 3 w2-quit-restore)))
          (set-window-parameter window2 'quit-restore nil)
          window1))))

(defun magit-display-buffer-fullframe-status-topleft-v1 (buffer)
  "Display BUFFER, filling entire frame if BUFFER is a status buffer.
When BUFFER derives from `magit-diff-mode' or
`magit-process-mode', try to display BUFFER to the top or left of
the current buffer rather than to the bottom or right, as
`magit-display-buffer-fullframe-status-v1' would.  Whether the
split is made vertically or horizontally is determined by
`split-window-preferred-function'."
  (display-buffer
   buffer
   (cond ((eq (with-current-buffer buffer major-mode)
              'magit-status-mode)
          '(magit--display-buffer-fullframe))
         ((with-current-buffer buffer
            (derived-mode-p 'magit-diff-mode 'magit-process-mode))
          '(magit--display-buffer-topleft))
         (t
          '(display-buffer-same-window)))))

(defun magit--display-buffer-fullcolumn (buffer alist)
  (when-let ((window (or (display-buffer-reuse-window buffer alist)
                         (display-buffer-same-window buffer alist)
                         (display-buffer-below-selected buffer alist))))
    (delete-other-windows-vertically window)
    window))

(defun magit-display-buffer-fullcolumn-most-v1 (buffer)
  "Display BUFFER using the full column except in some cases.
For most cases where BUFFER's `major-mode' derives from
`magit-mode', display it in the selected window and grow that
window to the full height of the frame, deleting other windows in
that column as necessary.  However, display BUFFER in another
window if 1) BUFFER's mode derives from `magit-process-mode', or
2) BUFFER's mode derives from `magit-diff-mode', provided that
the mode of the current buffer derives from `magit-log-mode' or
`magit-cherry-mode'."
  (display-buffer
   buffer
   (cond ((and (or git-commit-mode
                   (derived-mode-p 'magit-log-mode 'magit-cherry-mode))
               (with-current-buffer buffer
                 (derived-mode-p 'magit-diff-mode)))
          nil)
         ((with-current-buffer buffer
            (derived-mode-p 'magit-process-mode))
          nil)
         (t
          '(magit--display-buffer-fullcolumn)))))

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

(defun magit-mode-get-buffer (mode &optional create frame value)
  (when create
    (error "`magit-mode-get-buffer's CREATE argument is obsolete"))
  (if-let ((topdir (magit-toplevel)))
      (--first (with-current-buffer it
                 (and (eq major-mode mode)
                      (equal magit--default-directory topdir)
                      (if value
                          (and magit-buffer-locked-p
                               (equal (magit-buffer-lock-value) value))
                        (not magit-buffer-locked-p))))
               (if frame
                   (mapcar #'window-buffer
                           (window-list (unless (eq frame t) frame)))
                 (buffer-list)))
    (magit--not-inside-repository-error)))

(defun magit-generate-new-buffer (mode &optional value)
  (let* ((name (funcall magit-generate-buffer-name-function mode value))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq magit--default-directory default-directory)
      (setq magit-buffer-locked-p (and value t))
      (magit-restore-section-visibility-cache mode))
    (when magit-uniquify-buffer-names
      (add-to-list 'uniquify-list-buffers-directory-modes mode)
      (with-current-buffer buffer
        (setq list-buffers-directory (abbreviate-file-name default-directory)))
      (let ((uniquify-buffer-name-style
             (if (memq uniquify-buffer-name-style '(nil forward))
                 'post-forward-angle-brackets
               uniquify-buffer-name-style)))
        (uniquify-rationalize-file-buffer-names
         name (file-name-directory (directory-file-name default-directory))
         buffer)))
    buffer))

(defun magit-generate-buffer-name-default-function (mode &optional value)
  "Generate buffer name for a MODE buffer in the current repository.
The returned name is based on `magit-buffer-name-format' and
takes `magit-uniquify-buffer-names' and VALUE, if non-nil, into
account."
  (let ((m (substring (symbol-name mode) 0 -5))
        (v (and value (format "%s" (if (listp value) value (list value)))))
        (n (if magit-uniquify-buffer-names
               (file-name-nondirectory
                (directory-file-name default-directory))
             (abbreviate-file-name default-directory))))
    (format-spec
     magit-buffer-name-format
     `((?m . ,m)
       (?M . ,(if (eq mode 'magit-status-mode) "magit" m))
       (?v . ,(or v ""))
       (?V . ,(if v (concat " " v) ""))
       (?t . ,n)
       (?x . ,(if magit-uniquify-buffer-names "" "*"))
       (?T . ,(if magit-uniquify-buffer-names n (concat n "*")))))))

(defun magit-toggle-buffer-lock ()
  "Lock the current buffer to its value or unlock it.

Locking a buffer to its value prevents it from being reused to
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
      (if-let ((unlocked (magit-mode-get-buffer major-mode)))
          (let ((locked (current-buffer)))
            (switch-to-buffer unlocked nil t)
            (kill-buffer locked))
        (setq magit-buffer-locked-p nil)
        (rename-buffer (funcall magit-generate-buffer-name-function
                                major-mode)))
    (if-let ((value (magit-buffer-lock-value)))
        (if-let ((locked (magit-mode-get-buffer major-mode nil nil value)))
            (let ((unlocked (current-buffer)))
              (switch-to-buffer locked nil t)
              (kill-buffer unlocked))
          (setq magit-buffer-locked-p t)
          (rename-buffer (funcall magit-generate-buffer-name-function
                                  major-mode value)))
      (user-error "Buffer has no value it could be locked to"))))

(defvar magit-buffer-lock-functions '((forge-topic-mode . car))
  "Provide buffer-locking support for third-party modes.
An alist of symbols to functions.

The symbol must be the major-mode the locked buffer will have.

The function must take a single argument, a list of refresh
arguments (the value of `magit-refresh-args') and return a
value that identifies the buffer (i.e., its 'lock value').
If the third-party mode is invoked as

    (magit-mode-setup-internal #\\='my-mode \\='(1 2 3) t)

the function will be invoked as

    (funcall lock-func \\='(1 2 3))

if the cons (my-mode . lock-func) is in this list.

This variable is intended for third-party extensions;
`magit-buffer-lock-value' implements all built-in behavior.

See also `magit-toggle-buffer-lock'.")

(cl-defun magit-buffer-lock-value
    (&optional (mode major-mode)
               (args magit-refresh-args))
  "Find an appropriate buffer lock value for MODE under ARGS.
See also `magit-buffer-lock-functions'."
  (cl-case mode
    (magit-cherry-mode
     (pcase-let ((`(,upstream ,head) args))
       (concat head ".." upstream)))
    (magit-diff-mode
     (pcase-let ((`(,rev-or-range ,const ,_args ,files) args))
       (nconc (cons (or rev-or-range
                        (if (member "--cached" const)
                            (progn (setq const (delete "--cached" const))
                                   'staged)
                          'unstaged))
                    const)
              (and files (cons "--" files)))))
    (magit-log-mode
     (pcase-let ((`(,revs ,_args ,files) args))
       (if (and revs files)
           (append revs (cons "--" files))
         (append revs files))))
    (magit-refs-mode
     (pcase-let ((`(,ref ,args) args))
       (cons (or ref "HEAD") args)))
    (magit-revision-mode
     (pcase-let ((`(,rev ,_const ,_args ,files) args))
       (if files (cons rev files) (list rev))))
    ((magit-reflog-mode   ; (ref ~args)
      magit-stash-mode    ; (stash _const _args _files)
      magit-stashes-mode) ; (ref)
     (car args))
    (t
     (--when-let (cdr (assq mode magit-buffer-lock-functions))
       (funcall it args)))))

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

;;; Refresh Magit Buffers

(defvar inhibit-magit-refresh nil)

(defun magit-refresh ()
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`magit-mode', and refresh the corresponding status buffer.

Run hooks `magit-pre-refresh-hook' and `magit-post-refresh-hook'."
  (interactive)
  (unless inhibit-magit-refresh
    (unwind-protect
        (let ((start (current-time))
              (magit--refresh-cache (or magit--refresh-cache
                                        (list (cons 0 0)))))
          (when magit-refresh-verbose
            (message "Refreshing magit..."))
          (magit-run-hook-with-benchmark 'magit-pre-refresh-hook)
          (when (derived-mode-p 'magit-mode)
            (magit-refresh-buffer))
          (--when-let (and magit-refresh-status-buffer
                           (not (derived-mode-p 'magit-status-mode))
                           (magit-mode-get-buffer 'magit-status-mode))
            (with-current-buffer it
              (magit-refresh-buffer)))
          (magit-auto-revert-buffers)
          (cond
           ((and (not this-command)
                 (memq last-command magit-post-commit-hook-commands))
            (magit-run-hook-with-benchmark 'magit-post-commit-hook))
           ((memq this-command magit-post-stage-hook-commands)
            (magit-run-hook-with-benchmark 'magit-post-stage-hook))
           ((memq this-command magit-post-unstage-hook-commands)
            (magit-run-hook-with-benchmark 'magit-post-unstage-hook)))
          (magit-run-hook-with-benchmark 'magit-post-refresh-hook)
          (when magit-refresh-verbose
            (message "Refreshing magit...done (%.3fs, cached %s/%s)"
                     (float-time (time-subtract (current-time) start))
                     (caar magit--refresh-cache)
                     (+ (caar magit--refresh-cache)
                        (cdar magit--refresh-cache)))))
      (run-hooks 'magit-unwind-refresh-hook))))

(defun magit-refresh-all ()
  "Refresh all buffers belonging to the current repository.

Refresh all Magit buffers belonging to the current repository,
and revert buffers that visit files located inside the current
repository.

Run hooks `magit-pre-refresh-hook' and `magit-post-refresh-hook'."
  (interactive)
  (magit-run-hook-with-benchmark 'magit-pre-refresh-hook)
  (dolist (buffer (magit-mode-get-buffers))
    (with-current-buffer buffer (magit-refresh-buffer)))
  (magit-auto-revert-buffers)
  (magit-run-hook-with-benchmark 'magit-post-refresh-hook))

(defvar-local magit-refresh-start-time nil)

(defun magit-refresh-buffer ()
  "Refresh the current Magit buffer."
  (setq magit-refresh-start-time (current-time))
  (let ((refresh (intern (format "%s-refresh-buffer"
                                 (substring (symbol-name major-mode) 0 -5))))
        (magit--refresh-cache (or magit--refresh-cache (list (cons 0 0)))))
    (when (functionp refresh)
      (when magit-refresh-verbose
        (message "Refreshing buffer `%s'..." (buffer-name)))
      (let* ((buffer (current-buffer))
             (windows
              (--mapcat (with-selected-window it
                          (with-current-buffer buffer
                            (when-let ((section (magit-current-section)))
                              (list
                               (nconc (list it section)
                                      (magit-refresh-get-relative-position))))))
                        (or (get-buffer-window-list buffer nil t)
                            (list (selected-window))))))
        (deactivate-mark)
        (setq magit-section-highlight-overlays nil)
        (setq magit-section-highlighted-section nil)
        (setq magit-section-highlighted-sections nil)
        (setq magit-section-unhighlight-sections nil)
        (magit-process-unset-mode-line-error-status)
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
  (when-let ((section (magit-current-section)))
    (let ((start (oref section start)))
      (list (count-lines start (point))
            (- (point) (line-beginning-position))
            (and (magit-hunk-section-p section)
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

;;; Save File-Visiting Buffers

(defvar disable-magit-save-buffers nil)

(defun magit-pre-command-hook ()
  (setq disable-magit-save-buffers nil))
(add-hook 'pre-command-hook #'magit-pre-command-hook)

(defvar magit-after-save-refresh-buffers nil)

(defun magit-after-save-refresh-buffers ()
  (dolist (buffer magit-after-save-refresh-buffers)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (magit-refresh-buffer))))
  (setq magit-after-save-refresh-buffers nil)
  (remove-hook 'post-command-hook 'magit-after-save-refresh-buffers))

(defun magit-after-save-refresh-status ()
  "Refresh the status buffer of the current repository.

This function is intended to be added to `after-save-hook'.

If the status buffer does not exist or the file being visited in
the current buffer isn't inside the working tree of a repository,
then do nothing.

Note that refreshing a Magit buffer is done by re-creating its
contents from scratch, which can be slow in large repositories.
If you are not satisfied with Magit's performance, then you
should obviously not add this function to that hook."
  (when (and (not disable-magit-save-buffers)
             (magit-inside-worktree-p t))
    (--when-let (ignore-errors (magit-mode-get-buffer 'magit-status-mode))
      (add-to-list 'magit-after-save-refresh-buffers it)
      (add-hook 'post-command-hook 'magit-after-save-refresh-buffers))))

(defun magit-maybe-save-repository-buffers ()
  "Maybe save file-visiting buffers belonging to the current repository.
Do so if `magit-save-repository-buffers' is non-nil.  You should
not remove this from any hooks, instead set that variable to nil
if you so desire."
  (when (and magit-save-repository-buffers
             (not disable-magit-save-buffers))
    (setq disable-magit-save-buffers t)
    (let ((msg (current-message)))
      (magit-save-repository-buffers
       (eq magit-save-repository-buffers 'dontask))
      (when (and msg
                 (current-message)
                 (not (equal msg (current-message))))
        (message "%s" msg)))))

(add-hook 'magit-pre-refresh-hook #'magit-maybe-save-repository-buffers)
(add-hook 'magit-pre-call-git-hook #'magit-maybe-save-repository-buffers)
(add-hook 'magit-pre-start-git-hook #'magit-maybe-save-repository-buffers)

(defvar-local magit-inhibit-refresh-save nil)

(defun magit-save-repository-buffers (&optional arg)
  "Save file-visiting buffers belonging to the current repository.
After any buffer where `buffer-save-without-query' is non-nil
is saved without asking, the user is asked about each modified
buffer which visits a file in the current repository.  Optional
argument (the prefix) non-nil means save all with no questions."
  (interactive "P")
  (when-let ((topdir (magit-rev-parse-safe "--show-toplevel")))
    (let ((remote (file-remote-p topdir))
          (save-some-buffers-action-alist
           `((?Y (lambda (buffer)
                   (with-current-buffer buffer
                     (setq buffer-save-without-query t)
                     (save-buffer)))
                 "to save the current buffer and remember choice")
             (?N (lambda (buffer)
                   (with-current-buffer buffer
                     (setq magit-inhibit-refresh-save t)))
                 "to skip the current buffer and remember choice")
             ,@save-some-buffers-action-alist)))
      (save-some-buffers
       arg (lambda ()
             (and (not magit-inhibit-refresh-save)
                  buffer-file-name
                  (file-exists-p (file-name-directory buffer-file-name))
                  ;; Avoid needlessly connecting to unrelated remotes.
                  (equal (file-remote-p buffer-file-name)
                         remote)
                  (string-prefix-p topdir (file-truename buffer-file-name))
                  (equal (magit-rev-parse-safe "--show-toplevel")
                         topdir)))))))

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

;;; Repository-Local Cache

(defvar magit-repository-local-cache nil
  "Alist mapping `magit-toplevel' paths to alists of key/value pairs.")

(defun magit-repository-local-repository ()
  "Return the key for the current repository."
  (or (bound-and-true-p magit--default-directory)
      (magit-toplevel)))

(defun magit-repository-local-set (key value &optional repository)
  "Set the repository-local VALUE for KEY.

Unless specified, REPOSITORY is the current buffer's repository.

If REPOSITORY is nil (meaning there is no current repository),
then the value is not cached, and we return nil."
  (let* ((repokey (or repository (magit-repository-local-repository)))
         (cache (assoc repokey magit-repository-local-cache)))
    ;; Don't cache values for a nil REPOSITORY, as the 'set' and 'get'
    ;; calls for some KEY may happen in unrelated contexts.
    (when repokey
      (if cache
          (let ((keyvalue (assoc key (cdr cache))))
            (if keyvalue
                ;; Update pre-existing value for key.
                (setcdr keyvalue value)
              ;; No such key in repository-local cache.
              (push (cons key value) (cdr cache))))
        ;; No cache for this repository.
        (push (cons repokey (list (cons key value)))
              magit-repository-local-cache)))))

(defun magit-repository-local-exists-p (key &optional repository)
  "Non-nil when a repository-local value exists for KEY.

Returns a (KEY . value) cons cell.

The KEY is matched using `equal'.

Unless specified, REPOSITORY is the current buffer's repository."
  (let* ((repokey (or repository (magit-repository-local-repository)))
         (cache (assoc repokey magit-repository-local-cache)))
    (and cache
         (assoc key (cdr cache)))))

(defun magit-repository-local-get (key &optional default repository)
  "Return the repository-local value for KEY.

Return DEFAULT if no value for KEY exists.

The KEY is matched using `equal'.

Unless specified, REPOSITORY is the current buffer's repository."
  (let ((keyvalue (magit-repository-local-exists-p key repository)))
    (if keyvalue
        (cdr keyvalue)
      default)))

(defun magit-repository-local-delete (key &optional repository)
  "Delete the repository-local value for KEY.

Unless specified, REPOSITORY is the current buffer's repository."
  (let* ((repokey (or repository (magit-repository-local-repository)))
         (cache (assoc repokey magit-repository-local-cache)))
    (when cache
      ;; There is no `assoc-delete-all'.
      (setf (cdr cache)
            (cl-delete key (cdr cache) :key #'car :test #'equal)))))

(defun magit-zap-caches ()
  "Zap caches for the current repository.
Remove the repository's entry from `magit-repository-local-cache'
and set `magit-section-visibility-cache' to nil in all of the
repository's Magit buffers."
  (interactive)
  (magit-with-toplevel
    (setq magit-repository-local-cache
          (cl-delete default-directory
                     magit-repository-local-cache
                     :key #'car :test #'equal)))
  (dolist (buffer (magit-mode-get-buffers))
    (with-current-buffer buffer
      (setq magit-section-visibility-cache nil))))

;;; Utilities

(defun magit-run-hook-with-benchmark (hook)
  (when hook
    (if magit-refresh-verbose
        (let ((start (current-time)))
          (message "Running %s..." hook)
          (run-hooks hook)
          (message "Running %s...done (%.3fs)" hook
                   (float-time (time-subtract (current-time) start))))
      (run-hooks hook))))

;;; _
(provide 'magit-mode)
;;; magit-mode.el ends here
