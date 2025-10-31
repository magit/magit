;;; magit-mode.el --- Create and refresh Magit buffers  -*- lexical-binding:t -*-

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

;; This library implements the abstract major-mode `magit-mode' from
;; which almost all other Magit major-modes derive.  The code in here
;; is mostly concerned with creating and refreshing Magit buffers.

;;; Code:

(require 'magit-base)
(require 'magit-git)

(require 'benchmark)
(require 'browse-url)
(require 'format-spec)
(require 'help-mode)

(require 'transient)

(defvar bookmark-make-record-function)

(eval-when-compile (require 'elp))
(declare-function elp-reset-all "elp" ())
(declare-function elp-instrument-package "elp" (prefix))
(declare-function elp-results "elp" ())
(declare-function elp-restore-all "elp" ())

(defvar magit--wip-inhibit-autosave)
(defvar magit-wip-after-save-local-mode)
(declare-function magit-wip-get-ref "magit-wip" ())
(declare-function magit-wip-commit-worktree "magit-wip" (ref files msg))

;;; Options

(defcustom magit-mode-hook nil
  "Hook run when entering a mode derived from Magit mode."
  :package-version '(magit . "4.4.2")
  :group 'magit-modes
  :type 'hook
  :options (list #'bug-reference-mode))

(defcustom magit-setup-buffer-hook
  (list #'magit-maybe-save-repository-buffers
        'magit-set-buffer-margins) ; from magit-margin.el
  "Hook run by `magit-setup-buffer'.

This is run right after displaying the buffer and right before
generating or updating its content.  `magit-mode-hook' and other,
more specific, `magit-mode-*-hook's on the other hand are run
right before displaying the buffer.  Usually one of these hooks
should be used instead of this one."
  :package-version '(magit . "2.3.0")
  :group 'magit-modes
  :type 'hook
  :options (list #'magit-maybe-save-repository-buffers
                 'magit-set-buffer-margins))

(defcustom magit-pre-refresh-hook
  (list #'magit-maybe-save-repository-buffers)
  "Hook run before refreshing in `magit-refresh'.

This hook, or `magit-post-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`magit-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(magit . "2.4.0")
  :group 'magit-refresh
  :type 'hook
  :options (list #'magit-maybe-save-repository-buffers))

(defcustom magit-post-refresh-hook
  ;; Do not function-quote to avoid circular dependencies.
  '(magit-auto-revert-buffers
    magit-run-post-stage-hook
    magit-run-post-unstage-hook)
  "Hook run after refreshing in `magit-refresh'.

This hook, or `magit-pre-refresh-hook', should be used
for functions that are not tied to a particular buffer.

To run a function with a particular buffer current, use
`magit-refresh-buffer-hook' and use `derived-mode-p'
inside your function."
  :package-version '(magit . "2.4.0")
  :group 'magit-refresh
  :type 'hook
  :options '(magit-auto-revert-buffers
             magit-run-post-stage-hook
             magit-run-post-unstage-hook))

(defcustom magit-display-buffer-function #'magit-display-buffer-traditional
  "The function used to display a Magit buffer.

All Magit buffers (buffers whose major-modes derive from
`magit-mode') are displayed using `magit-display-buffer',
which in turn uses the function specified here."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type `(radio (function-item ,#'magit-display-buffer-traditional)
                (function-item ,#'magit-display-buffer-same-window-except-diff-v1)
                (function-item ,#'magit-display-buffer-fullframe-status-v1)
                (function-item ,#'magit-display-buffer-fullframe-status-topleft-v1)
                (function-item ,#'magit-display-buffer-fullcolumn-most-v1)
                (function-item ,#'display-buffer)
                (function :tag "Function")))

(defcustom magit-pre-display-buffer-hook
  (list #'magit-save-window-configuration)
  "Hook run by `magit-display-buffer' before displaying the buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type 'hook
  :get #'magit-hook-custom-get
  :options (list #'magit-save-window-configuration))

(defcustom magit-post-display-buffer-hook (list #'magit-maybe-set-dedicated)
  "Hook run by `magit-display-buffer' after displaying the buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type 'hook
  :get #'magit-hook-custom-get
  :options (list #'magit-maybe-set-dedicated))

(defcustom magit-generate-buffer-name-function
  #'magit-generate-buffer-name-default-function
  "The function used to generate the name for a Magit buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-buffers
  :type `(radio (function-item ,#'magit-generate-buffer-name-default-function)
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

The value should always contain \"%m\" or \"%M\", \"%v\" or \"%V\", and
\"%t\".  If `magit-uniquify-buffer-names' is non-nil, then the
value must end with \"%t\" or \"%t%x\".  See issue #2841.

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

(defcustom magit-bury-buffer-function #'magit-mode-quit-window
  "The function used to bury or kill the current Magit buffer."
  :package-version '(magit . "3.2.0")
  :group 'magit-buffers
  :type `(radio (function-item ,#'quit-window)
                (function-item ,#'magit-mode-quit-window)
                (function-item ,#'magit-restore-window-configuration)
                (function :tag "Function")))

(defcustom magit-prefix-use-buffer-arguments 'selected
  "Whether certain prefix commands reuse arguments active in relevant buffer.

This affects the transient prefix commands `magit-diff',
`magit-log' and `magit-show-refs'.

Valid values are:

`always': Always use the set of arguments that is currently
  active in the respective buffer, provided that buffer exists
  of course.
`selected': Use the set of arguments from the respective
  buffer, but only if it is displayed in a window of the current
  frame.  This is the default.
`current': Use the set of arguments from the respective buffer,
  but only if it is the current buffer.
`never': Never use the set of arguments from the respective
  buffer.

For more information see info node `(magit)Transient Arguments
and Buffer Variables'."
  :package-version '(magit . "3.0.0")
  :group 'magit-buffers
  :group 'magit-commands
  :group 'magit-diff
  :group 'magit-log
  :type '(choice
          (const :tag "Always use args from buffer" always)
          (const :tag "Use args from buffer if displayed in frame" selected)
          (const :tag "Use args from buffer if it is current" current)
          (const :tag "Never use args from buffer" never)))

(defcustom magit-direct-use-buffer-arguments 'selected
  "Whether certain commands reuse arguments active in relevant buffer.

This affects certain commands such as `magit-show-commit' that
are suffixes of the diff or log transient prefix commands, but
only if they are invoked directly, i.e., *not* as a suffix.

Valid values are:

`always': Always use the set of arguments that is currently
  active in the respective buffer, provided that buffer exists
  of course.
`selected': Use the set of arguments from the respective
  buffer, but only if it is displayed in a window of the current
  frame.  This is the default.
`current': Use the set of arguments from the respective buffer,
  but only if it is the current buffer.
`never': Never use the set of arguments from the respective
  buffer.

For more information see info node `(magit)Transient Arguments
and Buffer Variables'."
  :package-version '(magit . "3.0.0")
  :group 'magit-buffers
  :group 'magit-commands
  :group 'magit-diff
  :group 'magit-log
  :type '(choice
          (const :tag "Always use args from buffer" always)
          (const :tag "Use args from buffer if displayed in frame" selected)
          (const :tag "Use args from buffer if it is current" current)
          (const :tag "Never use args from buffer" never)))

(defcustom magit-region-highlight-hook
  '(magit-diff-update-hunk-region) ; from magit-diff.el
  "Functions used to highlight the region.

Each function is run with the current section as only argument
until one of them returns non-nil.  If all functions return nil,
then fall back to regular region highlighting."
  :package-version '(magit . "2.1.0")
  :group 'magit-refresh
  :type 'hook
  :options '(magit-diff-update-hunk-region))

(defcustom magit-create-buffer-hook nil
  "Normal hook run while creating a new `magit-mode' buffer.
Runs before the buffer is populated with sections.  Also see
`magit-post-create-buffer-hook'."
  :package-version '(magit . "2.90.0")
  :group 'magit-refresh
  :type 'hook)

(defcustom magit-post-create-buffer-hook nil
  "Normal hook run after creating a new `magit-mode' buffer.
Runs after the buffer is populated with sections for the first
time.  Also see `magit-create-buffer-hook' (which runs earlier)
and `magit-refresh-buffer-hook' (which runs on every refresh)."
  :package-version '(magit . "4.0.0")
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

;;; Key Bindings

(defvar-keymap magit-mode-map
  :doc "Parent keymap for all keymaps of modes derived from `magit-mode'."
  :parent magit-section-mode-map
  ;; Don't function-quote but make sure all commands are autoloaded.
  "C-<return>"  'magit-visit-thing
  "RET"         'magit-visit-thing
  "M-TAB"       'magit-dired-jump
  "M-<tab>"     'magit-section-cycle-diffs
  "SPC"         'magit-diff-show-or-scroll-up
  "S-SPC"       'magit-diff-show-or-scroll-down
  "DEL"         'magit-diff-show-or-scroll-down
  "+"           'magit-diff-more-context
  "-"           'magit-diff-less-context
  "0"           'magit-diff-default-context
  "a" 'magit-cherry-apply
  "A" 'magit-cherry-pick
  "b" 'magit-branch
  "B" 'magit-bisect
  "c" 'magit-commit
  "C" 'magit-clone
  "d" 'magit-diff
  "D" 'magit-diff-refresh
  "e" 'magit-ediff-dwim
  "E" 'magit-ediff
  "f" 'magit-fetch
  "F" 'magit-pull
  "g" 'magit-refresh
  "G" 'magit-refresh-all
  "h" 'magit-dispatch
  "?" 'magit-dispatch
  "H" 'magit-describe-section
  "i" 'magit-gitignore
  "I" 'magit-init
  "j" 'magit-status-quick
  "J" 'magit-display-repository-buffer
  "k" 'magit-delete-thing
  "K" 'magit-file-untrack
  "l" 'magit-log
  "L" 'magit-log-refresh
  "m" 'magit-merge
  "M" 'magit-remote
  ;; "n" magit-section-forward in magit-section-mode-map
  ;; "N" forge-dispatch, added by forge package
  "o" 'magit-submodule
  "O" 'magit-subtree
  ;; "p" magit-section-backward in magit-section-mode-map
  "P" 'magit-push
  "q" 'magit-mode-bury-buffer
  "Q" 'magit-git-command
  ":" 'magit-git-command
  "r" 'magit-rebase
  "R" 'magit-file-rename
  "s" 'magit-stage-files
  "S" 'magit-stage-modified
  "t" 'magit-tag
  "T" 'magit-notes
  "u" 'magit-unstage-files
  "U" 'magit-unstage-all
  "v" 'magit-revert-no-commit
  "V" 'magit-revert
  "w" 'magit-am
  "W" 'magit-patch
  "x" 'magit-reset-quickly
  "X" 'magit-reset
  "y" 'magit-show-refs
  "Y" 'magit-cherry
  "z" 'magit-stash
  "Z" 'magit-worktree
  "%" 'magit-worktree
  "$" 'magit-process-buffer
  "!" 'magit-run
  ">" 'magit-sparse-checkout
  "C-c C-c" 'magit-dispatch
  "C-c C-r" 'magit-next-reference
  "C-c C-e" 'magit-edit-thing
  "C-c C-o" 'magit-browse-thing
  "C-c C-w" 'magit-copy-thing
  "C-w"     'magit-copy-section-value
  "M-w"     'magit-copy-buffer-revision
  "<remap> <mouse-set-point>"     'magit-mouse-set-point
  "<remap> <back-to-indentation>" 'magit-back-to-indentation
  "<remap> <previous-line>"       'magit-previous-line
  "<remap> <next-line>"           'magit-next-line
  "<remap> <evil-previous-line>"  'evil-previous-visual-line
  "<remap> <evil-next-line>"      'evil-next-visual-line)

(defun magit-delete-thing ()
  "This is a placeholder command, which signals an error if called.
Where applicable, other keymaps remap this command to another,
which actually deletes the thing at point."
  (declare (completion ignore))
  (interactive)
  (user-error "There is no thing at point that could be deleted"))

(defun magit-visit-thing ()
  "This is a placeholder command, which may signal an error if called.
Where applicable, other keymaps remap this command to another,
which actually visits the thing at point."
  (declare (completion ignore))
  (interactive)
  (cond-let
    ((eq transient-current-command 'magit-dispatch)
     (call-interactively (key-binding (this-command-keys))))
    ([url (thing-at-point 'url t)]
     (browse-url url))
    ((user-error "There is no thing at point that could be visited"))))

(defun magit-edit-thing ()
  "This is a placeholder command, which may signal an error if called.
Where applicable, other keymaps remap this command to another,
which actually lets you edit the thing at point, likely in another
buffer."
  (declare (completion ignore))
  (interactive)
  (if (eq transient-current-command 'magit-dispatch)
      (call-interactively (key-binding (this-command-keys)))
    (user-error "There is no thing at point that could be edited")))

(defun magit-browse-thing ()
  "This is a placeholder command, which may signal an error if called.
Where applicable, other keymaps remap this command to another,
which actually visits thing at point using `browse-url'."
  (declare (completion ignore))
  (interactive)
  (if-let ((url (thing-at-point 'url t)))
      (browse-url url)
    (user-error "There is no thing at point that could be browsed")))

(defun magit-copy-thing ()
  "This is a placeholder command, which signals an error if called.
Where applicable, other keymaps remap this command to another,
which actually copies some representation of the thing at point
to the kill ring."
  (declare (completion ignore))
  (interactive)
  (user-error "There is no thing at point that we know how to copy"))

;;;###autoload
(defun magit-info ()
  "Visit the Magit manual."
  (interactive)
  (info "magit"))

(defvar bug-reference-map)
(with-eval-after-load 'bug-reference
  (keymap-set bug-reference-map "<remap> <magit-visit-thing>"
              'bug-reference-push-button))

(easy-menu-define magit-mode-menu magit-mode-map
  "Magit menu."
  ;; Similar to `magit-dispatch' but exclude:
  ;; - commands that are available from context menus:
  ;;   apply, reverse, discard, stage, unstage,
  ;;   cherry-pick, revert, reset,
  ;;   describe-section
  ;; - commands that are available from submenus:
  ;;   git-command, ediff-dwim
  ;; - and: refresh-all, status-jump, status-quick.
  '("Magit"
    "---" "Inspect"
    ["     Bisect..."             magit-bisect t]
    ["     Cherries..."           magit-cherry t]
    ["     Diff..."               magit-diff t]
    ["     Ediff..."              magit-ediff t]
    ["     Log..."                magit-log t]
    ["     References..."         magit-show-refs t]
    "---" "Manipulate"
    ["     Commit..."             magit-commit t]
    ["     Stash..."              magit-stash t]
    ["     Tag..."                magit-tag t]
    "---"
    ["     Branch..."             magit-branch t]
    ["     Remote..."             magit-remote t]
    "---"
    ["     Merge..."              magit-merge t]
    ["     Rebase..."             magit-rebase t]
    "---" "Transfer"
    ["     Fetch..."              magit-fetch t]
    ["     Pull..."               magit-pull t]
    ["     Push..."               magit-push t]
    "---" "Setup"
    ["     Clone..."              magit-clone t]
    ["     Ignore..."             magit-gitignore t]
    ["     Init..."               magit-init t]
    "---"
    ("Advanced"
     ["Run..."                    magit-run t]
     "---"
     ["Apply patches..."          magit-am t]
     ["Format patches..."         magit-patch t]
     "---"
     ["Note..."                   magit-notes t]
     "---"
     ["Submodule..."              magit-submodule t]
     ["Subtree..."                magit-subtree t]
     ["Worktree..."               magit-worktree t])
    "---"
    ["Show command dispatcher..." magit-dispatch t]
    ["Show manual"                magit-info t]
    ["Show another buffer"        magit-display-repository-buffer t]
    "---"
    ("Change buffer arguments"
     ["Diff arguments"            magit-diff-refresh t]
     ["Log arguments"             magit-log-refresh t])
    ["Refresh buffer"             magit-refresh t]
    ["Bury buffer"                magit-mode-bury-buffer t]))

;;; Mode

(define-derived-mode magit-mode magit-section-mode "Magit"
  "Parent major mode from which Magit major modes inherit.

Magit is documented in info node `(magit)'."
  :interactive nil
  :group 'magit
  (magit-hack-dir-local-variables)
  (face-remap-add-relative 'header-line 'magit-header-line)
  (setq mode-line-process (magit-repository-local-get 'mode-line-process))
  (setq-local revert-buffer-function #'magit-revert-buffer)
  (setq-local bookmark-make-record-function #'magit--make-bookmark)
  (setq-local imenu-create-index-function #'magit--imenu-create-index)
  (setq-local imenu-default-goto-function #'magit--imenu-goto-function)
  (setq-local isearch-filter-predicate #'magit-section--open-temporarily))

(defun magit-hack-dir-local-variables ()
  "Like `hack-dir-local-variables-non-file-buffer' but ignore some variables."
  (let ((ignored-local-variables
         `(show-trailing-whitespace
           ,@ignored-local-variables)))
    (hack-dir-local-variables-non-file-buffer)))

;;; Local Variables

(defvar-local magit-buffer-arguments nil)
(defvar-local magit-buffer-diff-type nil)
(defvar-local magit-buffer-diff-args nil)
(defvar-local magit-buffer-diff-files nil)
(defvar-local magit-buffer-diff-files-suspended nil)
(defvar-local magit-buffer-file-name nil)
(defvar-local magit-buffer-files nil)
(defvar-local magit-buffer-log-args nil)
(defvar-local magit-buffer-log-files nil)
(defvar-local magit-buffer-range nil)
(defvar-local magit-buffer-range-hashed nil)
(defvar-local magit-buffer-refname nil)
(defvar-local magit-buffer-revision nil)
(defvar-local magit-buffer-revision-hash nil)
(defvar-local magit-buffer-revisions nil)
(defvar-local magit-buffer-typearg nil)
(defvar-local magit-buffer-upstream nil)

;; These variables are also used in file-visiting buffers.
;; Because the user may change the major-mode, they have
;; to be permanent buffer-local.
(put 'magit-buffer-file-name 'permanent-local t)
(put 'magit-buffer-refname 'permanent-local t)
(put 'magit-buffer-revision 'permanent-local t)
(put 'magit-buffer-revision-hash 'permanent-local t)

;; `magit-status' re-enables mode function but its refresher
;; function does not reinstate this.
(put 'magit-buffer-diff-files-suspended 'permanent-local t)

(defun magit-buffer-file-name ()
  "Return `magit-buffer-file-name' or if that is nil `buffer-file-name'.
In an indirect buffer get the value for its base buffer."
  (or magit-buffer-file-name
      (buffer-file-name (buffer-base-buffer))))

(defun magit-buffer-revision ()
  "Return `magit-buffer-revision' or if that is nil \"{worktree}\".
If not visiting a blob or file, or the file isn't being tracked,
return nil."
  (or magit-buffer-revision
      (and buffer-file-name
           (magit-file-tracked-p buffer-file-name)
           "{worktree}")))

(cl-defgeneric magit-buffer-value ()
  "Return the value of the current buffer.
The \"value\" identifies what is being displayed in the buffer.
The buffer's major-mode should derive from `magit-section-mode'."
  nil)

(defvar-local magit-previous-section nil)
(put 'magit-previous-section 'permanent-local t)

;;; Setup Buffer

(defmacro magit-setup-buffer (mode &optional locked &rest args)
  "\n\n(fn MODE &optional LOCKED &key BUFFER DIRECTORY \
INITIAL-SECTION SELECT-SECTION &rest BINDINGS)"
  (declare (indent 2)
           (debug (form [&optional locked]
                        [&rest keywordp form]
                        [&rest (symbolp form)])))
  (let (kwargs)
    (while (keywordp (car args))
      (push (pop args) kwargs)
      (push (pop args) kwargs))
    `(magit-setup-buffer-internal
      ,mode ,locked
      ,(cons 'list (mapcar (pcase-lambda (`(,var ,form))
                             `(list ',var ,form))
                           args))
      ,@(nreverse kwargs))))

(cl-defun magit-setup-buffer-internal
    ( mode locked bindings
      &key buffer directory initial-section select-section)
  (let* ((value   (and locked
                       (with-temp-buffer
                         (pcase-dolist (`(,var ,val) bindings)
                           (set (make-local-variable var) val))
                         (let ((major-mode mode))
                           (magit-buffer-value)))))
         (buffer  (if buffer
                      (get-buffer-create buffer)
                    (magit-get-mode-buffer mode value)))
         (section (and buffer (magit-current-section)))
         (created (not buffer)))
    (unless buffer
      (setq buffer (magit-generate-new-buffer mode value)))
    (with-current-buffer buffer
      (setq magit-previous-section section)
      (when directory
        (setq default-directory directory))
      (funcall mode)
      (magit-xref-setup #'magit-setup-buffer-internal bindings)
      (pcase-dolist (`(,var ,val) bindings)
        (set (make-local-variable var) val))
      (when created
        (run-hooks 'magit-create-buffer-hook)))
    (magit-display-buffer buffer)
    (with-current-buffer buffer
      (run-hooks 'magit-setup-buffer-hook)
      (magit-refresh-buffer created
                            :initial-section initial-section
                            :select-section select-section)
      (when created
        (run-hooks 'magit-post-create-buffer-hook)))
    buffer))

;;; Display Buffer

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
      (let ((old-frame (selected-frame))
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
              '(nil (inhibit-same-window . t))
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
         ('(display-buffer-same-window)))))

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
   (cond ((and (or (bound-and-true-p git-commit-mode)
                   (derived-mode-p 'magit-log-mode
                                   'magit-cherry-mode
                                   'magit-reflog-mode))
               (with-current-buffer buffer
                 (derived-mode-p 'magit-diff-mode)))
          nil)
         ((with-current-buffer buffer
            (derived-mode-p 'magit-process-mode))
          nil)
         ('(magit--display-buffer-fullcolumn)))))

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

;;; Get Buffer

(defvar-local magit--default-directory nil
  "Value of `default-directory' when buffer is generated.
This exists to prevent a let-bound `default-directory' from
tricking `magit-get-mode-buffer' or `magit-mode-get-buffers'
into thinking a buffer belongs to a repo that it doesn't.")
(put 'magit--default-directory 'permanent-local t)

(defun magit-mode-get-buffers ()
  (let ((topdir (magit-toplevel)))
    (seq-filter (##with-current-buffer %
                  (and (derived-mode-p 'magit-mode)
                       (equal magit--default-directory topdir)))
                (buffer-list))))

(defvar-local magit-buffer-locked-p nil)
(put 'magit-buffer-locked-p 'permanent-local t)

(defun magit-get-mode-buffer (mode &optional value frame)
  "Return buffer belonging to the current repository whose major-mode is MODE.

If no such buffer exists then return nil.  Multiple buffers with
the same major-mode may exist for a repository but only one can
exist that hasn't been locked to its value.  Return that buffer
\(or nil if there is no such buffer) unless VALUE is non-nil, in
which case return the buffer that has been locked to that value.

If FRAME is nil or omitted, then consider all buffers.  Otherwise
  only consider buffers that are displayed in some live window
  on some frame.
If `all', then consider all buffers on all frames.
If `visible', then only consider buffers on all visible frames.
If `selected' or t, then only consider buffers on the selected
  frame.
If a frame, then only consider buffers on that frame."
  (let ((topdir (magit--toplevel-safe)))
    (cl-flet* ((b (buffer)
                 (with-current-buffer buffer
                   (and (eq major-mode mode)
                        (equal magit--default-directory topdir)
                        (if value
                            (and magit-buffer-locked-p
                                 (equal (magit-buffer-value) value))
                          (not magit-buffer-locked-p))
                        buffer)))
               (w (window)
                 (b (window-buffer window)))
               (f (frame)
                 (seq-some #'w (window-list frame 'no-minibuf))))
      (pcase-exhaustive frame
        ('nil                   (seq-some #'b (buffer-list)))
        ('all                   (seq-some #'f (frame-list)))
        ('visible               (seq-some #'f (visible-frame-list)))
        ((or 'selected 't)      (seq-some #'w (window-list (selected-frame))))
        ((guard (framep frame)) (seq-some #'w (window-list frame)))))))

(defun magit-generate-new-buffer (mode &optional value directory)
  (let* ((default-directory (or directory (magit--toplevel-safe)))
         (name (funcall magit-generate-buffer-name-function mode value))
         (buffer (generate-new-buffer name)))
    (with-current-buffer buffer
      (setq magit--default-directory default-directory)
      (setq magit-buffer-locked-p (and value t))
      (magit-restore-section-visibility-cache mode))
    (magit--maybe-uniquify-buffer-names buffer name mode)
    buffer))

(defun magit-generate-buffer-name-default-function (mode &optional value)
  "Generate buffer name for a MODE buffer in the current repository.
The returned name is based on `magit-buffer-name-format' and
takes `magit-uniquify-buffer-names' and VALUE, if non-nil, into
account."
  (let ((m (substring (symbol-name mode) 0 -5))
        (v (and value (format "%s" (ensure-list value))))
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
       (?x . ,(if magit-uniquify-buffer-names "" "*"))))))

(defun magit--maybe-uniquify-buffer-names (buffer name mode)
  (when magit-uniquify-buffer-names
    (cl-pushnew mode uniquify-list-buffers-directory-modes)
    (with-current-buffer buffer
      (setq list-buffers-directory (abbreviate-file-name default-directory)))
    (let ((uniquify-buffer-name-style
           (if (memq uniquify-buffer-name-style '(nil forward))
               'post-forward-angle-brackets
             uniquify-buffer-name-style)))
      (uniquify-rationalize-file-buffer-names
       name (file-name-directory (directory-file-name default-directory))
       buffer))))

;;; Buffer Lock

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
  (cond-let
    (magit-buffer-locked-p
     (if-let ((unlocked (magit-get-mode-buffer major-mode)))
         (let ((locked (current-buffer)))
           (switch-to-buffer unlocked nil t)
           (kill-buffer locked))
       (setq magit-buffer-locked-p nil)
       (let ((name (funcall magit-generate-buffer-name-function major-mode))
             (buffer (current-buffer))
             (mode major-mode))
         (rename-buffer (generate-new-buffer-name name))
         (with-temp-buffer
           (magit--maybe-uniquify-buffer-names buffer name mode)))))
    ([value (magit-buffer-value)]
     (if-let ((locked (magit-get-mode-buffer major-mode value)))
         (let ((unlocked (current-buffer)))
           (switch-to-buffer locked nil t)
           (kill-buffer unlocked))
       (setq magit-buffer-locked-p t)
       (let ((name (funcall magit-generate-buffer-name-function
                            major-mode value))
             (buffer (current-buffer))
             (mode major-mode))
         (rename-buffer (generate-new-buffer-name name))
         (with-temp-buffer
           (magit--maybe-uniquify-buffer-names buffer name mode)))))
    ((user-error "Buffer has no value it could be locked to"))))

;;; Bury Buffer

(defun magit-mode-bury-buffer (&optional kill-buffer)
  "Bury or kill the current buffer.

Use `magit-bury-buffer-function' to bury the buffer when called
without a prefix argument or to kill it when called with a single
prefix argument.

With two prefix arguments, always kill the current and all other
Magit buffers, associated with this repository."
  (interactive "P")
  (if (>= (prefix-numeric-value kill-buffer) 16)
      (mapc #'kill-buffer (magit-mode-get-buffers))
    (funcall magit-bury-buffer-function kill-buffer)))

(defun magit-mode-quit-window (kill-buffer)
  "Quit the selected window and bury its buffer.

This behaves similar to `quit-window', but when the window
was originally created to display a Magit buffer and the
current buffer is the last remaining Magit buffer that was
ever displayed in the selected window, then delete that
window."
  (if (or (one-window-p)
          (seq-find (pcase-lambda (`(,buffer))
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

;;; Refresh Buffers

(defvar magit-inhibit-refresh nil)

(defun magit-refresh ()
  "Refresh some buffers belonging to the current repository.

Refresh the current buffer if its major mode derives from
`magit-mode', and refresh the corresponding status buffer.

Run hooks `magit-pre-refresh-hook', `magit-post-refresh-hook'
and `magit-unwind-refresh-hook'."
  (interactive)
  (unless magit-inhibit-refresh
    (unwind-protect
        (let ((start (current-time))
              (magit--refresh-cache (or magit--refresh-cache
                                        (list (cons 0 0)))))
          (when magit-refresh-verbose
            (message "Refreshing magit..."))
          (magit-run-hook-with-benchmark 'magit-pre-refresh-hook)
          (cond ((derived-mode-p 'magit-mode)
                 (magit-refresh-buffer))
                ((derived-mode-p 'tabulated-list-mode)
                 (revert-buffer)))
          (when-let ((buffer (and magit-refresh-status-buffer
                                  (not (derived-mode-p 'magit-status-mode))
                                  (magit-get-mode-buffer 'magit-status-mode))))
            (with-current-buffer buffer
              (magit-refresh-buffer)))
          (magit-run-hook-with-benchmark 'magit-post-refresh-hook)
          (when magit-refresh-verbose
            (let* ((c (caar magit--refresh-cache))
                   (a (+ c (cdar magit--refresh-cache))))
              (message "Refreshing magit...done (%.3fs, cached %s/%s (%.0f%%))"
                       (float-time (time-since start))
                       c a (* (/ c (* a 1.0)) 100)))))
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
  (magit-run-hook-with-benchmark 'magit-post-refresh-hook))

(defvar-local magit--refresh-start-time nil)

(cl-defun magit-refresh-buffer ( &optional created
                                 &key initial-section select-section)
  "Refresh the current Magit buffer.
The arguments are for internal use."
  (interactive)
  (when-let ((refresh (magit--refresh-buffer-function)))
    (let ((magit--refreshing-buffer-p t)
          (magit--refresh-start-time (current-time))
          (magit--refresh-cache (or magit--refresh-cache (list (cons 0 0))))
          (action (if created "Creating" "Refreshing")))
      (when magit-refresh-verbose
        (message "%s buffer `%s'..." action (buffer-name)))
      (cond
       (created
        (funcall refresh)
        (cond (initial-section (funcall initial-section))
              (select-section (funcall select-section))))
       (t
        (deactivate-mark)
        (setq magit-section-pre-command-section nil)
        (setq magit-section-highlight-overlays nil)
        (setq magit-section-selection-overlays nil)
        (setq magit-section-highlighted-sections nil)
        (setq magit-section-focused-sections nil)
        (let ((positions (magit--refresh-buffer-get-positions)))
          (funcall refresh)
          (cond (select-section (funcall select-section))
                ((magit--refresh-buffer-set-positions positions))))))
      (let ((magit-section-cache-visibility nil))
        (magit-section-show magit-root-section))
      (run-hooks 'magit-refresh-buffer-hook)
      (magit-section-update-highlight)
      (set-buffer-modified-p nil)
      (push (current-buffer) magit-section--refreshed-buffers)
      (when magit-refresh-verbose
        (message "%s buffer `%s'...done (%.3fs)" action (buffer-name)
                 (float-time (time-since magit--refresh-start-time)))))))

(defun magit--refresh-buffer-function ()
  (let ((fn (intern (format "%s-refresh-buffer"
                            (substring (symbol-name major-mode) 0 -5)))))
    (and (functionp fn)
         (lambda ()
           (let ((inhibit-read-only t))
             (erase-buffer)
             (save-excursion (funcall fn)))))))

(defun magit--refresh-buffer-get-positions ()
  (or (let ((buffer (current-buffer)))
        (mapcan
         (lambda (window)
           (with-selected-window window
             (with-current-buffer buffer
               (and-let ((section (magit-section-at)))
                 `((,window
                    ,section
                    ,@(magit-section-get-relative-position section)
                    ,@(and-let ((ws (magit-section-at (window-start))))
                        (list ws
                              (car (magit-section-get-relative-position ws))
                              (window-start)))))))))
         ;; For hunks we run `magit-section-movement-hook' (once for
         ;; each window displaying the buffer).  The selected window
         ;; comes first in this list, but we want to process it last.
         (nreverse (get-buffer-window-list buffer nil t))))
      (and$ (magit-section-at)
            `((nil ,$ ,@(magit-section-get-relative-position $))))))

(defun magit--refresh-buffer-set-positions (positions)
  (pcase-dolist
      (`(,window ,section ,line ,char ,ws-section ,ws-line ,window-start)
       positions)
    (if window
        (with-selected-window window
          (magit-section-goto-successor section line char)
          (cond-let
            ((derived-mode-p 'magit-log-mode))
            ((or (not window-start)
                 (> window-start (point))))
            ((magit-section-equal ws-section (magit-section-at window-start))
             (set-window-start window window-start t))
            ([pos (save-excursion
                    (and (magit-section-goto-successor--same
                          ws-section ws-line 0)
                         (point)))]
             (set-window-start window pos t))))
      ;; We must make sure this does not call `set-window-start',
      ;; which the HUNK METHOD does by calling `magit-section-goto'
      ;; because that runs the `magit-section-goto-successor-hook'
      ;; and thus `magit-hunk-set-window-start'.  The window does
      ;; not display this buffer, so the window start would be set
      ;; for the wrong buffer.  Originally reported in #4196 and
      ;; fixed with 482c25a3204468a4f6c2fe12ff061666b61f5f4d.
      (let ((magit-section-movement-hook nil))
        (magit-section-goto-successor section line char)))))

(defun magit-revert-buffer (_ignore-auto _noconfirm)
  "Wrapper around `magit-refresh-buffer' suitable as `revert-buffer-function'."
  (magit-refresh-buffer))

(defun magit-profile-refresh-buffer ()
  "Profile refreshing the current Magit buffer."
  (interactive)
  (require (quote elp))
  (elp-reset-all)
  (message "Profiling Magit and Forge...")
  (elp-instrument-package "magit-")
  (elp-instrument-package "forge-")
  (magit-refresh-buffer)
  (message "Profiling Magit and Forge...done")
  (elp-results)
  (elp-reset-all))

(defun magit-toggle-profiling ()
  "Start profiling Magit, or if in progress, stop and display the results."
  (interactive)
  (require (quote elp))
  (cond ((catch 'in-progress
           (mapatoms (##and (get % elp-timer-info-property)
                            (throw 'in-progress t))))
         (message "Stop profiling and display results...")
         (elp-results)
         (elp-restore-all))
        (t
         (message "Start profiling Magit and Forge...")
         (elp-reset-all)
         (elp-instrument-package "magit-")
         (elp-instrument-package "forge-"))))

;;; Save File-Visiting Buffers

(defvar magit--disable-save-buffers nil)

(defun magit-pre-command-hook ()
  (setq magit--disable-save-buffers nil))
(add-hook 'pre-command-hook #'magit-pre-command-hook)

(defvar magit-after-save-refresh-buffers nil)

(defun magit-after-save-refresh-buffers ()
  (unless magit-inhibit-refresh
    (dolist (buffer magit-after-save-refresh-buffers)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (magit-refresh-buffer))))
    (setq magit-after-save-refresh-buffers nil)
    (remove-hook 'post-command-hook #'magit-after-save-refresh-buffers)))

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
  (when-let ((_(not magit-inhibit-refresh))
             (_(magit-inside-worktree-p t))
             (buf (ignore-errors (magit-get-mode-buffer 'magit-status-mode))))
    (cl-pushnew buf magit-after-save-refresh-buffers)
    (add-hook 'post-command-hook #'magit-after-save-refresh-buffers)))

(defun magit-maybe-save-repository-buffers ()
  "Maybe save file-visiting buffers belonging to the current repository.
Do so if `magit-save-repository-buffers' is non-nil.  You should
not remove this from any hooks, instead set that variable to nil
if you so desire."
  (when (and magit-save-repository-buffers
             (not magit--disable-save-buffers))
    (setq magit--disable-save-buffers t)
    (let ((msg (current-message)))
      (magit-save-repository-buffers
       (eq magit-save-repository-buffers 'dontask))
      (when (and msg
                 (current-message)
                 (not (equal msg (current-message))))
        (message "%s" msg)))))

(defvar-local magit-inhibit-refresh-save nil)

(defvar magit-save-repository-buffers-predicate
  (lambda (topdir)
    (let ((remote (file-remote-p default-directory))
          (topdirs nil)
          ;; If the current file is modified and resides inside
          ;; a repository, and a let-binding is in effect, which
          ;; places us in another repository, then this binding
          ;; is needed to prevent that file from being saved.
          (default-directory default-directory))
      (and buffer-file-name
           (setq default-directory (file-name-directory buffer-file-name))
           ;; Check whether the repository still exists.
           (file-exists-p default-directory)
           ;; Check whether refreshing is disabled.
           (not magit-inhibit-refresh-save)
           ;; Check whether the visited file is either on the
           ;; same remote as the repository, or both are on
           ;; the local system.
           (equal (file-remote-p buffer-file-name) remote)
           ;; Delayed checks that are more expensive for remote
           ;; repositories, due to the required network access.
           ;;
           ;; Check whether the file is inside the repository.
           (equal (or (cdr (assoc default-directory topdirs))
                      (let ((top (magit-rev-parse-safe "--show-toplevel")))
                        (push (cons default-directory top) topdirs)
                        top))
                  topdir)
           ;; Check whether the file is actually writable.
           (file-writable-p buffer-file-name))))
  "Predicate for `magit-save-repository-buffers'.

This function is called for each buffer that might need saving with
one argument, the working tree of the respective repository.  If it
returns non-nil, the current buffer is saved.")

(defun magit-save-repository-buffers (&optional arg)
  "Save file-visiting buffers belonging to the current repository.
After any buffer where `buffer-save-without-query' is non-nil
is saved without asking, the user is asked about each modified
buffer, which visits a file in the current repository.  Optional
argument (the prefix) non-nil means save all with no questions."
  (interactive "P")
  (when-let ((topdir (magit-rev-parse-safe "--show-toplevel")))
    (let ((save-some-buffers-action-alist
           `((?Y ,(##with-current-buffer %
                    (setq buffer-save-without-query t)
                    (save-buffer))
                 "to save the current buffer and remember choice")
             (?N ,(##with-current-buffer %
                    (setq magit-inhibit-refresh-save t))
                 "to skip the current buffer and remember choice")
             ,@save-some-buffers-action-alist))
          ;; Create a single wip commit for all saved files, for
          ;; which `magit-wip-after-save-local-mode' is enabled.
          (magit--wip-inhibit-autosave t)
          (saved nil))
      (unwind-protect
          (save-some-buffers
           arg
           (lambda ()
             (and (funcall magit-save-repository-buffers-predicate topdir)
                  (prog1 t
                    (when magit-wip-after-save-local-mode
                      (push (expand-file-name buffer-file-name) saved))))))
        (when saved
          (let ((default-directory topdir))
            (magit-wip-commit-worktree
             (magit-wip-get-ref)
             saved
             (if (cdr saved)
                 (format "autosave %s files after save" (length saved))
               (format "autosave %s after save"
                       (file-relative-name (car saved)))))))))))

;;; Restore Window Configuration

(defvar magit-inhibit-save-previous-winconf nil)

(defvar-local magit-previous-window-configuration nil)
(put 'magit-previous-window-configuration 'permanent-local t)

(defun magit-save-window-configuration ()
  "Save the current window configuration.

Later, when the buffer is buried, it may be restored by
`magit-restore-window-configuration'."
  (cond (magit-inhibit-save-previous-winconf
         (when (eq magit-inhibit-save-previous-winconf 'unset)
           (setq magit-previous-window-configuration nil)))
        ((not (get-buffer-window (current-buffer) (selected-frame)))
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
          (setq magit-previous-window-configuration nil)))
      (set-buffer (with-selected-window (selected-window)
                    (current-buffer))))))

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

(defun magit-insert-xref-buttons ()
  "Insert xref buttons."
  (when (and (not magit-buffer-locked-p)
             (or help-xref-stack help-xref-forward-stack))
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

(defvar magit-xref-modes
  ;; Do not function-quote to avoid circular dependencies.
  '(magit-log-mode
    magit-reflog-mode
    magit-diff-mode
    magit-revision-mode)
  "List of modes for which to insert navigation buttons.")

(defun magit-xref-setup (fn args)
  (when (memq major-mode magit-xref-modes)
    (when help-xref-stack-item
      (push (cons (point) help-xref-stack-item) help-xref-stack)
      (setq help-xref-forward-stack nil))
    (when-let ((tail (nthcdr 30 help-xref-stack)))
      (setcdr tail nil))
    (setq help-xref-stack-item
          (list 'magit-xref-restore fn default-directory args))))

(defun magit-xref-restore (fn dir args)
  (setq default-directory dir)
  (funcall fn major-mode nil args)
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

Return a (KEY . VALUE) cons cell.

The KEY is matched using `equal'.

Unless specified, REPOSITORY is the current buffer's repository."
  (and-let ((cache (assoc (or repository
                              (magit-repository-local-repository))
                          magit-repository-local-cache)))
    (assoc key (cdr cache))))

(defun magit-repository-local-get (key &optional default repository)
  "Return the repository-local value for KEY.

Return DEFAULT if no value for KEY exists.

The KEY is matched using `equal'.

Unless specified, REPOSITORY is the current buffer's repository."
  (if-let ((keyvalue (magit-repository-local-exists-p key repository)))
      (cdr keyvalue)
    default))

(defun magit-repository-local-delete (key &optional repository)
  "Delete the repository-local value for KEY.

Unless specified, REPOSITORY is the current buffer's repository.
If REPOSITORY is `all', then delete the value for KEY for all
repositories."
  (cond-let
    ((eq repository 'all)
     (dolist (cache magit-repository-local-cache)
       (setf cache (compat-call assoc-delete-all key cache))))
    ([cache (assoc (or repository (magit-repository-local-repository))
                   magit-repository-local-cache)]
     (setf cache (compat-call assoc-delete-all key cache)))))

(defmacro magit--with-repository-local-cache (key &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((k (gensym)))
    `(let ((,k ,key))
       (if-let ((kv (magit-repository-local-exists-p ,k)))
           (cdr kv)
         (let ((v ,(macroexp-progn body)))
           (magit-repository-local-set ,k v)
           v)))))

(defun magit-preserve-section-visibility-cache ()
  (when (derived-mode-p 'magit-status-mode 'magit-refs-mode)
    (magit-repository-local-set
     (cons major-mode 'magit-section-visibility-cache)
     magit-section-visibility-cache)))

(defun magit-restore-section-visibility-cache (mode)
  (setq magit-section-visibility-cache
        (magit-repository-local-get
         (cons mode 'magit-section-visibility-cache))))

(defun magit-zap-caches (&optional all)
  "Zap caches for the current repository.

Remove the repository's entry from `magit-repository-local-cache',
remove the host's entry from `magit--host-git-version-cache', and
set `magit-section-visibility-cache' to nil for all Magit buffers
of the repository.

With a prefix argument or if optional ALL is non-nil, discard the
mentioned caches completely."
  (interactive)
  (cond (all
         (setq magit-repository-local-cache nil)
         (setq magit--host-git-version-cache nil)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'magit-mode)
               (setq magit-section-visibility-cache nil)))))
        (t
         (magit-with-toplevel
           (setq magit-repository-local-cache
                 (cl-delete default-directory
                            magit-repository-local-cache
                            :key #'car :test #'equal))
           (setq magit--host-git-version-cache
                 (cl-delete (file-remote-p default-directory)
                            magit--host-git-version-cache
                            :key #'car :test #'equal)))
         (dolist (buffer (magit-mode-get-buffers))
           (with-current-buffer buffer
             (setq magit-section-visibility-cache nil))))))

;;; Utilities

(defun magit-toggle-verbose-refresh ()
  "Toggle whether Magit refreshes buffers verbosely.
Enabling this helps figuring out which sections are bottlenecks.
The additional output can be found in the *Messages* buffer."
  (interactive)
  (setq magit-refresh-verbose (not magit-refresh-verbose))
  (message "%s verbose refreshing"
           (if magit-refresh-verbose "Enabled" "Disabled")))

(defun magit-run-hook-with-benchmark (hook)
  (cond
   ((not hook))
   (magit-refresh-verbose
    (message "Running %s..." hook)
    (message "Running %s...done (%.3fs)" hook
             (benchmark-elapse
               (run-hook-wrapped
                hook
                (lambda (fn)
                  (message "  %-50s %f" fn (benchmark-elapse (funcall fn))))))))
   ((run-hooks hook))))

(defun magit-file-region-line-numbers ()
  "Return the bounds of the region as line numbers.
The returned value has the form (BEGINNING-LINE END-LINE).  If
the region end at the beginning of a line, do not include that
line.  Avoid including the line after the end of the file."
  (and (magit-buffer-file-name)
       (region-active-p)
       (not (= (region-beginning) (region-end) (1+ (buffer-size))))
       (let ((beg (region-beginning))
             (end (min (region-end) (buffer-size))))
         (list (line-number-at-pos beg t)
               (line-number-at-pos (if (= (magit--bol-position end) end)
                                       (max beg (1- end))
                                     end)
                                   t)))))

;;; _
(provide 'magit-mode)
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
;;; magit-mode.el ends here
