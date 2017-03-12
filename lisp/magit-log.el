;;; magit-log.el --- inspect Git history  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2017  The Magit Project Contributors
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

;; This library implements support for looking at Git logs, including
;; special logs like reflogs and cherry-logs, as well as for selecting
;; a commit from a log.

;;; Code:

(require 'magit-core)
(require 'magit-diff)

(declare-function magit-blob-visit 'magit)
(declare-function magit-insert-head-branch-header 'magit)
(declare-function magit-insert-upstream-branch-header 'magit)
(declare-function magit-read-file-from-rev 'magit)
(declare-function magit-show-commit 'magit)
(defvar magit-refs-indent-cherry-lines)
(defvar magit-refs-margin)
(defvar magit-refs-show-commit-count)
(defvar magit-buffer-margin)
(defvar magit-status-margin)
(defvar magit-status-sections-hook)

(require 'ansi-color)
(require 'crm)

;;; Options
;;;; Log Mode

(defgroup magit-log nil
  "Inspect and manipulate Git history."
  :link '(info-link "(magit)Logging")
  :group 'magit-modes)

(defcustom magit-log-mode-hook nil
  "Hook run after entering Magit-Log mode."
  :group 'magit-log
  :type 'hook)

(defcustom magit-log-arguments '("-n256" "--graph" "--decorate")
  "The log arguments used in `magit-log-mode' buffers."
  :package-version '(magit . "2.3.0")
  :group 'magit-git-arguments
  :group 'magit-log
  :type '(repeat (string :tag "Argument")))

(defcustom magit-log-remove-graph-args '("--follow" "--grep" "-G" "-S" "-L")
  "The log arguments that cause the `--graph' argument to be dropped."
  :package-version '(magit . "2.3.0")
  :group 'magit-log
  :type '(repeat (string :tag "Argument"))
  :options '("--follow" "--grep" "-G" "-S" "-L"))

(defcustom magit-log-revision-headers-format "\
%+b
Author:    %aN <%aE>
Committer: %cN <%cE>"
  "Additional format string used with the `++header' argument."
  :package-version '(magit . "2.3.0")
  :group 'magit-log
  :type 'string)

(defcustom magit-log-auto-more nil
  "Insert more log entries automatically when moving past the last entry.
Only considered when moving past the last entry with
`magit-goto-*-section' commands."
  :group 'magit-log
  :type 'boolean)

(defcustom magit-log-margin '(t age magit-log-margin-width t 18)
  "Format of the margin in `magit-log-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the committer date.  It can be one
  of `age' (to show the age of the commit), `age-abbreviated' (to
  abbreviate the time unit to a character), or a string (suitable
  for `format-time-string') to show the actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(magit . "2.9.0")
  :group 'magit-log
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize 'magit-custom-initialize-reset
  :set (apply-partially #'magit-margin-set-variable 'magit-log-mode))

(defcustom magit-log-show-refname-after-summary nil
  "Whether to show refnames after commit summaries.
This is useful if you use really long branch names."
  :package-version '(magit . "2.2.0")
  :group 'magit-log
  :type 'boolean)

(defface magit-log-graph
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the graph part of the log output."
  :group 'magit-faces)

(defface magit-log-author
  '((((class color) (background light)) :foreground "firebrick")
    (((class color) (background  dark)) :foreground "tomato"))
  "Face for the author part of the log output."
  :group 'magit-faces)

(defface magit-log-date
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for the date part of the log output."
  :group 'magit-faces)

;;;; File Log

(defcustom magit-log-buffer-file-locked t
  "Whether `magit-log-buffer-file' uses a decicated buffer."
  :package-version '(magit . "2.7.0")
  :group 'magit-commands
  :group 'magit-log
  :type 'boolean)

;;;; Select Mode

(defcustom magit-log-select-arguments '("-n256" "--decorate")
  "The log arguments used in `magit-log-select-mode' buffers."
  :package-version '(magit . "2.3.0")
  :group 'magit-log
  :type '(repeat (string :tag "Argument")))

(defcustom magit-log-select-show-usage 'both
  "Whether to show usage information when selecting a commit from a log.
The message can be shown in the `echo-area' or the `header-line', or in
`both' places.  If the value isn't one of these symbols, then it should
be nil, in which case no usage information is shown."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type '(choice (const :tag "in echo-area" echo-area)
                 (const :tag "in header-line" header-line)
                 (const :tag "in both places" both)
                 (const :tag "nowhere")))

(defcustom magit-log-select-margin
  (list (nth 0 magit-log-margin)
        (nth 1 magit-log-margin)
        'magit-log-margin-width t
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-log-select-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the committer date.  It can be one
  of `age' (to show the age of the commit), `age-abbreviated' (to
  abbreviate the time unit to a character), or a string (suitable
  for `format-time-string') to show the actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(magit . "2.9.0")
  :group 'magit-log
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize 'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-log-select-mode))

;;;; Cherry Mode

(defcustom magit-cherry-sections-hook
  '(magit-insert-cherry-headers
    magit-insert-cherry-commits)
  "Hook run to insert sections into the cherry buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-log
  :type 'hook)

(defcustom magit-cherry-margin
  (list (nth 0 magit-log-margin)
        (nth 1 magit-log-margin)
        'magit-log-margin-width t
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-cherry-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the committer date.  It can be one
  of `age' (to show the age of the commit), `age-abbreviated' (to
  abbreviate the time unit to a character), or a string (suitable
  for `format-time-string') to show the actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(magit . "2.9.0")
  :group 'magit-log
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize 'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-cherry-mode))

;;;; Reflog Mode

(defcustom magit-reflog-arguments '("-n256")
  "The log arguments used in `magit-reflog-mode' buffers."
  :package-version '(magit . "2.3.0")
  :group 'magit-git-arguments
  :type '(repeat (string :tag "Argument")))

(defcustom magit-reflog-margin
  (list (nth 0 magit-log-margin)
        (nth 1 magit-log-margin)
        'magit-log-margin-width nil
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-reflog-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the committer date.  It can be one
  of `age' (to show the age of the commit), `age-abbreviated' (to
  abbreviate the time unit to a character), or a string (suitable
  for `format-time-string') to show the actual date.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(magit . "2.9.0")
  :group 'magit-log
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize 'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-reflog-mode))

(defface magit-reflog-commit '((t :foreground "green"))
  "Face for commit commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-amend '((t :foreground "magenta"))
  "Face for amend commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-merge '((t :foreground "green"))
  "Face for merge, checkout and branch commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-checkout '((t :foreground "blue"))
  "Face for checkout commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-reset '((t :foreground "red"))
  "Face for reset commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-rebase '((t :foreground "magenta"))
  "Face for rebase commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-cherry-pick '((t :foreground "green"))
  "Face for cherry-pick commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-remote '((t :foreground "cyan"))
  "Face for pull and clone commands in reflogs."
  :group 'magit-faces)

(defface magit-reflog-other '((t :foreground "cyan"))
  "Face for other commands in reflogs."
  :group 'magit-faces)

;;;; Log Sections

(defcustom magit-log-section-commit-count 10
  "How many recent commits to show in certain log sections.
How many recent commits `magit-insert-recent-commits' and
`magit-insert-unpulled-from-upstream-or-recent' (provided
the upstream isn't ahead of the current branch) show."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'number)

(defcustom magit-log-section-arguments '("-n256" "--decorate")
  "The log arguments used in buffers that show other things besides logs."
  :package-version '(magit . "2.4.0")
  :group 'magit-git-arguments
  :group 'magit-log
  :group 'magit-status
  :type '(repeat (string :tag "Argument")))

;;; Commands
;;;; Popups

(defvar magit-log-popup
  '(:variable magit-log-arguments
    :man-page "git-log"
    :switches ((?g "Show graph"              "--graph")
               (?c "Show graph in color"     "--color")
               (?d "Show refnames"           "--decorate")
               (?S "Show signatures"         "--show-signature")
               (?u "Show diffs"              "--patch")
               (?s "Show diffstats"          "--stat")
               (?h "Show header"             "++header")
               (?D "Simplify by decoration"  "--simplify-by-decoration")
               (?f "Follow renames when showing single-file log" "--follow"))
    :options  ((?n "Limit number of commits" "-n")
               (?f "Limit to files"          "-- " magit-read-files)
               (?a "Limit to author"         "--author=")
               (?o "Order commits by"        "++order=" magit-log-select-order)
               (?g "Search messages"         "--grep=")
               (?G "Search changes"          "-G")
               (?S "Search occurrences"      "-S")
               (?L "Trace line evolution"    "-L" magit-read-file-trace))
    :actions  ((?l "Log current"             magit-log-current)
               (?L "Log local branches"      magit-log-branches)
               (?r "Reflog current"          magit-reflog-current)
               (?o "Log other"               magit-log)
               (?b "Log all branches"        magit-log-all-branches)
               (?O "Reflog other"            magit-reflog)
               (?h "Log HEAD"                magit-log-head)
               (?a "Log all references"      magit-log-all)
               (?H "Reflog HEAD"             magit-reflog-head))
    :default-action magit-log-current
    :max-action-columns 3))

(defvar magit-log-mode-refresh-popup
  '(:variable magit-log-arguments
    :man-page "git-log"
    :switches ((?g "Show graph"              "--graph")
               (?c "Show graph in color"     "--color")
               (?d "Show refnames"           "--decorate")
               (?S "Show signatures"         "--show-signature")
               (?u "Show diffs"              "--patch")
               (?s "Show diffstats"          "--stat")
               (?D "Simplify by decoration"  "--simplify-by-decoration")
               (?f "Follow renames when showing single-file log" "--follow"))
    :options  ((?n "Limit number of commits" "-n")
               (?f "Limit to files"          "-- " magit-read-files)
               (?a "Limit to author"         "--author=")
               (?o "Order commits by"        "++order=" magit-log-select-order)
               (?g "Search messages"         "--grep=")
               (?G "Search changes"          "-G")
               (?S "Search occurrences"      "-S")
               (?L "Trace line evolution"    "-L" magit-read-file-trace))
    :actions  ((?g "Refresh"       magit-log-refresh)
               (?L "Toggle margin" magit-toggle-margin)
               (?s "Set defaults"  magit-log-set-default-arguments) nil
               (?w "Save defaults" magit-log-save-default-arguments))
    :max-action-columns 2))

(defvar magit-reflog-mode-refresh-popup
  '(:variable magit-reflog-arguments
    :man-page "git-reflog"
    :options  ((?n "Limit number of commits" "-n"))))

(defvar magit-log-refresh-popup
  '(:variable magit-log-arguments
    :man-page "git-log"
    :switches ((?g "Show graph"          "--graph")
               (?c "Show graph in color" "--color")
               (?d "Show refnames"       "--decorate"))
    :options  ((?n "Limit number of commits" "-n")
               (?o "Order commits by"        "++order=" magit-log-select-order))
    :actions  ("Refresh"
               (?g "buffer"                   magit-log-refresh)
               (?s "buffer and set defaults"  magit-log-set-default-arguments)
               (?w "buffer and save defaults" magit-log-save-default-arguments)
               "Margin"
               (?L "toggle visibility" magit-toggle-margin)
               (?l "cycle style"       magit-cycle-margin-style)
               (?d "toggle details"    magit-toggle-margin-details))
    :max-action-columns 1))

(magit-define-popup-keys-deferred 'magit-log-popup)
(magit-define-popup-keys-deferred 'magit-log-mode-refresh-popup)
(magit-define-popup-keys-deferred 'magit-log-refresh-popup)

(defun magit-read-file-trace (&rest _ignored)
  (let ((file  (magit-read-file-from-rev "HEAD" "File"))
        (trace (magit-read-string "Trace")))
    (if (string-match
         "^\\(/.+/\\|:[^:]+\\|[0-9]+,[-+]?[0-9]+\\)\\(:\\)?$" trace)
        (concat trace (or (match-string 2 trace) ":") file)
      (user-error "Trace is invalid, see man git-log"))))

(defun magit-log-select-order (&rest _ignored)
  (magit-read-char-case "Order commits by " t
    (?t "[t]opography"     "topo")
    (?a "[a]uthor date"    "author-date")
    (?c "[c]ommitter date" "date")))

(defun magit-log-arguments (&optional refresh)
  (cond ((memq magit-current-popup
               '(magit-log-popup magit-log-refresh-popup))
         (magit-popup-export-file-args magit-current-popup-args))
        ((derived-mode-p 'magit-log-mode)
         (list (nth 1 magit-refresh-args)
               (nth 2 magit-refresh-args)))
        (refresh
         (list magit-log-section-arguments nil))
        (t
         (-if-let (buffer (magit-mode-get-buffer 'magit-log-mode))
             (with-current-buffer buffer
               (list (nth 1 magit-refresh-args)
                     (nth 2 magit-refresh-args)))
           (list (default-value 'magit-log-arguments) nil)))))

(defun magit-log-popup (arg)
  "Popup console for log commands."
  (interactive "P")
  (let ((magit-log-refresh-popup
         (pcase major-mode
           (`magit-log-mode magit-log-mode-refresh-popup)
           (_               magit-log-refresh-popup)))
        (magit-log-arguments
         (-if-let (buffer (magit-mode-get-buffer 'magit-log-mode))
             (with-current-buffer buffer
               (magit-popup-import-file-args (nth 1 magit-refresh-args)
                                             (nth 2 magit-refresh-args)))
           (default-value 'magit-log-arguments))))
    (magit-invoke-popup 'magit-log-popup nil arg)))

;;;###autoload
(defun magit-log-buffer-file-popup ()
  "Popup console for log commands.

This is a variant of `magit-log-popup' which shows the same popup
but which limits the log to the file being visited in the current
buffer."
  (interactive)
  (-if-let (file (magit-file-relative-name))
      (let ((magit-log-arguments
             (magit-popup-import-file-args
              (-if-let (buffer (magit-mode-get-buffer 'magit-log-mode))
                  (with-current-buffer buffer
                    (nth 2 magit-refresh-args))
                (default-value 'magit-log-arguments))
              (list file))))
        (magit-invoke-popup 'magit-log-popup nil nil))
    (user-error "Buffer isn't visiting a file")))

(defun magit-log-refresh-popup (arg)
  "Popup console for changing log arguments in the current buffer."
  (interactive "P")
  (magit-log-refresh-assert)
  (let ((magit-log-refresh-popup
         (cond ((derived-mode-p 'magit-log-select-mode)
                magit-log-refresh-popup)
               ((derived-mode-p 'magit-log-mode)
                (let ((def (copy-sequence magit-log-refresh-popup)))
                  (plist-put def :switches (plist-get magit-log-popup :switches))
                  (plist-put def :options  (plist-get magit-log-popup :options))
                  def))
               (t
                magit-log-refresh-popup)))
        (magit-log-arguments
         (cond ((derived-mode-p 'magit-log-select-mode)
                (cadr magit-refresh-args))
               ((derived-mode-p 'magit-log-mode)
                (magit-popup-import-file-args (nth 1 magit-refresh-args)
                                              (nth 2 magit-refresh-args)))
               (t
                magit-log-section-arguments))))
    (magit-invoke-popup 'magit-log-refresh-popup nil arg)))

;;;; Refresh Commands

(defun magit-log-refresh (args files)
  "Set the local log arguments for the current buffer."
  (interactive (magit-log-arguments t))
  (magit-log-refresh-assert)
  (cond ((derived-mode-p 'magit-log-select-mode)
         (setcar (cdr magit-refresh-args) args))
        ((derived-mode-p 'magit-log-mode)
         (setcdr magit-refresh-args (list args files)))
        (t
         (setq-local magit-log-section-arguments args)))
  (magit-refresh))

(defun magit-log-set-default-arguments (args files)
  "Set the global log arguments for the current buffer."
  (interactive (magit-log-arguments t))
  (magit-log-refresh-assert)
  (cond ((derived-mode-p 'magit-log-select-mode)
         (customize-set-variable 'magit-log-select-arguments args)
         (setcar (cdr magit-refresh-args) args))
        ((derived-mode-p 'magit-log-mode)
         (customize-set-variable 'magit-log-arguments args)
         (setcdr magit-refresh-args (list args files)))
        (t
         (customize-set-variable 'magit-log-section-arguments args)
         (kill-local-variable    'magit-log-section-arguments)))
  (magit-refresh))

(defun magit-log-save-default-arguments (args files)
  "Set and save the global log arguments for the current buffer."
  (interactive (magit-log-arguments t))
  (magit-log-refresh-assert)
  (cond ((derived-mode-p 'magit-log-select-mode)
         (customize-save-variable 'magit-log-select-arguments args)
         (setcar (cdr magit-refresh-args) args))
        ((derived-mode-p 'magit-log-mode)
         (customize-save-variable 'magit-log-arguments args)
         (setcdr magit-refresh-args (list args files)))
        (t
         (customize-save-variable 'magit-log-section-arguments args)
         (kill-local-variable     'magit-log-section-arguments)))
  (magit-refresh))

(defun magit-log-refresh-assert ()
  (cond ((derived-mode-p 'magit-reflog-mode)
         (user-error "Cannot change log arguments in reflog buffers"))
        ((derived-mode-p 'magit-cherry-mode)
         (user-error "Cannot change log arguments in cherry buffers"))))

;;;; Log Commands

(defvar magit-log-read-revs-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map crm-local-completion-map)
    (define-key map "\s" 'self-insert-command)
    map))

(defun magit-log-read-revs (&optional use-current)
  (or (and use-current (--when-let (magit-get-current-branch) (list it)))
      (let* ((choose-completion-string-functions
              '(crm--choose-completion-string))
             (minibuffer-completion-table #'crm--collection-fn)
             (minibuffer-completion-confirm t)
             (crm-completion-table
              `(,@(and (file-exists-p (magit-git-dir "FETCH_HEAD"))
                       (list "FETCH_HEAD"))
                ,@(magit-list-refnames)))
             (crm-separator "\\(\\.\\.\\.?\\|[, ]\\)")
             (default (or (magit-branch-or-commit-at-point)
                          (unless use-current
                            (magit-get-previous-branch))))
             (input (read-from-minibuffer
                     (format "Log rev,s%s: "
                             (if default (format " (%s)" default) ""))
                     nil magit-log-read-revs-map
                     nil 'magit-revision-history default)))
        (when (string-equal input "")
          (or (setq input default)
              (user-error "Nothing selected")))
        (split-string input "[, ]" t))))

;;;###autoload
(defun magit-log-current (revs &optional args files)
  "Show log for the current branch.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer."
  (interactive (cons (magit-log-read-revs t)
                     (magit-log-arguments)))
  (magit-log revs args files))

;;;###autoload
(defun magit-log (revs &optional args files)
  "Show log for one or more revs read from the minibuffer.
The user can input any revision or revisions separated by a
space, or even ranges, but only branches and tags, and a
representation of the commit at point, are available as
completion candidates."
  (interactive (cons (magit-log-read-revs)
                     (magit-log-arguments)))
  (require 'magit)
  (magit-mode-setup #'magit-log-mode revs args files)
  (magit-log-goto-same-commit))

;;;###autoload
(defun magit-log-head (&optional args files)
  "Show log for `HEAD'."
  (interactive (magit-log-arguments))
  (magit-log (list "HEAD") args files))

;;;###autoload
(defun magit-log-branches (&optional args files)
  "Show log for all local branches and `HEAD'."
  (interactive (magit-log-arguments))
  (magit-log (if (magit-get-current-branch)
                 (list "--branches")
               (list "HEAD" "--branches"))
             args files))

;;;###autoload
(defun magit-log-all-branches (&optional args files)
  "Show log for all local and remote branches and `HEAD'."
  (interactive (magit-log-arguments))
  (magit-log (if (magit-get-current-branch)
                 (list "--branches" "--remotes")
               (list "HEAD" "--branches" "--remotes"))
             args files))

;;;###autoload
(defun magit-log-all (&optional args files)
  "Show log for all references and `HEAD'."
  (interactive (magit-log-arguments))
  (magit-log (if (magit-get-current-branch)
                 (list "--all")
               (list "HEAD" "--all"))
             args files))

;;;###autoload
(defun magit-log-buffer-file (&optional follow beg end)
  "Show log for the blob or file visited in the current buffer.
With a prefix argument or when `--follow' is part of
`magit-log-arguments', then follow renames."
  (interactive (if (region-active-p)
                   (list current-prefix-arg
                         (1- (line-number-at-pos (region-beginning)))
                         (1- (line-number-at-pos (region-end))))
                 (list current-prefix-arg)))
  (require 'magit)
  (-if-let (file (magit-file-relative-name))
      (magit-mode-setup-internal
       #'magit-log-mode
       (list (list (or magit-buffer-refname
                       (magit-get-current-branch)
                       "HEAD"))
             (let ((args (car (magit-log-arguments))))
               (when (and follow (not (member "--follow" args)))
                 (push "--follow" args))
               (when (and beg end)
                 (setq args (cons (format "-L%s,%s:%s" beg end file)
                                  (cl-delete "-L" args :test
                                             'string-prefix-p)))
                 (setq file nil))
               args)
             (and file (list file)))
       magit-log-buffer-file-locked)
    (user-error "Buffer isn't visiting a file"))
  (magit-log-goto-same-commit))

;;;###autoload
(defun magit-reflog-current ()
  "Display the reflog of the current branch."
  (interactive)
  (magit-reflog (magit-get-current-branch)))

;;;###autoload
(defun magit-reflog (ref)
  "Display the reflog of a branch."
  (interactive (list (magit-read-local-branch-or-ref "Show reflog for")))
  (require 'magit)
  (magit-mode-setup #'magit-reflog-mode ref magit-reflog-arguments))

;;;###autoload
(defun magit-reflog-head ()
  "Display the `HEAD' reflog."
  (interactive)
  (magit-reflog "HEAD"))

;;;; Limit Commands

(defun magit-log-toggle-commit-limit ()
  "Toggle the number of commits the current log buffer is limited to.
If the number of commits is currently limited, then remove that
limit.  Otherwise set it to 256."
  (interactive)
  (magit-log-set-commit-limit (lambda (&rest _) nil)))

(defun magit-log-double-commit-limit ()
  "Double the number of commits the current log buffer is limited to."
  (interactive)
  (magit-log-set-commit-limit '*))

(defun magit-log-half-commit-limit ()
  "Half the number of commits the current log buffer is limited to."
  (interactive)
  (magit-log-set-commit-limit '/))

(defun magit-log-set-commit-limit (fn)
  (let* ((val (car (magit-log-arguments t)))
         (arg (--first (string-match "^-n\\([0-9]+\\)?$" it) val))
         (num (and arg (string-to-number (match-string 1 arg))))
         (num (if num (funcall fn num 2) 256)))
    (setq val (delete arg val))
    (setcar (cdr magit-refresh-args)
            (if (and num (> num 0))
                (cons (format "-n%i" num) val)
              val)))
  (magit-refresh))

(defun magit-log-get-commit-limit ()
  (--when-let (--first (string-match "^-n\\([0-9]+\\)?$" it)
                       (car (magit-log-arguments t)))
    (string-to-number (match-string 1 it))))

;;;; Other Commands

(defun magit-log-bury-buffer (&optional arg)
  "Bury the current buffer or the revision buffer in the same frame.
Like `magit-mode-bury-buffer' (which see) but with a negative
prefix argument instead bury the revision buffer, provided it
is displayed in the current frame."
  (interactive "p")
  (if (< arg 0)
      (let* ((buf (magit-mode-get-buffer 'magit-revision-mode))
             (win (and buf (get-buffer-window buf (selected-frame)))))
        (if win
            (with-selected-window win
              (with-current-buffer buf
                (magit-mode-bury-buffer (> (abs arg) 1))))
          (user-error "No revision buffer in this frame")))
    (magit-mode-bury-buffer (> arg 1))))

;;;###autoload
(defun magit-log-move-to-parent (&optional n)
  "Move to the Nth parent of the current commit."
  (interactive "p")
  (when (derived-mode-p 'magit-log-mode)
    (magit-section-when commit
      (let ((parent-rev (format "%s^%s" (magit-section-value it) (or n 1))))
        (-if-let (parent-hash (magit-rev-parse "--short" parent-rev))
            (-if-let (section (--first (equal (magit-section-value it)
                                              parent-hash)
                                       (magit-section-siblings it 'next)))
                (magit-section-goto section)
              (user-error
               (substitute-command-keys
                (concat "Parent " parent-hash " not found.  Try typing "
                        "\\[magit-log-double-commit-limit] first"))))
          (user-error "Parent %s does not exist" parent-rev))))))

;;; Log Mode

(defvar magit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-c\C-b" 'magit-go-backward)
    (define-key map "\C-c\C-f" 'magit-go-forward)
    (define-key map "\C-c\C-n" 'magit-log-move-to-parent)
    (define-key map "=" 'magit-log-toggle-commit-limit)
    (define-key map "+" 'magit-log-double-commit-limit)
    (define-key map "-" 'magit-log-half-commit-limit)
    (define-key map "q" 'magit-log-bury-buffer)
    map)
  "Keymap for `magit-log-mode'.")

(define-derived-mode magit-log-mode magit-mode "Magit Log"
  "Mode for looking at Git log.

This mode is documented in info node `(magit)Log Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[magit-branch-popup] to see available branch commands.
Type \\[magit-merge-popup] to merge the branch or commit at point.
Type \\[magit-cherry-pick-popup] to apply the commit at point.
Type \\[magit-reset] to reset `HEAD' to the commit at point.

\\{magit-log-mode-map}"
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer))

(defvar magit-log-disable-graph-hack-args
  '("-G" "--grep" "--author")
  "Arguments which disable the graph speedup hack.")

(defun magit-log-refresh-buffer (revs args files)
  (setq header-line-format
        (propertize
         (concat " Commits in " (mapconcat 'identity revs  " ")
                 (and files (concat " touching "
                                    (mapconcat 'identity files " "))))
         'face 'magit-header-line))
  (unless (= (length files) 1)
    (setq args (remove "--follow" args)))
  (when (--any-p (string-match-p
                  (concat "^" (regexp-opt magit-log-remove-graph-args)) it)
                 args)
    (setq args (remove "--graph" args)))
  (unless (member "--graph" args)
    (setq args (remove "--color" args)))
  (-when-let* ((limit (magit-log-get-commit-limit))
               (limit (* 2 limit)) ; increase odds for complete graph
               (count (and (= (length revs) 1)
                           (> limit 1024) ; otherwise it's fast enough
                           (setq revs (car revs))
                           (not (string-match-p "\\.\\." revs))
                           (not (member revs '("--all" "--branches")))
                           (-none-p (lambda (arg)
                                      (--any-p (string-prefix-p it arg)
                                               magit-log-disable-graph-hack-args))
                                    args)
                           (magit-git-string "rev-list" "--count"
                                             "--first-parent" args revs))))
    (setq revs (if (< (string-to-number count) limit)
                   revs
                 (format "%s~%s..%s" revs limit revs))))
  (magit-insert-section (logbuf)
    (magit-insert-log revs args files)))

(defun magit-insert-log (revs &optional args files)
  "Insert a log section.
Do not add this to a hook variable."
  (let ((magit-git-global-arguments
         (remove "--literal-pathspecs" magit-git-global-arguments)))
    (magit-git-wash (apply-partially #'magit-log-wash-log 'log)
      "log"
      (format "--format=%%h%s %s[%%aN][%%at]%%s%s"
              (if (member "--decorate" args) "%d" "")
              (if (member "--show-signature" args)
                  (progn (setq args (remove "--show-signature" args)) "%G?")
                "")
              (if (member "++header" args)
                  (if (member "--graph" (setq args (remove "++header" args)))
                      (concat "\n" magit-log-revision-headers-format "\n")
                    (concat "\n" magit-log-revision-headers-format "\n"))
                ""))
      (progn
        (--when-let (--first (string-match "^\\+\\+order=\\(.+\\)$" it) args)
          (setq args (cons (format "--%s-order" (match-string 1 it))
                           (remove it args))))
        (if (member "--decorate" args)
            (cons "--decorate=full" (remove "--decorate" args))
          args))
      "--use-mailmap" "--no-prefix" revs "--" files)))

(defvar magit-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] 'magit-show-commit)
    (define-key map "a" 'magit-cherry-apply)
    map)
  "Keymap for `commit' sections.")

(defvar magit-module-commit-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] 'magit-show-commit)
    map)
  "Keymap for `module-commit' sections.")

(defconst magit-log-heading-re
  (concat "^"
          "\\(?4:[-_/|\\*o. ]*\\)"                 ; graph
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?:\\(?3:([^()]+)\\) \\)?"            ; refs
          "\\(?7:[BGUXYREN]\\)?"                   ; gpg
          "\\[\\(?5:[^]]*\\)\\]"                   ; author
          "\\[\\(?6:[^]]*\\)\\]"                   ; date
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-cherry-re
  (concat "^"
          "\\(?8:[-+]\\) "                         ; cherry
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-module-re
  (concat "^"
          "\\(?:\\(?11:[<>]\\) \\)?"               ; side
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-bisect-vis-re
  (concat "^"
          "\\(?4:[-_/|\\*o. ]*\\)"                 ; graph
          "\\(?1:[0-9a-fA-F]+\\) "                 ; sha1
          "\\(?:\\(?3:([^()]+)\\) \\)?"            ; refs
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-bisect-log-re
  (concat "^# "
          "\\(?3:bad:\\|skip:\\|good:\\) "         ; "refs"
          "\\[\\(?1:[^]]+\\)\\] "                  ; sha1
          "\\(?2:.*\\)$"))                         ; msg

(defconst magit-log-reflog-re
  (concat "^"
          "\\(?1:[^\0]+\\)\0"                      ; sha1
          "\\(?5:[^\0]*\\)\0"                      ; author
          "\\(?:\\(?:[^@]+@{\\(?6:[^}]+\\)}\0"     ; date
          "\\(?10:merge \\|autosave \\|restart \\|[^:\n]+: \\)?" ; refsub
          "\\(?2:.*\\)?\\)\\|\0\\)$"))             ; msg

(defconst magit-reflog-subject-re
  (concat "\\(?1:[^ ]+\\) ?"                       ; command
          "\\(?2:\\(?: ?-[^ ]+\\)+\\)?"            ; option
          "\\(?: ?(\\(?3:[^)]+\\))\\)?"))          ; type

(defconst magit-log-stash-re
  (concat "^"
          "\\(?1:[^\0]+\\)\0"                      ; "sha1"
          "\\(?5:[^\0]*\\)\0"                      ; author
          "\\(?6:[^\0]+\\)\0"                      ; date
          "\\(?2:.*\\)$"))                         ; msg

(defvar magit-log-count nil)

(defun magit-log-wash-log (style args)
  (setq args (-flatten args))
  (when (and (member "--graph" args)
             (member "--color" args))
    (let ((ansi-color-apply-face-function
           (lambda (beg end face)
             (put-text-property beg end 'font-lock-face
                                (or face 'magit-log-graph)))))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (when (eq style 'cherry)
    (reverse-region (point-min) (point-max)))
  (let ((magit-log-count 0)
        (abbrev (magit-abbrev-length)))
    (magit-wash-sequence (apply-partially 'magit-log-wash-rev style abbrev))
    (if (derived-mode-p 'magit-log-mode)
        (when (eq magit-log-count (magit-log-get-commit-limit))
          (magit-insert-section (longer)
            (insert-text-button
             (substitute-command-keys
              (format "Type \\<%s>\\[%s] to show more history"
                      'magit-log-mode-map
                      'magit-log-double-commit-limit))
             'action (lambda (_button)
                       (magit-log-double-commit-limit))
             'follow-link t
             'mouse-face 'magit-section-highlight)))
      (insert ?\n))))

(cl-defun magit-log-wash-rev (style abbrev)
  (when (derived-mode-p 'magit-log-mode)
    (cl-incf magit-log-count))
  (looking-at (pcase style
                (`log        magit-log-heading-re)
                (`cherry     magit-log-cherry-re)
                (`module     magit-log-module-re)
                (`reflog     magit-log-reflog-re)
                (`stash      magit-log-stash-re)
                (`bisect-vis magit-log-bisect-vis-re)
                (`bisect-log magit-log-bisect-log-re)))
  (magit-bind-match-strings
      (hash msg refs graph author date gpg cherry _ refsub side) nil
    (let ((align (not (member "--stat" (cadr magit-refresh-args))))
          (non-graph-re (if (eq style 'bisect-vis)
                            magit-log-bisect-vis-re
                          magit-log-heading-re)))
      (magit-delete-line)
      ;; If the reflog entries have been pruned, the output of `git
      ;; reflog show' includes a partial line that refers to the hash
      ;; of the youngest expired reflog entry.
      (when (and (eq style 'reflog) (not date))
        (cl-return-from magit-log-wash-rev t))
      (magit-insert-section section (commit hash)
        (pcase style
          (`stash      (setf (magit-section-type section) 'stash))
          (`module     (setf (magit-section-type section) 'module-commit))
          (`bisect-log (setq hash (magit-rev-parse "--short" hash))))
        (when cherry
          (when (and (derived-mode-p 'magit-refs-mode)
                     magit-refs-show-commit-count)
            (insert (make-string magit-refs-indent-cherry-lines ?\s)))
          (insert (propertize cherry 'face (if (string= cherry "-")
                                               'magit-cherry-equivalent
                                             'magit-cherry-unmatched)))
          (insert ?\s))
        (when side
          (insert (propertize side 'face (if (string= side "<")
                                             'magit-cherry-equivalent
                                           'magit-cherry-unmatched)))
          (insert ?\s))
        (when align
          (insert (propertize hash 'face 'magit-hash) ?\s))
        (when graph
          (insert graph))
        (unless align
          (insert (propertize hash 'face 'magit-hash) ?\s))
        (when (and refs (not magit-log-show-refname-after-summary))
          (insert (magit-format-ref-labels refs) ?\s))
        (when (eq style 'reflog)
          (insert (format "%-2s " (1- magit-log-count)))
          (when refsub
            (insert (magit-reflog-format-subject
                     (substring refsub 0 (if (string-match-p ":" refsub) -2 -1))))))
        (when msg
          (insert (propertize msg 'face
                              (pcase (and gpg (aref gpg 0))
                                (?G 'magit-signature-good)
                                (?B 'magit-signature-bad)
                                (?U 'magit-signature-untrusted)
                                (?X 'magit-signature-expired)
                                (?Y 'magit-signature-expired-key)
                                (?R 'magit-signature-revoked)
                                (?E 'magit-signature-error)))))
        (when (and refs magit-log-show-refname-after-summary)
          (insert ?\s)
          (insert (magit-format-ref-labels refs)))
        (insert ?\n)
        (when (memq style '(log reflog stash))
          (goto-char (line-beginning-position))
          (when (and refsub
                     (string-match "\\`\\([^ ]\\) \\+\\(..\\)\\(..\\)" date))
            (setq date (+ (string-to-number (match-string 1 date))
                          (* (string-to-number (match-string 2 date)) 60 60)
                          (* (string-to-number (match-string 3 date)) 60))))
          (save-excursion
            (backward-char)
            (magit-log-format-margin author date)))
        (when (and (eq style 'cherry)
                   (magit-buffer-margin-p))
          (save-excursion
            (backward-char)
            (apply #'magit-log-format-margin
                   (split-string (magit-rev-format "%aN%x00%ct") "\0"))))
        (when (and graph
                   (not (eobp))
                   (not (looking-at non-graph-re)))
          (when (looking-at "")
            (magit-insert-heading)
            (delete-char 1)
            (magit-insert-section (commit-header)
              (forward-line)
              (magit-insert-heading)
              (re-search-forward "")
              (backward-delete-char 1)
              (forward-char)
              (insert ?\n))
            (delete-char 1))
          (if (looking-at "^\\(---\\|\n\s\\|\ndiff\\)")
              (let ((limit (save-excursion
                             (and (re-search-forward non-graph-re nil t)
                                  (match-beginning 0)))))
                (unless (magit-section-content magit-insert-section--current)
                  (magit-insert-heading))
                (delete-char (if (looking-at "\n") 1 4))
                (magit-diff-wash-diffs (list "--stat") limit))
            (when align
              (setq align (make-string (1+ abbrev) ? )))
            (when (and (not (eobp)) (not (looking-at non-graph-re)))
              (when align
                (setq align (make-string (1+ abbrev) ? )))
              (while (and (not (eobp)) (not (looking-at non-graph-re)))
                (when align
                  (save-excursion (insert align)))
                (magit-make-margin-overlay)
                (forward-line))
              ;; When `--format' is used and its value isn't one of the
              ;; predefined formats, then `git-log' does not insert a
              ;; separator line.
              (save-excursion
                (forward-line -1)
                (looking-at "[-_/|\\*o. ]*"))
              (setq graph (match-string 0))
              (unless (string-match-p "[/\\]" graph)
                (insert graph ?\n))))))))
  t)

(defun magit-log-maybe-show-more-commits (section)
  "When point is at the end of a log buffer, insert more commits.

Log buffers end with a button \"Type + to show more history\".
When the use of a section movement command puts point on that
button, then automatically show more commits, without the user
having to press \"+\".

This function is called `magit-section-movement-hook',
and exists mostly for backward compatibility reasons."
  (when (and (eq (magit-section-type section) 'longer)
             magit-log-auto-more)
    (magit-log-double-commit-limit)
    (forward-line -1)
    (magit-section-forward)))

(defvar magit--update-revision-buffer nil)

(defun magit-log-maybe-update-revision-buffer (&optional _)
  "When moving in the log buffer, update the revision buffer.
If there is no revision buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-log-mode)
    (magit-log-maybe-update-revision-buffer-1)))

(defun magit-log-maybe-update-revision-buffer-1 ()
  (unless magit--update-revision-buffer
    (-when-let* ((commit (magit-section-when 'commit))
                 (buffer (magit-mode-get-buffer 'magit-revision-mode nil t)))
      (setq magit--update-revision-buffer (list commit buffer))
      (run-with-idle-timer
       magit-update-other-window-delay nil
       (lambda ()
         (-let [(rev buf) magit--update-revision-buffer]
           (setq magit--update-revision-buffer nil)
           (when (buffer-live-p buf)
             (let ((magit-display-buffer-noselect t))
               (apply #'magit-show-commit rev (magit-diff-arguments)))))
         (setq magit--update-revision-buffer nil))))))

(defvar magit--update-blob-buffer nil)

(defun magit-log-maybe-update-blob-buffer (&optional _)
  "When moving in the log buffer, update the blob buffer.
If there is no blob buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-log-mode)
    (magit-log-maybe-update-blob-buffer-1)))

(defun magit-log-maybe-update-blob-buffer-1 ()
  (unless magit--update-revision-buffer
    (-when-let* ((commit (magit-section-when 'commit))
                 (buffer (--first (with-current-buffer it magit-buffer-revision)
                                  (-map #'window-buffer (window-list)))))
        (setq magit--update-blob-buffer (list commit buffer))
        (run-with-idle-timer
         magit-update-other-window-delay nil
         (lambda ()
           (-let [(rev buf) magit--update-blob-buffer]
             (setq magit--update-blob-buffer nil)
             (when (buffer-live-p buf)
               (save-excursion
                 (with-selected-window (get-buffer-window buf)
                   (with-current-buffer buf
                     (magit-blob-visit (list (magit-rev-parse rev)
                                             (magit-file-relative-name
                                              magit-buffer-file-name))
                                       (line-number-at-pos))))))))))))

(defun magit-log-goto-same-commit ()
  (-when-let* ((prev magit-previous-section)
               (rev  (cond ((magit-section-match 'commit prev)
                            (magit-section-value prev))
                           ((magit-section-match 'branch prev)
                            (magit-rev-format
                             "%h" (magit-section-value prev)))))
               (same (--first (equal (magit-section-value it) rev)
                              (magit-section-children magit-root-section))))
    (goto-char (magit-section-start same))))

;;; Log Margin

(defun magit-log-format-margin (author date)
  (-when-let (option (magit-margin-option))
    (-let [(_ style width details details-width)
           (or magit-buffer-margin
               (symbol-value option))]
      (magit-make-margin-overlay
       (concat (and details
                    (concat (propertize (truncate-string-to-width
                                         (or author "")
                                         details-width
                                         nil ?\s (make-string 1 magit-ellipsis))
                                        'face 'magit-log-author)
                            " "))
               (propertize
                (if (stringp style)
                    (format-time-string
                     style
                     (seconds-to-time (string-to-number date)))
                  (-let* ((abbr (eq style 'age-abbreviated))
                          ((cnt unit) (magit--age date abbr)))
                    (format (format (if abbr "%%2i%%-%ic" "%%2i %%-%is")
                                    (- width (if details (1+ details-width) 0)))
                            cnt unit)))
                'face 'magit-log-date))))))

(defun magit-log-margin-width (style details details-width)
  (+ (if details (1+ details-width) 0)
     (if (stringp style)
         (length (format-time-string style))
       (+ 2 ; two digits
          1 ; trailing space
          (if (eq style 'age-abbreviated)
              1  ; single character
            (+ 1 ; gap after digits
               (apply #'max (--map (max (length (nth 1 it))
                                        (length (nth 2 it)))
                                   magit--age-spec))))))))

;;; Select Mode

(defvar magit-log-select-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-log-mode-map)
    (define-key map "\C-c\C-b" 'undefined)
    (define-key map "\C-c\C-f" 'undefined)
    (define-key map "."        'magit-log-select-pick)
    (define-key map "e"        'magit-log-select-pick)
    (define-key map "\C-c\C-c" 'magit-log-select-pick)
    (define-key map "q"        'magit-log-select-quit)
    (define-key map "\C-c\C-k" 'magit-log-select-quit)
    map)
  "Keymap for `magit-log-select-mode'.")

(put 'magit-log-select-pick :advertised-binding [?\C-c ?\C-c])
(put 'magit-log-select-quit :advertised-binding [?\C-c ?\C-k])

(define-derived-mode magit-log-select-mode magit-log-mode "Magit Select"
  "Mode for selecting a commit from history.

This mode is documented in info node `(magit)Select from Log'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit at point.

\\<magit-log-select-mode-map>\
Type \\[magit-log-select-pick] to select the commit at point.
Type \\[magit-log-select-quit] to abort without selecting a commit."
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer))

(defun magit-log-select-refresh-buffer (rev args)
  (magit-insert-section (logbuf)
    (magit-insert-log rev args)))

(defvar-local magit-log-select-pick-function nil)
(defvar-local magit-log-select-quit-function nil)

(defun magit-log-select (pick &optional msg quit branch)
  (declare (indent defun))
  (magit-mode-setup #'magit-log-select-mode
                    (or branch (magit-get-current-branch) "HEAD")
                    magit-log-select-arguments)
  (magit-log-goto-same-commit)
  (setq magit-log-select-pick-function pick)
  (setq magit-log-select-quit-function quit)
  (when magit-log-select-show-usage
    (setq msg (substitute-command-keys
               (format-spec
                (if msg
                    (if (string-suffix-p "," msg)
                        (concat msg " or %q to abort")
                      msg)
                  "Type %p to select commit at point, or %q to abort")
                '((?p . "\\[magit-log-select-pick]")
                  (?q . "\\[magit-log-select-quit]")))))
    (when (memq magit-log-select-show-usage '(both header-line))
      (setq header-line-format (propertize (concat " " msg) 'face 'bold)))
    (when (memq magit-log-select-show-usage '(both echo-area))
      (message "%s" msg))))

(defun magit-log-select-pick ()
  "Select the commit at point and act on it.
Call `magit-log-select-pick-function' with the selected
commit as argument."
  (interactive)
  (let ((fun magit-log-select-pick-function)
        (rev (magit-commit-at-point)))
    (quit-restore-window nil 'kill)
    (funcall fun rev)))

(defun magit-log-select-quit ()
  "Abort selecting a commit, don't act on any commit."
  (interactive)
  (quit-restore-window nil 'kill)
  (when magit-log-select-quit-function
    (funcall magit-log-select-quit-function)))

;;; Cherry Mode

(defvar magit-cherry-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "q" 'magit-log-bury-buffer)
    (define-key map "L" 'magit-margin-popup)
    map)
  "Keymap for `magit-cherry-mode'.")

(define-derived-mode magit-cherry-mode magit-mode "Magit Cherry"
  "Mode for looking at commits not merged upstream.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[magit-cherry-pick-popup] to apply the commit at point.

\\{magit-cherry-mode-map}"
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload
(defun magit-cherry (head upstream)
  "Show commits in a branch that are not merged in the upstream branch."
  (interactive
   (let  ((head (magit-read-branch "Cherry head")))
     (list head (magit-read-other-branch "Cherry upstream" head
                                         (magit-get-upstream-branch head)))))
  (require 'magit)
  (magit-mode-setup #'magit-cherry-mode upstream head))

(defun magit-cherry-refresh-buffer (_upstream _head)
  (magit-insert-section (cherry)
    (run-hooks 'magit-cherry-sections-hook)))

(defun magit-insert-cherry-headers ()
  "Insert headers appropriate for `magit-cherry-mode' buffers."
  (magit-insert-head-branch-header (nth 1 magit-refresh-args))
  (magit-insert-upstream-branch-header (nth 1 magit-refresh-args)
                                       (nth 0 magit-refresh-args)
                                       "Upstream: ")
  (insert ?\n))

(defun magit-insert-cherry-commits ()
  "Insert commit sections into a `magit-cherry-mode' buffer."
  (magit-insert-section (cherries)
    (magit-insert-heading "Cherry commits:")
    (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
      "cherry" "-v" "--abbrev" magit-refresh-args)))

;;; Reflog Mode

(defvar magit-reflog-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-log-mode-map)
    (define-key map "L" 'magit-margin-popup)
    map)
  "Keymap for `magit-reflog-mode'.")

(define-derived-mode magit-reflog-mode magit-log-mode "Magit Reflog"
  "Mode for looking at Git reflog.

This mode is documented in info node `(magit)Reflog'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-visit-thing] or \\[magit-diff-show-or-scroll-up] \
to visit the commit at point.

Type \\[magit-cherry-pick-popup] to apply the commit at point.
Type \\[magit-reset] to reset `HEAD' to the commit at point.

\\{magit-reflog-mode-map}"
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer))

(defun magit-reflog-refresh-buffer (ref args)
  (setq header-line-format
        (propertize (concat " Reflog for " ref) 'face 'magit-header-line))
  (magit-insert-section (reflogbuf)
    (magit-git-wash (apply-partially 'magit-log-wash-log 'reflog)
      "reflog" "show" "--format=%h%x00%aN%x00%gd%x00%gs" "--date=raw"
      args ref "--")))

(defvar magit-reflog-labels
  '(("commit"      . magit-reflog-commit)
    ("amend"       . magit-reflog-amend)
    ("merge"       . magit-reflog-merge)
    ("checkout"    . magit-reflog-checkout)
    ("branch"      . magit-reflog-checkout)
    ("reset"       . magit-reflog-reset)
    ("rebase"      . magit-reflog-rebase)
    ("cherry-pick" . magit-reflog-cherry-pick)
    ("initial"     . magit-reflog-commit)
    ("pull"        . magit-reflog-remote)
    ("clone"       . magit-reflog-remote)
    ("autosave"    . magit-reflog-commit)
    ("restart"     . magit-reflog-reset)))

(defun magit-reflog-format-subject (subject)
  (let* ((match (string-match magit-reflog-subject-re subject))
         (command (and match (match-string 1 subject)))
         (option  (and match (match-string 2 subject)))
         (type    (and match (match-string 3 subject)))
         (label (if (string= command "commit")
                    (or type command)
                  command))
         (text (if (string= command "commit")
                   label
                 (mapconcat #'identity
                            (delq nil (list command option type))
                            " "))))
    (format "%-16s "
            (propertize text 'face
                        (or (cdr (assoc label magit-reflog-labels))
                            'magit-reflog-other)))))

;;; Log Sections
;;;; Standard Log Sections

(defvar magit-unpulled-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] 'magit-diff-dwim)
    map)
  "Keymap for `unpulled' sections.")

(magit-define-section-jumper magit-jump-to-unpulled-from-upstream
  "Unpulled from @{upstream}" unpulled "..@{upstream}")

(defun magit-insert-unpulled-from-upstream ()
  "Insert commits that haven't been pulled from the upstream yet."
  (when (magit-git-success "rev-parse" "@{upstream}")
    (magit-insert-section (unpulled "..@{upstream}")
      (magit-insert-heading
        (format (propertize "Unpulled from %s:" 'face 'magit-section-heading)
                (magit-get-upstream-branch)))
      (magit-insert-log "..@{upstream}" magit-log-section-arguments))))

(magit-define-section-jumper magit-jump-to-unpulled-from-pushremote
  "Unpulled from <push-remote>" unpulled
  (concat ".." (magit-get-push-branch)))

(defun magit-insert-unpulled-from-pushremote ()
  "Insert commits that haven't been pulled from the push-remote yet."
  (--when-let (magit-get-push-branch)
    (unless (and (equal (magit-rev-name it)
                        (magit-rev-name "@{upstream}"))
                 (or (memq 'magit-insert-unpulled-from-upstream
                           magit-status-sections-hook)
                     (memq 'magit-insert-unpulled-from-upstream-or-recent
                           magit-status-sections-hook)))
      (magit-insert-section (unpulled (concat ".." it))
        (magit-insert-heading
          (format (propertize "Unpulled from %s:" 'face 'magit-section-heading)
                  (propertize it 'face 'magit-branch-remote)))
        (magit-insert-log (concat ".." it) magit-log-section-arguments)))))

(defvar magit-unpushed-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] 'magit-diff-dwim)
    map)
  "Keymap for `unpushed' sections.")

(magit-define-section-jumper magit-jump-to-unpushed-to-upstream
  "Unpushed to @{upstream}" unpushed "@{upstream}..")

(defun magit-insert-unpushed-to-upstream ()
  "Insert commits that haven't been pushed to the upstream yet."
  (when (magit-git-success "rev-parse" "@{upstream}")
    (magit-insert-section (unpushed "@{upstream}..")
      (magit-insert-heading
        (format (propertize "Unmerged into %s:" 'face 'magit-section-heading)
                (magit-get-upstream-branch)))
      (magit-insert-log "@{upstream}.." magit-log-section-arguments))))

(magit-define-section-jumper magit-jump-to-unpushed-to-pushremote
  "Unpushed to <push-remote>" unpushed
  (concat (magit-get-push-branch) ".."))

(defun magit-insert-unpushed-to-pushremote ()
  "Insert commits that haven't been pushed to the push-remote yet."
  (--when-let (magit-get-push-branch)
    (unless (and (equal (magit-rev-name it)
                        (magit-rev-name "@{upstream}"))
                 (memq 'magit-insert-unpushed-to-upstream
                       magit-status-sections-hook))
      (magit-insert-section (unpushed (concat it ".."))
        (magit-insert-heading
          (format (propertize "Unpushed to %s:" 'face 'magit-section-heading)
                  (propertize it 'face 'magit-branch-remote)))
        (magit-insert-log (concat it "..") magit-log-section-arguments)))))

(defun magit-insert-recent-commits (&optional collapse)
  "Insert section showing recent commits.
Show the last `magit-log-section-commit-count' commits."
  (let* ((start (format "HEAD~%s" magit-log-section-commit-count))
         (range (and (magit-rev-verify start)
                     (concat start "..HEAD"))))
    (magit-insert-section (recent range collapse)
      (magit-insert-heading "Recent commits:")
      (magit-insert-log range
                        (cons (format "-%d" magit-log-section-commit-count)
                              magit-log-section-arguments)))))

(defun magit-insert-unpulled-from-upstream-or-recent ()
  "Insert section showing unpulled or recent commits.
If an upstream is configured for the current branch and it is
ahead of the current branch, then show the commits that have
not yet been pulled into the current branch.  If no upstream is
configured or if the upstream is not ahead of the current branch,
then show the last `magit-log-section-commit-count' commits."
  (let ((upstream (magit-rev-parse "@{upstream}")))
    (if (or (not upstream)
            (equal upstream (magit-rev-parse "HEAD")))
        (magit-insert-recent-commits t)
      (magit-insert-unpulled-from-upstream))))

;;;; Auxiliary Log Sections

(defun magit-insert-unpulled-cherries ()
  "Insert section showing unpulled commits.
Like `magit-insert-unpulled-from-upstream' but prefix each commit
which has not been applied yet (i.e. a commit with a patch-id
not shared with any local commit) with \"+\", and all others with
\"-\"."
  (when (magit-git-success "rev-parse" "@{upstream}")
    (magit-insert-section (unpulled "..@{upstream}")
      (magit-insert-heading "Unpulled commits:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg)
        (magit-get-current-branch) "@{upstream}"))))

(defun magit-insert-unpushed-cherries ()
  "Insert section showing unpushed commits.
Like `magit-insert-unpushed-to-upstream' but prefix each commit
which has not been applied to upstream yet (i.e. a commit with
a patch-id not shared with any upstream commit) with \"+\", and
all others with \"-\"."
  (when (magit-git-success "rev-parse" "@{upstream}")
    (magit-insert-section (unpushed "@{upstream}..")
      (magit-insert-heading "Unpushed commits:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
        "cherry" "-v" (magit-abbrev-arg) "@{upstream}"))))

(provide 'magit-log)
;;; magit-log.el ends here
