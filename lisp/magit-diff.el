;;; magit-diff.el --- inspect Git diffs

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

;; This library implements support for looking at Git diffs and
;; commits.

;;; Code:

(require 'git-commit)
(require 'magit-core)

;; For `magit-diff-popup'
(declare-function magit-stash-show 'magit-stash)
;; For `magit-diff-visit-file'
(declare-function magit-dired-jump 'magit)
(declare-function magit-find-file-noselect 'magit)
(declare-function magit-status-internal 'magit)
;; For `magit-diff-wash-revision'
(declare-function magit-insert-tags-header 'magit)
;; For `magit-diff-while-committing'
(declare-function magit-commit-message-buffer 'magit)
;; For `magit-show-commit' and `magit-diff-show-or-scroll'
(declare-function magit-blame-chunk-get 'magit-blame)
(declare-function magit-blame-mode 'magit-blame)
(defvar magit-blame-mode)

(require 'diff-mode)
(require 'smerge-mode)

;;; Options
;;;; Diff Mode

(defgroup magit-diff nil
  "Inspect and manipulate Git diffs."
  :group 'magit-modes)

(custom-add-to-group 'magit-diff 'smerge-refine-ignore-whitespace
                     'custom-variable)

(defcustom magit-diff-mode-hook nil
  "Hook run after entering Magit-Diff mode."
  :group 'magit-diff
  :type 'hook)

(defcustom magit-diff-buffer-name-format "*magit-diff: %a*"
  "Name format for buffers used to display a diff.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'string)

(defcustom magit-diff-switch-buffer-function 'pop-to-buffer
  "Function used to display and possibly select a diff buffer.

By default `pop-to-buffer' is used to display the diff buffer in
another window.  If the value is nil, then that function is also
used, except when the current buffer is another Magit buffer.
Then the window is reused; the diff buffer replaces the buffer
which was previously shown.  Another function can be used, but
that's not recommended, e.g. `switch-to-buffer' likely is not
what you want.

Note that the value of this variable is ignored when the diff
buffer is automatically shown along side a buffer used to write
a commit message."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type '(choice (function-item pop-to-buffer)
                 (function nil)
                 (const :tag "Context sensitive (nil)" nil)))

(defcustom magit-diff-expansion-threshold 1.0
  "After how many seconds not to expand anymore diffs.

Except in status buffers, diffs are usually start out fully
expanded.  Because that can take a long time, all diffs that
haven't been fontified during a refresh before the treshold
defined here are instead displayed with their bodies collapsed.

Note that this can cause sections that were previously expanded
to be collapsed.  So you should not pick a very low value here.

The hook function `magit-diff-expansion-treshold' has to be a
member of `magit-section-set-visibility-hook' for this option
to have any effect"
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'float)

(defcustom magit-diff-highlight-hunk-body t
  "Whether to highlight bodies of selected hunk sections.
This only has an effect if `magit-diff-highlight' is a
member of `magit-section-highlight-hook', which see."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-show-lines-boundary t
  "Whether to delimit hunk-internal region with thin lines.

When a hunk-internal region (used to stage just the lines that
fall into the region instead of the complete hunk) only covers
context lines, then these lines are the only visual indicator
for the region.  In character-only terminals it's not possible
to draw thin lines."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-show-diffstat t
  "Whether to show diffstat in diff buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-show-xref-buttons t
  "Whether to show buffer history buttons in diff buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-auto-show
  '(commit stage-all log-oneline log-select blame-follow)
  "Whether to automatically show relevant diff or commit.

When this option is non-nil certain operations cause the relevant
changes to be displayed automatically.

`commit'
`stage-all'
`log-oneline'
`log-follow'
`log-select'
`blame-follow'

In the event that expanding very large patches takes a long time
\\<global-map>\\[keyboard-quit] can be used to abort that step.
This is especially useful when you would normally not look at the
changes, e.g. because you are committing some binary files."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'sexp)

(defun magit-diff-auto-show-p (op)
  (if (eq (car magit-diff-auto-show) 'not)
      (not (memq op (cdr magit-diff-auto-show)))
    (memq op magit-diff-auto-show)))

(defcustom magit-diff-refine-hunk nil
  "Whether to show word-granularity differences within diff hunks.

nil    never show fine differences.
t      show fine differences for the current diff hunk only.
`all'  show fine differences for all displayed diff hunks."
  :group 'magit-diff
  :safe (lambda (val) (memq val '(nil t all)))
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Current" t)
                 (const :tag "All" all)))

(put 'magit-diff-refine-hunk 'permanent-local t)

(defcustom magit-diff-paint-whitespace t
  "Specify where to highlight whitespace errors.
See `magit-highlight-trailing-whitespace',
`magit-highlight-indentation'.  The symbol t means in all diffs,
`status' means only in the status buffer, and nil means nowhere."
  :group 'magit-diff
  :safe (lambda (val) (memq val '(t nil status)))
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In status buffer" status)))

(defcustom magit-diff-highlight-trailing t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `magit-diff-paint-whitespace' is non-nil."
  :group 'magit-diff
  :safe 'booleanp
  :type 'boolean)

(defcustom magit-diff-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `magit-diff-paint-whitespace' is non-nil.

The value is a list of cons cells.  The car is a regular
expression, and the cdr is the value that applies to repositories
whose directory matches the regular expression.  If more than one
element matches, then the *last* element in the list applies.
The default value should therefore come first in the list.

If the value is `tabs', highlight indentation with tabs.  If the
value is an integer, highlight indentation with at least that
many spaces.  Otherwise, highlight neither."
  :group 'magit-diff
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil)))))

;;;; Revision Mode

(defgroup magit-revision nil
  "Inspect and manipulate Git commits."
  :group 'magit-modes)

(defcustom magit-revision-mode-hook nil
  "Hook run after entering Magit-Revision mode."
  :group 'magit-revision
  :type 'hook)

(defcustom magit-revision-buffer-name-format "*magit-rev: %a*"
  "Name format for buffers used to display a commit.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type 'string)

(defcustom magit-revision-show-diffstat t
  "Whether to show diffstat in revision buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type 'boolean)

(defcustom magit-revision-show-notes t
  "Whether to show notes in revision buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :safe 'booleanp
  :type 'boolean)

(defcustom magit-revision-show-xref-buttons t
  "Whether to show buffer history buttons in revision buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type 'boolean)

(defcustom magit-revision-insert-related-refs t
  "Whether to show related refs in revision buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type 'boolean)

;;; Faces

(defface magit-diff-file-heading
  '((t :weight bold))
  "Face for diff file headings."
  :group 'magit-faces)

(defface magit-diff-file-heading-highlight
  '((t :inherit (magit-diff-file-heading magit-section-highlight)))
  "Face for current diff file headings."
  :group 'magit-faces)

(defface magit-diff-file-heading-selection
  '((((class color) (background light))
     :inherit magit-diff-file-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     :inherit magit-diff-file-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff file headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading
  '((((class color) (background light))
     :background "grey80"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey25"
     :foreground "grey70"))
  "Face for diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading-highlight
  '((((class color) (background light))
     :background "grey75"
     :foreground "grey30")
    (((class color) (background dark))
     :background "grey35"
     :foreground "grey70"))
  "Face for current diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading-selection
  '((((class color) (background light))
     :inherit magit-diff-hunk-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     :inherit magit-diff-hunk-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-lines-heading
  '((((class color) (background light))
     :inherit magit-diff-hunk-heading-highlight
     :background "LightSalmon3")
    (((class color) (background dark))
     :inherit magit-diff-hunk-heading-highlight
     :foreground "grey80"
     :background "salmon4"))
  "Face for diff hunk heading when lines are marked."
  :group 'magit-faces)

(defface magit-diff-lines-boundary
  '((t :inherit magit-diff-lines-heading))
  "Face for boundary of marked lines in diff hunk."
  :group 'magit-faces)

(defface magit-diff-conflict-heading
  '((t :inherit magit-diff-hunk-heading))
  "Face for conflict markers."
  :group 'magit-faces)

(defface magit-diff-added
  '((((class color) (background light))
     :background "#ddffdd"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#335533"
     :foreground "#aaccaa"))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed
 '((((class color) (background light))
     :background "#ffdddd"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#553333"
     :foreground "#ccaaaa"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-our
  '((t :inherit magit-diff-removed))
  "Face for lines in a diff for our side in a conflict."
  :group 'magit-faces)

(defface magit-diff-base
  '((((class color) (background light))
     :background "#ffffcc"
     :foreground "#aaaa11")
    (((class color) (background dark))
     :background "#555522"
     :foreground "#ccccbb"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'magit-faces)

(defface magit-diff-their
  '((t :inherit magit-diff-added))
  "Face for lines in a diff for their side in a conflict."
  :group 'magit-faces)

(defface magit-diff-context
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'magit-faces)

(defface magit-diff-added-highlight
  '((((class color) (background light))
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :background "#336633"
     :foreground "#bbddbb"))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed-highlight
  '((((class color) (background light))
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     :background "#663333"
     :foreground "#ddbbbb"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-our-highlight
  '((t :inherit magit-diff-removed-highlight))
  "Face for lines in a diff for our side in a conflict."
  :group 'magit-faces)

(defface magit-diff-base-highlight
  '((((class color) (background light))
     :background "#eeeebb"
     :foreground "#aaaa11")
    (((class color) (background dark))
     :background "#666622"
     :foreground "#ddddaa"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'magit-faces)

(defface magit-diff-their-highlight
  '((t :inherit magit-diff-added-highlight))
  "Face for lines in a diff for their side in a conflict."
  :group 'magit-faces)

(defface magit-diff-context-highlight
  '((((class color) (background light))
     :background "grey85"
     :foreground "grey50")
    (((class color) (background dark))
     :background "grey20"
     :foreground "grey70"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-whitespace-warning
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors added lines."
  :group 'magit-faces)

(defface magit-diffstat-added
  '((((class color) (background light)) :foreground "#22aa22")
    (((class color) (background  dark)) :foreground "#448844"))
  "Face for plus sign in diffstat."
  :group 'magit-faces)

(defface magit-diffstat-removed
  '((((class color) (background light)) :foreground "#aa2222")
    (((class color) (background  dark)) :foreground "#aa4444"))
  "Face for minus sign in diffstat."
  :group 'magit-faces)

;;; Commands

;;;###autoload (autoload 'magit-diff-popup "magit-diff" nil t)
(magit-define-popup magit-diff-popup
  "Popup console for diff commands."
  'magit-diff
  :man-page "git-diff"
  :switches '((?f "Show surrounding functions" "--function-context")
              (?b "Ignore whitespace changes"  "--ignore-space-change")
              (?w "Ignore all whitespace"      "--ignore-all-space"))
  :options  '((?f "Limit to files" "-- " magit-read-files)
              (?u "Context lines"  "-U"  read-from-minibuffer)
              (?m "Detect renames" "-M"  read-from-minibuffer)
              (?c "Detect copies"  "-C"  read-from-minibuffer)
              (?a "Diff algorithm" "--diff-algorithm="
                  magit-diff-select-algorithm))
  :actions  '((?d "Dwim"          magit-diff-dwim)
              (?u "Diff unstaged" magit-diff-unstaged)
              (?c "Show commit"   magit-show-commit)
              (?r "Diff commits"  magit-diff)
              (?s "Diff staged"   magit-diff-staged)
              (?t "Show stash"    magit-stash-show)
              (?p "Diff paths"    magit-diff-paths)
              (?w "Diff worktree" magit-diff-working-tree))
  :default-action 'magit-diff-dwim
  :max-action-columns 3)

(defun magit-diff-read-args (&optional no-files)
  (let* ((args  (magit-diff-arguments))
         (files (--first (string-prefix-p "-- " it) args)))
    (when files
      (setq args  (delete files args)
            files (split-string (substring files 3) ",")))
    (if no-files
        (list args)
      (list args files))))

(with-no-warnings
  (magit-define-popup magit-diff-refresh-popup
    "Popup console for changing diff arguments in the current buffer."
    'magit-diff nil 'magit-diff-section-arguments
    :man-page "git-diff"
    :switches '((?f "Show surrounding functions" "--function-context")
                (?b "Ignore whitespace changes"  "--ignore-space-change")
                (?w "Ignore all whitespace"      "--ignore-all-space"))
    :options  '((?u "Context lines"  "-U" read-from-minibuffer)
                (?m "Detect renames" "-M" read-from-minibuffer)
                (?c "Detect copies"  "-C" read-from-minibuffer)
                (?a "Diff algorithm" "--diff-algorithm="
                    magit-diff-select-algorithm))
    :actions  '((?g "Refresh"       magit-diff-refresh)
                (?s "Set defaults"  magit-diff-set-default-arguments)
                (?w "Save defaults" magit-diff-save-default-arguments)
                (?t "Toggle hunk refinement" magit-diff-toggle-refine-hunk))))

(defadvice magit-diff-refresh-popup (around get-current-arguments activate)
  (if (derived-mode-p 'magit-diff-mode)
      (let ((magit-diff-section-arguments (cadr magit-refresh-args)))
        ad-do-it)
    ad-do-it))

(defun magit-diff-select-algorithm (&rest _ignore)
  (magit-read-char-case nil t
    (?d "[d]efault"   "default")
    (?m "[m]inimal"   "minimal")
    (?p "[p]atience"  "patience")
    (?h "[h]istogram" "histogram")))

;;;###autoload
(defun magit-diff-dwim (&optional args files)
  "Show changes for the thing at point."
  (interactive (magit-diff-read-args))
  (--if-let (magit-region-values 'commit 'branch)
      (progn (deactivate-mark)
             (magit-diff (concat (car (last it)) ".." (car it))))
    (--when-let (magit-current-section)
      (let ((value (magit-section-value it)))
        (magit-section-case
          ([* unstaged] (magit-diff-unstaged args))
          ([* staged] (magit-diff-staged nil args))
          (unpushed (magit-diff-unpushed args))
          (unpulled (magit-diff-unpulled args))
          (branch   (-if-let (tracked (magit-get-tracked-ref value))
                        (magit-diff (format "%s...%s" tracked value) args)
                      (call-interactively 'magit-diff)))
          (commit   (magit-show-commit value nil nil args))
          (stash    (magit-stash-show  value nil args))
          (t        (call-interactively 'magit-diff)))))))

;;;###autoload
(defun magit-diff (range &optional args files)
  "Show differences between two commits.
RANGE should be a range (A..B or A...B) but can also be a single
commit.  If one side of the range is omitted, then it defaults
to HEAD.  If just a commit is given, then changes in the working
tree relative to that commit are shown."
  (interactive (cons (magit-read-range-or-commit "Diff for range")
                     (magit-diff-read-args)))
  (magit-mode-setup magit-diff-buffer-name-format
                    magit-diff-switch-buffer-function
                    #'magit-diff-mode
                    #'magit-diff-refresh-buffer range args files))

;;;###autoload
(defun magit-diff-working-tree (&optional rev args files)
  "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (magit-read-branch-or-commit "Diff working tree and commit"))
         (magit-diff-read-args)))
  (magit-diff (or rev "HEAD") args files))

;;;###autoload
(defun magit-diff-staged (&optional rev args files)
  "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (magit-read-branch-or-commit "Diff index and commit"))
         (magit-diff-read-args)))
  (magit-diff rev (cons "--cached" args)))

;;;###autoload
(defun magit-diff-unstaged (&optional args files)
  "Show changes between the working tree and the index."
  (interactive (magit-diff-read-args))
  (magit-diff nil args files))

;;;###autoload
(defun magit-diff-unpushed (&optional args files)
  "Show unpushed changes."
  (interactive (magit-diff-read-args))
  (-if-let (tracked (magit-get-tracked-ref))
      (magit-diff (concat tracked "...") args files)
    (error "Detached HEAD or upstream unset")))

;;;###autoload
(defun magit-diff-unpulled (&optional args files)
  "Show unpulled changes."
  (interactive (magit-diff-read-args))
  (-if-let (tracked (magit-get-tracked-ref))
      (magit-diff (concat "..." tracked) args files)
    (error "Detached HEAD or upstream unset")))

;;;###autoload
(defun magit-diff-while-committing (&optional args)
  "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be commited."
  (interactive (magit-diff-read-args t))
  (let* ((toplevel (magit-toplevel))
         (diff-buf (magit-mode-get-buffer magit-diff-buffer-name-format
                                          'magit-diff-mode toplevel)))
    (if (magit-commit-message-buffer)
        (if (and (or ;; most likely an explicit amend
                     (not (magit-anything-staged-p))
                     ;; explicitly toggled from within diff
                     (and (eq (current-buffer) diff-buf)))
                 (or (not diff-buf)
                     (with-current-buffer diff-buf
                       (or ;; default to include last commit
                           (not (equal (magit-toplevel) toplevel))
                           ;; toggle to include last commit
                           (not (car magit-refresh-args))))))
            (magit-diff-while-amending args)
          (magit-diff-staged nil args))
      (user-error "No commit in progress"))))

(define-key git-commit-mode-map
  (kbd "C-c C-d") 'magit-diff-while-committing)

(defun magit-diff-while-amending (&optional args)
  (magit-diff "HEAD^" (cons "--cached" args)))

;;;###autoload
(defun magit-diff-paths (a b)
  "Show changes between any two files on disk."
  (interactive (list (read-file-name "First file: " nil nil t)
                     (read-file-name "Second file: " nil nil t)))
  (magit-diff nil
              (list "--no-index")
              (list (expand-file-name a)
                    (expand-file-name b))))

(defvar-local magit-diff-hidden-files nil)
(put 'magit-diff-hidden-files 'permanent-local t)

;;;###autoload
(defun magit-show-commit (commit &optional noselect module args)
  "Show the commit at point.
If there is no commit at point or with a prefix argument prompt
for a commit."
  (interactive
   (let* ((mcommit (magit-section-when module-commit))
          (atpoint (or (and magit-blame-mode (magit-blame-chunk-get :hash))
                       mcommit
                       (magit-branch-or-commit-at-point)
                       (magit-tag-at-point))))
     (nconc (list (or (and (not current-prefix-arg) atpoint)
                      (magit-read-branch-or-commit "Show commit" atpoint))
                  nil (and mcommit (magit-section-parent-value
                                    (magit-current-section))))
            (magit-diff-read-args t))))
  (let ((default-directory (if module
                               (file-name-as-directory
                                (expand-file-name module (magit-toplevel)))
                             default-directory)))
    (unless (magit-rev-verify commit)
      (user-error "%s is not a commit" commit))
    (-when-let (buffer (magit-mode-get-buffer
                        magit-revision-buffer-name-format
                        'magit-revision-mode))
      (with-current-buffer buffer
        (let ((prev (car magit-refresh-args)))
          (unless (equal commit prev)
            (dolist (child (cdr (magit-section-children magit-root-section)))
              (when (eq (magit-section-type child) 'file)
                (let ((file (magit-section-value child)))
                  (if (magit-section-hidden child)
                      (add-to-list 'magit-diff-hidden-files file)
                    (setq magit-diff-hidden-files
                          (delete file magit-diff-hidden-files))))))))))
    (magit-mode-setup magit-revision-buffer-name-format
                      (if noselect 'display-buffer 'pop-to-buffer)
                      #'magit-revision-mode
                      #'magit-revision-refresh-buffer
                      commit args)))

(defun magit-diff-refresh-arguments ()
  (cond ((memq magit-current-popup '(magit-diff-popup magit-diff-refresh-popup))
         magit-current-popup-args)
        ((derived-mode-p 'magit-diff-mode)
         (--filter (not (member it '("--cached" "--no-index" "--")))
                   (cadr magit-refresh-args)))
        (t
         magit-diff-section-arguments)))

(defun magit-diff-refresh (args)
  "Set the local diff arguments for the current buffer."
  (interactive (list (magit-diff-refresh-arguments)))
  (cond ((derived-mode-p 'magit-diff-mode)
         (setq magit-refresh-args (list (car magit-refresh-args) args)))
        (t
         (setq magit-diff-section-arguments args)))
  (magit-refresh))

(defun magit-diff-set-default-arguments (args)
  "Set the global diff arguments for the current buffer."
  (interactive (list (magit-diff-refresh-arguments)))
  (cond ((derived-mode-p 'magit-diff-mode)
         (customize-set-variable 'magit-diff-arguments args)
         (setq magit-refresh-args (list (car magit-refresh-args) args)))
        (t
         (customize-set-variable 'magit-diff-section-arguments args)
         (kill-local-variable 'magit-diff-section-arguments)))
  (magit-refresh))

(defun magit-diff-save-default-arguments (args)
  "Set and save the global diff arguments for the current buffer."
  (interactive (list (magit-diff-refresh-arguments)))
  (cond ((derived-mode-p 'magit-diff-mode)
         (customize-save-variable 'magit-diff-arguments args)
         (setq magit-refresh-args (list (car magit-refresh-args) args)))
        (t
         (customize-save-variable 'magit-diff-section-arguments args)
         (kill-local-variable 'magit-diff-section-arguments)))
  (magit-refresh))

(defun magit-diff-less-context (&optional count)
  "Decrease the context for diff hunks by COUNT lines."
  (interactive "p")
  (magit-diff-set-context `(lambda (cur) (max 0 (- (or cur 0) ,count)))))

(defun magit-diff-more-context (&optional count)
  "Increase the context for diff hunks by COUNT lines."
  (interactive "p")
  (magit-diff-set-context `(lambda (cur) (+ (or cur 0) ,count))))

(defun magit-diff-default-context ()
  "Reset context for diff hunks to the default height."
  (interactive)
  (magit-diff-set-context #'ignore))

(defun magit-diff-set-context (fn)
  (let* ((def (--if-let (magit-get "diff.context") (string-to-number it) 3))
         (val (magit-diff-refresh-arguments))
         (arg (--first (string-match "^-U\\([0-9]+\\)?$" it) val))
         (num (--if-let (match-string 1 arg) (string-to-number it) def))
         (val (delete arg val))
         (num (funcall fn num))
         (arg (and num (not (= num def)) (format "-U%i" num)))
         (val (if arg (cons arg val) val)))
    (if (derived-mode-p 'magit-diff-mode)
        (setq magit-refresh-args (list (car magit-refresh-args) val))
      (setq magit-diff-section-arguments val)))
  (magit-refresh))

(defun magit-diff-context-p ()
  (--if-let (--first (string-match "^-U\\([0-9]+\\)$" it)
                     (magit-diff-arguments))
      (not (equal "-U0" it))
    t))

(defun magit-diff-toggle-refine-hunk (&optional style)
  "Turn diff-hunk refining on or off.

If hunk refining is currently on, then hunk refining is turned off.
If hunk refining is off, then hunk refining is turned on, in
`selected' mode (only the currently selected hunk is refined).

With a prefix argument, the \"third choice\" is used instead:
If hunk refining is currently on, then refining is kept on, but
the refining mode (`selected' or `all') is switched.
If hunk refining is off, then hunk refining is turned on, in
`all' mode (all hunks refined).

Customize variable `magit-diff-refine-hunk' to change the default mode."
  (interactive "P")
  (setq-local magit-diff-refine-hunk
              (if style
                  (if (eq magit-diff-refine-hunk 'all) t 'all)
                (not magit-diff-refine-hunk)))
  (magit-diff-update-hunk-refinement))

(defun magit-diff-visit-file (file &optional other-window force-worktree)
  "From a diff, visit the corresponding file at the appropriate position.

When the file is already being displayed in another window of the
same frame, then just select that window and adjust point.  With
a prefix argument also display in another window.

If the diff shows changes in the worktree, the index, or `HEAD',
then visit the actual file.  Otherwise when the diff is about
an older commit, then visit the respective blob using
`magit-find-file'.  Also see `magit-diff-visit-file-worktree'
which, as the name suggests always visits the actual file."
  (interactive (list (or (magit-file-at-point)
                         (user-error "No file at point"))
                     current-prefix-arg))
  (if (file-accessible-directory-p file)
      (magit-diff-visit-directory file other-window)
    (let ((current (magit-current-section))
          rev diff hunk line col buffer)
      (when (and (derived-mode-p 'magit-revision-mode)
                 (not force-worktree))
        (setq rev (car (last magit-refresh-args 2)))
        (when (magit-rev-head-p rev)
          (setq rev nil)))
      (setq hunk
            (pcase (magit-diff-scope)
              ((or `hunk `region) current)
              ((or `file `files)  (car (magit-section-children current)))
              (`list (car (magit-section-children
                           (car (magit-section-children current)))))))
      (when hunk
        (setq line (magit-diff-hunk-line   hunk)
              col  (magit-diff-hunk-column hunk)))
      (setq buffer (if rev
                       (magit-find-file-noselect rev file)
                     (or (get-file-buffer file)
                         (find-file-noselect file))))
      (if (or other-window (get-buffer-window buffer))
          (pop-to-buffer buffer)
        (switch-to-buffer buffer))
      (when line
        (goto-char (point-min))
        (forward-line (1- line))
        (when col
          (move-to-column col)))))
  (when (magit-anything-unmerged-p file)
    (smerge-start-session)))

(defun magit-diff-visit-file-worktree (file &optional other-window)
  "From a diff, visit the corresponding file at the appropriate position.

When the file is already being displayed in another window of the
same frame, then just select that window and adjust point.  With
a prefix argument also display in another window.

The actual file in the worktree is visited. The positions in the
hunk headers get less useful the \"older\" the changes are, and
as a result, jumping to the appropriate position gets less
reliable.

Also see `magit-diff-visit-file-worktree' which visits the
respective blob, unless the diff shows changes in the worktree,
the index, or `HEAD'."
  (interactive (list (or (magit-file-at-point)
                         (user-error "No file at point"))
                     current-prefix-arg))
  (magit-diff-visit-file file other-window t))

(defun magit-diff-hunk-line (section)
  (let* ((value  (magit-section-value section))
         (prefix (- (length value) 2))
         (stop   (line-number-at-pos))
         (line   (car (last value))))
    (string-match "^\\+\\([0-9]+\\)" line)
    (setq line (string-to-number (match-string 1 line)))
    (save-excursion
      (goto-char (magit-section-content section))
      (while (< (line-number-at-pos) stop)
        (unless (string-match-p
                 "-" (buffer-substring (point) (+ (point) prefix)))
          (cl-incf line))
        (forward-line)))
    line))

(defun magit-diff-hunk-column (section)
  (if (looking-at "-")
      0
    (max 0 (- (+ (current-column) 2)
              (length (magit-section-value section))))))

(defun magit-diff-visit-directory (directory &optional other-window)
  (if (equal (magit-toplevel directory)
             (magit-toplevel))
      (magit-dired-jump other-window)
    (magit-status-internal directory (if other-window
                                         'pop-to-buffer
                                       'switch-to-buffer))))

(defun magit-diff-show-or-scroll-up ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer up.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-diff-show-or-scroll 'scroll-up))

(defun magit-diff-show-or-scroll-down ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer down.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-diff-show-or-scroll 'scroll-down))

(defun magit-diff-show-or-scroll (fn)
  (let (rev cmd buf win)
    (if magit-blame-mode
        (setq rev (magit-blame-chunk-get :hash)
              cmd 'magit-show-commit
              buf (magit-mode-get-buffer
                   magit-revision-buffer-name-format 'magit-revision-mode))
      (magit-section-case
        (commit (setq rev (magit-section-value it)
                      cmd 'magit-show-commit
                      buf (magit-mode-get-buffer
                           magit-revision-buffer-name-format 'magit-revision-mode)))
        (stash  (setq rev (magit-section-value it)
                      cmd 'magit-stash-show
                      buf (magit-mode-get-buffer
                           magit-diff-buffer-name-format 'magit-diff-mode)))))
    (if rev
        (if (and buf
                 (setq win (get-buffer-window buf))
                 (with-current-buffer buf
                   (equal (if (eq cmd 'magit-stash-show)
                              (concat rev "^2^.." rev)
                            rev)
                          (car magit-refresh-args))))
            (with-selected-window win
              (condition-case err
                  (funcall fn)
                (error
                 (goto-char (pcase fn
                              (`scroll-up   (point-min))
                              (`scroll-down (point-max)))))))
          (funcall cmd rev t))
      (call-interactively 'magit-show-commit))))

;;; Diff Mode

(defvar magit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-c\C-d" 'magit-diff-while-committing)
    (define-key map "\C-c\C-b" 'magit-go-backward)
    (define-key map "\C-c\C-f" 'magit-go-forward)
    (define-key map "\s" 'scroll-up)
    (define-key map "\d" 'scroll-down)
    (define-key map "j" 'magit-jump-to-diffstat-or-diff)
    map)
  "Keymap for `magit-diff-mode'.")

(define-derived-mode magit-diff-mode magit-mode "Magit Diff"
  "Mode for looking at a Git diff.
This mode is documented in info node `(magit)Diffing'.

\\<magit-diff-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-diff-visit-file] to visit the file at point.
Type \\[magit-apply] to apply the change at point to the worktree.
Type \\[magit-reverse] to reverse the change at point in the worktree.
\n\\{magit-diff-mode-map}"
  :group 'magit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun magit-diff-refresh-buffer (range &optional args files)
  (setq header-line-format
        (propertize
         (if (member "--no-index" args)
             (apply #'format " Differences between %s and %s" files)
           (concat (if range
                       (if (string-match-p "\\.\\." range)
                           (format " Changes in %s" range)
                         (format " Changes from %s to working tree" range))
                     (if (member "--cached" args)
                         " Staged changes"
                       " Unstaged changes"))
                   (pcase (length files)
                     (0)
                     (1 (concat " in file " (car files)))
                     (_ (concat " in files "
                                (mapconcat #'identity files ", "))))))
         'face 'magit-header-line))
    (magit-insert-section (diffbuf)
      (magit-git-wash #'magit-diff-wash-diffs
        "diff" range "-p" (and magit-diff-show-diffstat "--stat")
        "--no-prefix" args "--" files)))

(defvar magit-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-return] 'magit-diff-visit-file-worktree)
    (define-key map "\r" 'magit-diff-visit-file)
    (define-key map "a"  'magit-apply)
    (define-key map "k"  'magit-discard)
    (define-key map "K"  'magit-file-untrack)
    (define-key map "R"  'magit-file-rename)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    (define-key map "v"  'magit-reverse)
    map)
  "Keymap for `file' sections.")

(defvar magit-hunk-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-return] 'magit-diff-visit-file-worktree)
    (define-key map "\r" 'magit-diff-visit-file)
    (define-key map "a"  'magit-apply)
    (define-key map "C"  'magit-commit-add-log)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    (define-key map "v"  'magit-reverse)
    map)
  "Keymap for `hunk' sections.")

(defconst magit-diff-headline-re
  (concat "^\\(@@@?\\|diff\\|Submodule\\|"
          "\\* Unmerged path\\|merged\\|changed in both\\)"))

(defconst magit-diff-statline-re
  (concat "^ ?"
          "\\(.*\\)"     ; file
          "\\( +| +\\)"  ; separator
          "\\([0-9]+\\|Bin\\(?: +[0-9]+ -> [0-9]+ bytes\\)?$\\) ?"
          "\\(\\+*\\)"   ; add
          "\\(-*\\)$"))  ; del

(defconst magit-diff-submodule-re
  (concat "^Submodule \\([^ ]+\\) \\(?:"
          "\\([^ ]+ (new submodule)\\)\\|"
          "\\([^ ]+ (submodule deleted)\\)\\|"
          "\\(contains \\(?:modified\\|untracked\\) content\\)\\|"
          "\\([^ :]+\\)\\( (rewind)\\)?:\\)$"))

(defun magit-diff-wash-diffs (args &optional diffstats)
  (unless diffstats
    (setq diffstats (magit-diff-wash-diffstat)))
  (when (re-search-forward magit-diff-headline-re nil t)
    (goto-char (line-beginning-position))
    (magit-wash-sequence
     (lambda ()
       (magit-diff-wash-diff args (pop diffstats))))
    (insert ?\n))
  (goto-char (point-max))
  (magit-xref-insert-buttons))

(defun magit-jump-to-diffstat-or-diff ()
  "Jump to the diffstat or diff.
When point is on a file inside the diffstat section, then jump
to the respective diff section, otherwise jump to the diffstat
section or a child thereof."
  (interactive)
  (--if-let (magit-get-section
             (append (magit-section-case
                       ([file diffstat] `((file . ,(magit-section-value it))))
                       (file `((file . ,(magit-section-value it)) (diffstat)
                               ,@(and (derived-mode-p 'magit-revision-mode)
                                      '((headers)))))
                       (t (if (derived-mode-p 'magit-revision-mode)
                              '((diffstat) (headers))
                            '((diffstat)))))
                     (magit-section-ident magit-root-section)))
      (magit-section-goto it)
    (user-error "No diffstat in this buffer")))

(defun magit-diff-wash-diffstat ()
  (let (heading children (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string 1))
      (magit-delete-match)
      (goto-char beg)
      (magit-insert-section it (diffstat)
        (insert heading)
        (magit-insert-heading)
        (magit-wash-sequence
         (lambda ()
           (when (looking-at magit-diff-statline-re)
             (magit-bind-match-strings (file sep cnt add del) nil
               (magit-delete-line)
               (magit-insert-section (file file)
                 (insert " " (propertize file 'face 'magit-filename) sep cnt
                         " ")
                 (when add
                   (insert (propertize add 'face 'magit-diffstat-added)))
                 (when del
                   (insert (propertize del 'face 'magit-diffstat-removed)))
                 (insert "\n"))))))
        (setq children (magit-section-children it))))
    children))

(defun magit-diff-wash-diff (args diffstat)
  (cond
   ((looking-at magit-diff-submodule-re)
    (magit-diff-wash-submodule))
   ((looking-at "^\\* Unmerged path \\(.*\\)")
    (let ((file (magit-decode-git-path (match-string 1))))
      (magit-delete-line)
      (unless (and (derived-mode-p 'magit-status-mode)
                   (not (member "--cached" args)))
        (magit-insert-section (file file)
          (magit-insert (propertize (format "unmerged   %s" file)
                                    'face 'magit-diff-file-heading) nil ?\n))))
    t)
   ((looking-at "^\\(merged\\|changed in both\\)")
    (let ((status (if (equal (match-string 1) "merged") 'merged 'conflict))
          file orig modes blobs blobA blobB)
      (magit-delete-line)
      (while (looking-at
              "^  \\([^ ]+\\) +[0-9]\\{6\\} \\([a-z0-9]\\{40\\}\\) \\(.+\\)$")
        (magit-bind-match-strings (side blob name) nil
          (pcase side
            ("result" (setq file name blobB blob))
            ("our"    (setq orig name blobA blob))
            ("their"  (setq file name blobB blob))))
        (magit-delete-line))
      (setq orig (magit-decode-git-path orig))
      (setq file (magit-decode-git-path file))
      (magit-diff-insert-file-section file orig status modes
                                      (concat blobA ".." blobB))))
   ((looking-at "^diff --\\(git\\|cc\\|combined\\) \\(?:\\(.+?\\) \\2\\)?")
    (let ((status (cond ((equal (match-string 1) "git")        "modified")
                        ((derived-mode-p 'magit-revision-mode) "resolved")
                        (t                                     "unmerged")))
          (orig (match-string 2))
          (file (match-string 2))
          modes blobs)
      (magit-delete-line)
      (while (not (or (eobp) (looking-at magit-diff-headline-re)))
        (if (looking-at "^old mode \\([^\n]+\\)\nnew mode \\([^\n]+\\)\n")
            (progn (setq modes (match-string 0))
                   (magit-delete-match))
          (cond
           ((looking-at "^--- \\([^/].*?\\)\t?$") ; i.e. not /dev/null
            (setq orig (match-string 1)))
           ((looking-at "^\\+\\+\\+ \\([^/].*?\\)\t?$")
            (setq file (match-string 1)))
           ((looking-at "^\\(copy\\|rename\\) from \\(.+\\)$")
            (setq orig (match-string 2)))
           ((looking-at "^\\(copy\\|rename\\) to \\(.+\\)$")
            (setq file (match-string 2))
            (setq status (if (equal (match-string 1) "copy") "new file" "renamed")))
           ((looking-at "^\\(new file\\|deleted\\)")
            (setq status (match-string 1)))
           ((looking-at "^index \\([^\s\n]+\\)")
            (setq blobs (match-string 1))))
          (magit-delete-line)))
      (setq orig (magit-decode-git-path orig))
      (setq file (magit-decode-git-path file))
      (when diffstat
        (setf (magit-section-value diffstat) file))
      (magit-diff-insert-file-section file orig status modes blobs)))))

(defun magit-diff-insert-file-section (file orig status modes blobs)
  (magit-insert-section section
    (file file (or (equal status "deleted")
                   (derived-mode-p 'magit-status-mode)))
    (insert (propertize (format "%-10s %s\n" status
                                (if (equal orig file)
                                    file
                                  (format "%s -> %s" orig file)))
                        'face 'magit-diff-file-heading))
    (magit-insert-heading)
    (unless (equal orig file)
      (setf (magit-section-source section) orig))
    (setf (magit-section-blobs section) blobs)
    (when modes
      (magit-insert-section (hunk)
        (insert modes)))
    (magit-wash-sequence #'magit-diff-wash-hunk)))

(defun magit-diff-wash-submodule ()
  (magit-bind-match-strings (module new deleted dirty range rewind) nil
    (magit-delete-line)
    (when (and dirty
               (looking-at magit-diff-submodule-re)
               (string= (match-string 1) module))
      (setq range (match-string 5))
      (magit-delete-line))
    (while (looking-at "^  \\([<>]\\) \\(.+\\)$")
      (magit-delete-line))
    (if range
        (let ((default-directory
                (file-name-as-directory
                 (expand-file-name module (magit-toplevel)))))
          (setf (magit-section-value
                 (magit-insert-section (file module t)
                   (magit-insert-heading
                     (concat (propertize (concat "modified   " module)
                                         'face 'magit-diff-file-heading)
                             " ("
                             (if rewind "rewind" "new commits")
                             (and dirty ", modified content")
                             ")"))
                   (unless rewind
                     (magit-git-wash
                         (apply-partially 'magit-log-wash-log 'module)
                       "log" "--oneline" "--left-right" range)
                     (delete-char -1))))
                module))
      (magit-insert-section (file module)
        (magit-insert
         (concat (propertize (if new
                                 (concat "new module " module)
                               (concat "modified   " module))
                             'face 'magit-diff-file-heading)
                 (cond (dirty   " (modified content)")
                       (deleted " (deleted submodule)")))
         nil ?\n)))))

(defun magit-diff-wash-hunk ()
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    (let ((heading (match-string 0))
          (value (cons (match-string 2) (split-string (match-string 1)))))
      (magit-delete-line)
      (magit-insert-section it (hunk value)
        (insert (propertize (concat heading "\n") 'face 'magit-diff-hunk-heading))
        (magit-insert-heading)
        (while (not (or (eobp) (looking-at magit-diff-headline-re)))
          (forward-line))
        (setf (magit-section-end it) (point))
        (setf (magit-section-washer it) #'magit-diff-paint-hunk)))
    t))


(defun magit-diff-expansion-threshold (section)
  "Keep new diff sections collapsed if washing takes to long."
  (and (memq (magit-section-type section) '(file))
       (> (float-time (time-subtract (current-time) magit-refresh-start-time))
          magit-diff-expansion-threshold)
       'hide))

;;; Revision Mode

(define-derived-mode magit-revision-mode magit-diff-mode "Magit"
  "Mode for looking at a Git commit.
This mode is documented in info node `(magit)Commit Buffer'.

\\<magit-revision-mode-map>\
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-diff-visit-file] to visit the hunk or file at point.
Type \\[magit-apply] to apply the change at point to the worktree.
Type \\[magit-reverse] to reverse the change at point in the worktree.
\n\\{magit-revision-mode-map}"
  :group 'magit-revision
  (hack-dir-local-variables-non-file-buffer))

(defun magit-revision-refresh-buffer (commit args)
  (magit-insert-section (commitbuf)
    (magit-git-wash #'magit-diff-wash-revision
      "show" "-p" "--cc" "--decorate=full" "--format=fuller" "--no-prefix"
      (and magit-revision-show-diffstat "--stat")
      (and magit-revision-show-notes "--notes")
      args commit "--")))

(defun magit-diff-wash-revision (args)
  (magit-diff-wash-tag)
  (let (children)
    (looking-at "^commit \\([a-z0-9]+\\)\\(?: \\(.+\\)\\)?$")
    (magit-bind-match-strings (rev refs) nil
      (magit-delete-line)
      (setq header-line-format
            (propertize (concat " Commit " rev) 'face 'magit-header-line))
      (magit-insert-section (headers)
        (magit-insert-heading (char-to-string magit-ellipsis))
        (when refs
          (magit-insert (format "References: %s\n"
                                (magit-format-ref-labels refs))))
        (while (looking-at "^\\([a-z]+\\):")
          (when (string-equal (match-string 1) "Merge")
            (magit-delete-line))
          (forward-line 1))
        (re-search-forward "^\\(\\(---\\)\\|    .\\)")
        (goto-char (line-beginning-position))
        (if (match-string 2)
            (progn (magit-delete-match)
                   (insert ?\n)
                   (magit-insert-section (message)
                     (insert "    (no message)\n")))
          (let ((bound (save-excursion
                         (when (re-search-forward "^diff" nil t)
                           (copy-marker (match-beginning 0)))))
                (summary (buffer-substring-no-properties
                          (point) (line-end-position))))
            (magit-delete-line)
            (magit-insert-section (message)
              (insert summary ?\n)
              (magit-insert-heading)
              (cond ((re-search-forward "^---" bound t)
                     (magit-delete-match))
                    ((re-search-forward "^.[^ ]" bound t)
                     (goto-char (1- (match-beginning 0))))))))
        (forward-line)
        (when magit-revision-insert-related-refs
          (magit-revision-insert-related-refs rev))
        (setq children (magit-diff-wash-diffstat))
        (forward-line)))
    (magit-diff-wash-diffs args children)))

(defun magit-diff-wash-tag ()
  (when (looking-at "^tag \\(.+\\)$")
    (let ((tag (match-string 1)))
      (magit-delete-line)
      (magit-insert-section (tag tag)
        (magit-insert-heading
          (propertize "Tag " 'face 'magit-section-heading)
          (propertize  tag   'face 'magit-tag))
        (while (looking-at "^\\([a-z]+\\):")
          (forward-line))
        (forward-line)
        (magit-insert-section (message)
          (forward-line)
          (magit-insert-heading)
          (while (not (looking-at "\ncommit [a-z0-9]\\{40\\}"))
            (forward-line)))
        (forward-line)))))

(defun magit-revision-insert-related-refs (rev)
  (let ((parents    (magit-commit-parents rev))
        (merged     (magit-list-merged-branches rev))
        (containing (magit-list-containing-branches rev))
        (follows    (magit-get-current-tag rev t))
        (precedes   (magit-get-next-tag rev t))
        branch)
    (magit-insert-section (related-refs)
      (magit-insert
       (let ((p (length parents)) (m (length merged)) (s (length containing)))
         (format "%s parent commit%s, %s merged branch%s, %s containing branch%s\n"
                 p (if (= p 1) "" "s")
                 m (if (= m 1) "" "es")
                 s (if (= s 1) "" "es"))))
      (magit-insert-heading)
      (dolist (commit parents)
        (magit-insert-section (commit commit)
          (let ((line (magit-rev-format "%h %s" commit)))
            (string-match "^\\([^ ]+\\) \\(.*\\)" line)
            (magit-bind-match-strings (commit msg) line
              (insert " Parent     | ")
              (insert (propertize commit 'face 'magit-hash))
              (insert " " msg "\n")))))
      (when merged
        (insert " Merged     |")
        (while (and (< (+ (- (point) (line-beginning-position))
                          (length (car merged)) 9)
                       (window-width))
                    (setq branch (pop merged)))
          (insert ?\s)
          (magit-insert-section (branch branch)
            (magit-insert branch 'magit-branch-local)))
        (when merged
          (insert (format " (%s more)" (length merged))))
        (insert ?\n))
      (when containing
        (insert " Containing |")
        (while (and (< (+ (- (point) (line-beginning-position))
                          (length (car containing)) 9)
                       (window-width))
                    (setq branch (pop containing)))
          (insert ?\s)
          (magit-insert-section (branch branch)
            (magit-insert branch 'magit-branch-local)))
        (when containing
          (insert (format " (%s more)" (length containing))))
        (insert ?\n))
      (when follows
        (let ((tag (car  follows))
              (cnt (cadr follows)))
          (magit-insert-section (tag tag)
            (magit-insert (format " Follows    | %s (%s)\n"
                                  (propertize tag 'face 'magit-tag)
                                  (propertize (number-to-string cnt)
                                              'face 'magit-branch-local))))))
      (when precedes
        (let ((tag (car  precedes))
              (cnt (cadr precedes)))
          (magit-insert-section (tag tag)
            (magit-insert (format " Precedes   | %s (%s)\n"
                                  (propertize tag 'face 'magit-tag)
                                  (propertize (number-to-string cnt)
                                              'face 'magit-tag))))))
      (insert ?\n))))

(defun magit-revision-set-visibility (section)
  "Preserve section visibility when displaying another commit in."
  (and (derived-mode-p 'magit-revision-mode)
       (eq (magit-section-type section) 'file)
       (member (magit-section-value section) magit-diff-hidden-files)
       'hide))

;;; Diff Sections

(defvar magit-unstaged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-unstaged)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    map)
  "Keymap for the `unstaged' section.")

(magit-define-section-jumper unstaged "Unstaged changes")

(defun magit-insert-unstaged-changes ()
  "Insert section showing unstaged changes."
  (magit-insert-section (unstaged)
    (magit-insert-heading "Unstaged changes:")
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" magit-diff-section-arguments "--no-prefix")))

(defvar magit-staged-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-staged)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    (define-key map "v"  'magit-reverse)
    map)
  "Keymap for the `staged' section.")

(magit-define-section-jumper staged "Staged changes")

(defun magit-insert-staged-changes ()
  "Insert section showing staged changes."
  (magit-insert-section (staged)
    (magit-insert-heading "Staged changes:")
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" "--cached" magit-diff-section-arguments "--no-prefix")))

;;; Diff Type

(defun magit-diff-type (&optional section)
  "Return the diff type of SECTION.

The returned type is one of the symbols `staged', `unstaged',
`committed', or `undefined'.  This type serves a similar purpose
as the general type common to all sections (which is stored in
the `type' slot of the corresponding `magit-section' struct) but
takes additional information into account.  When the SECTION
isn't related to diffs and the buffer containing it also isn't
a diff-only buffer, then return nil.

Currently the type can also be one of `tracked' and `untracked'
but these values are not handled explicitly everywhere they
should be and a possible fix could be to just return nil here.

The section has to be a `diff' or `hunk' section, or a section
whose children are of type `diff'.  If optional SECTION is nil,
return the diff type for the current section.  In buffers whose
major mode is `magit-diff-mode' SECTION is ignored and the type
is determined using other means.  In `magit-revision-mode'
buffers the type is always `committed'.

Do not confuse this with `magit-diff-scope' (which see)."
  (--when-let (or section (magit-current-section))
    (cond ((derived-mode-p 'magit-diff-mode)
           (if (car magit-refresh-args)
               'undefined
             (if (member "--cached" (cadr magit-refresh-args))
                 'staged
               'unstaged)))
          ((derived-mode-p 'magit-revision-mode 'magit-stash-mode) 'committed)
          ((derived-mode-p 'magit-status-mode)
           (let ((stype (magit-section-type it)))
             (if (memq stype '(staged unstaged tracked untracked))
                 stype
               (pcase stype
                 (`file (let* ((parent (magit-section-parent it))
                               (type   (magit-section-type parent)))
                          (if (eq type 'file)
                              (magit-diff-type parent)
                            type)))
                 (`hunk (-> it magit-section-parent magit-section-parent
                            magit-section-type))))))
          (t 'undefined))))

(cl-defun magit-diff-scope (&optional (section nil ssection) strict)
  "Return the diff scope of SECTION or the selected section(s).

A diff's \"scope\" describes what part of a diff is selected, it is
a symbol, one of `region', `hunk', `hunks', `file', `files', or
`list'.  Do not confuse this with the diff \"type\", as returned by
`magit-diff-type'.

If optional SECTION is non-nil, then return the scope of that,
ignoring the sections selected by the region.  Otherwise return
the scope of the current section, or if the region is active and
selects a valid group of diff related sections, the type of these
sections, i.e. `hunks' or `files'.  If SECTION, or if that is nil
the current section, is a `hunk' section; and the region region
starts and ends inside the body of a that section, then the type
is `region'.

If optional STRICT is non-nil then return nil if the diff type of
the section at point is `untracked' or the section at point is not
actually a `diff' but a `diffstat' section."
  (let ((siblings (and (not ssection) (magit-region-sections))))
    (setq section (or section (car siblings) (magit-current-section)))
    (when (and section
               (or (not strict)
                   (and (not (eq (magit-diff-type section) 'untracked))
                        (not (eq (--when-let (magit-section-parent section)
                                   (magit-section-type it))
                                 'diffstat)))))
      (pcase (list (magit-section-type section)
                   (and siblings t)
                   (and (region-active-p) t)
                   ssection)
        (`(hunk nil   t  ,_)
         (if (magit-section-internal-region-p section) 'region 'hunk))
        (`(hunk   t   t nil) 'hunks)
        (`(hunk  ,_  ,_  ,_) 'hunk)
        (`(file   t   t nil) 'files)
        (`(file  ,_  ,_  ,_) 'file)
        (`(,(or `staged `unstaged `untracked
                `stashed-index `stashed-worktree `stashed-untracked)
           nil ,_ ,_) 'list)))))

;;; Diff Highlight

(defun magit-diff-unhighlight (section selection)
  "Remove the highlighting of the diff-related SECTION."
  (when (eq (magit-section-type section) 'hunk)
    (magit-diff-paint-hunk section selection nil)
    t))

(defun magit-diff-highlight (section selection)
  "Highlight the diff-related SECTION and return t.
If SECTION is not a diff-related section, then do nothing and
return nil.  If SELECTION is non-nil then it is a list of sections
selected by the region, including SECTION.  All of these sections
are highlighted."
  (-when-let (scope (magit-diff-scope section t))
    (cond ((eq scope 'region)
           (magit-diff-paint-hunk section selection t))
          (selection
           (dolist (section selection)
             (magit-diff-highlight-recursive section selection)))
          (t
           (magit-diff-highlight-recursive section)))
    t))

(defun magit-diff-highlight-recursive (section &optional selection)
  (if (magit-section-match 'module-commit section)
      (magit-section-highlight section nil)
    (pcase (magit-diff-scope section)
      (`list (magit-diff-highlight-list section selection))
      (`file (magit-diff-highlight-file section selection))
      (`hunk (magit-diff-highlight-heading section selection)
             (magit-diff-paint-hunk section selection t)))))

(defun magit-diff-highlight-list (section &optional selection)
  (let ((beg (magit-section-start   section))
        (cnt (magit-section-content section))
        (end (magit-section-end     section)))
    (unless (and (region-active-p)
                 (= end (1+ (region-end))))
      (magit-section-make-overlay beg cnt 'magit-section-highlight)
      (unless (magit-section-hidden section)
        (dolist (child (magit-section-children section))
          (magit-diff-highlight-recursive child selection))))
    (when magit-diff-highlight-hunk-body
      (magit-section-make-overlay (1- end) end 'magit-section-highlight))))

(defun magit-diff-highlight-file (section &optional selection)
  (magit-diff-highlight-heading section selection)
  (unless (magit-section-hidden section)
    (dolist (child (magit-section-children section))
      (magit-diff-highlight-recursive child selection))))

(defun magit-diff-highlight-heading (section &optional selection)
  (magit-section-make-overlay
   (magit-section-start section)
   (or (magit-section-content section)
       (magit-section-end     section))
   (pcase (list (magit-section-type section)
                (and (member section selection) t))
     (`(file   t) 'magit-diff-file-heading-selection)
     (`(file nil) 'magit-diff-file-heading-highlight)
     (`(hunk   t) 'magit-diff-hunk-heading-selection)
     (`(hunk nil) 'magit-diff-hunk-heading-highlight))))

;;; Hunk Paint

(cl-defun magit-diff-paint-hunk
    (section &optional selection
             (highlight (magit-section-selected-p section selection)))
  (let (paint)
    (unless magit-diff-highlight-hunk-body
      (setq highlight nil))
    (cond (highlight
           (unless (magit-section-hidden section)
             (add-to-list 'magit-section-highlighted-sections section)
             (cond ((memq section magit-section-unhighlight-sections)
                    (setq magit-section-unhighlight-sections
                          (delq section magit-section-unhighlight-sections)))
                   (magit-diff-highlight-hunk-body
                    (setq paint t)))))
          (t
           (cond ((and (magit-section-hidden section)
                       (memq section magit-section-unhighlight-sections))
                  (add-to-list 'magit-section-highlighted-sections section)
                  (setq magit-section-unhighlight-sections
                        (delq section magit-section-unhighlight-sections)))
                 (t
                  (setq paint t)))))
    (when paint
      (save-excursion
        (goto-char (magit-section-start section))
        (let ((end (magit-section-end section))
              (merging (looking-at "@@@"))
              (stage nil))
          (forward-line)
          (while (< (point) end)
            (put-text-property
             (point) (1+ (line-end-position)) 'face
             (cond
              ((looking-at "^\\+\\+?\\([<=|>]\\)\\{7\\}")
               (setq stage (pcase (list (match-string 1) highlight)
                             (`("<" nil) 'magit-diff-our)
                             (`("<"   t) 'magit-diff-our-highlight)
                             (`("|" nil) 'magit-diff-base)
                             (`("|"   t) 'magit-diff-base-highlight)
                             (`("=" nil) 'magit-diff-their)
                             (`("="   t) 'magit-diff-their-highlight)
                             (`(">" nil) nil)))
               'magit-diff-conflict-heading)
              ((looking-at (if merging  "^\\(\\+\\| \\+\\)" "^\\+"))
               (magit-diff-paint-whitespace merging)
               (or stage
                   (if highlight 'magit-diff-added-highlight 'magit-diff-added)))
              ((looking-at (if merging  "^\\(-\\| -\\)" "^-"))
               (if highlight 'magit-diff-removed-highlight 'magit-diff-removed))
              (t
               (if highlight 'magit-diff-context-highlight 'magit-diff-context))))
            (forward-line))))))
  (magit-diff-update-hunk-refinement section))

(defun magit-diff-paint-whitespace (merging)
  (when (and magit-diff-paint-whitespace
             (or (derived-mode-p 'magit-status-mode)
                 (not (eq magit-diff-paint-whitespace 'status))))
    (let ((prefix (if merging "^[-\\+\s]\\{2\\}" "^[-\\+]"))
          (indent
           (if (local-variable-p 'magit-diff-highlight-indentation)
               magit-diff-highlight-indentation
             (setq-local
              magit-diff-highlight-indentation
              (cdr (--first (string-match-p (car it) default-directory)
                            (nreverse
                             (default-value
                               'magit-diff-highlight-indentation))))))))
      (when (and magit-diff-highlight-trailing
                 (looking-at (concat prefix ".*?\\([ \t]+\\)$")))
        (magit-put-face-property (match-beginning 1) (match-end 1)
                                 'magit-diff-whitespace-warning))
      (when (or (and (eq indent 'tabs)
                     (looking-at (concat prefix "\\( *\t[ \t]*\\)")))
                (and (integerp indent)
                     (looking-at (format "%s\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                         prefix indent))))
        (magit-put-face-property (match-beginning 1) (match-end 1)
                                 'magit-diff-whitespace-warning)))))

(defun magit-diff-update-hunk-refinement (&optional section)
  (if section
      (unless (magit-section-hidden section)
        (pcase (list magit-diff-refine-hunk
                     (magit-section-refined section)
                     (eq section (magit-current-section)))
          ((or `(all nil ,_) `(t nil t))
           (setf (magit-section-refined section) t)
           (save-excursion
             (goto-char (magit-section-start section))
             ;; `diff-refine-hunk' does not handle combined diffs.
             (unless (looking-at "@@@")
               (diff-refine-hunk))))
          ((or `(nil t ,_) `(t t nil))
           (setf (magit-section-refined section) nil)
           (remove-overlays (magit-section-start section)
                            (magit-section-end   section)
                            'diff-mode 'fine))))
    (cl-labels ((recurse (section)
                         (if (magit-section-match 'hunk section)
                             (magit-diff-update-hunk-refinement section)
                           (--each (magit-section-children section)
                             (recurse it)))))
      (recurse magit-root-section))))


;;; Highlight Region

(defvar magit-diff-unmarked-lines-keep-foreground t)

(defun magit-diff-update-hunk-region (section)
  (when (and (eq (magit-diff-scope section t) 'region)
             (not (and (eq this-command 'mouse-drag-region)
                       (eq (mark) (point)))))
    (let ((sbeg (magit-section-start section))
          (cbeg (magit-section-content section))
          (rbeg (save-excursion (goto-char (region-beginning))
                                (line-beginning-position)))
          (rend (save-excursion (goto-char (region-end))
                                (line-end-position)))
          (send (magit-section-end section))
          (face (if magit-diff-highlight-hunk-body
                    'magit-diff-context-highlight
                  'magit-diff-context)))
      (when magit-diff-unmarked-lines-keep-foreground
        (setq face (list :background (face-attribute face :background))))
      (cl-flet ((ov (start end &rest args)
                  (let ((ov (make-overlay start end nil t)))
                    (overlay-put ov 'evaporate t)
                    (while args (overlay-put ov (pop args) (pop args)))
                    (push ov magit-region-overlays)
                    ov)))
        (ov sbeg cbeg 'face 'magit-diff-lines-heading
            'display (concat (magit-diff-hunk-region-header section) "\n"))
        (ov cbeg rbeg 'face face 'priority 2)
        (when (and (window-system) magit-diff-show-lines-boundary)
          (ov rbeg (1+ rbeg) 'before-string
              (propertize (concat (propertize "\s" 'display '(space :height (1)))
                                  (propertize "\n" 'line-height t))
                          'face 'magit-diff-lines-boundary))
          (ov rend (1+ rend) 'after-string
              (propertize (concat (propertize "\s" 'display '(space :height (1)))
                                  (propertize "\n" 'line-height t))
                          'face 'magit-diff-lines-boundary)))
        (ov (1+ rend) send 'face face 'priority 2)))))

;;; Diff Extract

(defun magit-diff-file-header (section)
  (when (eq (magit-section-type section) 'hunk)
    (setq section (magit-section-parent section)))
  (when (eq (magit-section-type section) 'file)
    (let* ((file (magit-section-value section))
           (orig (or (magit-section-source section) file)))
      (format "diff --git a/%s b/%s\nindex %s\n--- a/%s\n+++ b/%s\n"
              orig file (magit-section-blobs section) orig file))))

(defun magit-diff-hunk-region-header (section)
  (nth 4 (split-string (magit-diff-hunk-region-patch section) "\n")))

(defun magit-diff-hunk-region-patch (section &optional args)
  (unless (magit-diff-context-p)
    (user-error "Not enough context to apply region.  Increase the context"))
  (when (string-match "^diff --cc" (magit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (let ((op (if (member "--reverse" args) "+" "-"))
        (sbeg (magit-section-start section))
        (rbeg (save-excursion
                (goto-char (region-beginning))
                (line-beginning-position)))
        (rend (region-end))
        (send (magit-section-end section))
        (patch (list (magit-diff-file-header section))))
    (save-excursion
      (goto-char sbeg)
      (while (< (point) send)
        (looking-at "\\(.\\)\\([^\n]*\n\\)")
        (cond ((or (string-match-p "[@ ]" (match-string-no-properties 1))
                   (and (>= (point) rbeg)
                        (<= (point) rend)))
               (push (match-string-no-properties 0) patch))
              ((equal op (match-string-no-properties 1))
               (push (concat " " (match-string-no-properties 2)) patch)))
        (forward-line)))
    (with-temp-buffer
      (insert (mapconcat 'identity (reverse patch) ""))
      (diff-fixup-modifs (point-min) (point-max))
      (setq patch (buffer-string)))
    patch))

;;; magit-diff.el ends soon
(provide 'magit-diff)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-diff.el ends here
