;;; magit-diff.el --- inspect Git diffs

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

(require 'git-commit-mode)
(require 'magit-core)

;; For `magit-diff-while-committing'
(declare-function magit-commit-message-buffer 'magit)
;; For `magit-show-commit' and `magit-diff-diff-show-or-scroll'
(declare-function magit-blame-chunk-get 'magit-blame)
(declare-function magit-blame-mode 'magit-blame)
(defvar magit-blame-mode)

(require 'diff-mode)

;;; Options
;;;; Diff Mode

(defgroup magit-diff nil
  "Inspect and manipulate Git diffs."
  :group 'magit-modes)

(defcustom magit-diff-buffer-name-format "*magit-diff: %a*"
  "Name format for buffers used to display a diff.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'string)

(defcustom magit-diff-highlight-hunk-body t
  "Whether to highlight bodies of selected hunk sections.
This only has an effect if `magit-diff-highlight' is a
member of `magit-section-highlight-hook', which see."
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

(defcustom magit-diff-refine-hunk nil
  "Whether to show word-granularity differences within diff hunks.

nil    never show fine differences.
t      show fine differences for the selected diff hunk only.
`all'  show fine differences for all displayed diff hunks."
  :group 'magit-diff
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Selected only" t)
                 (const :tag "All" all)))

(defcustom magit-diff-paint-whitespace t
  "Specify where to highlight whitespace errors.
See `magit-highlight-trailing-whitespace',
`magit-highlight-indentation'.  The symbol t means in all diffs,
`status' means only in the status buffer, and nil means nowhere."
  :group 'magit-diff
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "In status buffer" status)))

(defcustom magit-diff-highlight-trailing t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `magit-diff-paint-whitespace' is non-nil."
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `magit-diff-paint-whitespace' is non-nil.

The value is a list of cons cells.  The car is a regular
expression, and the cdr is the value that applies to repositories
whose directory matches the regular expression.  If more than one
element matches, then the *last* element in the list applies.
The default value should therefor come first in the list.

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

(defcustom magit-revision-buffer-name-format "*magit-rev: %a*"
  "Name format for buffers used to display a commit.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type 'string)

(defcustom magit-revision-show-diffstat t
  "Whether to show diffstat in commit buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type 'boolean)

(defcustom magit-revision-show-notes t
  "Whether to show notes in commit buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type 'boolean)

(defcustom magit-revision-show-xref-buttons t
  "Whether to show buffer history buttons in commit buffers."
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
  "Face for plus sign in diffstats."
  :group 'magit-faces)

(defface magit-diffstat-removed
  '((((class color) (background light)) :foreground "#aa2222")
    (((class color) (background  dark)) :foreground "#aa4444"))
  "Face for minus sign in diffstats."
  :group 'magit-faces)

;;; Commands

(magit-define-popup magit-diff-popup
  "Key menu for diffing."
  'magit-popups
  :man-page "git-diff"
  :switches '((?W "Show surrounding functions"   "--function-context")
              (?b "Ignore whitespace changes"    "--ignore-space-change")
              (?w "Ignore all whitespace"        "--ignore-all-space"))
  :options  '((?h "Context lines" "-U" read-from-minibuffer)
              (?a "Diff algorithm"
                  "--diff-algorithm=" magit-diff-select-algorithm))
  :actions  '((?d "Diff unstaged"     magit-diff-unstaged)
              (?c "Show commit"       magit-show-commit)
              (?R "Refresh diff"      magit-popup-set-local-variable)
              (?i "Diff staged"       magit-diff-staged)
              (?r "Diff commits"      magit-diff)
              (?S "Refresh and set"   magit-popup-set-variable)
              (?w "Diff working tree" magit-diff-working-tree)
              (?p "Diff paths"        magit-diff-paths)
              (?W "Refresh and save"  magit-popup-save-variable))
  :default-action 'magit-diff-working-tree
  :max-action-columns 3)

(defvar magit-diff-switch-buffer-function 'pop-to-buffer)

;;;###autoload
(defun magit-diff (range &optional args)
    "Show differences between two commits.
RANGE should be a range (A..B or A...B) but can also be a single
commit.  If one side of the range is omitted, then it defaults
to HEAD.  If just a commit is given, then changes in the working
tree relative to that commit are shown."
  (interactive (list (magit-read-range-or-commit "Diff for range")))
  (magit-mode-setup magit-diff-buffer-name-format
                    magit-diff-switch-buffer-function
                    #'magit-diff-mode
                    #'magit-diff-refresh-buffer range args))

;;;###autoload
(defun magit-diff-working-tree (&optional rev)
  "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer."
  (interactive
   (and current-prefix-arg
        (list (magit-read-branch-or-commit "Diff working tree and commit"))))
  (magit-diff (or rev "HEAD")))

;;;###autoload
(defun magit-diff-staged (&optional commit)
  "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer."
  (interactive
   (and current-prefix-arg
        (list (magit-read-branch-or-commit "Diff index and commit"))))
  (magit-diff nil (cons "--cached" (and commit (list commit)))))

;;;###autoload
(defun magit-diff-unstaged ()
  "Show changes between the working tree and the index."
  (interactive)
  (magit-diff nil))

;;;###autoload
(defun magit-diff-unpushed ()
  "Show unpushed changes."
  (interactive)
  (-if-let (tracked (magit-get-tracked-branch nil t))
      (magit-diff (concat tracked "..."))
    (error "Detached HEAD or upstream unset")))

;;;###autoload
(defun magit-diff-unpulled ()
  "Show unpulled changes."
  (interactive)
  (-if-let (tracked (magit-get-tracked-branch nil t))
      (magit-diff (concat "..." tracked))
    (error "Detached HEAD or upstream unset")))

;;;###autoload
(defun magit-diff-while-committing ()
  (interactive)
  (let* ((toplevel (magit-get-top-dir))
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
                           (not (equal (magit-get-top-dir) toplevel))
                           ;; toggle to include last commit
                           (not (car magit-refresh-args))))))
            (magit-diff-while-amending)
          (magit-diff-staged))
      (user-error "No commit in progress"))))

(define-key git-commit-mode-map
  (kbd "C-c C-d") 'magit-diff-while-committing)

(defun magit-diff-while-amending ()
  (magit-diff "HEAD^" (list "--cached")))

;;;###autoload
(defun magit-diff-paths (a b)
  "Show changes between any two files on disk."
  (interactive (list (read-file-name "First file: " nil nil t)
                     (read-file-name "Second file: " nil nil t)))
  (magit-diff nil (list "--no-index" "--" a b)))

;;;###autoload
(defun magit-show-commit (commit &optional noselect module)
  "Show the commit at point.
If there is no commit at point or with a prefix argument prompt
for a commit."
  (interactive
   (let* ((mcommit (magit-section-when mcommit))
          (atpoint (or (and magit-blame-mode (magit-blame-chunk-get :hash))
                       mcommit (magit-branch-or-commit-at-point)
                       (magit-section-when tag
                         (concat (magit-section-value it) "^{commit}")))))
     (list (or (and (not current-prefix-arg) atpoint)
               (magit-read-branch-or-commit "Show commit" atpoint))
           nil (and mcommit (magit-section-parent-value
                             (magit-current-section))))))
  (let ((default-directory (if module
                               (file-name-as-directory
                                (expand-file-name module (magit-get-top-dir)))
                             default-directory)))
    (when (magit-git-failure "cat-file" "commit" commit)
      (user-error "%s is not a commit" commit))
    (magit-mode-setup magit-revision-buffer-name-format
                      (if noselect 'display-buffer 'pop-to-buffer)
                      #'magit-revision-mode
                      #'magit-revision-refresh-buffer
                      commit)))

(defun magit-diff-less-context (&optional count)
  "Decrease the context for diff hunks by COUNT."
  (interactive "p")
  (setq magit-diff-arguments
        (cons (format "-U%i" (max 0 (- (magit-diff-previous-context-lines)
                                       count)))
              magit-diff-arguments))
  (magit-refresh))

(defun magit-diff-more-context (&optional count)
  "Increase the context for diff hunks by COUNT."
  (interactive "p")
  (setq magit-diff-arguments
        (cons (format "-U%i" (+ (magit-diff-previous-context-lines) count))
              magit-diff-arguments))
  (magit-refresh))

(defun magit-diff-default-context ()
  "Reset context for diff hunks to the default size."
  (interactive)
  (magit-diff-previous-context-lines)
  (magit-refresh))

(defun magit-diff-previous-context-lines ()
  (--if-let (--first (string-match "^-U\\([0-9]+\\)$" it) magit-diff-arguments)
      (progn (setq magit-diff-arguments (delete it magit-diff-arguments))
             (string-to-number (match-string 1 it)))
    3))

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
                 (goto-char (cl-case fn
                              (scroll-up   (point-min))
                              (scroll-down (point-max)))))))
          (funcall cmd rev t))
      (call-interactively 'magit-show-commit))))

(defun magit-diff-auto-show-p (op)
  (if (eq (car magit-diff-auto-show) 'not)
      (not (memq op (cdr magit-diff-auto-show)))
    (memq op magit-diff-auto-show)))

(defun magit-diff-select-algorithm (&optional noop1 noop2)
  (magit-read-char-case nil t
    (?d "[d]efault/myers" "default")
    (?m "[m]inimal"       "minimal")
    (?p "[p]atience"      "patience")
    (?h "[h]istogram"     "histogram")))

;;; Diff Mode

(defvar magit-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-c\C-d" 'magit-diff-while-committing)
    (define-key map "\C-c\C-b" 'magit-go-backward)
    (define-key map "\C-c\C-f" 'magit-go-forward)
    (define-key map "\s" 'scroll-up)
    (define-key map "\d" 'scroll-down)
    (define-key map "j" 'magit-jump-to-diffstats)
    map)
  "Keymap for `magit-diff-mode'.")

(define-derived-mode magit-diff-mode magit-mode "Magit Diff"
  "Mode for looking at a Git diff.
This mode is documented in info node `(magit)Diffing'.

\\<magit-diff-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-file] to visit the file at point.
Type \\[magit-apply] to apply the change at point to the worktree.
Type \\[magit-reverse] to reverse the change at point in the worktree.
\n\\{magit-diff-mode-map}"
  :group 'magit-diff)

;; This variable is only a temporary hack.  Eventually it will
;; be possible to set some of these arguments in the diff popup.
(defconst magit-diff-extra-options '("-M" "-C" "--no-prefix"))

(defun magit-diff-refresh-buffer (range &optional args)
  (magit-insert-section (diffbuf)
    (magit-insert-heading
      (if range
          (if (string-match-p "\\.\\." range)
              (format "Changes in %s" range)
            (format "Changes from %s to working tree" range))
        (if (member "--cached" args)
            "Staged changes"
          "Unstaged changes")))
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" "-p" (and magit-diff-show-diffstat "--stat")
      magit-diff-extra-options
      range args magit-diff-arguments "--")))

(defvar magit-file-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-visit-file)
    (define-key map "a"  'magit-apply)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    (define-key map "v"  'magit-reverse)
    map)
  "Keymap for `file' sections.")

(defvar magit-hunk-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-visit-file)
    (define-key map "a"  'magit-apply)
    (define-key map "C"  'magit-commit-add-log)
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    (define-key map "u"  'magit-unstage)
    (define-key map "v"  'magit-reverse)
    map)
  "Keymap for `hunk' sections.")

(defconst magit-diff-headline-re
  "^\\(@@@?\\|diff\\|\\* Unmerged path\\|Submodule\\)")

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
          "\\(contains modified content\\)\\|"
          "\\([^:]+\\):\\)$"))

(defun magit-diff-wash-diffs (args)
  (let ((diffstats (magit-diff-wash-diffstats)))
    (when (re-search-forward magit-diff-headline-re nil t)
      (goto-char (line-beginning-position))
      (magit-wash-sequence
       (lambda ()
         (magit-diff-wash-diff args (pop diffstats))))
      (insert ?\n)))
  (goto-char (point-max))
  (magit-xref-insert-buttons))

(magit-define-section-jumper diffstats "Diffstats")

(defun magit-diff-wash-diffstats ()
  (let (heading diffstats (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-string 1))
      (magit-delete-match)
      (goto-char beg)
      (magit-insert-section it (diffstats)
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
        (setq diffstats (magit-section-children it))))
    diffstats))

(defun magit-diff-wash-diff (args diffstat)
  (cond
   ((looking-at magit-diff-submodule-re)
    (magit-bind-match-strings (module new dirty range) nil
      (magit-delete-line)
      (when (and dirty
                 (looking-at magit-diff-submodule-re)
                 (string= (match-string 1) module))
        (setq range (match-string 4))
        (magit-delete-line))
      (while (looking-at "^  \\([<>]\\) \\(.+\\)$")
        (magit-delete-line))
      (if range
          (let ((default-directory
                  (file-name-as-directory
                   (expand-file-name module (magit-get-top-dir)))))
            (setf (magit-section-value
                   (magit-insert-section (file module t)
                     (magit-insert-heading
                       (concat (propertize (concat "modified   " module)
                                           'face 'magit-diff-file-heading)
                               " ("
                               (and range "new commits")
                               (and dirty ", modified content")
                               ")"))
                     (magit-git-wash (apply-partially 'magit-log-wash-log 'module)
                       "log" "--oneline" "--left-right" range)
                     (delete-char -1)))
                  module))
        (magit-insert-section (file module)
          (magit-insert
           (concat (propertize (if new
                                   (concat "new module " module)
                                 (concat "modified   " module))
                               'face 'magit-diff-file-heading)
                   (and dirty " (modified content)"))
           nil ?\n)))))
   ((looking-at "^\\* Unmerged path \\(.*\\)")
    (let ((file (magit-decode-git-path (match-string 1))))
      (magit-delete-line)
      (unless (and (derived-mode-p 'magit-status-mode)
                   (not (member "--cached" args)))
        (magit-insert-section (file file)
          (magit-insert (propertize (format "unmerged   %s" file)
                                    'face 'magit-diff-file-heading) nil ?\n))))
    t)
   ((looking-at "^diff --\\(git\\|cc\\|combined\\) \\(?:\\(.+?\\) \\2\\)?")
    (let ((status (cond ((equal (match-string 1) "git")        "modified")
                        ((derived-mode-p 'magit-revision-mode) "resolved")
                        (t                                     "unmerged")))
          (orig (match-string 2))
          (file (match-string 2))
          modes)
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
            (setq status (match-string 1))))
          (magit-delete-line)))
      (setq orig (magit-decode-git-path orig))
      (setq file (magit-decode-git-path file))
      (when diffstat
        (setf (magit-section-value diffstat) file))
      (magit-insert-section it
        (file file (or (equal status "deleted")
                       (derived-mode-p 'magit-status-mode)))
        (insert (propertize (format "%-10s %s\n" status
                                    (if (equal orig file)
                                        file
                                      (format "%s -> %s" orig file)))
                            'face 'magit-diff-file-heading))
        (magit-insert-heading)
        (unless (equal orig file)
          (setf (magit-section-source it) orig))
        (when modes
          (magit-insert-section (hunk)
            (insert modes)))
        (magit-wash-sequence #'magit-diff-wash-hunk))))))

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
        (magit-diff-paint-hunk it nil nil (eq magit-diff-refine-hunk 'all))))
    t))

(defun magit-diff-toggle-refine-hunk (&optional other)
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
  (let ((old magit-diff-refine-hunk))
    (setq-local magit-diff-refine-hunk
                (if other
                    (if (eq old 'all) t 'all)
                  (not old)))
    (if (or (eq old 'all)
            (eq magit-diff-refine-hunk 'all))
        (magit-refresh)
      (dolist (section magit-section-highlighted-sections)
        (when (eq (magit-section-type section) 'hunk)
          (if  magit-diff-refine-hunk
              (magit-diff-refine-hunk section)
            (magit-diff-unrefine-hunk section)))))
    (message "magit-diff-refine-hunk: %s" magit-diff-refine-hunk)))

;;; Revision Mode

(defvar magit-revision-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-diff-mode-map)
    map)
  "Keymap for `magit-revision-mode'.")

(define-derived-mode magit-revision-mode magit-mode "Magit"
  "Mode for looking at a Git commit.
This mode is documented in info node `(magit)Commit Buffer'.

\\<magit-revision-mode-map>\
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-file] to visit the hunk or file at point.
Type \\[magit-apply] to apply the change at point to the worktree.
Type \\[magit-reverse] to reverse the change at point in the worktree.
\n\\{magit-revision-mode-map}"
  :group 'magit-revision)

(defun magit-revision-refresh-buffer (commit)
  (magit-insert-section (commitbuf)
    (magit-git-wash #'magit-diff-wash-revision
      "show" "-p" "--cc" "--decorate=full" "--format=fuller"
      (and magit-revision-show-diffstat "--stat")
      (and magit-revision-show-notes "--notes")
      magit-diff-arguments
      magit-diff-extra-options
      commit)))

(defun magit-diff-wash-revision (args)
  (looking-at "^commit \\([a-z0-9]+\\)\\(?: \\(.+\\)\\)?$")
  (magit-bind-match-strings (rev refs) nil
    (magit-delete-line)
    (magit-insert-section (headers)
      (magit-insert-heading
        (and refs (concat (magit-format-ref-labels refs) " "))
        (propertize rev 'face 'magit-hash))
      (while (re-search-forward "^\\([a-z]+\\): +\\(.+\\)$" nil t)
        (magit-bind-match-strings (keyword revs) nil
          (when (string-match-p keyword "Merge")
            (magit-delete-match 2)
            (dolist (rev (split-string revs))
              (magit-diff-insert-commit-button rev)
              (insert ?\s)))))
      (forward-line)))
  (forward-line)
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
             (goto-char (1- (match-beginning 0)))))))
  (forward-line)
  (magit-diff-wash-diffs args))

(defun magit-diff-insert-commit-button (hash)
  (magit-insert-section (commit hash)
    (insert-text-button hash
                        'help-echo "Visit commit"
                        'action (lambda (button)
                                  (save-excursion
                                    (goto-char button)
                                    (call-interactively #'magit-show-commit)))
                        'follow-link t
                        'mouse-face 'magit-section-highlight
                        'face 'magit-hash)))

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
  (magit-insert-section (unstaged)
    (magit-insert-heading "Unstaged changes:")
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" magit-diff-arguments magit-diff-extra-options)))

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
  (magit-insert-section (staged)
    (magit-insert-heading "Staged changes:")
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" "--cached" magit-diff-arguments magit-diff-extra-options)))

;;; Diff Type

(defun magit-diff-type (&optional section)
  (--when-let (or section (magit-current-section))
    (cond ((derived-mode-p 'magit-diff-mode)
           (or (and (not (nth 0 magit-refresh-args))
                    (not (nth 1 magit-refresh-args))
                    (cond ((not (nth 2 magit-refresh-args))
                           'unstaged)
                          ((member "--cached" (cddr magit-refresh-args))
                           'staged)))
               'undefined))
          ((derived-mode-p 'magit-revision-mode 'magit-stash-mode) 'committed)
          ((derived-mode-p 'magit-status-mode)
           (let ((stype (magit-section-type it)))
             (if (memq stype '(staged unstaged untracked))
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

(defun magit-diff-scope (&optional section strict singular)
  (let ((siblings (and (not singular) (magit-region-sections))))
    (setq section (or section (car siblings) (magit-current-section)))
    (when (and section
               (or (not strict)
                   (and (not (eq (magit-diff-type section) 'untracked))
                        (not (eq (--when-let (magit-section-parent section)
                                   (magit-section-type it))
                                 'diffstats)))))
      (pcase (list (magit-section-type section)
                   (and siblings t)
                   (and (region-active-p) t)
                   singular)
        (`(hunk nil nil  ,_) 'hunk)
        (`(hunk nil   t  ,_)
         (if (and (magit-section-internal-region-p section)
                  (not (magit-section-position-in-heading-p
                        section (region-beginning))))
             (if singular 'hunk 'region)
           'hunk))
        (`(hunk   t   t   t) 'hunk)
        (`(hunk   t   t nil) 'hunks)
        (`(file nil  ,_  ,_) 'file)
        (`(file   t   t   t) 'file)
        (`(file   t   t nil) 'files)
        (`(,(or `staged `unstaged `untracked
                `stashed-index `stashed-worktree `stashed-untracked)
           nil ,_ ,_) 'list)))))

;;; Diff Highlight

(defun magit-diff-unhighlight (section)
  (when (eq (magit-section-type section) 'hunk)
    (magit-diff-paint-hunk section)
    t))

(defun magit-diff-highlight (section siblings)
  (-when-let (scope (magit-diff-scope section t))
    (cond ((eq scope 'region)
           (magit-diff-paint-hunk section nil t)
           (magit-face-remap-set-base 'region 'face-override-spec)
           (magit-diff-highlight-lines section))
          (siblings
           (magit-face-remap-set-base 'region 'face-override-spec)
           (dolist (section siblings)
             (magit-diff-highlight-recursive section siblings)))
          (t
           (magit-diff-highlight-recursive section)))
    t))

(defun magit-diff-highlight-recursive (section &optional siblings)
  (let ((scope (magit-diff-scope section nil t)))
    (pcase scope
      (`list (magit-diff-highlight-list section))
      (`file (magit-diff-highlight-heading section siblings))
      (`hunk (magit-diff-highlight-heading section siblings)
             (magit-diff-paint-hunk section siblings t)))
    (dolist (child (magit-section-children section))
      (magit-diff-highlight-recursive child siblings))))

(defun magit-diff-highlight-list (section)
  (let ((beg (magit-section-start   section))
        (cnt (magit-section-content section))
        (end (magit-section-end     section)))
    (magit-section-make-overlay     beg  cnt 'magit-section-highlight)
    (magit-section-make-overlay (1- end) end 'magit-section-highlight)))

(defun magit-diff-highlight-heading (section &optional siblings)
  (magit-section-make-overlay
   (magit-section-start section)
   (or (magit-section-content section)
       (magit-section-end     section))
   (pcase (magit-section-type section)
     (`file (if (member section siblings)
                'magit-diff-file-heading-selection
              'magit-diff-file-heading-highlight))
     (`hunk (if (member section siblings)
                'magit-diff-hunk-heading-selection
              'magit-diff-hunk-heading-highlight)))))

(defun magit-diff-highlight-lines (section)
  (let ((face (if magit-diff-highlight-hunk-body
                  'magit-diff-context-highlight
                'magit-diff-context))
        (sbeg (magit-section-start section))
        (cbeg (magit-section-content section))
        (rbeg (save-excursion (goto-char (region-beginning))
                              (line-beginning-position)))
        (rend (save-excursion (goto-char (region-end))
                              (line-end-position)))
        (send (magit-section-end section)))
    (overlay-put (magit-section-make-overlay
                  sbeg cbeg 'magit-diff-lines-heading)
                 'display (concat (magit-diff-hunk-region-header section) "\n"))
    (overlay-put (magit-section-make-overlay cbeg rbeg face) 'priority 2)
    (overlay-put (magit-section-make-overlay rbeg (1+ rbeg) nil)
                 'before-string
                 (propertize
                  (concat (propertize "\s" 'display '(space :height (1)))
                          (propertize "\n" 'line-height t))
                  'face 'magit-diff-lines-boundary))
    (overlay-put (magit-section-make-overlay rend (1+ rend) nil)
                 'after-string
                 (propertize
                  (concat (propertize "\s" 'display '(space :height (1)))
                          (propertize "\n" 'line-height t))
                  'face 'magit-diff-lines-boundary))
    (overlay-put (magit-section-make-overlay (1+ rend) send face) 'priority 2)))

;;; Hunk Paint

(defun magit-diff-paint-hunk (section &optional siblings highlight refine)
  (let ((paint (not highlight)))
    (when highlight
      (push section magit-section-highlighted-sections)
      (cond ((memq section magit-section-unhighlight-sections)
             (setq magit-section-unhighlight-sections
                   (delete section magit-section-unhighlight-sections)))
            (magit-diff-highlight-hunk-body
             (setq paint t))))
    (when paint
      (save-excursion
        (goto-char (magit-section-start section))
        (let ((end (magit-section-end section))
              (merging (looking-at "@@@")))
          (forward-line)
          (while (< (point) end)
            (put-text-property
             (point) (1+ (line-end-position)) 'face
             (cond
              ((looking-at "^\\+\\+[<=|>]\\{7\\}") 'magit-diff-conflict-heading)
              ((looking-at (if merging  "^\\(\\+\\| \\+\\)" "^\\+"))
               (magit-diff-paint-whitespace merging)
               (if highlight 'magit-diff-added-highlight 'magit-diff-added))
              ((looking-at (if merging  "^\\(-\\| -\\)" "^-"))
               (if highlight 'magit-diff-removed-highlight 'magit-diff-removed))
              (t
               (if highlight 'magit-diff-context-highlight 'magit-diff-context))))
            (forward-line)))))
    (cond (refine (magit-diff-refine-hunk section))
          ((eq magit-diff-refine-hunk t)
           (if highlight
               (magit-diff-refine-hunk section)
             (magit-diff-unrefine-hunk section))))))

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

(defun magit-diff-refine-hunk (hunk)
  (save-excursion
    (goto-char (magit-section-start hunk))
    ;; `diff-refine-hunk' does not handle combined diffs.
    (unless (looking-at "@@@")
      (diff-refine-hunk))))

(defun magit-diff-unrefine-hunk (hunk)
  (remove-overlays (magit-section-start hunk)
                   (magit-section-end hunk)
                   'diff-mode 'fine))

;;; Diff Extract

(defun magit-diff-file-header (section)
  (when (eq (magit-section-type section) 'hunk)
    (setq section (magit-section-parent section)))
  (when (eq (magit-section-type section) 'file)
    (let* ((file (magit-section-value section))
           (orig (or (magit-section-source section) file)))
      (format "diff --git a/%s b/%s\n--- a/%s\n+++ b/%s\n" orig file orig file))))

(defun magit-diff-hunk-region-header (section)
  (nth 3 (split-string (magit-diff-hunk-region-patch section) "\n")))

(defun magit-diff-hunk-region-patch (section &optional args)
  (when (member "-U0" magit-diff-arguments)
    (user-error "Not enough context to apply region.  Increase the context"))
  (when (string-match "^diff --cc" (magit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (let ((op (if (member "--reverse" args) "+" "-"))
        (sbeg (magit-section-start section))
        (rbeg (region-beginning))
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
