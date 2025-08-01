;;; magit-diff.el --- Inspect Git diffs  -*- lexical-binding:t -*-

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

;; This library implements support for looking at Git diffs and
;; commits.

;;; Code:

(require 'magit-core)
(require 'git-commit)

(eval-when-compile (require 'ansi-color))
(require 'diff-mode)
(require 'image)
(require 'smerge-mode)

;; For `magit-diff--get-value'
(defvar magit-status-use-buffer-arguments)
;; For `magit-diff-popup'
(declare-function magit-stash-show "magit-stash" (stash &optional args files))
;; For `magit-diff-visit-file'
(declare-function magit-find-file-noselect "magit-files" (rev file &optional revert))
(declare-function magit-status-setup-buffer "magit-status" (&optional directory))
;; For `magit-diff-while-committing'
(declare-function magit-commit-diff-1 "magit-commit" ())
(declare-function magit-commit-message-buffer "magit-commit" ())
;; For `magit-insert-revision-gravatar'
(defvar gravatar-size)
;; For `magit-show-commit' and `magit-diff-show-or-scroll'
(declare-function magit-current-blame-chunk "magit-blame" (&optional type noerror))
(declare-function magit-blame-mode "magit-blame" (&optional arg))
(defvar magit-blame-mode)
;; For `magit-diff-show-or-scroll'
(declare-function git-rebase-current-line "git-rebase" (&optional batch))
;; For `magit-diff-unmerged'
(declare-function magit-merge-in-progress-p "magit-merge" ())
(declare-function magit--merge-range "magit-merge" (&optional head))
;; For `magit-diff--dwim'
(declare-function forge--pullreq-range "ext:forge-pullreq"
                  (pullreq &optional endpoints))
(declare-function forge--pullreq-ref "ext:forge-pullreq" (pullreq))
;; For `magit-diff-wash-diff'
(declare-function ansi-color-apply-on-region "ansi-color")
;; For `magit-diff-wash-submodule'
(declare-function magit-log-wash-log "magit-log" (style args))
;; For keymaps and menus
(declare-function magit-apply "magit-apply" (&rest args))
(declare-function magit-stage "magit-apply" (&optional indent))
(declare-function magit-unstage "magit-apply" ())
(declare-function magit-discard "magit-apply" ())
(declare-function magit-reverse "magit-apply" (&rest args))
(declare-function magit-file-rename "magit-files" (file newname))
(declare-function magit-file-untrack "magit-files" (files &optional force))
(declare-function magit-commit-add-log "magit-commit" ())
(declare-function magit-diff-trace-definition "magit-log" ())
(declare-function magit-patch-save "magit-patch" (files &optional arg))
(declare-function magit-do-async-shell-command "magit-dired" (file))
(declare-function magit-add-change-log-entry "magit-extras"
                  (&optional whoami file-name other-window))
(declare-function magit-add-change-log-entry-other-window "magit-extras"
                  (&optional whoami file-name))
(declare-function magit-diff-edit-hunk-commit "magit-extras" ())
(declare-function magit-smerge-keep-current "magit-apply" ())
(declare-function magit-smerge-keep-all "magit-apply" ())
(declare-function magit-smerge-keep-upper "magit-apply" ())
(declare-function magit-smerge-keep-base "magit-apply" ())
(declare-function magit-smerge-keep-lower "magit-apply" ())

(eval-and-compile
  (cl-pushnew 'orig-rev eieio--known-slot-names)
  (cl-pushnew 'action-type eieio--known-slot-names)
  (cl-pushnew 'target eieio--known-slot-names))

(define-obsolete-variable-alias 'magit-diff-section-base-map
  'magit-diff-section-map "Magit 4.0.0")

(define-obsolete-variable-alias 'magit-wash-message-hook
  'magit-revision-wash-message-hook "Magit 4.3.0")

(make-obsolete-variable 'magit-diff-highlight-keywords
                        'magit-revision-wash-message-hook
                        "Magit 4.3.0")

;;; Options
;;;; Diff Mode

(defgroup magit-diff nil
  "Inspect and manipulate Git diffs."
  :link '(info-link "(magit)Diffing")
  :group 'magit-commands
  :group 'magit-modes)

(defcustom magit-diff-mode-hook nil
  "Hook run after entering Magit-Diff mode."
  :group 'magit-diff
  :type 'hook)

(defcustom magit-diff-sections-hook
  (list #'magit-insert-diff
        #'magit-insert-xref-buttons)
  "Hook run to insert sections into a `magit-diff-mode' buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-diff
  :type 'hook)

(defcustom magit-diff-expansion-threshold 60
  "After how many seconds not to expand anymore diffs.

Except in status buffers, diffs usually start out fully expanded.
Because that can take a long time, all diffs that haven't been
fontified during a refresh before the threshold defined here are
instead displayed with their bodies collapsed.

Note that this can cause sections that were previously expanded
to be collapsed.  So you should not pick a very low value here.

The hook function `magit-diff-expansion-threshold' has to be a
member of `magit-section-set-visibility-hook' for this option
to have any effect."
  :package-version '(magit . "2.9.0")
  :group 'magit-diff
  :type 'float)

(defcustom magit-diff-highlight-hunk-body t
  "Whether to highlight bodies of selected hunk sections."
  :package-version '(magit . "2.1.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-highlight-hunk-region-functions
  (list #'magit-diff-highlight-hunk-region-dim-outside
        #'magit-diff-highlight-hunk-region-using-overlays)
  "The functions used to highlight the hunk-internal region.

`magit-diff-highlight-hunk-region-dim-outside' overlays the outside
of the hunk internal selection with a face that causes the added and
removed lines to have the same background color as context lines.
This function should not be removed from the value of this option.

`magit-diff-highlight-hunk-region-using-overlays' and
`magit-diff-highlight-hunk-region-using-underline' emphasize the
region by placing delimiting horizontal lines before and after it.
The underline variant was implemented because Eli said that is
how we should do it.  However the overlay variant actually works
better.  Also see https://github.com/magit/magit/issues/2758.

Instead of, or in addition to, using delimiting horizontal lines,
to emphasize the boundaries, you may wish to emphasize the text
itself, using `magit-diff-highlight-hunk-region-using-face'.

In terminal frames it's not possible to draw lines as the overlay
and underline variants normally do, so there they fall back to
calling the face function instead."
  :package-version '(magit . "2.9.0")
  :set-after '(magit-diff-show-lines-boundaries)
  :group 'magit-diff
  :type 'hook
  :options (list #'magit-diff-highlight-hunk-region-dim-outside
                 #'magit-diff-highlight-hunk-region-using-underline
                 #'magit-diff-highlight-hunk-region-using-overlays
                 #'magit-diff-highlight-hunk-region-using-face))

(defcustom magit-diff-unmarked-lines-keep-foreground t
  "Whether `magit-diff-highlight-hunk-region-dim-outside' preserves foreground.
When this is set to nil, then that function only adjusts the
foreground color but added and removed lines outside the region
keep their distinct foreground colors."
  :package-version '(magit . "2.9.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-refine-hunk nil
  "Whether to show word-granularity differences within diff hunks.

`nil'  Never show fine differences.
`all'  Show fine differences for all displayed diff hunks.
`t'    Refine each hunk once it becomes the current section.
       Keep the refinement when another section is selected.
       Refreshing the buffer removes all refinement.  This
       variant is only provided for performance reasons."
  :group 'magit-diff
  :safe (##memq % '(nil t all))
  :type '(choice (const :tag "No refinement" nil)
                 (const :tag "Immediately refine all hunks" all)
                 (const :tag "Refine each hunk when moving to it" t)))

(put 'magit-diff-refine-hunk 'permanent-local t)

(defcustom magit-diff-refine-ignore-whitespace smerge-refine-ignore-whitespace
  "Whether to ignore whitespace changes in word-granularity differences."
  :package-version '(magit . "3.0.0")
  :set-after '(smerge-refine-ignore-whitespace)
  :group 'magit-diff
  :safe 'booleanp
  :type 'boolean)

(defcustom magit-diff-adjust-tab-width nil
  "Whether to adjust the width of tabs in diffs.

Determining the correct width can be expensive if it requires
opening large and/or many files, so the widths are cached in
the variable `magit-diff--tab-width-cache'.  Set that to `nil'
to invalidate the cache.

`nil'     Never adjust tab width.  Use `tab-width's value from
          the Magit buffer itself instead.

`t'       If the corresponding file-visiting buffer exits, then
          use `tab-width's value from that buffer.  Doing this is
          cheap, so this value is used even if a corresponding
          cache entry exists.

`always'  If there is no such buffer, then temporarily visit the
          file to determine the value.

NUMBER    Like `always', but don't visit files larger than NUMBER
          bytes."
  :package-version '(magit . "2.12.0")
  :group 'magit-diff
  :type '(choice (const   :tag "Never" nil)
                 (const   :tag "If file-visiting buffer exists" t)
                 (integer :tag "If file isn't larger than N bytes")
                 (const   :tag "Always" always)))

(defcustom magit-diff-paint-whitespace t
  "Specify where to highlight whitespace errors.

`nil'          Never highlight whitespace errors.
`t'            Highlight whitespace errors everywhere.
`uncommitted'  Only highlight whitespace errors in diffs
               showing uncommitted changes.

For backward compatibility `status' is treated as a synonym
for `uncommitted'.

The option `magit-diff-paint-whitespace-lines' controls for
what lines (added/remove/context) errors are highlighted.

The options `magit-diff-highlight-trailing' and
`magit-diff-highlight-indentation' control what kind of
whitespace errors are highlighted."
  :group 'magit-diff
  :safe (##memq % '(t nil uncommitted status))
  :type '(choice (const :tag "In all diffs" t)
                 (const :tag "Only in uncommitted changes" uncommitted)
                 (const :tag "Never" nil)))

(defcustom magit-diff-paint-whitespace-lines t
  "Specify in what kind of lines to highlight whitespace errors.

`t'       Highlight only in added lines.
`both'    Highlight in added and removed lines.
`all'     Highlight in added, removed and context lines."
  :package-version '(magit . "3.0.0")
  :group 'magit-diff
  :safe (##memq % '(t both all))
  :type '(choice (const :tag "In added lines" t)
                 (const :tag "In added and removed lines" both)
                 (const :tag "In added, removed and context lines" all)))

(defcustom magit-diff-highlight-trailing t
  "Whether to highlight whitespace at the end of a line in diffs.
Used only when `magit-diff-paint-whitespace' is non-nil."
  :group 'magit-diff
  :safe 'booleanp
  :type 'boolean)

(defcustom magit-diff-highlight-indentation nil
  "Highlight the \"wrong\" indentation style.
Used only when `magit-diff-paint-whitespace' is non-nil.

The value is an alist of the form ((REGEXP . INDENT)...).  The
path to the current repository is matched against each element
in reverse order.  Therefore if a REGEXP matches, then earlier
elements are not tried.

If the used INDENT is `tabs', highlight indentation with tabs.
If INDENT is an integer, highlight indentation with at least
that many spaces.  Otherwise, highlight neither."
  :group 'magit-diff
  :type `(repeat (cons (string :tag "Directory regexp")
                       (choice (const :tag "Tabs" tabs)
                               (integer :tag "Spaces" :value ,tab-width)
                               (const :tag "Neither" nil)))))

(defcustom magit-diff-hide-trailing-cr-characters
  (and (memq system-type '(ms-dos windows-nt)) t)
  "Whether to hide ^M characters at the end of a line in diffs."
  :package-version '(magit . "2.6.0")
  :group 'magit-diff
  :type 'boolean)

(defcustom magit-diff-extra-stat-arguments nil
  "Additional arguments to be used alongside `--stat'.

A list of zero or more arguments or a function that takes no
argument and returns such a list.  These arguments are allowed
here: `--stat-width', `--stat-name-width', `--stat-graph-width'
and `--compact-summary'.  See the git-diff(1) manpage."
  :package-version '(magit . "3.0.0")
  :group 'magit-diff
  :type `(radio (function-item ,#'magit-diff-use-window-width-as-stat-width)
                function
                (list string)
                (const :tag "None" nil)))

(defcustom magit-format-file-function #'magit-format-file-default
  "Function used to format lines representing a file.

This function is used for file headings in diffs, in diffstats and for
lists of files (such as the untracked files).  Depending on the caller,
it receives either three or five arguments; the signature has to be
\(kind file face &optional status orig).  KIND is one of `diff',
`module', `stat' and `list'."
  :package-version '(magit . "4.3.1")
  :group 'magit-diff
  :type `(radio (function-item ,#'magit-format-file-default)
                (function-item ,#'magit-format-file-all-the-icons)
                (function-item ,#'magit-format-file-nerd-icons)
                function))

;;;; File Diff

(defcustom magit-diff-buffer-file-locked t
  "Whether `magit-diff-buffer-file' uses a dedicated buffer."
  :package-version '(magit . "2.7.0")
  :group 'magit-commands
  :group 'magit-diff
  :type 'boolean)

;;;; Revision Mode

(defgroup magit-revision nil
  "Inspect and manipulate Git commits."
  :link '(info-link "(magit)Revision Buffer")
  :group 'magit-modes)

(defcustom magit-revision-mode-hook
  (list #'bug-reference-mode
        #'goto-address-mode)
  "Hook run after entering Magit-Revision mode."
  :group 'magit-revision
  :type 'hook
  :options '(bug-reference-mode
             goto-address-mode))

(defcustom magit-revision-sections-hook
  (list #'magit-insert-revision-tag
        #'magit-insert-revision-headers
        #'magit-insert-revision-message
        #'magit-insert-revision-notes
        #'magit-insert-revision-diff
        #'magit-insert-xref-buttons)
  "Hook run to insert sections into a `magit-revision-mode' buffer."
  :package-version '(magit . "2.3.0")
  :group 'magit-revision
  :type 'hook)

(defcustom magit-revision-wash-message-hook
  (list #'magit-highlight-squash-markers
        #'magit-highlight-bracket-keywords)
  "Functions used to highlight parts of a commit message.

These functions are called in order, in a buffer narrowed to the commit
message.  They should set text properties as they see fit, usually just
`font-lock-face'.  Before each function is called, point is at the
beginning of the narrowed region of the buffer.

See also the related `magit-log-wash-summary-hook'.  You likely want to
use the same functions for both hooks."
  :package-version '(magit . "4.3.0")
  :group 'magit-log
  :type 'hook
  :options (list #'magit-highlight-squash-markers
                 #'magit-highlight-bracket-keywords))

(defcustom magit-revision-headers-format "\
Author:     %aN <%aE>
AuthorDate: %ad
Commit:     %cN <%cE>
CommitDate: %cd
"
  "Format string used to insert headers in revision buffers.

All headers in revision buffers are inserted by the section
inserter `magit-insert-revision-headers'.  Some of the headers
are created by calling `git show --format=FORMAT' where FORMAT
is the format specified here.  Other headers are hard coded or
subject to option `magit-revision-insert-related-refs'."
  :package-version '(magit . "2.3.0")
  :group 'magit-revision
  :type 'string)

(defcustom magit-revision-insert-related-refs t
  "Whether to show related branches in revision buffers.

`nil'   Don't show any related branches.
`t'     Show related local branches.
`all'   Show related local and remote branches.
`mixed' Show all containing branches and local merged branches.

See user option `magit-revision-insert-related-refs-display-alist'
to hide specific sets of related branches."
  :package-version '(magit . "2.1.0")
  :group 'magit-revision
  :type '(choice (const :tag "Do not" nil)
                 (const :tag "Local only" t)
                 (const :tag "All related" all)
                 (const :tag "All containing, local merged" mixed)))

(defcustom magit-revision-insert-related-refs-display-alist nil
  "How `magit-insert-revision-headers' displays related branch types.

This is an alist, with recognised keys being the symbols
`parents', `merged', `contained', `follows', and `precedes';
and the supported values for each key being:

`nil'   Hide these related branches.
`t'     Show these related branches.

Keys which are not present in the alist have an implicit value `t'
\(so the default alist value of `nil' means all related branch types
will be shown.)

The types to be shown are additionally subject to user option
`magit-revision-insert-related-refs'."
  :package-version '(magit . "3.3.1")
  :group 'magit-revision
  :type '(alist :key-type (symbol :tag "Type of related branch")
                :value-type (boolean :tag "Display"))
  :options (mapcar (lambda (sym)
                     `(,sym (choice (const :tag "Hide" nil)
                                    (const :tag "Show" t))))
                   '(parents merged contained follows precedes)))

(defcustom magit-revision-use-hash-sections 'quicker
  "Whether to turn hashes inside the commit message into sections.

If non-nil, then hashes inside the commit message are turned into
`commit' sections.  There is a trade off to be made between
performance and reliability:

- `slow' calls git for every word to be absolutely sure.
- `quick' skips words less than seven characters long.
- `quicker' additionally skips words that don't contain a number.
- `quickest' uses all words that are at least seven characters
  long and which contain at least one number as well as at least
  one letter.

If `nil', then no hashes are turned into sections, but you can
still visit the commit at point using \"RET\"."
  :package-version '(magit . "2.12.0")
  :group 'magit-revision
  :type '(choice (const :tag "Use sections, quickest" quickest)
                 (const :tag "Use sections, quicker" quicker)
                 (const :tag "Use sections, quick" quick)
                 (const :tag "Use sections, slow" slow)
                 (const :tag "Don't use sections" nil)))

(defcustom magit-revision-show-gravatars nil
  "Whether to show gravatar images in revision buffers.

If `nil', then don't insert any gravatar images.  If `t', then
insert both images.  If `author' or `committer', then insert
only the respective image.

If you have customized the option `magit-revision-header-format'
and want to insert the images then you might also have to specify
where to do so.  In that case the value has to be a cons-cell of
two regular expressions.  The car specifies where to insert the
author's image.  The top half of the image is inserted right
after the matched text, the bottom half on the next line in the
same column.  The cdr specifies where to insert the committer's
image, accordingly.  Either the car or the cdr may be `nil'."
  :package-version '(magit . "2.3.0")
  :group 'magit-revision
  :type '(choice
          (const :tag "Don't show gravatars" nil)
          (const :tag "Show gravatars" t)
          (const :tag "Show author gravatar" author)
          (const :tag "Show committer gravatar" committer)
          (cons  :tag "Show gravatars using custom regexps"
                 (choice (const  :tag "No author image" nil)
                         (regexp :tag "Author regexp"    "^Author:     "))
                 (choice (const  :tag "No committer image" nil)
                         (regexp :tag "Committer regexp" "^Commit:     ")))))

(defcustom magit-revision-fill-summary-line nil
  "Whether to fill excessively long summary lines.

If this is an integer, then the summary line is filled if it is
longer than either the limit specified here or `window-width'.

You may want to only set this locally in \".dir-locals-2.el\" for
repositories known to contain bad commit messages.

The body of the message is left alone because (a) most people who
write excessively long summary lines usually don't add a body and
\(b) even people who have the decency to wrap their lines may have
a good reason to include a long line in the body sometimes."
  :package-version '(magit . "2.90.0")
  :group 'magit-revision
  :type '(choice (const   :tag "Don't fill" nil)
                 (integer :tag "Fill if longer than")))

(defcustom magit-revision-filter-files-on-follow nil
  "Whether to honor file filter if log arguments include --follow.

When a commit is displayed from a log buffer, the resulting
revision buffer usually shares the log's file arguments,
restricting the diff to those files.  However, there's a
complication when the log arguments include --follow: if the log
follows a file across a rename event, keeping the file
restriction would mean showing an empty diff in revision buffers
for commits before the rename event.

When this option is nil, the revision buffer ignores the log's
filter if the log arguments include --follow.  If non-nil, the
log's file filter is always honored."
  :package-version '(magit . "3.0.0")
  :group 'magit-revision
  :type 'boolean)

;;;; Visit Commands

(defcustom magit-diff-visit-previous-blob t
  "Whether `magit-diff-visit-file' may visit the previous blob.

When this is t and point is on a removed line in a diff for a
committed change, then `magit-diff-visit-file' visits the blob
from the last revision which still had that line."
  :package-version '(magit . "2.9.0")
  :group 'magit-diff
  :type 'boolean)

;;; Faces

(defface magit-diff-file-heading
  '((t :extend t :weight bold))
  "Face for diff file headings."
  :group 'magit-faces)

(defface magit-diff-file-heading-highlight
  '((t :extend t :inherit magit-section-highlight))
  "Face for current diff file headings."
  :group 'magit-faces)

(defface magit-diff-file-heading-selection
  '((((class color) (background light))
     :extend t
     :inherit magit-diff-file-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     :extend t
     :inherit magit-diff-file-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff file headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading
  '((((class color) (background light))
     :extend t
     :background "grey90"
     :foreground "grey20")
    (((class color) (background dark))
     :extend t
     :background "grey25"
     :foreground "grey95"))
  "Face for diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading-highlight
  '((((class color) (background light))
     :extend t
     :background "grey80"
     :foreground "grey20")
    (((class color) (background dark))
     :extend t
     :background "grey35"
     :foreground "grey95"))
  "Face for current diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-hunk-heading-selection
  '((((class color) (background light))
     :extend t
     :inherit magit-diff-hunk-heading-highlight
     :foreground "salmon4")
    (((class color) (background dark))
     :extend t
     :inherit magit-diff-hunk-heading-highlight
     :foreground "LightSalmon3"))
  "Face for selected diff hunk headings."
  :group 'magit-faces)

(defface magit-diff-hunk-region
  `((t :inherit bold
       :extend ,(ignore-errors (face-attribute 'region :extend))))
  "Face used by `magit-diff-highlight-hunk-region-using-face'.

This face is overlaid over text that uses other hunk faces,
and those normally set the foreground and background colors.
The `:foreground' and especially the `:background' properties
should be avoided here.  Setting the latter would cause the
loss of information.  Good properties to set here are `:weight'
and `:slant'."
  :group 'magit-faces)

(defface magit-diff-conflict-heading-highlight
  '((t :inherit magit-diff-hunk-heading-highlight))
  "Face for conflict markers."
  :group 'magit-faces)

(defface magit-diff-revision-summary
  '((t :inherit magit-diff-hunk-heading))
  "Face for commit message summaries."
  :group 'magit-faces)

(defface magit-diff-revision-summary-highlight
  '((t :inherit magit-diff-hunk-heading-highlight))
  "Face for highlighted commit message summaries."
  :group 'magit-faces)

(defface magit-diff-lines-heading
  '((((class color) (background light))
     :extend t
     :inherit magit-diff-hunk-heading-highlight
     :background "LightSalmon3")
    (((class color) (background dark))
     :extend t
     :inherit magit-diff-hunk-heading-highlight
     :foreground "grey80"
     :background "salmon4"))
  "Face for diff hunk heading when lines are marked."
  :group 'magit-faces)

(defface magit-diff-lines-boundary
  '((t :extend t :inherit magit-diff-lines-heading))
  "Face for boundary of marked lines in diff hunk."
  :group 'magit-faces)

(defface magit-diff-conflict-heading
  '((t :inherit magit-diff-hunk-heading))
  "Face for conflict markers."
  :group 'magit-faces)

(defface magit-diff-added
  '((((class color) (background light))
     :extend t
     :background "#ddffdd"
     :foreground "#22aa22")
    (((class color) (background dark))
     :extend t
     :background "#335533"
     :foreground "#ddffdd"))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed
  '((((class color) (background light))
     :extend t
     :background "#ffdddd"
     :foreground "#aa2222")
    (((class color) (background dark))
     :extend t
     :background "#553333"
     :foreground "#ffdddd"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-our
  '((t :inherit magit-diff-removed))
  "Face for lines in a diff for our side in a conflict."
  :group 'magit-faces)

(defface magit-diff-base
  '((((class color) (background light))
     :extend t
     :background "#ffffcc"
     :foreground "#aaaa11")
    (((class color) (background dark))
     :extend t
     :background "#555522"
     :foreground "#ffffcc"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'magit-faces)

(defface magit-diff-their
  '((t :inherit magit-diff-added))
  "Face for lines in a diff for their side in a conflict."
  :group 'magit-faces)

(defface magit-diff-context
  '((((class color) (background light))
     :extend t
     :foreground "grey50")
    (((class color) (background  dark))
     :extend t
     :foreground "grey70"))
  "Face for lines in a diff that are unchanged."
  :group 'magit-faces)

(defface magit-diff-added-highlight
  '((((class color) (background light))
     :extend t
     :background "#cceecc"
     :foreground "#22aa22")
    (((class color) (background dark))
     :extend t
     :background "#336633"
     :foreground "#cceecc"))
  "Face for lines in a diff that have been added."
  :group 'magit-faces)

(defface magit-diff-removed-highlight
  '((((class color) (background light))
     :extend t
     :background "#eecccc"
     :foreground "#aa2222")
    (((class color) (background dark))
     :extend t
     :background "#663333"
     :foreground "#eecccc"))
  "Face for lines in a diff that have been removed."
  :group 'magit-faces)

(defface magit-diff-our-highlight
  '((t :inherit magit-diff-removed-highlight))
  "Face for lines in a diff for our side in a conflict."
  :group 'magit-faces)

(defface magit-diff-base-highlight
  '((((class color) (background light))
     :extend t
     :background "#eeeebb"
     :foreground "#aaaa11")
    (((class color) (background dark))
     :extend t
     :background "#666622"
     :foreground "#eeeebb"))
  "Face for lines in a diff for the base side in a conflict."
  :group 'magit-faces)

(defface magit-diff-their-highlight
  '((t :inherit magit-diff-added-highlight))
  "Face for lines in a diff for their side in a conflict."
  :group 'magit-faces)

(defface magit-diff-context-highlight
  '((((class color) (background light))
     :extend t
     :background "grey95"
     :foreground "grey50")
    (((class color) (background dark))
     :extend t
     :background "grey20"
     :foreground "grey70"))
  "Face for lines in the current context in a diff."
  :group 'magit-faces)

(defface magit-diff-whitespace-warning
  '((t :inherit trailing-whitespace))
  "Face for highlighting whitespace errors added lines."
  :group 'magit-faces)

(defface magit-diffstat-added
  '((((class color) (background light)) :foreground "#22aa22")
    (((class color) (background  dark)) :foreground "#448844"))
  "Face for addition indicator in diffstat."
  :group 'magit-faces)

(defface magit-diffstat-removed
  '((((class color) (background light)) :foreground "#aa2222")
    (((class color) (background  dark)) :foreground "#aa4444"))
  "Face for removal indicator in diffstat."
  :group 'magit-faces)

;;; Arguments
;;;; Prefix Classes

(defclass magit-diff-prefix (transient-prefix)
  ((history-key :initform 'magit-diff)
   (major-mode  :initform 'magit-diff-mode)))

(defclass magit-diff-refresh-prefix (magit-diff-prefix)
  ((history-key :initform 'magit-diff)
   (major-mode  :initform nil)))

;;;; Prefix Methods

(cl-defmethod transient-init-value ((obj magit-diff-prefix))
  (pcase-let ((`(,args ,files)
               (magit-diff--get-value 'magit-diff-mode 'prefix)))
    (when-let (((not (eq transient-current-command 'magit-dispatch)))
               (file (magit-file-relative-name)))
      (setq files (list file)))
    (oset obj value (if files `(("--" ,@files) ,@args) args))))

(cl-defmethod transient-init-value ((obj magit-diff-refresh-prefix))
  (oset obj value (if magit-buffer-diff-files
                      `(("--" ,@magit-buffer-diff-files)
                        ,@magit-buffer-diff-args)
                    magit-buffer-diff-args)))

(cl-defmethod transient-set-value ((obj magit-diff-prefix))
  (magit-diff--set-value obj))

(cl-defmethod transient-save-value ((obj magit-diff-prefix))
  (magit-diff--set-value obj 'save))

;;;; Argument Access

(defun magit-diff-arguments (&optional mode)
  "Return the current diff arguments."
  (if (memq transient-current-command '(magit-diff magit-diff-refresh))
      (magit--transient-args-and-files)
    (magit-diff--get-value (or mode 'magit-diff-mode) 'direct)))

(defun magit-diff--get-value (mode &optional use-buffer-args)
  (setq use-buffer-args
        (pcase-exhaustive use-buffer-args
          ('prefix magit-prefix-use-buffer-arguments)
          ('status magit-status-use-buffer-arguments)
          ('direct magit-direct-use-buffer-arguments)
          ('nil    magit-direct-use-buffer-arguments)
          ((or 'always 'selected 'current 'never)
           use-buffer-args)))
  (let (args files)
    (cond
     ((and (memq use-buffer-args '(always selected current))
           (eq major-mode mode))
      (setq args  magit-buffer-diff-args)
      (setq files magit-buffer-diff-files))
     ((when-let (((memq use-buffer-args '(always selected)))
                 (buffer (magit-get-mode-buffer
                          mode nil
                          (eq use-buffer-args 'selected))))
        (setq args  (buffer-local-value 'magit-buffer-diff-args buffer))
        (setq files (buffer-local-value 'magit-buffer-diff-files buffer))
        t))
     ((plist-member (symbol-plist mode) 'magit-diff-current-arguments)
      (setq args (get mode 'magit-diff-current-arguments)))
     ((when-let ((elt (assq (intern (format "magit-diff:%s" mode))
                            transient-values)))
        (setq args (cdr elt))
        t))
     (t
      (setq args (get mode 'magit-diff-default-arguments))))
    (list args files)))

(defun magit-diff--set-value (obj &optional save)
  (pcase-let* ((obj  (oref obj prototype))
               (mode (or (oref obj major-mode) major-mode))
               (key  (intern (format "magit-diff:%s" mode)))
               (`(,args ,files) (magit--transient-args-and-files)))
    (put mode 'magit-diff-current-arguments args)
    (when save
      (setf (alist-get key transient-values) args)
      (transient-save-values))
    (transient--history-push obj)
    (setq magit-buffer-diff-args args)
    (setq magit-buffer-diff-files files)
    (magit-refresh)))

;;; Commands
;;;; Prefix Commands

(transient-define-group magit-diff-infix-arguments
  ["Limit arguments"
   (magit:--)
   (magit-diff:--ignore-submodules)
   ("-b" "Ignore whitespace changes"  ("-b" "--ignore-space-change"))
   ("-w" "Ignore all whitespace"      ("-w" "--ignore-all-space"))
   ("-D" "Omit preimage for deletes"  ("-D" "--irreversible-delete")
    :level 5)]
  ["Context arguments"
   (magit-diff:-U)
   ("-W" "Show surrounding functions" ("-W" "--function-context"))]
  ["Tune arguments"
   (magit-diff:--diff-algorithm)
   (magit-diff:--diff-merges)
   (magit-diff:-M)
   (magit-diff:-C)
   (magit-diff:-R               :level 5)
   (magit-diff:--color-moved    :level 5)
   (magit-diff:--color-moved-ws :level 5)
   (magit-diff:--no-ext-diff)
   (magit-diff:--stat)
   (magit-diff:--show-signature)])

;;;###autoload (autoload 'magit-diff "magit-diff" nil t)
(transient-define-prefix magit-diff ()
  "Show changes between different versions."
  :man-page "git-diff"
  :class 'magit-diff-prefix
  'magit-diff-infix-arguments
  ["Actions"
   [("d" "Dwim"          magit-diff-dwim)
    ("r" "Diff range"    magit-diff-range)
    ("p" "Diff paths"    magit-diff-paths)]
   [("u" "Diff unstaged" magit-diff-unstaged)
    ("s" "Diff staged"   magit-diff-staged)
    ("w" "Diff worktree" magit-diff-working-tree)]
   [("c" "Show commit"   magit-show-commit)
    ("t" "Show stash"    magit-stash-show)]])

;;;###autoload (autoload 'magit-diff-refresh "magit-diff" nil t)
(transient-define-prefix magit-diff-refresh ()
  "Change the arguments used for the diff(s) in the current buffer."
  :man-page "git-diff"
  :class 'magit-diff-refresh-prefix
  'magit-diff-infix-arguments
  [["Refresh"
    ("g" "buffer"                   magit-diff-refresh)
    ("s" "buffer and set defaults"  transient-set-and-exit)
    ("w" "buffer and save defaults" transient-save-and-exit)]
   ["Toggle"
    ("t" "hunk refinement"          magit-diff-toggle-refine-hunk)
    ("F" "file filter"              magit-diff-toggle-file-filter)
    ("b" "buffer lock"              magit-toggle-buffer-lock
     :if-mode (magit-diff-mode magit-revision-mode magit-stash-mode))]
   [:if-mode magit-diff-mode
    :description "Do"
    ("r" "switch range type"        magit-diff-switch-range-type)
    ("f" "flip revisions"           magit-diff-flip-revs)]]
  (interactive)
  (when (derived-mode-p 'magit-merge-preview-mode)
    (user-error "Cannot use %s in %s" this-command major-mode))
  (if (not (eq transient-current-command 'magit-diff-refresh))
      (transient-setup 'magit-diff-refresh)
    (pcase-let ((`(,args ,files) (magit-diff-arguments)))
      (setq magit-buffer-diff-args args)
      (setq magit-buffer-diff-files files))
    (magit-refresh)))

;;;; Infix Commands

(transient-define-argument magit:-- ()
  :description "Limit to files"
  :class 'transient-files
  :key "--"
  :argument "--"
  :prompt "Limit to file,s: "
  :reader #'magit-read-files
  :multi-value t)

(defun magit-read-files (prompt initial-input history &optional list-fn)
  (magit-with-toplevel
    (magit-completing-read-multiple prompt
                                    (funcall (or list-fn #'magit-list-files))
                                    nil nil
                                    (or initial-input (magit-file-at-point))
                                    history)))

(transient-define-argument magit-diff:-U ()
  :description "Context lines"
  :class 'transient-option
  :argument "-U"
  :reader #'transient-read-number-N0)

(transient-define-argument magit-diff:-M ()
  :description "Detect renames"
  :class 'transient-option
  :argument "-M"
  :allow-empty t
  :reader #'transient-read-number-N+)

(transient-define-argument magit-diff:-C ()
  :description "Detect copies"
  :class 'transient-option
  :argument "-C"
  :allow-empty t
  :reader #'transient-read-number-N+)

(transient-define-argument magit-diff:--diff-algorithm ()
  :description "Diff algorithm"
  :class 'transient-option
  :key "-A"
  :argument "--diff-algorithm="
  :reader #'magit-diff-select-algorithm
  :always-read t)

(defun magit-diff-select-algorithm (&rest _ignore)
  (magit-read-char-case nil t
    (?u "[u]nspecified" nil)
    (?d "[d]efault"     "default")
    (?m "[m]inimal"     "minimal")
    (?p "[p]atience"    "patience")
    (?h "[h]istogram"   "histogram")))

(transient-define-argument magit-diff:--diff-merges ()
  :description "Diff merges"
  :class 'transient-option
  :key "-X"
  :argument "--diff-merges="
  :reader #'magit-diff-select-merges
  :always-read t)

(defun magit-diff-select-merges (&rest _ignore)
  (magit-read-char-case nil t
    (?u "[u]nspecified"    nil)
    (?o "[o]ff"            "off")
    (?f "[f]irst-parent"   "first-parent")
    (?c "[c]ombined"       "combined")
    (?d "[d]ense-combined" "dense-combined")))

(transient-define-argument magit-diff:--ignore-submodules ()
  :description "Ignore submodules"
  :class 'transient-option
  :key "-i"
  :argument "--ignore-submodules="
  :reader #'magit-diff-select-ignore-submodules)

(defun magit-diff-select-ignore-submodules (&rest _ignored)
  (magit-read-char-case "Ignore submodules " t
    (?u "[u]ntracked" "untracked")
    (?d "[d]irty"     "dirty")
    (?a "[a]ll"       "all")))

(transient-define-argument magit-diff:--color-moved ()
  :description "Color moved lines"
  :class 'transient-option
  :key "-m"
  :argument "--color-moved="
  :reader #'magit-diff-select-color-moved-mode)

(defun magit-diff-select-color-moved-mode (&rest _ignore)
  (magit-read-char-case "Color moved " t
    (?d "[d]efault" "default")
    (?p "[p]lain"   "plain")
    (?b "[b]locks"  "blocks")
    (?z "[z]ebra"   "zebra")
    (?Z "[Z] dimmed-zebra" "dimmed-zebra")))

(transient-define-argument magit-diff:--color-moved-ws ()
  :description "Whitespace treatment for --color-moved"
  :class 'transient-option
  :key "=w"
  :argument "--color-moved-ws="
  :reader #'magit-diff-select-color-moved-ws-mode)

(defun magit-diff-select-color-moved-ws-mode (&rest _ignore)
  (magit-read-char-case "Ignore whitespace " t
    (?i "[i]ndentation"  "allow-indentation-change")
    (?e "[e]nd of line"  "ignore-space-at-eol")
    (?s "[s]pace change" "ignore-space-change")
    (?a "[a]ll space"    "ignore-all-space")
    (?n "[n]o"           "no")))

(transient-define-argument magit-diff:-R ()
  :description "Reverse sides"
  :class 'transient-switch
  :argument "-R"
  :if 'magit-diff-argument-predicate)

(transient-define-argument magit-diff:--no-ext-diff ()
  :description "Disallow external diff drivers"
  :class 'transient-switch
  :argument "--no-ext-diff"
  :key "-x")

(transient-define-argument magit-diff:--stat ()
  :description "Show stats"
  :class 'transient-switch
  :argument "--stat"
  :key "-s"
  :if 'magit-diff-argument-predicate)

(transient-define-argument magit-diff:--show-signature ()
  :description "Show signature"
  :class 'transient-switch
  :argument "--show-signature"
  :key "=g"
  :if 'magit-diff-argument-predicate)

(defun magit-diff-argument-predicate ()
  (or (eq (oref transient--prefix command) 'magit-diff)
      (derived-mode-p 'magit-diff-mode)))

;;;; Setup Commands

;;;###autoload
(defun magit-diff-dwim (&optional args files)
  "Show changes for the thing at point.

For example, if point is on a commit, show the changes introduced by
that commit.  Likewise if point is on the section titled \"Unstaged
changes\", then show those changes in a separate buffer.  Generally
speaking, compare the thing at point with the most logical, trivial
and (in *any* situation) at least potentially useful other thing it
could be compared to.

When the region selects commits, then compare the two commits at
either end.  There are different ways two commits can be compared.
In the buffer showing the diff, you can control how the comparison,
is done, using \"D r\" and \"D f\".

This function does not always show the changes that you might want
to view in any given situation.  You can think of the changes being
shown as the smallest common denominator.  There is no AI involved.
If this command never does what you want, then ignore it, and instead
use the commands that allow you to explicitly specify what you need."
  (interactive (magit-diff-arguments))
  (let ((default-directory default-directory)
        (section (magit-current-section)))
    (cond
     ((magit-section-match 'module section)
      (setq default-directory
            (expand-file-name
             (file-name-as-directory (oref section value))))
      (magit-diff-range (oref section range)))
     (t
      (when (magit-section-match 'module-commit section)
        (setq args nil)
        (setq files nil)
        (setq default-directory
              (expand-file-name
               (file-name-as-directory (magit-section-parent-value section)))))
      (pcase (magit-diff--dwim)
        ('unmerged (magit-diff-unmerged args files))
        ('unstaged (magit-diff-unstaged args files))
        ('staged
         (let ((file (magit-file-at-point)))
           (if (and file (equal (cddr (car (magit-file-status file))) '(?D ?U)))
               ;; File was deleted by us and modified by them.  Show the latter.
               (magit-diff-unmerged args (list file))
             (magit-diff-staged nil args files))))
        (`(stash . ,value) (magit-stash-show value args))
        (`(commit . ,value)
         (magit-diff-range (format "%s^..%s" value value) args files))
        ((and range (pred stringp))
         (magit-diff-range range args files))
        (_ (call-interactively #'magit-diff-range)))))))

(defun magit-diff--dwim ()
  "Return information for performing DWIM diff.

The information can be in three forms:
1. TYPE
   A symbol describing a type of diff where no additional information
   is needed to generate the diff.  One of `staged', `unstaged',
   `unmerged', and `undefined'.
2. (TYPE . VALUE)
   Like #1 but the diff requires additional information, which is
   given by VALUE.  Currently, this includes `commit' and `stash',
   where VALUE is the given commit or stash, respectively.
3. RANGE
   A string indicating a diff range.

If no DWIM context is found, nil is returned."
  (cond
   ((and-let* ((commits (magit-region-values '(commit branch) t)))
      (progn
        (deactivate-mark)
        (concat (car (last commits)) ".." (car commits)))))
   (magit-buffer-refname
    (cons 'commit magit-buffer-refname))
   ((derived-mode-p 'magit-stash-mode)
    (cons 'commit
          (magit-section-case
            (commit (oref it value))
            (file (thread-first it
                    (oref parent)
                    (oref value)))
            (hunk (thread-first it
                    (oref parent)
                    (oref parent)
                    (oref value))))))
   ((derived-mode-p 'magit-revision-mode)
    (cons 'commit magit-buffer-revision))
   ((derived-mode-p 'magit-diff-mode)
    (pcase-exhaustive magit-buffer-diff-type
      ('committed magit-buffer-range)
      ((or 'unstaged 'staged 'undefined) magit-buffer-diff-type)))
   (t
    (magit-section-case
      ([* unstaged] 'unstaged)
      ([* staged] 'staged)
      (unmerged 'unmerged)
      (unpushed (magit-diff--range-to-endpoints (oref it value)))
      (unpulled (magit-diff--range-to-endpoints (oref it value)))
      (branch (let ((current (magit-get-current-branch))
                    (atpoint (oref it value)))
                (if (equal atpoint current)
                    (if-let ((upstream (magit-get-upstream-branch)))
                        (format "%s...%s" upstream current)
                      (if (magit-anything-modified-p)
                          current
                        (cons 'commit current)))
                  (format "%s...%s"
                          (or current "HEAD")
                          atpoint))))
      (commit (cons 'commit (oref it value)))
      ([file commit] (cons 'commit (oref (oref it parent) value)))
      ([hunk file commit]
       (cons 'commit (oref (oref (oref it parent) parent) value)))
      (stash (cons 'stash (oref it value)))
      (pullreq (forge--pullreq-range (oref it value) t))))))

(defun magit-diff--range-to-endpoints (range)
  (cond ((string-match "\\.\\.\\." range) (replace-match ".."  nil nil range))
        ((string-match "\\.\\."    range) (replace-match "..." nil nil range))
        (t range)))

(defun magit-diff--region-range (&optional interactive mbase)
  (and-let* ((commits (magit-region-values '(commit branch) t))
             (revA (car (last commits)))
             (revB (car commits)))
    (progn
      (when interactive
        (deactivate-mark))
      (if mbase
          (let ((base (magit-git-string "merge-base" revA revB)))
            (cond
             ((string= (magit-rev-parse revA) base)
              (format "%s..%s" revA revB))
             ((string= (magit-rev-parse revB) base)
              (format "%s..%s" revB revA))
             (interactive
              (let ((main (magit-completing-read "View changes along"
                                                 (list revA revB)
                                                 nil t nil nil revB)))
                (format "%s...%s"
                        (if (string= main revB) revA revB) main)))
             (t "%s...%s" revA revB)))
        (format "%s..%s" revA revB)))))

(defun magit-diff-read-range-or-commit (prompt &optional secondary-default mbase)
  "Read range or revision with special diff range treatment.
If MBASE is non-nil, prompt for which rev to place at the end of
a \"revA...revB\" range.  Otherwise, always construct
\"revA..revB\" range."
  (or (magit-diff--region-range t mbase)
      (magit-read-range prompt
                        (or (pcase (magit-diff--dwim)
                              (`(commit . ,value)
                               (format "%s^..%s" value value))
                              ((and range (pred stringp))
                               range))
                            secondary-default
                            (magit-get-current-branch)))))

;;;###autoload
(defun magit-diff-range (rev-or-range &optional args files)
  "Show differences between two commits.

REV-OR-RANGE should be a range or a single revision.  If it is a
revision, then show changes in the working tree relative to that
revision.  If it is a range, but one side is omitted, then show
changes relative to `HEAD'.

If the region is active, use the revisions on the first and last
line of the region as the two sides of the range.  With a prefix
argument, instead of diffing the revisions, choose a revision to
view changes along, starting at the common ancestor of both
revisions (i.e., use a \"...\" range)."
  (interactive (cons (magit-diff-read-range-or-commit "Diff for range"
                                                      nil current-prefix-arg)
                     (magit-diff-arguments)))
  (magit-diff-setup-buffer rev-or-range nil args files 'committed))

;;;###autoload
(defun magit-diff-working-tree (&optional rev args files)
  "Show changes between the current working tree and the `HEAD' commit.
With a prefix argument show changes between the working tree and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (magit-read-branch-or-commit "Diff working tree and commit"))
         (magit-diff-arguments)))
  (magit-diff-setup-buffer (or rev "HEAD") nil args files 'committed))

;;;###autoload
(defun magit-diff-staged (&optional rev args files)
  "Show changes between the index and the `HEAD' commit.
With a prefix argument show changes between the index and
a commit read from the minibuffer."
  (interactive
   (cons (and current-prefix-arg
              (magit-read-branch-or-commit "Diff index and commit"))
         (magit-diff-arguments)))
  (magit-diff-setup-buffer rev "--cached" args files 'staged))

;;;###autoload
(defun magit-diff-unstaged (&optional args files)
  "Show changes between the working tree and the index."
  (interactive (magit-diff-arguments))
  (magit-diff-setup-buffer nil nil args files 'unstaged))

;;;###autoload
(defun magit-diff-unmerged (&optional args files)
  "Show changes that are being merged."
  (interactive (magit-diff-arguments))
  (unless (magit-merge-in-progress-p)
    (user-error "No merge is in progress"))
  (magit-diff-setup-buffer (magit--merge-range) nil args files 'committed))

;;;###autoload
(defun magit-diff-while-committing ()
  "While committing, show the changes that are about to be committed.
While amending, invoking the command again toggles between
showing just the new changes or all the changes that will
be committed."
  (interactive)
  (unless (magit-commit-message-buffer)
    (user-error "No commit in progress"))
  (magit-commit-diff-1))

;;;###autoload
(defun magit-diff-buffer-file ()
  "Show diff for the blob or file visited in the current buffer.

When the buffer visits a blob, then show the respective commit.
When the buffer visits a file, then show the differences between
`HEAD' and the working tree.  In both cases limit the diff to
the file or blob."
  (interactive)
  (require 'magit)
  (if-let ((file (magit-file-relative-name)))
      (if magit-buffer-refname
          (magit-show-commit magit-buffer-refname
                             (car (magit-show-commit--arguments))
                             (list file))
        (save-buffer)
        (let ((line (line-number-at-pos))
              (col (current-column)))
          (with-current-buffer
              (magit-diff-setup-buffer (or (magit-get-current-branch) "HEAD")
                                       nil
                                       (car (magit-diff-arguments))
                                       (list file)
                                       'unstaged
                                       magit-diff-buffer-file-locked)
            (magit-diff--goto-file-position file line col))))
    (user-error "Buffer isn't visiting a file")))

;;;###autoload
(defun magit-diff-paths (a b)
  "Show changes between any two files on disk."
  (interactive (list (read-file-name "First file: " nil nil t)
                     (read-file-name "Second file: " nil nil t)))
  (magit-diff-setup-buffer nil "--no-index" nil
                           (list (magit-convert-filename-for-git
                                  (expand-file-name a))
                                 (magit-convert-filename-for-git
                                  (expand-file-name b)))
                           'undefined))

(defun magit-show-commit--arguments ()
  (pcase-let ((`(,args ,diff-files)
               (magit-diff-arguments 'magit-revision-mode)))
    (list args (if (derived-mode-p 'magit-log-mode)
                   (and (or magit-revision-filter-files-on-follow
                            (not (member "--follow" magit-buffer-log-args)))
                        magit-buffer-log-files)
                 diff-files))))

;;;###autoload
(defun magit-show-commit (rev &optional args files module)
  "Visit the revision at point in another buffer.
If there is no revision at point or with a prefix argument prompt
for a revision."
  (interactive
   (pcase-let* ((mcommit (magit-section-value-if 'module-commit))
                (atpoint (or mcommit
                             (magit-thing-at-point 'git-revision t)
                             (magit-branch-or-commit-at-point)))
                (`(,args ,files) (magit-show-commit--arguments)))
     (list (or (and (not current-prefix-arg) atpoint)
               (magit-read-branch-or-commit "Show commit" atpoint))
           args
           files
           (and mcommit
                (magit-section-parent-value (magit-current-section))))))
  (require 'magit)
  (let* ((file (magit-file-relative-name))
         (line (and file (line-number-at-pos))))
    (magit-with-toplevel
      (when module
        (setq default-directory
              (expand-file-name (file-name-as-directory module))))
      (unless (magit-commit-p rev)
        (user-error "%s is not a commit" rev))
      (when file
        (save-buffer))
      (let ((buf (magit-revision-setup-buffer rev args files)))
        (when file
          (let ((line (magit-diff-visit--offset file (list "-R" rev) line))
                (col (current-column)))
            (with-current-buffer buf
              (magit-diff--goto-file-position file line col))))))))

(defun magit-diff--locate-file-position (file line column &optional parent)
  (and-let*
      ((parent (pcase parent
                 ('unstaged (magit-get-section '((unstaged) (status))))
                 ('staged   (magit-get-section '((staged)   (status))))
                 ('nil (and (cl-typep (car (oref magit-root-section children))
                                      'magit-file-section)
                            magit-root-section))
                 (_ parent)))
       (diff (cl-find-if (##equal (oref % value) file)
                         (oref parent children)))
       (hunks (oref diff children)))
    (let (hunk pos found)
      (while (and (setq hunk (pop hunks))
                  (not pos))
        (when-let* ((range (oref hunk to-range))
                    (beg (car range))
                    (len (cadr range))
                    (end (+ beg len)))
          (cond
           ((> beg line)
            (setq pos (oref diff start)))
           ((<= beg line end)
            (save-excursion
              (goto-char (oref hunk content))
              (let ((l beg))
                (while (or (< l line)
                           (= (char-after) ?-))
                  (unless (= (char-after) ?-)
                    (cl-incf l))
                  (forward-line)))
              (setq found (if (= (char-after) ?+) 'line 'hunk))
              (forward-char (1+ column))
              (setq pos (point))))
           ((null hunks)
            (setq pos (oref hunk start))))))
      (and pos
           (list pos (or found file))))))

(defun magit-diff--goto-file-position (file line column &optional parent)
  (when-let ((pos (magit-diff--locate-file-position file line column parent)))
    (goto-char (car pos))
    (magit-section-reveal (magit-current-section))))

;;;; Setting Commands

(defun magit-diff-switch-range-type ()
  "Convert diff range type.
Change \"revA..revB\" to \"revA...revB\", or vice versa."
  (interactive)
  (if (and magit-buffer-range
           (derived-mode-p 'magit-diff-mode)
           (string-match magit-range-re magit-buffer-range))
      (setq magit-buffer-range
            (replace-match (if (string= (match-str 2 magit-buffer-range) "..")
                               "..."
                             "..")
                           t t magit-buffer-range 2))
    (user-error "No range to change"))
  (magit-refresh))

(defun magit-diff-flip-revs ()
  "Swap revisions in diff range.
Change \"revA..revB\" to \"revB..revA\"."
  (interactive)
  (if (and magit-buffer-range
           (derived-mode-p 'magit-diff-mode)
           (string-match magit-range-re magit-buffer-range))
      (progn
        (setq magit-buffer-range
              (concat (match-str 3 magit-buffer-range)
                      (match-str 2 magit-buffer-range)
                      (match-str 1 magit-buffer-range)))
        (magit-refresh))
    (user-error "No range to swap")))

(defun magit-diff-toggle-file-filter ()
  "Toggle the file restriction of the current buffer's diffs.
If the current buffer's mode is derived from `magit-log-mode',
toggle the file restriction in the repository's revision buffer
instead."
  (interactive)
  (cl-flet ((toggle ()
              (if (or magit-buffer-diff-files
                      magit-buffer-diff-files-suspended)
                  (cl-rotatef magit-buffer-diff-files
                              magit-buffer-diff-files-suspended)
                (setq magit-buffer-diff-files
                      (transient-infix-read 'magit:--)))
              (magit-refresh)))
    (cond
     ((derived-mode-p 'magit-log-mode
                      'magit-cherry-mode
                      'magit-reflog-mode)
      (if-let ((buffer (magit-get-mode-buffer 'magit-revision-mode)))
          (with-current-buffer buffer (toggle))
        (message "No revision buffer")))
     ((local-variable-p 'magit-buffer-diff-files)
      (toggle))
     (t
      (user-error "Cannot toggle file filter in this buffer")))))

(defun magit-diff-less-context (&optional count)
  "Decrease the context for diff hunks by COUNT lines."
  (interactive "p")
  (magit-diff-set-context (##max 0 (- (or % 0) count))))

(defun magit-diff-more-context (&optional count)
  "Increase the context for diff hunks by COUNT lines."
  (interactive "p")
  (magit-diff-set-context (##+ (or % 0) count)))

(defun magit-diff-default-context ()
  "Reset context for diff hunks to the default height."
  (interactive)
  (magit-diff-set-context #'ignore))

(defun magit-diff-set-context (fn)
  (when (derived-mode-p 'magit-merge-preview-mode)
    (user-error "Cannot use %s in %s" this-command major-mode))
  (let* ((def (if-let ((context (magit-get "diff.context")))
                  (string-to-number context)
                3))
         (val magit-buffer-diff-args)
         (arg (seq-find (##string-match "^-U\\([0-9]+\\)?$" %) val))
         (num (if-let ((str (and arg (match-str 1 arg))))
                  (string-to-number str)
                def))
         (val (delete arg val))
         (num (funcall fn num))
         (arg (and num (not (= num def)) (format "-U%d" num)))
         (val (if arg (cons arg val) val)))
    (setq magit-buffer-diff-args val))
  (magit-refresh))

(defun magit-diff-get-context ()
  (string-to-number
   (or (seq-some (##and (string-match "\\`-U\\([0-9]+\\)?\\'" %)
                        (match-str 1 %))
                 magit-buffer-diff-args)
       (magit-get "diff.context")
       "3")))

(defun magit-diff-context-p ()
  (if-let ((arg (seq-find (##string-match "^-U\\([0-9]+\\)$" %)
                          magit-buffer-diff-args)))
      (not (equal arg "-U0"))
    t))

(defun magit-diff-ignore-any-space-p ()
  (seq-some (##member % magit-buffer-diff-args)
            '("--ignore-cr-at-eol"
              "--ignore-space-at-eol"
              "--ignore-space-change" "-b"
              "--ignore-all-space" "-w"
              "--ignore-blank-space")))

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

;;;; Visit Commands
;;;;; Dwim Variants

(defun magit-diff-visit-file (&optional other-window)
  "From a diff visit a version of the file at point.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead.

The location of point inside the diff determines which file is
being visited.  The visited version depends on what changes the
diff is about.

1. If the diff shows uncommitted changes (i.e., stage or unstaged
   changes), then visit the file in the working tree (i.e., the
   same \"real\" file that `find-file' would visit).  In all
   other cases visit a \"blob\" (i.e., the version of a file as
   stored in some commit).

2. If point is on a removed line, then visit the blob for the
   first parent of the commit that removed that line, i.e., the
   last commit where that line still exists.

3. If point is on an added or context line, then visit the blob
   that adds that line, or if the diff shows from more than a
   single commit, then visit the blob from the last of these
   commits.

In the file-visiting buffer also go to the line that corresponds
to the line that point is on in the diff.

Note that this command only works if point is inside a diff.
In other cases `magit-find-file' (which see) has to be used."
  (interactive "P")
  (magit-diff-visit-file--internal nil (and other-window t)))

(defun magit-diff-visit-file-other-window ()
  "From a diff visit a version of the file at point in another window.
Like `magit-diff-visit-file' but always display in another window."
  (interactive)
  (magit-diff-visit-file--internal nil #'switch-to-buffer-other-window))

(defun magit-diff-visit-file-other-frame ()
  "From a diff visit a version of the file at point in another frame.
Like `magit-diff-visit-file' but always display in another frame."
  (interactive)
  (magit-diff-visit-file--internal nil #'switch-to-buffer-other-frame))

;;;;; Worktree Variants

(defun magit-diff-visit-worktree-file (&optional other-window)
  "From a diff visit the worktree version of the file at point.

Display the buffer in the selected window.  With a prefix
argument OTHER-WINDOW display the buffer in another window
instead.

Visit the worktree version of the appropriate file.  The location
of point inside the diff determines which file is being visited.

Unlike `magit-diff-visit-file' always visits the \"real\" file in
the working tree, i.e the \"current version\" of the file.

In the file-visiting buffer also go to the line that corresponds
to the line that point is on in the diff.  Lines that were added
or removed in the working tree, the index and other commits in
between are automatically accounted for."
  (interactive "P")
  (magit-diff-visit-file--internal t (and other-window t)))

(defun magit-diff-visit-worktree-file-other-window ()
  "From a diff visit the file at point in another window.
Like `magit-diff-visit-worktree-file' but display in another window."
  (interactive)
  (magit-diff-visit-file--internal t #'switch-to-buffer-other-window))

(defun magit-diff-visit-worktree-file-other-frame ()
  "From a diff visit the file at point in another frame.
Like `magit-diff-visit-worktree-file' but display in another frame."
  (interactive)
  (magit-diff-visit-file--internal t #'switch-to-buffer-other-frame))

;;;;; Internal

(defun magit-diff-visit-file--internal (force-worktree display)
  "From a diff visit the appropriate version of FILE.
If FORCE-WORKTREE is non-nil, then visit the worktree version of the
file, even if the diff is about a committed change.  DISPLAY controls
how the buffer is displayed.  If nil display in the same window, if
t display in another window, or if a function, use that to display."
  (let ((file (or (magit-diff--file)
                  (user-error "Cannot determine file to visit"))))
    (if (file-accessible-directory-p file)
        (magit-diff-visit-directory file display)
      (pcase-let ((`(,buf ,pos)
                   (magit-diff-visit-file--noselect force-worktree)))
        (pcase display
          ('nil (pop-to-buffer-same-window buf))
          ('t   (switch-to-buffer-other-window buf))
          (_    (funcall display buf)))
        (magit-diff-visit-file--setup buf pos)
        buf))))

(defun magit-diff-visit-directory (directory &optional other-window)
  "Visit DIRECTORY in some window.
Display the buffer in the selected window unless OTHER-WINDOW is
non-nil.  If DIRECTORY is the top-level directory of the current
repository, then visit the containing directory using Dired and
in the Dired buffer put point on DIRECTORY.  Otherwise display
the Magit-Status buffer for DIRECTORY."
  (if (equal (magit-toplevel directory)
             (magit-toplevel))
      (dired-jump other-window (concat directory "/."))
    (let ((display-buffer-overriding-action
           (if other-window
               '(nil (inhibit-same-window . t))
             '(display-buffer-same-window))))
      (magit-status-setup-buffer directory))))

(defun magit-diff-visit-file--setup (buf pos)
  (with-selected-window (or (get-buffer-window buf) (selected-window))
    (with-current-buffer buf
      (when pos
        (unless (<= (point-min) pos (point-max))
          (widen))
        (goto-char pos))
      (when (and buffer-file-name
                 (magit-anything-unmerged-p buffer-file-name))
        (smerge-start-session))
      (run-hooks 'magit-diff-visit-file-hook))))

(defun magit-diff-visit-file--noselect (&optional goto-file)
  (pcase-let*
      ((`(,old ,new)  (magit-diff-visit--sides))
       (goto-from     (and (not goto-file) (magit-diff-on-removed-line-p)))
       (goto-file     (or goto-file (equal magit-buffer-typearg "--no-index")))
       (`(,rev ,file) (if goto-from old new))
       (buffer        (magit-find-file-noselect (if goto-file "{worktree}" rev)
                                                file)))
    (list buffer
          (magit-diff-visit--position buffer rev file goto-from goto-file))))

(defun magit-diff-visit--sides ()
  (pcase-let* ((spec (magit-diff--dwim))
               (`(,old-rev . ,new-rev)
                (pcase spec
                  ((pred stringp)
                   (magit-split-range spec t))
                  (`(,(or 'commit 'stash) . ,rev)
                   (cons (magit-rev-abbrev (concat rev "^"))
                         (magit--abbrev-if-hash rev)))
                  ('staged    (cons (magit-rev-abbrev "HEAD") "{index}"))
                  ('unstaged  (cons (magit-rev-abbrev "HEAD") "{worktree}"))
                  ('nil       (cons "{worktree}" "{worktree}"))
                  ('unmerged  (cons "{worktree}" "{worktree}"))
                  ('undefined (cons "{worktree}" "{worktree}")) ;--no-index
                  (_          (error "BUG: Unexpected diff type %s" spec))))
               ((eieio source value)
                (magit-diff--file-section))
               (old-file (or source value))
               (new-file value))
    (when (equal magit-buffer-typearg "--no-index")
      (setq old-file (concat "/" old-file))
      (setq new-file (concat "/" new-file)))
    (list (list old-rev old-file)
          (list new-rev new-file))))

(defun magit-diff-visit--position (buffer rev file goto-from goto-file)
  (and-let* ((hunk (magit-diff--hunk-section)))
    (let* ((line   (magit-diff-hunk-line   hunk goto-from))
           (column (magit-diff-hunk-column hunk goto-from)))
      (with-current-buffer buffer
        (cond ((equal rev "{index}")
               (setq line (magit-diff-visit--offset file nil line)))
              ((equal rev "{worktree}"))
              (goto-file
               (setq line (magit-diff-visit--offset file rev line))))
        (save-restriction
          (widen)
          (goto-char (point-min))
          (forward-line (1- line))
          (move-to-column column)
          (point))))))

(defun magit-diff-hunk-line (section goto-from)
  (save-excursion
    (goto-char (line-beginning-position))
    (with-slots (content combined from-ranges from-range to-range) section
      (when (or from-range to-range)
        (when (< (point) content)
          (goto-char content)
          (re-search-forward "^[-+]"))
        (+ (car (if goto-from from-range to-range))
           (let ((prefix (if combined (length from-ranges) 1))
                 (target (point))
                 (offset 0))
             (goto-char content)
             (while (< (point) target)
               (unless (string-search
                        (if goto-from "+" "-")
                        (buffer-substring (point) (+ (point) prefix)))
                 (cl-incf offset))
               (forward-line))
             offset))))))

(defun magit-diff-hunk-column (section goto-from)
  (if (or (< (magit-point)
             (oref section content))
          (and (not goto-from)
               (= (char-after (line-beginning-position)) ?-)))
      0
    (max 0 (- (+ (current-column) 2)
              (length (oref section value))))))

(defun magit-diff-visit--offset (file rev line)
  (let ((offset 0))
    (with-temp-buffer
      (save-excursion
        (magit-with-toplevel
          (magit-git-insert "diff" rev "--" file)))
      (catch 'found
        (while (re-search-forward
                "^@@ -\\([0-9]+\\),\\([0-9]+\\) \\+\\([0-9]+\\),\\([0-9]+\\) @@.*\n"
                nil t)
          (let ((from-beg (string-to-number (match-str 1)))
                (from-len (string-to-number (match-str 2)))
                (  to-len (string-to-number (match-str 4))))
            (if (<= from-beg line)
                (if (< (+ from-beg from-len) line)
                    (cl-incf offset (- to-len from-len))
                  (let ((rest (- line from-beg)))
                    (while (> rest 0)
                      (pcase (char-after)
                        (?\s                  (cl-decf rest))
                        (?-  (cl-decf offset) (cl-decf rest))
                        (?+  (cl-incf offset)))
                      (forward-line))))
              (throw 'found nil))))))
    (+ line offset)))

;;;;; Movement

(defun magit-jump-to-diffstat-or-diff ()
  "Jump to the diffstat or diff.
When point is on a file inside the diffstat section, then jump
to the respective diff section, otherwise jump to the diffstat
section or a child thereof."
  (interactive)
  (if-let ((section (magit-get-section
                     (append (magit-section-case
                               ([file diffstat] `((file . ,(oref it value))))
                               (file `((file . ,(oref it value)) (diffstat)))
                               (t '((diffstat))))
                             (magit-section-ident magit-root-section)))))
      (magit-section-goto section)
    (user-error "No diffstat in this buffer")))

;;;; Scroll Commands

(defun magit-diff-show-or-scroll-up ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer up.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-diff-show-or-scroll #'scroll-up))

(defun magit-diff-show-or-scroll-down ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer down.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-diff-show-or-scroll #'scroll-down))

(defun magit-diff-show-or-scroll (fn)
  (let (rev cmd buf win)
    (cond
     ((and (bound-and-true-p magit-blame-mode)
           (fboundp 'magit-current-blame-chunk))
      (setq rev (oref (magit-current-blame-chunk) orig-rev))
      (setq cmd #'magit-show-commit)
      (setq buf (magit-get-mode-buffer 'magit-revision-mode)))
     ((derived-mode-p 'git-rebase-mode)
      (with-slots (action-type target)
          (git-rebase-current-line)
        (if (not (eq action-type 'commit))
            (user-error "No commit on this line")
          (setq rev target)
          (setq cmd #'magit-show-commit)
          (setq buf (magit-get-mode-buffer 'magit-revision-mode)))))
     (t
      (magit-section-case
        (branch
         (setq rev (magit-ref-maybe-qualify (oref it value)))
         (setq cmd #'magit-show-commit)
         (setq buf (magit-get-mode-buffer 'magit-revision-mode)))
        (commit
         (setq rev (oref it value))
         (setq cmd #'magit-show-commit)
         (setq buf (magit-get-mode-buffer 'magit-revision-mode)))
        (tag
         (setq rev (magit-rev-hash (oref it value)))
         (setq cmd #'magit-show-commit)
         (setq buf (magit-get-mode-buffer 'magit-revision-mode)))
        (stash
         (setq rev (oref it value))
         (setq cmd #'magit-stash-show)
         (setq buf (magit-get-mode-buffer 'magit-stash-mode))))))
    (if rev
        (if (and buf
                 (setq win (get-buffer-window buf))
                 (with-current-buffer buf
                   (and (equal rev magit-buffer-revision)
                        (equal (magit-rev-parse rev)
                               magit-buffer-revision-hash))))
            (with-selected-window win
              (condition-case nil
                  (funcall fn)
                (error
                 (goto-char (pcase fn
                              ('scroll-up   (point-min))
                              ('scroll-down (point-max)))))))
          (let ((magit-display-buffer-noselect t))
            (if (eq cmd #'magit-show-commit)
                (apply #'magit-show-commit rev (magit-show-commit--arguments))
              (funcall cmd rev))))
      (call-interactively #'magit-show-commit))))

;;;; Section Commands

(defun magit-section-cycle-diffs ()
  "Cycle visibility of diff-related sections in the current buffer."
  (interactive)
  (when-let ((sections
              (cond ((derived-mode-p 'magit-status-mode)
                     (mapcan (lambda (section)
                               (and section
                                    (progn
                                      (when (oref section hidden)
                                        (magit-section-show section))
                                      (oref section children))))
                             (list (magit-get-section '((staged)   (status)))
                                   (magit-get-section '((unstaged) (status))))))
                    ((derived-mode-p 'magit-diff-mode)
                     (seq-filter #'magit-file-section-p
                                 (oref magit-root-section children))))))
    (if (seq-some (##oref % hidden) sections)
        (dolist (s sections)
          (magit-section-show s)
          (magit-section-hide-children s))
      (let ((children (mapcan (##copy-sequence (oref % children)) sections)))
        (cond ((and (seq-some (##oref % hidden)   children)
                    (seq-some (##oref % children) children))
               (mapc #'magit-section-show-headings sections))
              ((seq-some #'magit-section-hidden-body children)
               (mapc #'magit-section-show-children sections))
              (t
               (mapc #'magit-section-hide sections)))))))

;;;; Jump Commands

(transient-define-prefix magit-revision-jump (&optional menu)
  "In a Magit-Revision buffer, jump to a section.
Show a menu to choose a section, unless point is on a file
heading, or with a prefix argument, in which case behave
like 'magit-jump-to-diffstat-or-diff'."
  [["Jump to"
    ("h" magit-jump-to-revision-headers)
    ("m" magit-jump-to-revision-message)
    ("n" magit-jump-to-revision-notes)
    ("s" magit-jump-to-revision-diffstat)
    ("d" magit-jump-to-revision-diff)]
   ["Jump using"
    ("j" "Imenu" imenu)]]
  (interactive (list (or (not (magit-section-match 'file))
                         current-prefix-arg)))
  (if menu
      (transient-setup 'magit-revision-jump)
    (magit-jump-to-diffstat-or-diff)))

(magit-define-section-jumper magit-jump-to-revision-headers
  "Headings" headers nil magit-insert-revision-headers)

(magit-define-section-jumper magit-jump-to-revision-message
  "Message" commit-message nil magit-insert-revision-message)

(magit-define-section-jumper magit-jump-to-revision-notes
  "Notes" notes nil magit-insert-revision-notes)

(magit-define-section-jumper magit-jump-to-revision-diffstat
  "Diffstat" diffstat nil magit-insert-revision-diff)

(transient-define-suffix magit-jump-to-revision-diff (&optional expand)
  :description "Diff"
  :inapt-if-not (##cl-find-if (##eq (oref % type) 'file)
                              (oref magit-root-section children))
  (interactive "P")
  (if-let ((section (cl-find-if (##eq (oref % type) 'file)
                                (oref magit-root-section children))))
      (progn (goto-char (oref section start))
             (when expand
               (with-local-quit (magit-section-show section))
               (recenter 0)))
    (message (format "No diff sections found"))))

;;; Diff Mode

(defvar-keymap magit-diff-mode-map
  :doc "Keymap for `magit-diff-mode'."
  :parent magit-mode-map
  "C-c C-d" #'magit-diff-while-committing
  "C-c C-b" #'magit-go-backward
  "C-c C-f" #'magit-go-forward
  "SPC"     #'scroll-up
  "DEL"     #'scroll-down
  "j"       #'magit-jump-to-diffstat-or-diff
  "<remap> <write-file>" #'magit-patch-save)

(define-derived-mode magit-diff-mode magit-mode "Magit Diff"
  "Mode for looking at a Git diff.

This mode is documented in info node `(magit)Diff Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the hunk or file at point.

Staging and applying changes is documented in info node
`(magit)Staging and Unstaging' and info node `(magit)Applying'.

\\<magit-hunk-section-map>Type \
\\[magit-apply] to apply the change at point, \
\\[magit-stage] to stage,
\\[magit-unstage] to unstage, \
\\[magit-discard] to discard, or \
\\[magit-reverse] to reverse it.

\\{magit-diff-mode-map}"
  :interactive nil
  :group 'magit-diff
  (magit-hack-dir-local-variables)
  (setq magit--imenu-item-types 'file))

(put 'magit-diff-mode 'magit-diff-default-arguments
     '("--stat" "--no-ext-diff"))

(defun magit-diff-setup-buffer ( range typearg args files
                                 &optional type locked)
  (require 'magit)
  (magit-setup-buffer #'magit-diff-mode locked
    (magit-buffer-range range)
    (magit-buffer-typearg typearg)
    (magit-buffer-diff-type type)
    (magit-buffer-diff-args args)
    (magit-buffer-diff-files files)
    (magit-buffer-diff-files-suspended nil)))

(defun magit-diff-refresh-buffer ()
  "Refresh the current `magit-diff-mode' buffer."
  (magit-set-header-line-format
   (if (equal magit-buffer-typearg "--no-index")
       (apply #'format "Differences between %s and %s" magit-buffer-diff-files)
     (concat (if magit-buffer-range
                 (if (string-match-p "\\(\\.\\.\\|\\^-\\)"
                                     magit-buffer-range)
                     (format "Changes in %s" magit-buffer-range)
                   (let ((msg "Changes from %s to %s")
                         (end (if (equal magit-buffer-typearg "--cached")
                                  "index"
                                "working tree")))
                     (if (member "-R" magit-buffer-diff-args)
                         (format msg end magit-buffer-range)
                       (format msg magit-buffer-range end))))
               (cond ((equal magit-buffer-typearg "--cached")
                      "Staged changes")
                     ((and (magit-repository-local-get 'this-commit-command)
                           (not (magit-anything-staged-p)))
                      "Uncommitting changes")
                     (t "Unstaged changes")))
             (pcase (length magit-buffer-diff-files)
               (0)
               (1 (concat " in file " (car magit-buffer-diff-files)))
               (_ (concat " in files "
                          (string-join magit-buffer-diff-files ", ")))))))
  (setq magit-buffer-range-hashed
        (and magit-buffer-range (magit-hash-range magit-buffer-range)))
  (magit-insert-section (diffbuf)
    (magit-run-section-hook 'magit-diff-sections-hook)))

(cl-defmethod magit-buffer-value (&context (major-mode magit-diff-mode))
  (nconc (cond (magit-buffer-range
                (delq nil (list magit-buffer-range magit-buffer-typearg)))
               ((equal magit-buffer-typearg "--cached")
                (list 'staged))
               (t
                (list 'unstaged magit-buffer-typearg)))
         (and magit-buffer-diff-files (cons "--" magit-buffer-diff-files))))

(cl-defmethod magit-menu-common-value ((_section magit-diff-section))
  (magit-diff-scope))

(defvar-keymap magit-diff-section-map
  :doc "Keymap for diff sections.
The classes `magit-file-section' and `magit-hunk-section' derive
from the abstract `magit-diff-section' class.  Accordingly this
keymap is the parent of their keymaps."
  "C-j"            #'magit-diff-visit-worktree-file
  "C-<return>"     #'magit-diff-visit-worktree-file
  "C-x 4 <return>" #'magit-diff-visit-file-other-window
  "C-x 5 <return>" #'magit-diff-visit-file-other-frame
  "&"              #'magit-do-async-shell-command
  "C"              #'magit-commit-add-log
  "C-x a"          #'magit-add-change-log-entry
  "C-x 4 a"        #'magit-add-change-log-entry-other-window
  "C-c C-t"        #'magit-diff-trace-definition
  "C-c C-e"        #'magit-diff-edit-hunk-commit
  "<remap> <magit-file-rename>"      #'magit-file-rename
  "<remap> <magit-file-untrack>"     #'magit-file-untrack
  "<remap> <magit-visit-thing>"      #'magit-diff-visit-file
  "<remap> <magit-revert-no-commit>" #'magit-reverse
  "<remap> <magit-delete-thing>"     #'magit-discard
  "<remap> <magit-unstage-files>"    #'magit-unstage
  "<remap> <magit-stage-files>"      #'magit-stage
  "<remap> <magit-cherry-apply>"     #'magit-apply
  "<8>" (magit-menu-item "Rename file" #'magit-file-rename
                         '(:enable (eq (magit-diff-scope) 'file)))
  "<7>" (magit-menu-item "Untrack %x" #'magit-file-untrack)
  "<6>" (magit-menu-item "Visit file" #'magit-diff-visit-file
                         '(:enable (memq (magit-diff-scope) '(file files))))
  "<5>" (magit-menu-item "Reverse %x" #'magit-reverse
                         '(:enable (not (memq (magit-diff-type)
                                              '(untracked unstaged)))))
  "<4>" (magit-menu-item "Discard %x" #'magit-discard
                         '(:enable (not (memq (magit-diff-type)
                                              '(committed undefined)))))
  "<3>" (magit-menu-item "Unstage %x" #'magit-unstage
                         '(:enable (eq (magit-diff-type) 'staged)))
  "<2>" (magit-menu-item "Stage %x"   #'magit-stage
                         '(:enable (eq (magit-diff-type) 'unstaged)))
  "<1>" (magit-menu-item "Apply %x" #'magit-apply
                         '(:enable (not (memq (magit-diff-type)
                                              '(unstaged staged))))))

(defvar-keymap magit-file-section-map
  ;; Even though this derived map doesn't add any bindings by default,
  ;; it is quite possible that some users would want to add their own.
  :doc "Keymap for `file' sections."
  :parent magit-diff-section-base-map)

(defvar-keymap magit-hunk-section-smerge-map
  :doc "Keymap bound to `smerge-command-prefix' in `magit-hunk-section-map'."
  "RET" #'magit-smerge-keep-current
  "a"   #'magit-smerge-keep-all
  "u"   #'magit-smerge-keep-upper
  "b"   #'magit-smerge-keep-base
  "l"   #'magit-smerge-keep-lower)

(defvar magit-hunk-section-map
  (let ((map (make-sparse-keymap))
        (key (key-description smerge-command-prefix)))
    (when (key-valid-p key)
      (keymap-set map key magit-hunk-section-smerge-map))
    (set-keymap-parent map magit-diff-section-base-map)
    map)
  "Keymap for `hunk' sections.")

;;; Diff Insert

(defvar magit-diff--reset-non-color-moved
  (list
   "-c" "color.diff.context=normal"
   "-c" "color.diff.plain=normal" ; historical synonym for context
   "-c" "color.diff.meta=normal"
   "-c" "color.diff.frag=normal"
   "-c" "color.diff.func=normal"
   "-c" "color.diff.old=normal"
   "-c" "color.diff.new=normal"
   "-c" "color.diff.commit=normal"
   "-c" "color.diff.whitespace=normal"
   ;; "git-range-diff" does not support "--color-moved", so we don't
   ;; need to reset contextDimmed, oldDimmed, newDimmed, contextBold,
   ;; oldBold, and newBold.
   ))

(defun magit-insert-diff ()
  "Insert the diff into this `magit-diff-mode' buffer."
  (magit--insert-diff t
    "diff" magit-buffer-range "-p" "--no-prefix"
    (and (member "--stat" magit-buffer-diff-args) "--numstat")
    magit-buffer-typearg
    magit-buffer-diff-args "--"
    magit-buffer-diff-files))

(defun magit--insert-diff (keep-error &rest args)
  (declare (indent 1))
  (pcase-let ((`(,cmd . ,args)
               (flatten-tree args))
              (magit-git-global-arguments
               (remove "--literal-pathspecs" magit-git-global-arguments)))
    ;; We need to generate diffs with --ita-visible-in-index so that
    ;; `magit-stage' can work with intent-to-add files (see #4026).
    (unless (equal cmd "merge-tree")
      (push "--ita-visible-in-index" args))
    (setq args (magit-diff--maybe-add-stat-arguments args))
    (when (cl-member-if (##string-prefix-p "--color-moved" %) args)
      (push "--color=always" args)
      (setq magit-git-global-arguments
            (append magit-diff--reset-non-color-moved
                    magit-git-global-arguments)))
    (magit--git-wash #'magit-diff-wash-diffs
        (if (member "--no-index" args)
            'wash-anyway
          (or keep-error t))
      cmd args)))

(defun magit-diff--maybe-add-stat-arguments (args)
  (if (member "--stat" args)
      (append (if (functionp magit-diff-extra-stat-arguments)
                  (funcall magit-diff-extra-stat-arguments)
                magit-diff-extra-stat-arguments)
              args)
    args))

;;; Diff Wash

(defconst magit-diff-conflict-headline-re
  (concat "^" (regexp-opt
               ;; Defined in merge-tree.c in this order.
               '("merged"
                 "added in remote"
                 "added in both"
                 "added in local"
                 "removed in both"
                 "changed in both"
                 "removed in local"
                 "removed in remote"))))

(defconst magit-diff-headline-re
  (concat "^\\(@@@?\\|diff\\|Submodule\\|"
          "\\* Unmerged path\\|"
          (substring magit-diff-conflict-headline-re 1)
          "\\)"))

(defconst magit-diff-statline-re
  (concat "^ ?"
          "\\(.*\\)"     ; file
          "\\( +| +\\)"  ; separator
          "\\([0-9]+\\|Bin\\(?: +[0-9]+ -> [0-9]+ bytes\\)?$\\) ?"
          "\\(\\+*\\)"   ; add
          "\\(-*\\)$"))  ; del

(defun magit-diff-use-window-width-as-stat-width ()
  "Use the `window-width' as the value of `--stat-width'."
  (and-let* ((window (get-buffer-window (current-buffer) 'visible)))
    (list (format "--stat-width=%d" (window-width window)))))

(defun magit-diff-wash-diffs (args &optional limit)
  (run-hooks 'magit-diff-wash-diffs-hook)
  (when (member "--show-signature" args)
    (magit-diff-wash-signature magit-buffer-revision-hash))
  (when (member "--stat" args)
    (magit-diff-wash-diffstat))
  (when (re-search-forward magit-diff-headline-re limit t)
    (goto-char (line-beginning-position))
    (magit-wash-sequence (##magit-diff-wash-diff args))
    (insert ?\n)))

(defun magit-diff-wash-signature (object)
  (cond
   ((looking-at "^No signature")
    (delete-line))
   ((looking-at "^gpg: ")
    (let (title end)
      (save-excursion
        (while (looking-at "^gpg: ")
          (cond
           ((looking-at "^gpg: Good signature from")
            (setq title (propertize
                         (buffer-substring (point) (line-end-position))
                         'face 'magit-signature-good)))
           ((looking-at "^gpg: Can't check signature")
            (setq title (propertize
                         (buffer-substring (point) (line-end-position))
                         'face '(italic bold)))))
          (forward-line))
        (setq end (point-marker)))
      (magit-insert-section (signature object title)
        (when title
          (magit-insert-heading title))
        (goto-char end)
        (set-marker end nil)
        (insert "\n"))))))

(defun magit-diff-wash-diffstat ()
  (let (heading (beg (point)))
    (when (re-search-forward "^ ?\\([0-9]+ +files? change[^\n]*\n\\)" nil t)
      (setq heading (match-str 1))
      (magit-delete-match)
      (goto-char beg)
      (magit-insert-section (diffstat)
        (magit-insert-heading
          (propertize heading 'font-lock-face 'magit-diff-file-heading))
        (let (files)
          (while (looking-at "^[-0-9]+\t[-0-9]+\t\\(.+\\)$")
            (push (magit-decode-git-path
                   (let ((f (match-str 1)))
                     (cond
                      ((string-match "{.* => \\(.*\\)}" f)
                       (replace-match (match-str 1 f) nil t f))
                      ((string-match " => " f)
                       (substring f (match-end 0)))
                      (t f))))
                  files)
            (magit-delete-line))
          (setq files (nreverse files))
          (while (looking-at magit-diff-statline-re)
            (magit-bind-match-strings (file sep cnt add del) nil
              (magit-delete-line)
              (when (string-match " +$" file)
                (setq sep (concat (match-str 0 file) sep))
                (setq file (substring file 0 (match-beginning 0))))
              (let ((le (length file)) ld)
                (setq file (magit-decode-git-path file))
                (setq ld (length file))
                (when (> le ld)
                  (setq sep (concat (make-string (- le ld) ?\s) sep))))
              (magit-insert-section (file (pop files))
                (insert (magit-format-file 'stat file 'magit-filename))
                (insert sep cnt " ")
                (when add
                  (insert (propertize add 'font-lock-face
                                      'magit-diffstat-added)))
                (when del
                  (insert (propertize del 'font-lock-face
                                      'magit-diffstat-removed)))
                (insert "\n")))))
        (if (looking-at "^$") (forward-line) (insert "\n"))))))

(defun magit-diff-wash-diff (args)
  (when (cl-member-if (##string-prefix-p "--color-moved" %) args)
    (require 'ansi-color)
    (ansi-color-apply-on-region (point-min) (point-max)))
  (cond
   ((looking-at "^Submodule")
    (magit-diff-wash-submodule))
   ((looking-at "^\\* Unmerged path \\(.*\\)")
    (let ((file (magit-decode-git-path (match-str 1))))
      (magit-delete-line)
      (unless (and (derived-mode-p 'magit-status-mode)
                   (not (member "--cached" args)))
        (magit-insert-section (file file)
          (insert (propertize
                   (format "unmerged   %s%s" file
                           (pcase (cddr (car (magit-file-status file)))
                             ('(?D ?D) " (both deleted)")
                             ('(?D ?U) " (deleted by us)")
                             ('(?U ?D) " (deleted by them)")
                             ('(?A ?A) " (both added)")
                             ('(?A ?U) " (added by us)")
                             ('(?U ?A) " (added by them)")
                             ('(?U ?U) "")))
                   'font-lock-face 'magit-diff-file-heading))
          (insert ?\n))))
    t)
   ((looking-at magit-diff-conflict-headline-re)
    (let ((long-status (match-str 0))
          (status "BUG")
          file orig base)
      (if (equal long-status "merged")
          (progn (setq status long-status)
                 (setq long-status nil))
        (setq status (pcase-exhaustive long-status
                       ("added in remote"   "new file")
                       ("added in both"     "new file")
                       ("added in local"    "new file")
                       ("removed in both"   "removed")
                       ("changed in both"   "changed")
                       ("removed in local"  "removed")
                       ("removed in remote" "removed"))))
      (magit-delete-line)
      (while (looking-at
              "^  \\([^ ]+\\) +[0-9]\\{6\\} \\([a-z0-9]\\{40,\\}\\) \\(.+\\)$")
        (magit-bind-match-strings (side _blob name) nil
          (pcase side
            ("result" (setq file name))
            ("our"    (setq orig name))
            ("their"  (setq file name))
            ("base"   (setq base name))))
        (magit-delete-line))
      (when orig (setq orig (magit-decode-git-path orig)))
      (when file (setq file (magit-decode-git-path file)))
      (magit-diff-insert-file-section
       (or file base) orig status nil nil nil nil long-status)))
   ;; The files on this line may be ambiguous due to whitespace.
   ;; That's okay. We can get their names from subsequent headers.
   ((looking-at "^diff --\
\\(?:\\(?1:git\\) \\(?:\\(?2:.+?\\) \\2\\)?\
\\|\\(?:cc\\|combined\\) \\(?3:.+\\)\\)")
    (let ((status (cond ((equal (match-str 1) "git")        "modified")
                        ((derived-mode-p 'magit-revision-mode) "resolved")
                        (t                                     "unmerged")))
          (orig nil)
          (file (or (match-str 2) (match-str 3)))
          (header (list (buffer-substring-no-properties
                         (line-beginning-position) (1+ (line-end-position)))))
          (modes nil)
          (rename nil)
          (binary nil))
      (magit-delete-line)
      (while (not (or (eobp) (looking-at magit-diff-headline-re)))
        (cond
         ((looking-at "old mode \\(?:[^\n]+\\)\nnew mode \\(?:[^\n]+\\)\n")
          (setq modes (match-str 0)))
         ((looking-at "deleted file .+\n")
          (setq status "deleted"))
         ((looking-at "new file .+\n")
          (setq status "new file"))
         ((looking-at "rename from \\(.+\\)\nrename to \\(.+\\)\n")
          (setq rename (match-str 0))
          (setq orig (match-str 1))
          (setq file (match-str 2))
          (setq status "renamed"))
         ((looking-at "copy from \\(.+\\)\ncopy to \\(.+\\)\n")
          (setq orig (match-str 1))
          (setq file (match-str 2))
          (setq status "new file"))
         ((looking-at "similarity index .+\n"))
         ((looking-at "dissimilarity index .+\n"))
         ((looking-at "index .+\n"))
         ((looking-at "--- \\(.+?\\)\t?\n")
          (unless (equal (match-str 1) "/dev/null")
            (setq orig (match-str 1))))
         ((looking-at "\\+\\+\\+ \\(.+?\\)\t?\n")
          (unless (equal (match-str 1) "/dev/null")
            (setq file (match-str 1))))
         ((looking-at "Binary files .+ and .+ differ\n")
          (setq binary t))
         ((looking-at "Binary files differ\n")
          (setq binary t))
         ;; TODO Use all combined diff extended headers.
         ((looking-at "mode .+\n"))
         ((error "BUG: Unknown extended header: %S"
                 (buffer-substring (point) (line-end-position)))))
        ;; These headers are treated as some sort of special hunk.
        (unless (or (string-prefix-p "old mode" (match-str 0))
                    (string-prefix-p "rename"   (match-str 0)))
          (push (match-str 0) header))
        (magit-delete-match))
      (when orig
        (setq orig (magit-decode-git-path orig)))
      (setq file (magit-decode-git-path file))
      (setq header (nreverse header))
      ;; KLUDGE `git-log' ignores `--no-prefix' when `-L' is used.
      (when (and (derived-mode-p 'magit-log-mode)
                 (seq-some (##string-prefix-p "-L" %)
                           magit-buffer-log-args))
        (when orig
          (setq orig (substring orig 2)))
        (setq file (substring file 2))
        (setq header (list (save-excursion
                             (string-match "diff [^ ]+" (car header))
                             (format "%s %s %s\n"
                                     (match-str 0 (car header))
                                     (or orig file)
                                     (or file orig)))
                           (format "--- %s\n" (or orig "/dev/null"))
                           (format "+++ %s\n" (or file "/dev/null")))))
      (setq header (string-join header))
      (magit-diff-insert-file-section
       file orig status modes rename header binary nil)))))

(defun magit-diff-insert-file-section
    (file orig status modes rename header binary long-status)
  (magit-insert-section
      ( file file
        (or (equal status "deleted") (derived-mode-p 'magit-status-mode))
        :source (and (not (equal orig file)) orig)
        :header header
        :binary binary)
    (magit-insert-heading
      (magit-format-file 'diff file 'magit-diff-file-heading status
                         (and (not (equal orig file)) orig))
      (cond ((and binary long-status)
             (format " (%s, binary)" long-status))
            ((or binary long-status)
             (format " (%s)" (if binary "binary" long-status)))))
    (when modes
      (magit-insert-section (hunk '(chmod))
        (magit-insert-heading (propertize modes 'face 'default))))
    (when rename
      (magit-insert-section (hunk '(rename))
        (magit-insert-heading (propertize rename 'face 'default))))
    (magit-wash-sequence #'magit-diff-wash-hunk)))

(defun magit-format-file (kind file face &optional status orig)
  (funcall magit-format-file-function kind file face status orig))

(defun magit-format-file-default (_kind file face &optional status orig)
  "Show only the Git status and the filename."
  (propertize (concat (and status (format "%-11s" status))
                      (if orig (format "%s -> %s" orig file) file))
              'font-lock-face face))

(defun magit-format-file-all-the-icons (kind file face &optional status orig)
  "Show the status, filename and icon (using the `all-the-icons' package).
You have to explicitly install the `all-the-icons' package, else this
function errors."
  (cl-flet ((icon (if (or (eq kind 'module) (string-suffix-p "/" file))
                      'all-the-icons-icon-for-dir
                    'all-the-icons-icon-for-file)))
    (cl-letf (((symbol-function 'all-the-icons-dir-is-submodule)
               (if (eq kind 'module)
                   (lambda (_) t)
                 (symbol-function 'all-the-icons-dir-is-submodule))))
      (propertize (concat (and status (format "%-11s" status))
                          (if orig
                              (format "%s %s -> %s %s"
                                      (icon orig) orig
                                      (icon file) file)
                            (format "%s %s" (icon file) file)))
                  'font-lock-face face))))

(defun magit-format-file-nerd-icons (kind file face &optional status orig)
  "Show the status, filename and icon (using the `nerd-icons' package).
You have to explicitly install the `nerd-icons' package, else this
function errors."
  (cl-flet ((icon (if (or (eq kind 'module) (string-suffix-p "/" file))
                      'nerd-icons-icon-for-dir
                    'nerd-icons-icon-for-file)))
    (cl-letf (((symbol-function 'nerd-icons-dir-is-submodule)
               (if (eq kind 'module)
                   (lambda (_) t)
                 (symbol-function 'nerd-icons-dir-is-submodule))))
      (propertize (concat (and status (format "%-11s" status))
                          (if orig
                              (format "%s %s -> %s %s"
                                      (icon orig) orig
                                      (icon file) file)
                            (format "%s %s" (icon file) file)))
                  'font-lock-face face))))

(defun magit-diff-wash-submodule ()
  ;; See `show_submodule_summary' in submodule.c and "this" commit.
  (when (looking-at "^Submodule \\([^ ]+\\)")
    (let ((module (match-str 1))
          untracked modified)
      (when (looking-at "^Submodule [^ ]+ contains untracked content$")
        (magit-delete-line)
        (setq untracked t))
      (when (looking-at "^Submodule [^ ]+ contains modified content$")
        (magit-delete-line)
        (setq modified t))
      (cond
       ((and (looking-at "^Submodule \\([^ ]+\\) \\([^ :]+\\)\\( (rewind)\\)?:$")
             (equal (match-str 1) module))
        (magit-bind-match-strings (_module range rewind) nil
          (magit-delete-line)
          (while (looking-at "^  \\([<>]\\) \\(.*\\)$")
            (magit-delete-line))
          (when rewind
            (setq range (replace-regexp-in-string "[^.]\\(\\.\\.\\)[^.]"
                                                  "..." range t t 1)))
          (magit-insert-section (module module t)
            (magit-insert-heading
              (magit-format-file 'module module 'magit-diff-file-heading
                                 "modified")
              " ("
              (cond (rewind "rewind")
                    ((string-search "..." range) "non-ff")
                    (t "new commits"))
              (and (or modified untracked)
                   (concat ", "
                           (and modified "modified")
                           (and modified untracked " and ")
                           (and untracked "untracked")
                           " content"))
              ")")
            (magit-insert-section-body
              (let ((default-directory
                     (file-name-as-directory
                      (expand-file-name module (magit-toplevel)))))
                (magit-git-wash (apply-partially #'magit-log-wash-log 'module)
                  "log" "--oneline" "--left-right" range)
                (delete-char -1))))))
       ((and (looking-at "^Submodule \\([^ ]+\\) \\([^ ]+\\) (\\([^)]+\\))$")
             (equal (match-str 1) module))
        (magit-bind-match-strings (_module _range msg) nil
          (magit-delete-line)
          (magit-insert-section (module module)
            (magit-insert-heading
              (magit-format-file 'module module 'magit-diff-file-heading
                                 "submodule")
              " (" msg ")"))))
       (t
        (magit-insert-section (module module)
          (magit-insert-heading
            (magit-format-file 'module module 'magit-diff-file-heading
                               "modified")
            " ("
            (and modified "modified")
            (and modified untracked " and ")
            (and untracked "untracked")
            " content)")))))))

(defun magit-diff-wash-hunk ()
  (when (looking-at "^@\\{2,\\} \\(.+?\\) @\\{2,\\}\\(?: \\(.*\\)\\)?")
    (let* ((heading  (match-str 0))
           (ranges   (mapcar
                      (lambda (str)
                        (let ((range
                               (mapcar #'string-to-number
                                       (split-string (substring str 1) ","))))
                          ;; A single line is +1 rather than +1,1.
                          (if (length= range 1)
                              (nconc range (list 1))
                            range)))
                      (split-string (match-str 1))))
           (about    (match-str 2))
           (combined (length= ranges 3))
           (value    (cons about ranges)))
      (magit-delete-line)
      (magit-insert-section
          ( hunk value nil
            :combined combined
            :from-range (if combined (butlast ranges) (car ranges))
            :to-range (car (last ranges))
            :about about)
        (magit-insert-heading
          (propertize (concat heading "\n")
                      'font-lock-face 'magit-diff-hunk-heading))
        (while (not (or (eobp) (looking-at "^[^-+\s\\]")))
          (forward-line))))
    t))

(defun magit-diff-expansion-threshold (section)
  "Keep new diff sections collapsed if washing takes too long."
  (and (magit-file-section-p section)
       (> (float-time (time-since magit--refresh-start-time))
          magit-diff-expansion-threshold)
       'hide))

(add-hook 'magit-section-set-visibility-hook #'magit-diff-expansion-threshold)

;;; Revision Mode

(defvar-keymap magit-revision-mode-map
  :doc "Keymap for `magit-revision-mode'."
  :parent magit-diff-mode-map
  "j" #'magit-revision-jump)

(define-derived-mode magit-revision-mode magit-diff-mode "Magit Rev"
  "Mode for looking at a Git commit.

This mode is documented in info node `(magit)Revision Buffer'.

\\<magit-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-thing] to visit the hunk or file at point.

Staging and applying changes is documented in info node
`(magit)Staging and Unstaging' and info node `(magit)Applying'.

\\<magit-hunk-section-map>Type \
\\[magit-apply] to apply the change at point, \
\\[magit-stage] to stage,
\\[magit-unstage] to unstage, \
\\[magit-discard] to discard, or \
\\[magit-reverse] to reverse it.

\\{magit-revision-mode-map}"
  :interactive nil
  :group 'magit-revision
  (magit-hack-dir-local-variables))

(put 'magit-revision-mode 'magit-diff-default-arguments
     '("--stat" "--no-ext-diff"))

(defun magit-revision-setup-buffer (rev args files)
  (magit-setup-buffer #'magit-revision-mode nil
    (magit-buffer-revision rev)
    (magit-buffer-range (format "%s^..%s" rev rev))
    (magit-buffer-diff-type 'committed)
    (magit-buffer-diff-args args)
    (magit-buffer-diff-files files)
    (magit-buffer-diff-files-suspended nil)))

(defun magit-revision-refresh-buffer ()
  (setq magit-buffer-revision-hash (magit-rev-hash magit-buffer-revision))
  (magit-set-header-line-format
   (concat (magit-object-type magit-buffer-revision-hash)
           " "  magit-buffer-revision
           (pcase (length magit-buffer-diff-files)
             (0)
             (1 (concat " limited to file " (car magit-buffer-diff-files)))
             (_ (concat " limited to files "
                        (string-join magit-buffer-diff-files ", "))))))
  (magit-insert-section (commitbuf)
    (magit-run-section-hook 'magit-revision-sections-hook)))

(cl-defmethod magit-buffer-value (&context (major-mode magit-revision-mode))
  (cons magit-buffer-revision magit-buffer-diff-files))

;;; Revision Sections

(defun magit-insert-revision-diff ()
  "Insert the diff into this `magit-revision-mode' buffer."
  (magit--insert-diff t
    "show" "-p" "--format=" "--no-prefix"
    (and (member "--stat" magit-buffer-diff-args) "--numstat")
    magit-buffer-diff-args
    (magit--rev-dereference magit-buffer-revision)
    "--" magit-buffer-diff-files))

(defun magit-insert-revision-tag ()
  "Insert tag message and headers into a revision buffer.
This function only inserts anything when `magit-show-commit' is
called with a tag as argument, when that is called with a commit
or a ref which is not a branch, then it inserts nothing."
  (when (equal (magit-object-type magit-buffer-revision) "tag")
    (magit-insert-section (taginfo)
      (let ((beg (point)))
        ;; "git verify-tag -v" would output what we need, but the gpg
        ;; output is send to stderr and we have no control over the
        ;; order in which stdout and stderr are inserted, which would
        ;; make parsing hard.  We are forced to use "git cat-file tag"
        ;; instead, which inserts the signature instead of verifying
        ;; it.  We remove that later and then insert the verification
        ;; output using "git verify-tag" (without the "-v").
        (magit-git-insert "cat-file" "tag" magit-buffer-revision)
        (goto-char beg)
        (forward-line 3)
        (delete-region beg (point)))
      (looking-at "^tagger \\([^<]+\\) <\\([^>]+\\)")
      (let ((heading (format "Tagger: %s <%s>"
                             (match-str 1)
                             (match-str 2))))
        (magit-delete-line)
        (magit-insert-heading
          (propertize heading 'font-lock-face
                      'magit-section-secondary-heading)))
      (forward-line)
      (magit-insert-section
          ( message nil nil
            :heading-highlight-face 'magit-diff-revision-summary-highlight)
        (let ((beg (point)))
          (forward-line)
          (magit--add-face-text-property
           beg (point) 'magit-diff-revision-summary))
        (magit-insert-heading)
        (if (re-search-forward "-----BEGIN PGP SIGNATURE-----" nil t)
            (goto-char (match-beginning 0))
          (goto-char (point-max)))
        (insert ?\n))
      (if (re-search-forward "-----BEGIN PGP SIGNATURE-----" nil t)
          (progn
            (let ((beg (match-beginning 0)))
              (re-search-forward "-----END PGP SIGNATURE-----\n")
              (delete-region beg (point)))
            (save-excursion
              (magit-process-git t "verify-tag" magit-buffer-revision))
            (magit-diff-wash-signature magit-buffer-revision))
        (goto-char (point-max)))
      (insert ?\n))))

(defvar-keymap magit-commit-message-section-map
  :doc "Keymap for `commit-message' sections."
  "<remap> <magit-visit-thing>"  #'magit-show-commit
  "<1>" (magit-menu-item "Visit %t" #'magit-show-commit
                         '(:enable (magit-thing-at-point 'git-revision t))))

(defun magit-insert-revision-message ()
  "Insert the commit message into a revision buffer."
  (magit-insert-section
      ( commit-message nil nil
        :heading-highlight-face 'magit-diff-revision-summary-highlight)
    (if-let* ((rev magit-buffer-revision)
              (msg (with-temp-buffer
                     (save-excursion (magit-rev-insert-format "%B" rev))
                     (magit-revision--wash-message))))
        (progn
          (save-excursion (insert msg))
          (magit-revision--wash-message-hashes)
          (save-excursion
            (magit--add-face-text-property (point)
                                           (progn (forward-line) (point))
                                           'magit-diff-revision-summary
                                           t nil t)
            (magit-insert-heading))
          (goto-char (point-max)))
      (insert "(no message)\n"))))

(defun magit-insert-revision-notes ()
  "Insert commit notes into a revision buffer."
  (let ((default (or (magit-get "core.notesRef") "refs/notes/commits")))
    (dolist (ref (magit-list-active-notes-refs))
      (when-let* ((rev magit-buffer-revision)
                  (msg (with-temp-buffer
                         (save-excursion
                           (magit-git-insert "-c" (concat "core.notesRef=" ref)
                                             "notes" "show" rev))
                         (magit-revision--wash-message))))
        (magit-insert-section
            ( notes ref (not (equal ref default))
              :heading-highlight-face 'magit-diff-hunk-heading-highlight)
          (save-excursion (insert msg))
          (magit-revision--wash-message-hashes)
          (save-excursion
            (end-of-line)
            (insert (format " (%s)"
                            (propertize (if (string-prefix-p "refs/notes/" ref)
                                            (substring ref 11)
                                          ref)
                                        'font-lock-face 'magit-refname))))
          (magit--add-face-text-property (point)
                                         (progn (forward-line) (point))
                                         'magit-diff-revision-summary
                                         t nil t)
          (magit-insert-heading)
          (goto-char (point-max))
          (insert ?\n))))))

(defun magit-revision--wash-message ()
  (let ((major-mode 'git-commit-mode))
    (hack-dir-local-variables)
    (hack-local-variables-apply))
  (unless (memq git-commit-major-mode '(nil text-mode))
    (funcall git-commit-major-mode)
    (font-lock-ensure))
  (when (> (point-max) (point-min))
    (save-excursion
      (while (search-forward "\r\n" nil t) ; Remove trailing CRs.
        (delete-region (match-beginning 0) (1+ (match-beginning 0)))))
    (when magit-revision-fill-summary-line
      (let ((fill-column (min magit-revision-fill-summary-line
                              (window-width (get-buffer-window nil t)))))
        (fill-region (point) (line-end-position))))
    (run-hook-wrapped 'magit-revision-wash-message-hook
                      (lambda (fn) (prog1 nil (save-excursion (funcall fn)))))
    (buffer-string)))

(defun magit-highlight-squash-markers ()
  "Highlight \"squash!\" and similar markers."
  (when (looking-at "\\(?:squash!\\|fixup!\\|amend!\\)")
    (magit--add-face-text-property (match-beginning 0) (match-end 0)
                                   'magit-keyword-squash)))

(defun magit-highlight-bracket-keywords ()
  "Highlight text between brackets."
  (while (re-search-forward "\\[[^][]*]" nil t)
    (put-text-property (match-beginning 0)
                       (match-end 0)
                       'font-lock-face 'magit-keyword)))

(defun magit-revision--wash-message-hashes ()
  (when magit-revision-use-hash-sections
    (save-excursion
      ;; Start after beg to prevent a (commit text) section from
      ;; starting at the same point as the (commit-message)
      ;; section.
      (while (not (eobp))
        (re-search-forward "\\_<" nil 'move)
        (let ((beg (point)))
          (re-search-forward "\\_>" nil t)
          (when (> (point) beg)
            (let ((text (buffer-substring-no-properties beg (point))))
              (when (pcase magit-revision-use-hash-sections
                      ('quickest ; false negatives and positives
                       (and (>= (length text) 7)
                            (string-match-p "[0-9]" text)
                            (string-match-p "[a-z]" text)))
                      ('quicker  ; false negatives (number-less hashes)
                       (and (>= (length text) 7)
                            (string-match-p "[0-9]" text)
                            (magit-commit-p text)))
                      ('quick    ; false negatives (short hashes)
                       (and (>= (length text) 7)
                            (magit-commit-p text)))
                      ('slow
                       (magit-commit-p text)))
                (put-text-property beg (point)
                                   'font-lock-face 'magit-hash)
                (let ((end (point)))
                  (goto-char beg)
                  (magit-insert-section (commit text)
                    (goto-char end)))))))))))

(defun magit-insert-revision-headers ()
  "Insert headers about the commit into a revision buffer."
  (magit-insert-section (headers)
    (magit-insert-heading nil
      (and-let* ((string (magit-rev-format "%D" magit-buffer-revision
                                           "--decorate=full")))
        (concat (magit-format-ref-labels string) " "))
      (propertize
       (magit-rev-parse (magit--rev-dereference magit-buffer-revision))
       'font-lock-face 'magit-hash))
    (let ((beg (point)))
      (magit-rev-insert-format magit-revision-headers-format
                               magit-buffer-revision)
      (magit-insert-revision-gravatars magit-buffer-revision beg))
    (when magit-revision-insert-related-refs
      (when (magit-revision-insert-related-refs-display-p 'parents)
        (dolist (parent (magit-commit-parents magit-buffer-revision))
          (magit-insert-section (commit parent)
            (let ((line (magit-rev-format "%h %s" parent)))
              (string-match "^\\([^ ]+\\) \\(.*\\)" line)
              (magit-bind-match-strings (hash msg) line
                (insert "Parent:     ")
                (insert (propertize hash 'font-lock-face 'magit-hash))
                (insert " " msg "\n"))))))
      (when (magit-revision-insert-related-refs-display-p 'merged)
        (magit--insert-related-refs
         magit-buffer-revision "--merged" "Merged"
         (eq magit-revision-insert-related-refs 'all)))
      (when (magit-revision-insert-related-refs-display-p 'contained)
        (magit--insert-related-refs
         magit-buffer-revision "--contains" "Contained"
         (memq magit-revision-insert-related-refs '(all mixed))))
      (when-let (((magit-revision-insert-related-refs-display-p 'follows))
                 (follows (magit-get-current-tag magit-buffer-revision t)))
        (let ((tag (car  follows))
              (cnt (cadr follows)))
          (magit-insert-section (tag tag)
            (insert
             (format "Follows:    %s (%s)\n"
                     (propertize tag 'font-lock-face 'magit-tag)
                     (propertize (number-to-string cnt)
                                 'font-lock-face 'magit-branch-local))))))
      (when-let (((magit-revision-insert-related-refs-display-p 'precedes))
                 (precedes (magit-get-next-tag magit-buffer-revision t)))
        (let ((tag (car  precedes))
              (cnt (cadr precedes)))
          (magit-insert-section (tag tag)
            (insert (format "Precedes:   %s (%s)\n"
                            (propertize tag 'font-lock-face 'magit-tag)
                            (propertize (number-to-string cnt)
                                        'font-lock-face 'magit-tag))))))
      (insert ?\n))))

(defun magit-revision-insert-related-refs-display-p (sym)
  "Whether to display related branches of type SYM.
Refer to user option `magit-revision-insert-related-refs-display-alist'."
  (if-let ((elt (assq sym magit-revision-insert-related-refs-display-alist)))
      (cdr elt)
    t))

(defun magit--insert-related-refs (rev arg title remote)
  (when-let ((refs (magit-list-related-branches arg rev (and remote "-a"))))
    (magit-insert-section (related-refs)
      (insert title ":" (make-string (- 10 (length title)) ?\s))
      (dolist (branch refs)
        (if (<= (+ (current-column) 1 (length branch))
                (window-width))
            (insert ?\s)
          (insert ?\n (make-string 12 ?\s)))
        (insert (propertize branch 'font-lock-face
                            (if (string-prefix-p "remotes/" branch)
                                'magit-branch-remote
                              'magit-branch-local))))
      (insert ?\n))))

(defun magit-insert-revision-gravatars (rev beg)
  (when (and magit-revision-show-gravatars
             (window-system))
    (require 'gravatar)
    (pcase-let ((`(,author . ,committer)
                 (pcase magit-revision-show-gravatars
                   ('t '("^Author:     " . "^Commit:     "))
                   ('author '("^Author:     " . nil))
                   ('committer '(nil . "^Commit:     "))
                   (_ magit-revision-show-gravatars))))
      (when-let ((email (and author (magit-rev-format "%aE" rev))))
        (magit-insert-revision-gravatar beg rev email author))
      (when-let ((email (and committer (magit-rev-format "%cE" rev))))
        (magit-insert-revision-gravatar beg rev email committer)))))

(defun magit-insert-revision-gravatar (beg rev email regexp)
  (save-excursion
    (goto-char beg)
    (when-let (((re-search-forward regexp nil t))
               (window (get-buffer-window)))
      (let* ((column   (length (match-str 0)))
             (font-obj (query-font (font-at (point) window)))
             (size     (* 2 (+ (aref font-obj 4)
                               (aref font-obj 5))))
             (align-to (+ column
                          (ceiling (/ size (aref font-obj 7) 1.0))
                          1))
             (gravatar-size (- size 2)))
        (ignore-errors ; service may be unreachable
          (gravatar-retrieve email #'magit-insert-revision-gravatar-cb
                             (list gravatar-size rev
                                   (point-marker)
                                   align-to column)))))))

(defun magit-insert-revision-gravatar-cb (image size rev marker align-to column)
  (unless (eq image 'error)
    (when-let ((buffer (marker-buffer marker)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char marker)
          ;; The buffer might display another revision by now or
          ;; it might have been refreshed, in which case another
          ;; process might already have inserted the image.
          (when (and (equal rev magit-buffer-revision)
                     (not (eq (car-safe
                               (car-safe
                                (get-text-property (point) 'display)))
                              'image)))
            (setf (image-property image :ascent) 'center)
            (setf (image-property image :relief) 1)
            (setf (image-property image :scale)  1)
            (setf (image-property image :height) size)
            (let ((top (list image '(slice 0.0 0.0 1.0 0.5)))
                  (bot (list image '(slice 0.0 0.5 1.0 1.0)))
                  (align `((space :align-to ,align-to))))
              (let ((inhibit-read-only t))
                (insert (propertize " " 'display top))
                (insert (propertize " " 'display align))
                (forward-line)
                (forward-char column)
                (insert (propertize " " 'display bot))
                (insert (propertize " " 'display align))))))))))

;;; Merge-Preview Mode

(define-derived-mode magit-merge-preview-mode magit-diff-mode "Magit Merge"
  "Mode for previewing a merge."
  :interactive nil
  :group 'magit-diff
  (magit-hack-dir-local-variables))

(put 'magit-merge-preview-mode 'magit-diff-default-arguments
     '("--no-ext-diff"))

(defun magit-merge-preview-setup-buffer (rev)
  (magit-setup-buffer #'magit-merge-preview-mode nil
    (magit-buffer-revision rev)
    (magit-buffer-range (format "%s^..%s" rev rev))))

(defun magit-merge-preview-refresh-buffer ()
  (let* ((branch (magit-get-current-branch))
         (head (or branch (magit-rev-verify "HEAD"))))
    (magit-set-header-line-format (format "Preview merge of %s into %s"
                                          magit-buffer-revision
                                          (or branch "HEAD")))
    (magit-insert-section (diffbuf)
      (magit--insert-diff t
        "merge-tree" (magit-git-string "merge-base" head magit-buffer-revision)
        head magit-buffer-revision))))

(cl-defmethod magit-buffer-value (&context (major-mode magit-merge-preview-mode))
  magit-buffer-revision)

;;; Hunk Section

(defun magit-hunk-set-window-start (section)
  "When SECTION is a `hunk', ensure that its beginning is visible.
It the SECTION has a different type, then do nothing."
  (when (magit-hunk-section-p section)
    (magit-section-set-window-start section)))

(add-hook 'magit-section-movement-hook #'magit-hunk-set-window-start)

(cl-defmethod magit-section-get-relative-position ((_section magit-hunk-section))
  (nconc (cl-call-next-method)
         (and (region-active-p)
              (progn
                (goto-char (line-beginning-position))
                (when  (looking-at "^[-+]") (forward-line))
                (while (looking-at "^[ @]") (forward-line))
                (let ((beg (magit-point)))
                  (list (cond
                         ((looking-at "^[-+]")
                          (forward-line)
                          (while (looking-at "^[-+]") (forward-line))
                          (while (looking-at "^ ")    (forward-line))
                          (forward-line -1)
                          (regexp-quote (buffer-substring-no-properties
                                         beg (line-end-position))))
                         (t t))))))))

(cl-defmethod magit-section-goto-successor ((section magit-hunk-section)
                                            line char &optional arg)
  (or (magit-section-goto-successor--same section line char)
      (and-let* ((parent (magit-get-section
                          (magit-section-ident
                           (oref section parent)))))
        (let* ((children (oref parent children))
               (siblings (magit-section-siblings section 'prev))
               (previous (nth (length siblings) children)))
          (if (not arg)
              (when-let ((sibling (or previous (car (last children)))))
                (magit-section-goto sibling)
                t)
            (when previous
              (magit-section-goto previous))
            (if (and (stringp arg)
                     (re-search-forward arg (oref parent end) t))
                (goto-char (match-beginning 0))
              (goto-char (oref (car (last children)) end))
              (forward-line -1)
              (while (looking-at "^ ")    (forward-line -1))
              (while (looking-at "^[-+]") (forward-line -1))
              (forward-line)))))
      (magit-section-goto-successor--related section)))

;;; Diff Sections

(defvar-keymap magit-unstaged-section-map
  :doc "Keymap for the `unstaged' section."
  "<remap> <magit-visit-thing>"  #'magit-diff-unstaged
  "<remap> <magit-stage-files>"  #'magit-stage
  "<remap> <magit-delete-thing>" #'magit-discard
  "<3>" (magit-menu-item "Discard all" #'magit-discard)
  "<2>" (magit-menu-item "Stage all"   #'magit-stage)
  "<1>" (magit-menu-item "Visit diff"  #'magit-diff-unstaged))

(magit-define-section-jumper magit-jump-to-unstaged
  "Unstaged changes" unstaged nil magit-insert-unstaged-changes)

(defun magit-insert-unstaged-changes ()
  "Insert section showing unstaged changes."
  (magit-insert-section (unstaged)
    (magit-insert-heading t "Unstaged changes")
    (magit--insert-diff nil
      "diff" magit-buffer-diff-args "--no-prefix"
      "--" magit-buffer-diff-files)))

(defvar-keymap magit-staged-section-map
  :doc "Keymap for the `staged' section."
  "<remap> <magit-revert-no-commit>" #'magit-reverse
  "<remap> <magit-delete-thing>"     #'magit-discard
  "<remap> <magit-unstage-files>"    #'magit-unstage
  "<remap> <magit-visit-thing>"      #'magit-diff-staged
  "<4>" (magit-menu-item "Reverse all" #'magit-reverse)
  "<3>" (magit-menu-item "Discard all" #'magit-discard)
  "<2>" (magit-menu-item "Unstage all" #'magit-unstage)
  "<1>" (magit-menu-item "Visit diff"  #'magit-diff-staged))

(magit-define-section-jumper magit-jump-to-staged
  "Staged changes" staged nil magit-insert-staged-changes)

(defun magit-insert-staged-changes ()
  "Insert section showing staged changes."
  ;; Avoid listing all files as deleted when visiting a bare repo.
  (unless (magit-bare-repo-p)
    (magit-insert-section (staged)
      (magit-insert-heading t "Staged changes")
      (magit--insert-diff nil
        "diff" "--cached" magit-buffer-diff-args "--no-prefix"
        "--" magit-buffer-diff-files))))

;;; Diff Information

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
  (when-let ((section (or section (magit-current-section))))
    (cond ((derived-mode-p 'magit-revision-mode 'magit-stash-mode) 'committed)
          ((derived-mode-p 'magit-diff-mode)
           (let ((range magit-buffer-range)
                 (const magit-buffer-typearg))
             (cond (magit-buffer-diff-type)
                   ((equal const "--no-index") 'undefined)
                   ((or (not range)
                        (equal range "HEAD")
                        (magit-rev-eq range "HEAD"))
                    (if (equal const "--cached")
                        'staged
                      'unstaged))
                   ((equal const "--cached")
                    (if (magit-rev-head-p range)
                        'staged
                      'undefined)) ; i.e., committed and staged
                   (t 'committed))))
          ((derived-mode-p 'magit-status-mode)
           (let ((stype (oref section type)))
             (if (memq stype '(staged unstaged tracked untracked))
                 stype
               (pcase stype
                 ((or 'file 'module)
                  (let* ((parent (oref section parent))
                         (type   (oref parent type)))
                    (if (memq type '(file module))
                        (magit-diff-type parent)
                      type)))
                 ('hunk (thread-first section
                          (oref parent)
                          (oref parent)
                          (oref type)))))))
          ((derived-mode-p 'magit-log-mode)
           (if (or (and (magit-section-match 'commit section)
                        (oref section children))
                   (magit-section-match [* file commit] section))
               'committed
             'undefined))
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
sections, i.e., `hunks' or `files'.  If SECTION, or if that is nil
the current section, is a `hunk' section; and the region region
starts and ends inside the body of a that section, then the type
is `region'.  If the region is empty after a mouse click, then
`hunk' is returned instead of `region'.

If optional STRICT is non-nil, then return nil if the diff type of
the section at point is `untracked' or the section at point is not
actually a `diff' but a `diffstat' section."
  (let ((siblings (and (not ssection) (magit-region-sections nil t))))
    (setq section (or section (car siblings) (magit-current-section)))
    (when (and section
               (or (not strict)
                   (and (not (eq (magit-diff-type section) 'untracked))
                        (not (eq (and-let* ((parent (oref section parent)))
                                   (oref parent type))
                                 'diffstat)))))
      (pcase (list (oref section type)
                   (and siblings t)
                   (magit-diff-use-hunk-region-p)
                   ssection)
        (`(hunk   nil   t  ,_)
         (if (magit-section-internal-region-p section) 'region 'hunk))
        ('(hunk     t   t nil) 'hunks)
        (`(hunk    ,_  ,_  ,_) 'hunk)
        ('(file     t   t nil) 'files)
        (`(file    ,_  ,_  ,_) 'file)
        ('(module   t   t nil) 'files)
        (`(module  ,_  ,_  ,_) 'file)
        (`(,(or 'staged 'unstaged 'untracked) nil ,_ ,_) 'list)))))

(defun magit-diff-use-hunk-region-p ()
  (and (region-active-p)
       ;; TODO implement this from first principals
       ;; currently it's trial-and-error
       (not (and (or (eq this-command #'mouse-drag-region)
                     (eq last-command #'mouse-drag-region)
                     ;; When another window was previously
                     ;; selected then the last-command is
                     ;; some byte-code function.
                     (byte-code-function-p last-command))
                 (eq (region-end) (region-beginning))))))

(defun magit-diff--hunk-section ()
  (and-let* ((section (magit-current-section))
             (scope (magit-diff-scope section)))
    ;; Currently the `hunk' type is also abused for file
    ;; mode changes, which we are not interested in here.
    (cl-flet ((first-hunk (file-section)
                (seq-find (##not (equal (oref % value) '(chmod)))
                          (oref file-section children))))
      (pcase scope
        ('hunk section)
        ('file (first-hunk section))
        ('list (and-let* ((first-file (car (oref section children))))
                 (first-hunk first-file)))
        ('module nil)))))

(defun magit-diff--file-section ()
  (and-let* ((section (magit-current-section))
             (scope (magit-diff-scope section)))
    (pcase scope
      ('hunk (oref section parent))
      ('file section)
      ('list (car (oref section children)))
      ('module section))))

(defun magit-diff--file ()
  (and-let* ((file-section (magit-diff--file-section))
             (file (or (and (magit-section-match 'hunk)
                            (magit-diff-on-removed-line-p)
                            (oref file-section source))
                       (oref file-section value))))
    (if (equal magit-buffer-typearg "--no-index")
        (concat "/" file)
      (expand-file-name file (magit-toplevel)))))

;;; Hunk Paint
;;;; Paint

(cl-defmethod magit-section-paint ((section magit-hunk-section) highlight)
  (unless magit-diff-highlight-hunk-body
    (setq highlight nil))
  (let ((end (oref section end))
        (merging (looking-at "@@@"))
        (diff-type (magit-diff-type))
        (stage nil)
        (tab-width (magit-diff-tab-width
                    (magit-section-parent-value section))))
    (forward-line)
    (while (< (point) end)
      (when (and magit-diff-hide-trailing-cr-characters
                 (char-equal ?\r (char-before (line-end-position))))
        (put-text-property (1- (line-end-position)) (line-end-position)
                           'invisible t))
      (put-text-property
       (point) (1+ (line-end-position)) 'font-lock-face
       (cond
        ((looking-at "^\\+\\+?\\([<=|>]\\)\\{7\\}")
         (setq stage (pcase (list (match-str 1) highlight)
                       ('("<" nil) 'magit-diff-our)
                       ('("<"   t) 'magit-diff-our-highlight)
                       ('("|" nil) 'magit-diff-base)
                       ('("|"   t) 'magit-diff-base-highlight)
                       ('("=" nil) 'magit-diff-their)
                       ('("="   t) 'magit-diff-their-highlight)
                       ('(">" nil) nil)))
         (if highlight
             'magit-diff-conflict-heading-highlight
           'magit-diff-conflict-heading))
        ((looking-at (if merging "^\\(\\+\\| \\+\\)" "^\\+"))
         (magit-diff-paint-tab merging tab-width)
         (magit-diff-paint-whitespace merging 'added diff-type)
         (or stage
             (if highlight 'magit-diff-added-highlight 'magit-diff-added)))
        ((looking-at (if merging "^\\(-\\| -\\)" "^-"))
         (magit-diff-paint-tab merging tab-width)
         (magit-diff-paint-whitespace merging 'removed diff-type)
         (if highlight 'magit-diff-removed-highlight 'magit-diff-removed))
        (t
         (magit-diff-paint-tab merging tab-width)
         (magit-diff-paint-whitespace merging 'context diff-type)
         (if highlight 'magit-diff-context-highlight 'magit-diff-context))))
      (forward-line)))
  (when (eq magit-diff-refine-hunk 'all)
    (magit-diff-update-hunk-refinement section))
  (oset section painted (if highlight 'highlight 'plain)))

;;;; Whitespace

(defvar magit-diff--tab-width-cache nil)

(defun magit-diff-tab-width (file)
  (setq file (expand-file-name file))
  (cl-flet ((cache (value)
              (let ((elt (assoc file magit-diff--tab-width-cache)))
                (if elt
                    (setcdr elt value)
                  (setq magit-diff--tab-width-cache
                        (cons (cons file value)
                              magit-diff--tab-width-cache))))
              value))
    (cond
     ((not magit-diff-adjust-tab-width)
      tab-width)
     ((and-let* ((buffer (find-buffer-visiting file)))
        (cache (buffer-local-value 'tab-width buffer))))
     ((and-let* ((elt (assoc file magit-diff--tab-width-cache)))
        (or (cdr elt)
            tab-width)))
     ((or (eq magit-diff-adjust-tab-width 'always)
          (and (numberp magit-diff-adjust-tab-width)
               (>= magit-diff-adjust-tab-width
                   (nth 7 (file-attributes file)))))
      (cache (buffer-local-value 'tab-width (find-file-noselect file))))
     (t
      (cache nil)
      tab-width))))

(defun magit-diff-paint-tab (merging width)
  (save-excursion
    (forward-char (if merging 2 1))
    (while (= (char-after) ?\t)
      (put-text-property (point) (1+ (point))
                         'display (list (list 'space :width width)))
      (forward-char))))

(defun magit-diff-paint-whitespace (merging line-type diff-type)
  (when (and magit-diff-paint-whitespace
             (or (not (memq magit-diff-paint-whitespace '(uncommitted status)))
                 (memq diff-type '(staged unstaged)))
             (cl-case line-type
               (added   t)
               (removed (memq magit-diff-paint-whitespace-lines '(all both)))
               (context (memq magit-diff-paint-whitespace-lines '(all)))))
    (let ((prefix (if merging "^[-\\+\s]\\{2\\}" "^[-\\+\s]"))
          (indent
           (if (local-variable-p 'magit-diff-highlight-indentation)
               magit-diff-highlight-indentation
             (setq-local
              magit-diff-highlight-indentation
              (cdr (seq-find (##string-match-p (car %) default-directory)
                             (nreverse
                              (default-value
                               'magit-diff-highlight-indentation))))))))
      (when (and magit-diff-highlight-trailing
                 (looking-at (concat prefix ".*?\\([ \t]+\\)?$")))
        (let ((ov (make-overlay (match-beginning 1) (match-end 1) nil t)))
          (overlay-put ov 'font-lock-face 'magit-diff-whitespace-warning)
          (overlay-put ov 'priority 2)
          (overlay-put ov 'evaporate t)))
      (when (or (and (eq indent 'tabs)
                     (looking-at (concat prefix "\\( *\t[ \t]*\\)")))
                (and (integerp indent)
                     (looking-at (format "%s\\([ \t]* \\{%s,\\}[ \t]*\\)"
                                         prefix indent))))
        (let ((ov (make-overlay (match-beginning 1) (match-end 1) nil t)))
          (overlay-put ov 'font-lock-face 'magit-diff-whitespace-warning)
          (overlay-put ov 'priority 2)
          (overlay-put ov 'evaporate t))))))

;;;; Refinement

(cl-defmethod magit-section--refine ((section magit-hunk-section))
  (when (eq magit-diff-refine-hunk t)
    (magit-diff-update-hunk-refinement section)))

(defun magit-diff-update-hunk-refinement (&optional section allow-remove)
  (if section
      (unless (oref section hidden)
        (pcase (list magit-diff-refine-hunk
                     (oref section refined)
                     (eq section (magit-current-section)))
          ((or `(all nil ,_) '(t nil t))
           (oset section refined t)
           (save-excursion
             (goto-char (oref section start))
             ;; `diff-refine-hunk' does not handle combined diffs.
             (unless (looking-at "@@@")
               (let ((smerge-refine-ignore-whitespace
                      magit-diff-refine-ignore-whitespace)
                     ;; Avoid fsyncing many small temp files.
                     (write-region-inhibit-fsync t))
                 (diff-refine-hunk)))))
          ((and (guard allow-remove)
                (or `(nil t ,_) '(t t nil)))
           (oset section refined nil)
           (remove-overlays (oref section start)
                            (oref section end)
                            'diff-mode 'fine))))
    (cl-labels ((recurse (section)
                  (if (magit-section-match 'hunk section)
                      (magit-diff-update-hunk-refinement section t)
                    (dolist (child (oref section children))
                      (recurse child)))))
      (recurse magit-root-section))))

;;; Hunk Region

(defun magit-diff-hunk-region-beginning ()
  (magit--bol-position (region-beginning)))

(defun magit-diff-hunk-region-end ()
  (magit--eol-position (region-end)))

(defun magit-diff-update-hunk-region (section)
  "Highlight the hunk-internal region if any."
  (when (and (eq (oref section type) 'hunk)
             (eq (magit-diff-scope section t) 'region))
    (magit-diff--make-hunk-overlay
     (oref section start)
     (1- (oref section content))
     'font-lock-face 'magit-diff-lines-heading
     'display (magit-diff-hunk-region-header section)
     'after-string (magit-diff--hunk-after-string 'magit-diff-lines-heading))
    (run-hook-with-args 'magit-diff-highlight-hunk-region-functions section)
    t))

(defun magit-diff-highlight-hunk-region-dim-outside (section)
  "Dim the parts of the hunk that are outside the hunk-internal region.
This is done by using the same foreground and background color
for added and removed lines as for context lines."
  (let ((face (if magit-diff-highlight-hunk-body
                  'magit-diff-context-highlight
                'magit-diff-context)))
    (when magit-diff-unmarked-lines-keep-foreground
      (setq face `(:extend t :background ,(face-attribute face :background))))
    (magit-diff--make-hunk-overlay (oref section content)
                                   (magit-diff-hunk-region-beginning)
                                   'font-lock-face face
                                   'priority 2)
    (magit-diff--make-hunk-overlay (1+ (magit-diff-hunk-region-end))
                                   (oref section end)
                                   'font-lock-face face
                                   'priority 2)))

(defun magit-diff-highlight-hunk-region-using-face (_section)
  "Highlight the hunk-internal region by making it bold.
Or rather highlight using the face `magit-diff-hunk-region', though
changing only the `:weight' and/or `:slant' is recommended for that
face."
  (magit-diff--make-hunk-overlay (magit-diff-hunk-region-beginning)
                                 (1+ (magit-diff-hunk-region-end))
                                 'font-lock-face 'magit-diff-hunk-region))

(defun magit-diff-highlight-hunk-region-using-overlays (section)
  "Emphasize the hunk-internal region using delimiting horizontal lines.
This is implemented as single-pixel newlines places inside overlays."
  (if (window-system)
      (let ((beg (magit-diff-hunk-region-beginning))
            (end (magit-diff-hunk-region-end))
            (str (propertize
                  (concat (propertize "\s" 'display '(space :height (1)))
                          (propertize "\n" 'line-height t))
                  'font-lock-face 'magit-diff-lines-boundary)))
        (magit-diff--make-hunk-overlay beg (1+ beg) 'before-string str)
        (magit-diff--make-hunk-overlay end (1+ end) 'after-string  str))
    (magit-diff-highlight-hunk-region-using-face section)))

(defun magit-diff-highlight-hunk-region-using-underline (section)
  "Emphasize the hunk-internal region using delimiting horizontal lines.
This is implemented by overlining and underlining the first and
last (visual) lines of the region."
  (if (window-system)
      (let* ((beg (magit-diff-hunk-region-beginning))
             (end (magit-diff-hunk-region-end))
             (beg-eol (save-excursion (goto-char beg)
                                      (end-of-visual-line)
                                      (point)))
             (end-bol (save-excursion (goto-char end)
                                      (beginning-of-visual-line)
                                      (point)))
             (color (face-background 'magit-diff-lines-boundary nil t)))
        (cl-flet ((ln (b e &rest face)
                    (magit-diff--make-hunk-overlay
                     b e 'font-lock-face face 'after-string
                     (magit-diff--hunk-after-string face))))
          (if (= beg end-bol)
              (ln beg beg-eol :overline color :underline color)
            (ln beg beg-eol :overline color)
            (ln end-bol end :underline color))))
    (magit-diff-highlight-hunk-region-using-face section)))

(defun magit-diff--make-hunk-overlay (start end &rest args)
  (let ((ov (make-overlay start end nil t)))
    (overlay-put ov 'evaporate t)
    (while args (overlay-put ov (pop args) (pop args)))
    (push ov magit-section--region-overlays)
    ov))

(defun magit-diff--hunk-after-string (face)
  (propertize "\s"
              'font-lock-face face
              'display (list 'space :align-to
                             `(+ (0 . right)
                                 ,(min (window-hscroll)
                                       (- (line-end-position)
                                          (line-beginning-position)))))
              ;; This prevents the cursor from being rendered at the
              ;; edge of the window.
              'cursor t))

;;; Utilities

(defun magit-diff-inside-hunk-body-p ()
  "Return t if point is inside the body of a hunk."
  (and-let* ((section (magit-current-section))
             ((cl-typep section 'magit-hunk-section))
             (content (oref section content)))
    (> (magit-point) content)))

(defun magit-diff-on-removed-line-p ()
  "Return t if point is on a removed line inside the body of a hunk."
  (let ((section (magit-current-section)))
    (and (cl-typep section 'magit-hunk-section)
         (not (oref section combined))
         (= (char-after (pos-bol)) ?-))))

(defun magit-diff--combined-p (section)
  (cl-assert (cl-typep section 'magit-file-section))
  (string-match-p "\\`diff --\\(combined\\|cc\\)" (oref section value)))

;;; Diff Extract

(defun magit-diff-file-header (section &optional no-rename)
  (when (magit-hunk-section-p section)
    (setq section (oref section parent)))
  (and (magit-file-section-p section)
       (let ((header (oref section header)))
         (if no-rename
             (replace-regexp-in-string
              "^--- \\(.+\\)" (oref section value) header t t 1)
           header))))

(defun magit-diff-hunk-region-header (section)
  (let ((patch (magit-diff-hunk-region-patch section)))
    (string-match "\n" patch)
    (substring patch 0 (1- (match-end 0)))))

(defun magit-diff-hunk-region-patch (section &optional args)
  (let ((op (if (member "--reverse" args) "+" "-"))
        (sbeg (oref section start))
        (rbeg (magit-diff-hunk-region-beginning))
        (rend (region-end))
        (send (oref section end))
        (patch nil))
    (save-excursion
      (goto-char sbeg)
      (while (< (point) send)
        (looking-at "\\(.\\)\\([^\n]*\n\\)")
        (cond ((or (string-match-p "[@ ]" (match-str 1))
                   (and (>= (point) rbeg)
                        (<= (point) rend)))
               (push (match-str 0) patch))
              ((equal op (match-str 1))
               (push (concat " " (match-str 2)) patch)))
        (forward-line)))
    (let ((buffer-list-update-hook nil)) ; #3759
      (with-temp-buffer
        (insert (string-join (reverse patch)))
        (diff-fixup-modifs (point-min) (point-max))
        (setq patch (buffer-string))))
    patch))

;;; _
(provide 'magit-diff)
;; Local Variables:
;; read-symbol-shorthands: (
;;   ("match-string" . "match-string")
;;   ("match-str" . "match-string-no-properties"))
;; End:
;;; magit-diff.el ends here
