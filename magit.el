;;; magit.el --- control Git from Emacs

;; Copyright (C) 2008-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Former-Maintainers:
;;	Nicolas Dudebout  <nicolas.dudebout@gatech.edu>
;;	Peter J. Weisberg <pj@irregularexpressions.net>
;;	Phil Jackson      <phil@shellarchive.co.uk>
;;	Rémi Vanicat      <vanicat@debian.org>
;;	Yann Hodique      <yann.hodique@gmail.com>

;; Keywords: vc tools
;; Package: magit
;; Package-Requires: ((cl-lib "0.5") (dash "2.8.0") (git-commit-mode "0.15.0") (git-rebase-mode "0.15.0") (with-editor "0.15.0"))

;; Magit requires at least GNU Emacs 24.1 and Git 1.7.2.5.

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

;; Invoking the `magit-status' command will show a buffer with the
;; status of the current Git repository and its working tree.  That
;; buffer offers key bindings for manipulating the status in simple
;; ways.
;;
;; The status buffer mainly shows the difference between the working
;; tree and the index, and the difference between the index and the
;; current HEAD.  You can add individual hunks from the working tree
;; to the index, and you can commit the index.
;;
;; See the Magit User Manual for more information.

;;; Code:

(when (version< emacs-version "24.1")
  (error "Magit requires at least GNU Emacs 24.1"))

(require 'cl-lib)
(require 'dash)

(require 'git-commit-mode)
(require 'git-rebase-mode)
(require 'with-editor)
(require 'magit-core)
(require 'magit-diff)
(require 'magit-apply)
(require 'magit-log)

(declare-function magit-blame-chunk-get 'magit-blame)
(declare-function magit-blame-mode 'magit-blame)
(defvar magit-blame-mode)

(require 'format-spec)
(require 'package nil t) ; used in `magit-version'
(require 'server) ; for `server-visit-hook'

(eval-when-compile (require 'dired-x))
(declare-function dired-jump 'dired-x)
(eval-when-compile (require 'epa)) ; for `epa-protocol'
(eval-when-compile (require 'epg))
(declare-function epg-sub-key-id 'epg)
(declare-function epg-key-sub-key-list 'epg)
(declare-function epg-key-user-id-list 'epg)
(declare-function epg-user-id-string 'epg)
(declare-function epg-decode-dn 'epg)
(declare-function epg-list-keys 'epg)
(eval-when-compile (require 'eshell))
(declare-function eshell-parse-arguments 'eshell)
(eval-when-compile (require 'smerge-mode))


;;; Options
;;;; Status Mode

(defgroup magit-status nil
  "Inspect and manipulate Git repositories."
  :group 'magit-modes)

(defcustom magit-status-mode-hook nil
  "Hook run when the `magit-status' buffer is created."
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-headers-hook
  '(magit-insert-status-tags-line)
  "Hook run to insert headers into the status buffer.

This hook is run by `magit-insert-status-headers', which always
inserts the \"Head\" and \"Upstream\" headers before the headers
listed here.  `magit-insert-status-headers' has to be a member
of `magit-insert-status-sections', or no headers are inserted."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-sections-hook
  '(magit-insert-status-headers
    magit-insert-merge-log
    magit-insert-rebase-sequence
    magit-insert-am-sequence
    magit-insert-sequencer-sequence
    magit-insert-bisect-output
    magit-insert-bisect-rest
    magit-insert-bisect-log
    magit-insert-untracked-files
    magit-insert-unstaged-changes
    magit-insert-staged-changes
    magit-insert-stashes
    magit-insert-unpulled-commits
    magit-insert-unpushed-commits)
  "Hook run to insert sections into the status buffer.

This option allows reordering the sections and adding sections
that are by default displayed in other Magit buffers.  Doing the
latter is currently not recommended because not all functions
that insert sections have been adapted yet.  Only inserters that
take no argument can be used and some functions exist that begin
with the `magit-insert-' prefix but do not insert a section.

Note that there are already plans to improve this and to add
similar hooks for other Magit modes."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-buffer-switch-function 'pop-to-buffer
  "Function used by `magit-status' to switch to the status buffer.

The function is given one argument, the status buffer."
  :group 'magit-status
  :type '(radio (function-item switch-to-buffer)
                (function-item pop-to-buffer)
                (function :tag "Other")))

(defcustom magit-status-buffer-name-format "*magit: %a*"
  "Name format for buffers used to display a repository's status.
The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'string)

;;;; Commit Mode

(defgroup magit-commit nil
  "Inspect and manipulate Git commits."
  :group 'magit-modes)

(defcustom magit-commit-buffer-name-format "*magit-commit: %a*"
  "Name format for buffers used to display a commit.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'string)

(defcustom magit-commit-show-diffstat t
  "Whether to show diffstat in commit buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

(defcustom magit-commit-show-notes t
  "Whether to show notes in commit buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

(defcustom magit-commit-show-xref-buttons t
  "Whether to show buffer history buttons in commit buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-commit
  :type 'boolean)

;; This variable is only a temporary hack.
(defvar magit-commit-extra-options '("--decorate=full" "--pretty=medium"))

;;;; Refs Mode

(defgroup magit-refs nil
  "Inspect and manipulate Git branches and tags."
  :group 'magit-modes)

(defcustom magit-refs-sections-hook
  '(magit-insert-branch-description
    magit-insert-local-branches
    magit-insert-remote-branches
    magit-insert-tags)
  "Hook run to insert sections into the references buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :type 'hook)

(defcustom magit-refs-buffer-name-format "*magit-refs: %a*"
  "Name format for buffers used to display and manage refs.

The following `format'-like specs are supported:
%a the absolute filename of the repository toplevel.
%b the basename of the repository toplevel."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :type 'string)

;;;; Miscellaneous

(defcustom magit-commit-ask-to-stage t
  "Whether to ask to stage everything when committing and nothing is staged."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-extend-override-date nil
  "Whether using `magit-commit-extend' changes the committer date."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-reword-override-date nil
  "Whether using `magit-commit-reword' changes the committer date."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-commit-squash-confirm t
  "Whether the commit targeted by squash and fixup has to be confirmed.
When non-nil then the commit at point (if any) is used as default
choice, otherwise it has to be confirmed.  This option only
affects `magit-commit-squash' and `magit-commit-fixup'.  The
\"instant\" variants always require confirmation because making
an error while using those is harder to recover from."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-merge-warn-dirty-worktree t
  "Whether to warn before merging with a dirty worktree."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-set-upstream-on-push nil
  "Whether `magit-push' may set upstream when pushing a branch.
This only applies if the branch does not have an upstream set yet.

nil        don't use --set-upstream.
t          ask if --set-upstream should be used.
`dontask'  always use --set-upstream.
`refuse'   refuse to push unless a remote branch has already been set."
  :group 'magit-commands
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Ask" t)
                 (const :tag "Ask if not set" askifnotset)
                 (const :tag "Refuse" refuse)
                 (const :tag "Always" dontask)))

(defcustom magit-stash-snapshot-message-format
  "Snapshot taken at %Y-%m-%d %H:%M:%S"
  "Format for messages of snapshot stashes."
  :package-version '(magit . "2.1.0")
  :group 'magit-commands
  :type 'string)

(defcustom magit-repository-directories nil
  "Directories containing Git repositories.
Magit will look into these directories for Git repositories
and offer them as choices for `magit-status'."
  :group 'magit
  :type '(repeat string))

(defcustom magit-repository-directories-depth 3
  "The maximum depth to look for Git repositories.
When looking for a Git repository below the directories in
`magit-repository-directories', Magit will only descend this
many levels deep."
  :group 'magit
  :type 'integer)

;;;; Faces

(defface magit-dimmed
  '((((class color) (background light)) :foreground "grey50")
    (((class color) (background  dark)) :foreground "grey50"))
  "Face for text that shouldn't stand out."
  :group 'magit-faces)

(defface magit-hash
  '((((class color) (background light)) :foreground "grey60")
    (((class color) (background  dark)) :foreground "grey40"))
  "Face for the sha1 part of the log output."
  :group 'magit-faces)

(defface magit-head
  '((((class color) (background light))
     :background "grey75"
     :foreground "grey15")
    (((class color) (background dark))
     :background "grey30"
     :foreground "grey90"))
  "Face for the symbolic ref \"HEAD\"."
  :group 'magit-faces)

(defface magit-tag
  '((((class color) (background light))
     :background "grey80"
     :foreground "Goldenrod4")
    (((class color) (background dark))
     :background "grey30"
     :foreground "LightGoldenrod2"))
  "Face for tag labels shown in log buffer."
  :group 'magit-faces)

(defface magit-branch-local
  '((((class color) (background light))
     :background "grey80"
     :foreground "SkyBlue4")
    (((class color) (background dark))
     :background "grey30"
     :foreground "LightSkyBlue1"))
  "Face for local branches."
  :group 'magit-faces)

(defface magit-branch-remote
  '((((class color) (background light))
     :background "grey80"
     :foreground "DarkOliveGreen4")
    (((class color) (background dark))
     :background "grey30"
     :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
  :group 'magit-faces)

(defface magit-refname
  '((((class color) (background light))
     :background "grey75"
     :foreground "grey30")
    (((class color) (background  dark))
     :background "grey30"
     :foreground "grey80"))
  "Face for refnames without a dedicated face."
  :group 'magit-faces)

(defface magit-refname-stash
  '((t :inherit magit-refname))
  "Face for wip refnames."
  :group 'magit-faces)

(defface magit-refname-wip
  '((t :inherit magit-refname))
  "Face for wip refnames."
  :group 'magit-faces)

(defface magit-signature-good
  '((t :foreground "green"))
  "Face for good signatures."
  :group 'magit-faces)

(defface magit-signature-bad
  '((t :foreground "red"))
  "Face for bad signatures."
  :group 'magit-faces)

(defface magit-signature-untrusted
  '((t :foreground "cyan"))
  "Face for good untrusted signatures."
  :group 'magit-faces)

(defface magit-bisect-good
  '((t :background "LightGreen"
       :foreground "DarkOliveGreen"))
  "Face for good bisect revisions."
  :group 'magit-faces)

(defface magit-bisect-skip
  '((t :background "LightGoldenrod"
       :foreground "DarkGoldenrod"))
  "Face for skipped bisect revisions."
  :group 'magit-faces)

(defface magit-bisect-bad
  '((t :background "IndianRed1"
       :foreground "IndianRed4"))
  "Face for bad bisect revisions."
  :group 'magit-faces)

(defface magit-cherry-unmatched
  '((t :foreground "cyan"))
  "Face for unmatched cherry commits.")

(defface magit-cherry-equivalent
  '((t :foreground "magenta"))
  "Face for equivalent cherry commits.")

;;; Inspect
;;;; Commit Mode

(defvar magit-commit-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-diff-mode-map)
    map)
  "Keymap for `magit-commit-mode'.")

(define-derived-mode magit-commit-mode magit-mode "Magit"
  "Mode for looking at a Git commit.
This mode is documented in info node `(magit)Commit Buffer'.

\\<magit-commit-mode-map>\
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-file] to visit the hunk or file at point.
Type \\[magit-apply] to apply the change at point to the worktree.
Type \\[magit-reverse] to reverse the change at point in the worktree.
\n\\{magit-commit-mode-map}"
  :group 'magit-commit)

;;;###autoload
(defun magit-show-commit (commit &optional noselect module)
  "Show the commit at point.
If there is no commit at point or with a prefix argument prompt
for a commit."
  (interactive
   (let* ((mcommit (magit-section-when mcommit))
          (atpoint (or (and magit-blame-mode (magit-blame-chunk-get :hash))
                       mcommit (magit-branch-or-commit-at-point)
                       (magit-section-when tag))))
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
    (magit-mode-setup magit-commit-buffer-name-format
                      (if noselect 'display-buffer 'pop-to-buffer)
                      #'magit-commit-mode
                      #'magit-refresh-commit-buffer
                      commit)))

(defun magit-show-or-scroll-up ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer up.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-show-or-scroll 'scroll-up))

(defun magit-show-or-scroll-down ()
  "Update the commit or diff buffer for the thing at point.

Either show the commit or stash at point in the appropriate
buffer, or if that buffer is already being displayed in the
current frame and contains information about that commit or
stash, then instead scroll the buffer down.  If there is no
commit or stash at point, then prompt for a commit."
  (interactive)
  (magit-show-or-scroll 'scroll-down))

(defun magit-show-or-scroll (fn)
  (let (rev cmd buf win)
    (if magit-blame-mode
        (setq rev (magit-blame-chunk-get :hash)
              cmd 'magit-show-commit
              buf (magit-mode-get-buffer
                   magit-commit-buffer-name-format 'magit-commit-mode))
      (magit-section-case
        (commit (setq rev (magit-section-value it)
                      cmd 'magit-show-commit
                      buf (magit-mode-get-buffer
                           magit-commit-buffer-name-format 'magit-commit-mode)))
        (stash  (setq rev (magit-section-value it)
                      cmd 'magit-diff-stash
                      buf (magit-mode-get-buffer
                           magit-diff-buffer-name-format 'magit-diff-mode)))))
    (if rev
        (if (and buf
                 (setq win (get-buffer-window buf))
                 (with-current-buffer buf
                   (equal (if (eq cmd 'magit-diff-stash)
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

(defun magit-refresh-commit-buffer (commit)
  (magit-insert-section (commitbuf)
    (magit-git-wash #'magit-wash-commit
      "log" "-1" "-p" "--cc" "--no-prefix"
      (and magit-commit-show-diffstat "--stat")
      (and magit-commit-show-notes "--notes")
      magit-diff-options
      magit-diff-extra-options
      magit-commit-extra-options
      commit)))

(defun magit-wash-commit (args)
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
              (magit-insert-commit-button rev)
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

(defun magit-insert-commit-button (hash)
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

;;;; Status Mode

(defvar magit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "jz" 'magit-jump-to-stashes)
    (define-key map "jn" 'magit-jump-to-untracked)
    (define-key map "ju" 'magit-jump-to-unstaged)
    (define-key map "js" 'magit-jump-to-staged)
    (define-key map "jf" 'magit-jump-to-unpulled)
    (define-key map "jp" 'magit-jump-to-unpushed)
    map)
  "Keymap for `magit-status-mode'.")

(eval-after-load 'dired-x
  '(define-key magit-status-mode-map [remap dired-jump] 'magit-dired-jump))

(define-derived-mode magit-status-mode magit-mode "Magit"
  "Mode for looking at Git status.
This mode is documented in info node `(magit)Status'.

\\<magit-status-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-dispatch-popup] to see available action popups.
Type \\[magit-section-toggle] to expand or hide the section at point.
Type \\[magit-visit-file] to visit the thing at point.
Type \\[magit-stage] to stage the change at point; \\[magit-unstage] to unstage.
Type \\[magit-commit-popup] to create a commit.
\n\\{magit-status-mode-map}"
  :group 'magit-status)

;;;###autoload
(defun magit-status (dir &optional switch-function)
  "Open a Magit status buffer for the Git repository containing DIR.
If DIR is not within a Git repository, offer to create a Git
repository in DIR.

Interactively, a prefix argument means to ask the user which
Git repository to use even if `default-directory' is under
Git control.  Two prefix arguments means to ignore
`magit-repository-directories' when asking for user input.

Depending on option `magit-status-buffer-switch-function' the
status buffer is shown in another window (the default) or the
current window.  Non-interactively optional SWITCH-FUNCTION
can be used to override this."
  (interactive (list (if current-prefix-arg
                         (magit-read-top-dir
                          (> (prefix-numeric-value current-prefix-arg) 4))
                       (or (magit-get-top-dir)
                           (magit-read-top-dir nil)))))
  (-when-let (default-directory
              (or (magit-get-top-dir dir)
                  (and (yes-or-no-p
                        (format "No repository in %s.  Create one? " dir))
                       (progn (magit-init dir)
                              (magit-get-top-dir dir)))))
    (magit-mode-setup magit-status-buffer-name-format
                      (or switch-function
                          magit-status-buffer-switch-function)
                      #'magit-status-mode
                      #'magit-refresh-status)))

(defun magit-refresh-status ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-insert-section (status)
    (run-hooks 'magit-status-sections-hook))
  (run-hooks 'magit-refresh-status-hook))

(defun magit-insert-status-headers (&optional branch upstream)
  (unless branch
    (setq branch (magit-get-current-branch)))
  (-if-let  (hash (magit-rev-parse "--verify" "HEAD"))
      (let ((line (magit-rev-format "%h %s" "HEAD")))
        (string-match "^\\([^ ]+\\) \\(.+\\)" line)
        (magit-bind-match-strings (hash msg) line
          (magit-insert-section (branch (or branch hash))
            (magit-insert-heading
              (magit-string-pad "Head: " 10)
              (propertize hash 'face 'magit-hash) " "
              (if branch
                  (propertize branch 'face 'magit-branch-local)
                (propertize "HEAD" 'face 'magit-head))
              " " msg "\n")
            (when (or upstream (setq upstream (magit-get-tracked-branch branch)))
              (setq line (or (magit-rev-format "%h %s" upstream) ""))
              (string-match "^\\([^ ]+\\) \\(.+\\)" line)
              (magit-bind-match-strings (hash msg) line
                (magit-insert-section (branch upstream)
                  (magit-insert
                   (concat
                    (magit-string-pad "Upstream: " 10)
                    (if hash (propertize hash 'face 'magit-hash) "missing") " "
                    (and (magit-get-boolean "branch" branch "rebase") "onto ")
                    (propertize
                     upstream 'face
                     (if (string= (magit-get "branch" branch "remote") ".")
                         'magit-branch-local
                       'magit-branch-remote))
                    " " msg "\n")))))
            (run-hooks 'magit-status-headers-hook)))
        ;; This belongs to no section but `magit-root-section'.
        (insert "\n"))
    (insert "In the beginning there was darkness\n\n")))

(defun magit-insert-status-tags-line ()
  (let* ((current-tag (magit-get-current-tag t))
         (next-tag (magit-get-next-tag t))
         (both-tags (and current-tag next-tag t)))
    (when (or current-tag next-tag)
      (magit-insert-section (tag (or current-tag next-tag))
        (magit-insert
         (concat
          (magit-string-pad (if both-tags "Tags: " "Tag: ") 10)
          (and current-tag (magit-format-status-tag-sentence
                            (car current-tag) (cadr current-tag) nil))
          (and both-tags ", ")
          (and next-tag (magit-format-status-tag-sentence
                         (car next-tag) (cadr next-tag) t))
          "\n"))))))

(defun magit-format-status-tag-sentence (tag count next)
  (concat (propertize tag 'face 'magit-tag)
          (and (> count 0)
               (format " (%s)"
                       (propertize (format "%s" count) 'face
                                   (if next 'magit-tag 'magit-branch-local))))))

(defvar magit-untracked-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    map)
  "Keymap for the `untracked' section.")

(magit-define-section-jumper untracked "Untracked files")

(defun magit-insert-untracked-files ()
  (--when-let (--mapcat (and (eq (aref it 0) ??) (list (substring it 3)))
                        (magit-git-lines "status" "--porcelain" "-unormal"))
    (magit-insert-section (untracked)
      (magit-insert-heading "Untracked files:")
      (magit-insert-untracked-files-1 it)
      (insert "\n"))))

(defun magit-insert-untracked-files-1 (files)
  (dolist (file files)
    (if (string-suffix-p "/" file)
        (magit-insert-section (file file t)
          (insert (directory-file-name file) "/\n")
            (magit-insert-heading)
            (magit-insert-files
             (--map (substring it 3)
                    (magit-git-lines "status" "--porcelain"
                                     "-unormal" "--" file))))
      (magit-insert-section (file file)
        (insert file "\n")))))

;;;; Refs Mode

(defvar magit-refs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    map)
  "Keymap for `magit-refs-mode'.")

(define-derived-mode magit-refs-mode magit-mode "Magit Branch"
  "Mode which lists and compares references.
This mode is documented in info node `(magit)Branches and Remotes'.

\\<magit-refs-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-branch-popup] to see available branch commands.
Type \\[magit-show-commit] or \\[magit-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-merge-popup] to merge the branch or commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
Type \\[magit-reset-head] to reset HEAD to the commit at point.
\n\\{magit-refs-mode-map}"
  :group 'magit-modes)

(magit-define-popup magit-show-refs-popup
  "Popup console for `magit-show-refs'."
  'magit-popups
  :man-page "git-branch"
  :switches '((?m "Merged to HEAD"            "--merged")
              (?M "Merged to master"          "--merged=master")
              (?n "Not merged to HEAD"        "--no-merged")
              (?N "Not merged to master"      "--no-merged=master"))
  :options  '((?c "Contains"   "--contains="  magit-read-branch-or-commit)
              (?m "Merged"     "--merged="    magit-read-branch-or-commit)
              (?n "Not merged" "--no-merged=" magit-read-branch-or-commit))
  :actions  '((?y "Show refs" magit-show-refs))
  :default-action 'magit-show-refs
  :use-prefix 'popup)

;;;###autoload
(defun magit-show-refs (&optional head args)
  "List and compare references in a dedicated buffer."
  (interactive (list (and (or current-prefix-arg magit-current-popup)
                          (magit-read-branch "Compare branch"))
                     magit-current-popup-args))
  (magit-mode-setup magit-refs-buffer-name-format nil
                    #'magit-refs-mode
                    #'magit-refresh-refs-buffer head args))

(defun magit-refresh-refs-buffer (&rest ignore)
  (magit-insert-section (branchbuf)
    (run-hooks 'magit-refs-sections-hook)))

(defconst magit-wash-branch-line-re
  (concat "^"
          "\\(?:[ \\*]\\) "
          "\\(?1:[^ ]+?\\)"                 ; branch
          "\\(?: +\\)"
          "\\(?2:[0-9a-fA-F]+\\) "          ; sha1
          "\\(?:\\["
          "\\(?4:[^:\n]+?\\)\\(?:: \\)?"    ; upstream
          "\\(?:ahead \\(?5:[0-9]+\\)\\)?"  ; ahead
          "\\(?:, \\)?"
          "\\(?:behind \\(?6:[0-9]+\\)\\)?" ; behind
          "\\] \\)?"
          "\\(?3:.*\\)"))                   ; message

(defvar magit-local-branch-format "%c %-25n %U%m\n")
(defvar magit-remote-branch-format "%c %-25n %m\n")
(defvar magit-tags-format "    %n\n")

(defvar magit-branch-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-show-commit)
    (define-key map "k"  'magit-branch-delete)
    (define-key map "R"  'magit-branch-rename)
    map)
  "Keymap for `branch' sections.")

(defvar magit-remote-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k"  'magit-remote-remove)
    (define-key map "R"  'magit-remote-rename)
    map)
  "Keymap for `remote' sections.")

(defun magit-insert-local-branches ()
  (magit-insert-section (local nil)
    (magit-insert-heading "Branches:")
    (let ((current  (magit-get-current-branch))
          (branches (magit-list-local-branch-names)))
      (dolist (line (magit-git-lines "branch" "-vv"
                                     (cadr magit-refresh-args)))
        (string-match magit-wash-branch-line-re line)
        (magit-bind-match-strings
            (branch hash message upstream ahead behind) line
          (magit-insert-branch
           branch current branches
           magit-local-branch-format 'magit-branch-local
           hash message upstream ahead behind))))
    (insert ?\n)))

(defun magit-insert-remote-branches ()
  (dolist (remote (magit-list-remotes))
    (magit-insert-section (remote remote)
      (magit-insert-heading
        (let ((pull (magit-get "remote" remote "url"))
              (push (magit-get "remote" remote "pushurl")))
          (format "%s (%s):" (capitalize remote)
                  (concat pull (and pull push ", ") push))))
      (let ((current  (magit-get-current-branch))
            (branches (magit-list-local-branch-names)))
        (dolist (line (magit-git-lines "branch" "-vvr"
                                       (cadr magit-refresh-args)))
          (when (string-match magit-wash-branch-line-re line)
            (magit-bind-match-strings (branch hash message) line
              (when (string-match-p (format "^%s/" remote) branch)
                (magit-insert-branch
                 branch current branches
                 magit-remote-branch-format 'magit-branch-remote hash message))))))
      (insert ?\n))))

(defun magit-insert-branch
    (branch current branches format face
            &optional hash message upstream ahead behind)
  (magit-insert-section it (branch branch t)
    (let* ((head  (or (car magit-refresh-args) current "HEAD"))
           (count (string-to-number
                   (magit-git-string
                    "rev-list" "--count" "--right-only"
                    (concat head "..." branch)))))
      (when upstream
        (setq upstream (propertize upstream 'face
                                   (if (member upstream branches)
                                       'magit-branch-local
                                     'magit-branch-remote))))
      (magit-insert-heading
        (format-spec
         format
         `((?a . ,(or ahead ""))
           (?b . ,(or behind ""))
           (?c . ,(cond
                   ((equal branch head)
                    (format "%3s" (if (equal branch current) "@" "#")))
                   ((> count 0)
                    (propertize (format "%3s" (number-to-string count))
                                'face 'magit-dimmed))
                   (t "   ")))
           (?h . ,(or (propertize hash 'face 'magit-hash) ""))
           (?m . ,(or message ""))
           (?n . ,(propertize branch 'face face))
           (?u . ,(or upstream ""))
           (?U . ,(if upstream
                      (format
                       (propertize "[%s%s] " 'face 'magit-dimmed)
                       upstream
                       (if (or ahead behind)
                           (concat ": " (and ahead (format "ahead %s" ahead))
                                   (and ahead behind ", ")
                                   (and behind (format "behind %s" behind)))
                         ""))
                    "")))))
      (when (> count 0)
        (if (magit-section-hidden it)
            (setf (magit-section-washer it)
                  `(lambda ()
                     (magit-insert-cherry-commits-1 ,head ,branch)
                     (insert (propertize "\n" 'magit-section ,it))))
          (magit-insert-cherry-commits-1 head branch)
          (insert ?\n))))))

(defvar magit-tag-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-show-commit)
    (define-key map "k"  'magit-tag-delete)
    map)
  "Keymap for `tag' sections.")

(defun magit-insert-tags ()
  (magit-insert-section (tags)
    (magit-insert-heading "Tags:")
    (dolist (tag (magit-git-lines "tag"))
      (magit-insert-section (tag tag)
        (magit-insert
         (format-spec magit-tags-format
                      `((?n . ,(propertize tag 'face 'magit-tag)))))))
    (insert ?\n)))

;;;; Files

;;;###autoload
(defun magit-find-file (rev file)
  (interactive (magit-find-file-read-args "Find file"))
  (switch-to-buffer (magit-find-file-noselect rev file)))

;;;###autoload
(defun magit-find-file-other-window (rev file)
  (interactive (magit-find-file-read-args "Find file in other window"))
  (switch-to-buffer-other-window (magit-find-file-noselect rev file)))

(defun magit-find-file-read-args (prompt)
  (let ((rev (magit-read-branch-or-commit "Find file from revision")))
    (list rev (magit-read-file-from-rev rev prompt))))

(defun magit-get-revision-buffer (rev file &optional create)
  (funcall (if create 'get-buffer-create 'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun magit-get-revision-buffer-create (rev file)
  (magit-get-revision-buffer rev file t))

(defun magit-find-file-noselect (rev file)
  (with-current-buffer (magit-get-revision-buffer-create rev file)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (magit-git-insert "cat-file" "-p" (concat rev ":" file)))
    (setq magit-buffer-revision  (magit-rev-format "%H" rev)
          magit-buffer-refname   rev
          magit-buffer-file-name (expand-file-name file (magit-get-top-dir)))
    (let ((buffer-file-name magit-buffer-file-name))
      (normal-mode t))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (run-hooks 'magit-find-file-hook)
    (current-buffer)))

(defun magit-find-file-index-noselect (file)
  (let* ((bufname (concat file ".~{index}~"))
         (origbuf (get-buffer bufname)))
    (with-current-buffer (get-buffer-create bufname)
      (when (or (not origbuf)
                (y-or-n-p (format "%s already exists; revert it? " bufname)))
        (let ((inhibit-read-only t)
              (temp (car (split-string
                          (magit-git-string "checkout-index" "--temp" file)
                          "\t"))))
          (erase-buffer)
          (insert-file-contents temp nil nil nil t)
          (delete-file temp)))
      (setq magit-buffer-revision  "{index}"
            magit-buffer-refname   "{index}"
            magit-buffer-file-name (expand-file-name file (magit-get-top-dir)))
      (let ((buffer-file-name magit-buffer-file-name))
        (normal-mode t))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (run-hooks 'magit-find-index-hook)
      (current-buffer))))

(defun magit-update-index ()
  (interactive)
  (let ((file (magit-file-relative-name)))
    (unless (equal magit-buffer-refname "{index}")
      (user-error "%s isn't visiting the index" file))
    (unless (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
      (user-error "Abort"))
    (let ((index (make-temp-file "index"))
          (buffer (current-buffer)))
      (with-temp-file index
        (insert-buffer-substring buffer))
      (magit-call-git "update-index" "--cacheinfo"
                      (substring (magit-git-string "ls-files" "-s" file) 0 6)
                      (magit-git-string "hash-object" "-t" "blob" "-w"
                                        (concat "--path=" file)
                                        "--" index)
                      file))
    (set-buffer-modified-p nil))
  (--when-let (magit-mode-get-buffer
               magit-status-buffer-name-format 'magit-status-mode)
    (with-current-buffer it (magit-refresh)))
  t)

(defun magit-visit-file (file &optional other-window)
  (interactive (list (magit-file-at-point) current-prefix-arg))
  (unless (file-exists-p file)
    (user-error "Can't visit deleted file: %s" file))
  (if (file-directory-p file)
      (progn
        (setq file (file-name-as-directory (expand-file-name file)))
        (if (equal (magit-get-top-dir (file-name-directory file))
                   (magit-get-top-dir))
            (magit-dired-jump other-window)
          (magit-status file (if other-window
                                 'pop-to-buffer
                               'switch-to-buffer))))
    (let ((pos (magit-section-when hunk
                 (magit-hunk-file-position it)))
          (buffer (or (get-file-buffer file)
                      (find-file-noselect file))))
      (if (or other-window (get-buffer-window buffer))
          (pop-to-buffer buffer)
        (switch-to-buffer buffer))
      (when pos
        (goto-char (point-min))
        (forward-line (1- (car pos)))
        (move-to-column (cdr pos))))
    (when (magit-anything-unmerged-p file)
      (smerge-start-session))))

(defun magit-hunk-file-position (section)
  (let* ((value (magit-section-value section))
         (hunk-line (line-number-at-pos (point)))
         (goto-line (car (last value)))
         (offset (- (length value) 2))
         (column (current-column)))
    (save-excursion
      (string-match "^\\+\\([0-9]+\\)" goto-line)
      (setq goto-line (string-to-number (match-string 1 goto-line)))
      (goto-char (magit-section-content section))
      (while (< (line-number-at-pos) hunk-line)
        (unless (string-match-p
                 "-" (buffer-substring (point) (+ (point) offset)))
          (cl-incf goto-line))
        (forward-line))
      (cons goto-line (if (looking-at "-") 0 (max 0 (- column offset)))))))

;;;###autoload
(defun magit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in other window.  If there
is no file at point then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window
              (file-truename (or (magit-file-at-point) default-directory))))

;;;; Bisect

(magit-define-popup magit-bisect-popup
  "Popup console for bisect commands."
  'magit-popups
  :man-page "git-bisect"
  :actions  '((?b "Bad"   magit-bisect-bad)
              (?g "Good"  magit-bisect-good)
              (?k "Skip"  magit-bisect-skip)
              (?r "Reset" magit-bisect-reset)
              (?s "Start" magit-bisect-start)
              (?u "Run"   magit-bisect-run)))

;;;###autoload
(defun magit-bisect-start (bad good)
  "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a know
good and a bad commit.  To move the session forward use the
other actions from the bisect popup (\
\\<magit-status-mode-map>\\[magit-bisect-popup])."
  (interactive
   (if (magit-bisect-in-progress-p)
       (user-error "Already bisecting")
     (let ((b (magit-read-branch-or-commit "Start bisect with bad revision")))
       (list b (magit-read-other-branch-or-commit "Good revision" b)))))
  (magit-bisect-async "start" (list bad good) t))

;;;###autoload
(defun magit-bisect-reset ()
  "After bisecting cleanup bisection state and return to original HEAD."
  (interactive)
  (when (magit-confirm 'reset-bisect "Reset bisect")
    (magit-run-git "bisect" "reset")
    (ignore-errors (delete-file (magit-git-dir "BISECT_CMD_OUTPUT")))))

;;;###autoload
(defun magit-bisect-good ()
  "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question."
  (interactive)
  (magit-bisect-async "good"))

;;;###autoload
(defun magit-bisect-bad ()
  "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question."
  (interactive)
  (magit-bisect-async "bad"))

;;;###autoload
(defun magit-bisect-skip ()
  "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one."
  (interactive)
  (magit-bisect-async "skip"))

;;;###autoload
(defun magit-bisect-run (cmdline)
  "Bisect automatically by running commands after each step."
  (interactive (list (read-shell-command "Bisect shell command: ")))
  (magit-bisect-async "run" (list cmdline)))

(defun magit-bisect-async (subcommand &optional args no-assert)
  (unless (or no-assert (magit-bisect-in-progress-p))
    (user-error "Not bisecting"))
  (let ((file (magit-git-dir "BISECT_CMD_OUTPUT"))
        (default-directory (magit-get-top-dir)))
    (ignore-errors (delete-file file))
    (magit-run-git-with-logfile file "bisect" subcommand args)
    (magit-process-wait)
    (magit-refresh)))

(defun magit-bisect-in-progress-p ()
  (file-exists-p (magit-git-dir "BISECT_LOG")))

(defun magit-insert-bisect-output ()
  (when (magit-bisect-in-progress-p)
    (let ((lines
           (or (magit-file-lines (magit-git-dir "BISECT_CMD_OUTPUT"))
               (list "Bisecting: (no saved bisect output)"
                     "It appears you have invoked `git bisect' from a shell."
                     "There is nothing wrong with that, we just cannot display"
                     "anything useful here.  Consult the shell output instead.")))
          (done-re "^[a-z0-9]\\{40\\} is the first bad commit$"))
      (magit-insert-section (bisect-output t)
        (magit-insert-heading
          (propertize (or (and (string-match done-re (car lines)) (pop lines))
                          (--first (string-match done-re it) lines)
                          (pop lines))
                      'face 'magit-section-heading))
        (dolist (line lines)
          (insert line "\n"))))
    (insert "\n")))

(defun magit-insert-bisect-rest ()
  (when (magit-bisect-in-progress-p)
    (magit-insert-section (bisect-view)
      (magit-insert-heading "Bisect Rest:")
      (magit-git-wash (apply-partially 'magit-log-wash-log 'bisect-vis)
        "bisect" "visualize" "git" "log"
        "--pretty=format:%h%d %s" "--decorate=full"))))

(defun magit-insert-bisect-log ()
  (when (magit-bisect-in-progress-p)
    (magit-insert-section (bisect-log)
      (magit-insert-heading "Bisect Log:")
      (magit-git-wash #'magit-wash-bisect-log "bisect" "log"))))

(defun magit-wash-bisect-log (args)
  (let (beg)
    (while (progn (setq beg (point-marker))
                  (re-search-forward "^\\(git bisect [^\n]+\n\\)" nil t))
      (magit-bind-match-strings (heading) nil
        (magit-delete-match)
        (save-restriction
          (narrow-to-region beg (point))
          (goto-char (point-min))
          (magit-insert-section (bisect-log nil t)
            (magit-insert-heading heading)
            (magit-wash-sequence
             (apply-partially 'magit-log-wash-line 'bisect-log
                              (magit-abbrev-length)))
            (insert ?\n)))))
    (when (re-search-forward
           "# first bad commit: \\[\\([a-z0-9]\\{40\\}\\)\\] [^\n]+\n" nil t)
      (magit-bind-match-strings (hash) nil
        (magit-delete-match)
        (magit-insert-section (bisect-log)
          (magit-insert (concat hash " is the first bad commit\n")))))))

;;; Manipulate
;;;; Init

;;;###autoload
(defun magit-init (directory)
  "Create or reinitialize a Git repository.
Read directory name and initialize it as new Git repository.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside; or when
the directory is the root of the existing repository, whether
it should be reinitialized.

Non-interactively DIRECTORY is always (re-)initialized."
  (interactive
   (let* ((dir (file-name-as-directory
                (expand-file-name
                 (read-directory-name "Create repository in: "))))
          (top (magit-get-top-dir dir)))
     (if (and top
              (not (yes-or-no-p
                    (if (string-equal top dir)
                        (format "Reinitialize existing repository %s? " dir)
                      (format "%s is a repository.  Create another in %s? "
                              top dir)))))
         (user-error "Abort")
       (list dir))))
  (magit-run-git "init" (expand-file-name directory)))

;;;###autoload
(defun magit-clone (repository directory)
  "Clone the REPOSITORY to DIRECTORY."
  (interactive
   (let ((url (magit-read-string "Clone repository")))
     (list url (read-directory-name
                "Clone to: " nil nil nil
                (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                     (match-string 1 url))))))
  (magit-run-git "clone" repository directory))

;;;; Commit

(with-no-warnings ; quiet 24.3.50 byte-compiler
(magit-define-popup magit-commit-popup
  "Popup console for commit commands."
  'magit-popups
  :man-page "git-commit"
  :switches '((?a "Stage all modified and deleted files"   "--all")
              (?e "Allow empty commit"                     "--allow-empty")
              (?v "Show diff of changes to be committed"   "--verbose")
              (?n "Bypass git hooks"                       "--no-verify")
              (?s "Add Signed-off-by line"                 "--signoff")
              (?R "Claim authorship and reset author date" "--reset-author"))
  :options  '((?A "Override the author"  "--author="        read-from-minibuffer)
              (?S "Sign using gpg"       "--gpg-sign="      magit-read-gpg-secret-key)
              (?C "Reuse commit message" "--reuse-message=" read-from-minibuffer))
  :actions  '((?c "Commit"         magit-commit)
              (?e "Extend"         magit-commit-extend)
              (?f "Fixup"          magit-commit-fixup)
              (?F "Instant Fixup"  magit-commit-instant-fixup)
              (?a "Amend"          magit-commit-amend)
              (?r "Reword"         magit-commit-reword)
              (?s "Squash"         magit-commit-squash)
              (?S "Instant Squash" magit-commit-instant-squash))
  :max-action-columns 4
  :default-action 'magit-commit))

(defun magit-commit-log-buffer ()
  (let ((topdir (magit-get-top-dir)))
    (--first (equal topdir (with-current-buffer it
                             (and git-commit-mode (magit-get-top-dir))))
             (append (buffer-list (selected-frame))
                     (buffer-list)))))

(defadvice magit-commit-popup (around pop-to-ongoing activate)
  (--if-let (magit-commit-log-buffer) (switch-to-buffer it) ad-do-it))

;;;###autoload
(defun magit-commit (&optional args)
  "Create a new commit on HEAD.
With a prefix argument amend to the commit at HEAD instead.
\n(git commit [--amend] ARGS)"
  (interactive (if current-prefix-arg
                   (list (cons "--amend" magit-current-popup-args))
                 (list magit-current-popup-args)))
  (when (setq args (magit-commit-assert args))
    (magit-commit-async 'magit-diff-staged args)))

;;;###autoload
(defun magit-commit-amend (&optional args)
  "Amend the last commit.
\n(git commit --amend ARGS)"
  (interactive (list magit-current-popup-args))
  (magit-commit-async 'magit-diff-while-amending "--amend" args))

;;;###autoload
(defun magit-commit-extend (&optional args override-date)
  "Amend the last commit, without editing the message.
With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-extend-override-date' can be
used to inverse the meaning of the prefix argument.
\n(git commit --amend --no-edit)"
  (interactive (list magit-current-popup-args
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (when (setq args (magit-commit-assert args (not override-date)))
    (let ((process-environment process-environment))
      (unless override-date
        (setenv "GIT_COMMITTER_DATE" (magit-rev-format "%cd")))
      (magit-commit-async nil "--amend" "--no-edit" args))))

;;;###autoload
(defun magit-commit-reword (&optional args override-date)
  "Reword the last commit, ignoring staged changes.

With a prefix argument do change the committer date, otherwise
don't.  The option `magit-commit-rewrite-override-date' can be
used to inverse the meaning of the prefix argument.

Non-interactively respect the optional OVERRIDE-DATE argument
and ignore the option.
\n(git commit --amend --only)"
  (interactive (list magit-current-popup-args
                     (if current-prefix-arg
                         (not magit-commit-reword-override-date)
                       magit-commit-reword-override-date)))
  (let ((process-environment process-environment))
    (unless override-date
      (setenv "GIT_COMMITTER_DATE" (magit-rev-format "%cd")))
    (magit-commit-async 'magit-diff-while-amending
                        "--amend" "--only" args)))

;;;###autoload
(defun magit-commit-fixup (&optional commit args confirm)
  "Create a fixup commit.
With a prefix argument the target commit has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.
\n(git commit --no-edit --fixup=COMMIT [ARGS])"
  (interactive (magit-commit-squash-read-args))
  (magit-commit-squash-internal 'magit-commit-fixup "--fixup"
                                commit args confirm))

;;;###autoload
(defun magit-commit-squash (&optional commit args confirm)
  "Create a squash commit.
With a prefix argument the target commit has to be confirmed.
Otherwise the commit at point may be used without confirmation
depending on the value of option `magit-commit-squash-confirm'.
\n(git commit --no-edit --squash=COMMIT [ARGS])"
  (interactive (magit-commit-squash-read-args))
  (magit-commit-squash-internal 'magit-commit-squash "--squash"
                                commit args confirm))

;;;###autoload
(defun magit-commit-instant-fixup (&optional commit args)
  "Create a fixup commit and instantly rebase.
\n(git commit --no-edit --fixup=COMMIT ARGS;
 git rebase -i COMMIT^ --autosquash --autostash)"
  (interactive (list (magit-commit-at-point) magit-current-popup-args))
  (magit-commit-squash-internal
   (lambda (c a)
     (when (setq c (magit-commit-fixup c a))
       (magit-rebase-autosquash (concat c "^"))))
   "--fixup" commit args t))

;;;###autoload
(defun magit-commit-instant-squash (&optional commit args)
  "Create a squash commit and instantly rebase.
\n(git commit --no-edit --squash=COMMIT ARGS;
 git rebase -i COMMIT^ --autosquash --autostash)"
  (interactive (list (magit-commit-at-point) magit-current-popup-args))
  (magit-commit-squash-internal
   (lambda (c a)
     (when (setq c (magit-commit-squash c a))
       (magit-rebase-autosquash (concat c "^"))))
   "--squash" commit args t))

(defun magit-commit-squash-read-args ()
  (list (magit-commit-at-point) magit-current-popup-args
        (or current-prefix-arg magit-commit-squash-confirm)))

(defun magit-commit-squash-internal (fn option commit args confirm)
  (when (setq args (magit-commit-assert args t))
    (if (and commit (not confirm))
        (let ((magit-diff-auto-show nil))
          (magit-commit-async 'magit-diff-staged "--no-edit"
                              (concat option "=" commit) args)
          commit)
      (magit-log-select
        `(lambda (commit) (,fn commit (list ,@args))))
      (when (magit-diff-auto-show-p 'log-select)
        (let ((magit-diff-switch-buffer-function 'display-buffer))
          (magit-diff-staged))))))

(defun magit-commit-assert (args &optional strict)
  (cond
   ((or (magit-anything-staged-p)
        (and (magit-anything-unstaged-p)
             ;; ^ Everything of nothing is still nothing.
             (member "--all" args))
        (and (not strict)
             ;; ^ For amend variants that don't make sense otherwise.
             (or (member "--amend" args)
                 (member "--allow-empty" args))))
    (or args (list "--")))
   ((and (magit-rebase-in-progress-p)
         (y-or-n-p "Nothing staged.  Continue in-progress rebase? "))
    (magit-commit-async nil "--continue")
    nil)
   ((and (file-exists-p (magit-git-dir "MERGE_MSG"))
         (not (magit-anything-unstaged-p)))
    (or args (list "--")))
   (magit-commit-ask-to-stage
    (when (magit-diff-auto-show-p 'stage-all)
      (magit-diff-unstaged))
    (prog1 (when (y-or-n-p "Nothing staged.  Stage and commit everything? ")
             (magit-run-git "add" "-u" ".")
             (or args (list "--")))
      (when (and (magit-diff-auto-show-p 'stage-all)
                 (derived-mode-p 'magit-diff-mode))
        (magit-mode-quit-window))))
   (t
    (user-error "Nothing staged"))))

(defun magit-commit-async (diff-fn &rest args)
  (magit-server-visit-args 'commit t args)
  (when (and diff-fn (magit-diff-auto-show-p 'commit))
    (let ((magit-inhibit-save-previous-winconf t))
      (funcall diff-fn)))
  (apply #'magit-run-git-with-editor "commit" args))

(defun magit-server-visit ()
  (when (or (string-match-p git-commit-filename-regexp buffer-file-name)
            (string-match-p git-rebase-filename-regexp buffer-file-name))
    (let ((type     (nth 0 magit-server-visit-args))
          (otherwin (nth 1 magit-server-visit-args))
          (topdir   (nth 2 magit-server-visit-args))
          (winconf  (nth 3 magit-server-visit-args))
          (args     (nth 4 magit-server-visit-args)))
      (setq-local server-window (if otherwin 'pop-to-buffer 'switch-to-buffer))
      (when (equal (magit-get-top-dir) topdir)
        (setq with-editor-previous-winconf winconf)
        (setq magit-refresh-args args)))))

(add-hook 'server-visit-hook 'magit-server-visit t)
(add-hook 'with-editor-filter-visit-hook 'magit-server-visit t)

(defun magit-read-file-trace (ignored)
  (let ((file  (magit-read-file-from-rev "HEAD" "File"))
        (trace (magit-read-string "Trace")))
    (if (string-match
         "^\\(/.+/\\|:[^:]+\\|[0-9]+,[-+]?[0-9]+\\)\\(:\\)?$" trace)
        (concat trace (or (match-string 2 trace) ":") file)
      (user-error "Trace is invalid, see man git-log"))))

(defvar magit-gpg-secret-key-hist nil)

(defun magit-read-gpg-secret-key (prompt &optional initial-input)
  (require 'epa)
  (let ((keys (--map (list (epg-sub-key-id (car (epg-key-sub-key-list it)))
                           (-when-let (id-obj (car (epg-key-user-id-list it)))
                             (let    ((id-str (epg-user-id-string id-obj)))
                               (if (stringp id-str)
                                   id-str
                                 (epg-decode-dn id-obj)))))
                     (epg-list-keys (epg-make-context epa-protocol) nil t))))
    (magit-completing-read prompt keys nil nil nil 'magit-gpg-secret-key-hist
                           (car (or magit-gpg-secret-key-hist keys)))))

(defvar magit-commit-add-log-insert-function 'magit-commit-add-log-insert)

(defun magit-commit-add-log ()
  "Add a stub for the current hunk into the commit message buffer.
If no commit is in progress, then initiate it.  Use the function
specified by variable `magit-commit-add-log-insert-function' to
actually insert the entry."
  (interactive)
  (let ((log (magit-commit-log-buffer)) buf pos)
    (save-window-excursion
      (call-interactively #'magit-visit-file)
      (setq buf (current-buffer)
            pos (point)))
    (unless log
      (unless (magit-commit-assert nil)
        (user-error "Abort"))
      (magit-commit)
      (while (not (setq log (magit-commit-log-buffer)))
        (sit-for 0.01)))
    (save-excursion
      (with-current-buffer buf
        (goto-char pos)
        (funcall magit-commit-add-log-insert-function log
                 (file-relative-name buffer-file-name (magit-get-top-dir))
                 (add-log-current-defun))))))

(defun magit-commit-add-log-insert (buffer file defun)
  (with-current-buffer buffer
    (goto-char (point-min))
    (cond ((not (re-search-forward (format "^\\* %s" (regexp-quote file))
                                   nil t))
           ;; No entry for file, create it.
           (goto-char (point-max))
           (forward-comment -1000)
           (unless (or (bobp) (looking-back "\\(\\*[^\n]+\\|\n\\)"))
             (insert "\n"))
           (insert (format "\n* %s" file))
           (when defun
             (insert (format " (%s)" defun)))
           (insert ": "))
          (defun
           ;; found entry for file, look for defun
           (let ((limit (save-excursion
                          (or (and (re-search-forward "^\\* " nil t)
                                   (match-beginning 0))
                              (progn (goto-char (point-max))
                                     (forward-comment -1000)
                                     (point))))))
             (cond ((re-search-forward
                     (format "(.*\\_<%s\\_>.*):" (regexp-quote defun))
                     limit t)
                    ;; found it, goto end of current entry
                    (if (re-search-forward "^(" limit t)
                        (backward-char 2)
                      (goto-char limit)))
                   (t
                    ;; not found, insert new entry
                    (goto-char limit)
                    (if (bolp)
                        (open-line 1)
                      (newline))
                    (insert (format "(%s): " defun))))))
          (t
           ;; found entry for file, look for its beginning
           (when (looking-at ":")
             (forward-char 2))))))

;;;; Branch

(magit-define-popup magit-branch-popup
  "Popup console for branch commands."
  'magit-popups
  :man-page "git-branch"
  :switches '((?t "Set upstream configuration" "--track"))
  :actions  '((?b "Checkout"          magit-checkout)
              (?c "Create"            magit-branch)
              (?B "Create & Checkout" magit-branch-and-checkout)
              (?u "Set upstream"      magit-branch-set-upstream)
              (?r "Rename"            magit-branch-rename)
              (?k "Delete"            magit-branch-delete))
  :default-arguments '("--track")
  :default-action 'magit-checkout)

;;;###autoload
(defun magit-checkout (revision)
  "Checkout REVISION, updating the index and the working tree.
If REVISION is a local branch then that becomes the current
branch.  If it is something else then `HEAD' becomes detached.
Checkout fails if the working tree or the staging area contain
changes.
\n(git checkout REVISION)."
  (interactive (list (magit-read-other-branch-or-commit "Checkout")))
  (magit-run-git "checkout" revision))

(defun magit-branch (branch start-point &optional args)
  "Create BRANCH at branch or revision START-POINT.
\n(git branch [ARGS] BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create branch"))
  (magit-run-git "branch" args branch start-point))

;;;###autoload
(defun magit-branch-and-checkout (branch start-point &optional args)
  "Create and checkout BRANCH at branch or revision START-POINT.
\n(git checkout [ARGS] -b BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create and checkout branch"))
  (magit-run-git "checkout" args "-b" branch start-point))

(defun magit-branch-read-args (prompt)
  (let* ((args magit-current-popup-args)
         (start (magit-read-branch-or-commit (concat prompt " starting at")))
         (branch
          (magit-read-string
           "Branch name"
           (and (member start (magit-list-remote-branch-names))
                (mapconcat #'identity (cdr (split-string start "/")) "/")))))
    (when (and (member "--track" args)
               (not (magit-branch-p start)))
      (setq args (delete "--track" args)))
    (list branch start args)))

;;;###autoload
(defun magit-branch-delete (branch &optional force)
  "Delete BRANCH.
Without a prefix argument deleting a branch that hasn't been
merged will fail.  With a prefix argument the deletion is forced.
When BRANCH is the current branch offer to first detach HEAD or
checkout the \"master\" branch.
\n(git branch -d|-D BRANCH || git push REMOTE :BRANCH)."
  (interactive (list (magit-read-branch (if current-prefix-arg
                                            "Force delete branch"
                                          "Delete branch")
                                        (magit-get-previous-branch))
                     current-prefix-arg))
  (let ((ref (magit-ref-fullname branch)))
    (unless ref
      (error "%s cannot be resolved" branch))
    (if (string-match "^refs/remotes/\\([^/]+\\)/\\(.+\\)" ref)
        (magit-run-git-async "push"      (match-string 1 ref)
                             (concat ":" (match-string 2 ref)))
      (cl-case (when (equal ref (magit-ref-fullname (magit-get-current-branch)))
                 (let ((msg (format "Branch %s is checked out.  " branch)))
                   (if (equal ref "refs/heads/master")
                       (magit-read-char-case msg nil
                         (?d "[d]etach HEAD & delete" 'detach)
                         (?a "[a]bort"                'abort))
                     (magit-read-char-case msg nil
                       (?d "[d]etach HEAD & delete"     'detach)
                       (?c "[c]heckout master & delete" 'master)
                       (?a "[a]bort"                    'abort)))))
        (detach (setq force t) (magit-call-git "checkout" "--detach"))
        (master (setq force t) (magit-call-git "checkout" "master"))
        (abort  (user-error "Branch %s not deleted" branch)))
      (magit-run-git "branch" (if force "-D" "-d") branch))))

;;;###autoload
(defun magit-branch-set-upstream (branch upstream)
  "Change the UPSTREAM branch of BRANCH."
  (interactive
   (let ((b (magit-read-local-branch "Change upstream of branch")))
     (list b (magit-read-other-branch "Change upstream to branch" b))))
  (if upstream
      (magit-run-git "branch" (concat "--set-upstream-to=" upstream) branch)
    (magit-run-git "branch" "--unset-upstream" branch)))

;;;###autoload
(defun magit-request-pull (url start)
  (interactive
   (let* ((remote (magit-read-remote "Remote"))
          (branch (magit-read-remote-branch "Branch" remote)))
     (list (magit-get "remote" remote "url")
           (magit-get-tracked-branch))))
  (let ((dir default-directory))
    ;; mu4e changes default-directory
    (compose-mail)
    (setq default-directory dir))
  (message-goto-body)
  (process-file magit-git-executable nil t nil "request-pull" start url)
  (set-buffer-modified-p nil))

;;;###autoload
(defun magit-branch-rename (old new &optional force)
  "Rename branch OLD to NEW.
With prefix, forces the rename even if NEW already exists.
\n(git branch -m|-M OLD NEW)."
  (interactive
   (let ((branch (magit-read-local-branch "Rename branch")))
     (list branch
           (magit-read-string (format "Rename branch '%s' to" branch))
           current-prefix-arg)))
  (unless (string= old new)
    (magit-run-git "branch" (if force "-M" "-m") old new)))

;;;###autoload
(defun magit-branch-edit-description (branch)
  "Edit the description of BRANCH."
  (interactive (list (magit-read-local-branch "Edit branch description")))
  (magit-run-git-with-editor "branch" "--edit-description"))

(defun magit-insert-branch-description ()
  (let ((branch (magit-get-current-branch)))
    (--when-let (magit-git-lines
                 "config" (format "branch.%s.description" branch))
      (magit-insert-section (branchdesc branch t)
        (magit-insert-heading branch ": " (car it))
        (insert (mapconcat 'identity (cdr it) "\n"))
        (insert "\n\n")))))

;;;; Merge

(magit-define-popup magit-merge-popup
  "Popup console for merge commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-merge"
  :switches '((?f "Fast-forward only" "--ff-only")
              (?n "No fast-forward"   "--no-ff")
              (?s "Squash"            "--squash"))
  :options  '((?s "Strategy" "--strategy=" read-from-minibuffer))
  :actions  '((?m "Merge"                  magit-merge)
              (?e "Merge and edit message" magit-merge-editmsg)
              (?n "Merge but don't commit" magit-merge-nocommit))
  :sequence-actions   '((?c "Commit merge" magit-commit)
                        (?a "Abort merge"  magit-merge-abort))
  :sequence-predicate 'magit-merge-state
  :default-action 'magit-merge)

;;;###autoload
(defun magit-merge (rev &optional args nocommit)
  "Merge commit REV into the current branch; using default message.

Unless there are conflicts or a prefix argument is used create a
merge commit using a generic commit message and without letting
the user inspect the result.  With a prefix argument pretend the
merge failed to give the user the opportunity to inspect the
merge.

\(git merge --no-edit|--no-commit [ARGS] REV)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     magit-current-popup-args
                     current-prefix-arg))
  (magit-merge-assert)
  (magit-run-git "merge" (if nocommit "--no-commit" "--no-edit") args rev))

;;;###autoload
(defun magit-merge-editmsg (rev &optional args)
  "Merge commit REV into the current branch; and edit message.
Perform the merge and prepare a commit message but let the user
edit it.
\n(git merge --edit [ARGS] rev)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     magit-current-popup-args))
  (magit-merge-assert)
  (magit-run-git-with-editor "merge" "--edit" args rev))

;;;###autoload
(defun magit-merge-nocommit (rev &optional args)
  "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.
\n(git merge --no-commit [ARGS] rev)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     magit-current-popup-args))
  (magit-merge-assert)
  (magit-run-git "merge" "--no-commit" args rev))

;;;###autoload
(defun magit-merge-abort ()
  "Abort the current merge operation.
\n(git merge --abort)"
  (interactive)
  (if (file-exists-p (magit-git-dir "MERGE_HEAD"))
      (when (magit-confirm 'abort-merge "Abort merge")
        (magit-run-git-async "merge" "--abort"))
    (user-error "No merge in progress")))

(defun magit-checkout-stage (file arg &optional restore-conflict)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((default-directory (magit-get-top-dir))
         (file (magit-completing-read "Checkout file"
                                      (magit-tracked-files) nil nil nil
                                      'magit-read-file-hist
                                      (magit-current-file))))
     (cond
      ((member file (magit-unmerged-files))
       (list file (magit-checkout-read-stage file)))
      ((yes-or-no-p (format "Restore conflicts in %s? " file))
       (list file "--merge" t))
      (t
       (user-error "Quit")))))
  (if restore-conflict
      (progn
        (with-temp-buffer
          (insert "0 0000000000000000000000000000000000000000\t" file "\n")
          (--> (magit-git-string "ls-tree" (magit-git-string
                                            "merge-base" "MERGE_HEAD" "HEAD")
                                 file)
            (replace-regexp-in-string "\t" " 1\t" it)
            (insert it "\n"))
          (--> (magit-git-string "ls-tree" "HEAD" file)
            (replace-regexp-in-string "\t" " 2\t" it)
            (insert it "\n"))
          (--> (magit-git-string "ls-tree" "MERGE_HEAD" file)
            (replace-regexp-in-string "\t" " 3\t" it)
            (insert it "\n"))
          (magit-run-git-with-input nil "checkout" arg file))
        (magit-refresh))
    (magit-call-git "checkout" arg file)
    (if (string= arg "--merge")
        (magit-refresh)
      (magit-run-git "add" file))))

(defun magit-merge-state ()
  (file-exists-p (magit-git-dir "MERGE_HEAD")))

(defun magit-merge-assert ()
  (or (not (magit-anything-modified-p))
      (not magit-merge-warn-dirty-worktree)
      (magit-confirm
       'merge-dirty
       "Running merge in a dirty worktree could cause data loss.  Continue")
      (user-error "Abort")))

(defun magit-checkout-read-stage (file)
  (magit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

(defun magit-insert-merge-log ()
  (-when-let (heads (mapcar 'magit-get-shortname
                            (magit-file-lines (magit-git-dir "MERGE_HEAD"))))
    (magit-insert-section (commit (car heads))
      (magit-insert-heading
        (format "Merging %s:" (mapconcat 'identity heads ", ")))
      (magit-insert-log
       (concat (magit-git-string "merge-base" "--octopus" "HEAD" (car heads))
               ".." (car heads))
       (let ((args magit-log-section-args))
         (unless (member "--decorate=full" magit-log-section-args)
           (push "--decorate=full" args))
         args)))))

;;;; Rebase

(magit-define-popup magit-rebase-popup
  "Key menu for rebasing."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-rebase"
  :switches '((?k "Keep empty commits" "--keep-empty")
              (?p "Preserve merges" "--preserve-merges")
              (?c "Lie about author date" "--committer-date-is-author-date")
              (?a "Autosquash" "--autosquash")
              (?A "Autostash" "--autostash"))
  :actions  '((?r "Rebase"      magit-rebase)
              (?o "Rebase onto" magit-rebase-onto)
              (?e "Interactive" magit-rebase-interactive)
              (?f "Autosquash"  magit-rebase-autosquash))
  :sequence-actions '((?r "Continue" magit-rebase-continue)
                      (?s "Skip"     magit-rebase-skip)
                      (?e "Edit"     magit-rebase-edit)
                      (?a "Abort"    magit-rebase-abort))
  :sequence-predicate 'magit-rebase-in-progress-p)

;;;###autoload
(defun magit-rebase (upstream &optional args)
  "Start an non-interactive rebase operation.
\n(git rebase UPSTREAM[^] [ARGS])"
  (interactive (if (magit-rebase-in-progress-p)
                   (list nil)
                 (list (magit-read-other-branch-or-commit
                        "Rebase to"
                        (magit-get-current-branch)
                        (magit-get-tracked-branch))
                       magit-current-popup-args)))
  (if upstream
      (progn (message "Rebasing...")
             (magit-rebase-async upstream args)
             (message "Rebasing...done"))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase (concat commit "^") (list ,@args))))))

;;;###autoload
(defun magit-rebase-onto (newbase upstream &optional args)
  "Start an non-interactive rebase operation, using `--onto'.
\n(git rebase --onto NEWBASE UPSTREAM[^] [ARGS])"
  (interactive (list (magit-read-other-branch-or-commit
                      "Rebase to"
                      (magit-get-current-branch)
                      (magit-get-tracked-branch))
                     nil))
  (if upstream
      (progn (message "Rebasing...")
             (magit-rebase-async "--onto" newbase upstream args)
             (message "Rebasing...done"))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-onto ,newbase (concat commit "^") ,args)))))

;;;###autoload
(defun magit-rebase-interactive (commit &optional args)
  "Start an interactive rebase operation.
\n(git rebase -i COMMIT[^] [ARGS])"
  (interactive (let ((commit (magit-commit-at-point)))
                 (list (and commit (concat commit "^"))
                       magit-current-popup-args)))
  (cond
   ((magit-rebase-in-progress-p)
    (magit-rebase-popup))
   ((setq commit (magit-rebase-interactive-assert commit))
    (magit-rebase-async "-i" commit args))
   (t
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-interactive (concat commit "^") (list ,@args)))))))

;;;###autoload
(defun magit-rebase-autosquash (commit &optional args)
  "Combine squash and fixup commits with their intended targets.
\n(git rebase -i COMMIT[^] --autosquash --autostash [ARGS])"
  (interactive (list (magit-get-tracked-branch) magit-current-popup-args))
  (if (setq commit (magit-rebase-interactive-assert commit))
      (let ((process-environment process-environment))
        (setenv "GIT_SEQUENCE_EDITOR" magit-success-executable)
        (magit-rebase-async "-i" commit "--autosquash" "--autostash" args))
    (magit-log-select
      `(lambda (commit)
         (magit-rebase-autosquash (concat commit "^") (list ,@args))))))

;;;###autoload
(defun magit-rebase-continue ()
  "Restart the current rebasing operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (if (magit-anything-unstaged-p)
          (error "Cannot continue rebase with unstaged changes")
        (magit-rebase-async "--continue"))
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-skip ()
  "Skip the current commit and restart the current rebase operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-rebase-async "--skip")
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-edit ()
  "Edit the todo list of the current rebase operation."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-rebase-async "--edit-todo")
    (user-error "No rebase in progress")))

;;;###autoload
(defun magit-rebase-abort ()
  "Abort the current rebase operation, restoring the original branch."
  (interactive)
  (if (magit-rebase-in-progress-p)
      (magit-run-git "rebase" "--abort")
    (user-error "No rebase in progress")))

(defun magit-rebase-async (&rest args)
  (apply #'magit-run-git-sequencer 'rebase "rebase" args)
  (set-process-sentinel magit-this-process #'magit-rebase-process-sentinel))

(defun magit-rebase-process-sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (magit-process-sentinel process event)
    (--when-let (magit-mode-get-buffer
                 magit-status-buffer-name-format
                 'magit-status-mode)
      (with-current-buffer it
        (--when-let (magit-get-section
                     `((commit . ,(magit-rebase-stopped-commit))
                       (rebase-sequence)
                       (status)))
          (goto-char (magit-section-start it)))))))

(defun magit-rebase-stopped-commit ()
  (let ((file (magit-git-dir "rebase-merge/done")))
    (when (file-exists-p file)
      (cadr (split-string (car (last (magit-file-lines file))))))))

(defun magit-rebase-interactive-assert (commit)
  (when commit
    (if (magit-git-lines "rev-list" "--merges" (concat commit "..HEAD"))
        (magit-read-char-case "Proceed despite merge in rebase range?  " nil
          (?c "[c]ontinue" commit)
          (?s "[s]elect other" nil)
          (?a "[a]bort" (user-error "Quit")))
      commit)))

(defun magit-rebase-in-progress-p ()
  "Return t if a rebase is in progress."
  (or (file-exists-p (magit-git-dir "rebase-merge"))
      (file-exists-p (magit-git-dir "rebase-apply/onto"))))

(defun magit-insert-rebase-sequence ()
  (when (magit-rebase-in-progress-p)
    (let ((interactive (file-directory-p (magit-git-dir "rebase-merge"))))
      (magit-insert-section (rebase-sequence)
        (magit-insert-heading
          (if interactive
              (format "Rebasing %s:"
                      (-> "rebase-merge/head-name"
                        magit-git-dir magit-file-line magit-get-shortname))
            (format "Rebasing %s onto %s:"
                    (-> "rebase-apply/head-name"
                      magit-git-dir magit-file-line magit-get-shortname)
                    (-> "rebase-apply/onto"
                      magit-git-dir magit-file-line magit-get-shortname))))
        (if interactive
            (progn
              (dolist (line (nreverse
                             (magit-file-lines
                              (magit-git-dir "rebase-merge/git-rebase-todo"))))
                (when (string-match "^\\([^# ]+\\) \\([^ ]+\\) \\(.*\\)$" line)
                  (magit-bind-match-strings (cmd hash msg) line
                    (magit-insert-section (commit hash)
                      (insert cmd " " (magit-format-rev-summary hash) ?\n)))))
              (-when-let (stop (magit-rebase-stopped-commit))
                (magit-insert-section (commit stop)
                  (insert "stop " (magit-format-rev-summary stop) ?\n))))
          (magit-insert-rebase-apply-sequence))
        (let ((onto (magit-file-line
                     (magit-git-dir (if interactive
                                        "rebase-merge/onto"
                                      "rebase-apply/onto")))))
          (dolist (hash (magit-git-lines "log" "--format=%H"
                                         (concat onto "..HEAD")))
            (magit-insert-section (commit hash)
              (insert "done " (magit-format-rev-summary hash) ?\n)))
          (magit-insert-section (commit onto)
            (insert "onto " (magit-format-rev-summary onto) ?\n)))
        (insert ?\n)))))

(defun magit-insert-rebase-apply-sequence ()
  (let* ((files (nreverse (directory-files (magit-git-dir "rebase-apply")
                                           t "^[0-9]\\{4\\}$")))
         (stop (car (last files))))
    (dolist (file files)
      (let (hash)
        (with-temp-buffer
          (insert-file-contents file)
          (re-search-forward "^From \\([^ ]+\\)")
          (setq hash (match-string 1)))
        (magit-insert-section (commit hash)
          (insert (if (eq file stop) "stop " "pick ")
                  (magit-format-rev-summary hash) ?\n))))))

;;;; Pick & Revert

(magit-define-popup magit-cherry-pick-popup
  "Popup console for cherry-pick commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-cherry-pick"
  :switches '((?s "Add Signed-off-by lines"            "--signoff")
              (?e "Edit commit messages"               "--edit")
              (?x "Reference cherry in commit message" "-x")
              (?F "Attempt fast-forward"               "--ff")
              (?m "Reply merge relative to parent"     "--mainline="))
  :options  '((?s "Strategy" "--strategy=" read-from-minibuffer))
  :actions  '((?A "Cherry Pick"  magit-cherry-pick)
              (?a "Cherry Apply" magit-cherry-apply))
  :sequence-actions '((?A "Continue" magit-sequencer-continue)
                      (?s "Skip"     magit-sequencer-skip)
                      (?a "Abort"    magit-sequencer-abort))
  :sequence-predicate 'magit-sequencer-in-progress-p
  :default-arguments '("--ff"))

;;;###autoload
(defun magit-cherry-pick (commit &optional args)
  (interactive (magit-sequencer-read-args 'cherry-pick "Cherry-pick"))
  (magit-assert-one-parent (car (if (listp commit)
                                    commit
                                  (split-string commit "\\.\\.")))
                           "cherry-pick")
  (magit-run-git-sequencer "cherry-pick" args commit))

;;;###autoload
(defun magit-cherry-apply (commit &optional args)
  (interactive (magit-sequencer-read-args 'cherry-pick "Apply commit"))
  (magit-assert-one-parent commit "cherry-pick")
  (magit-run-git-sequencer "cherry-pick" "--no-commit" args commit))


(magit-define-popup magit-revert-popup
  "Popup console for revert commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-revert"
  :switches '((?s "Add Signed-off-by lines" "--signoff"))
  :options  '((?s "Strategy" "--strategy="  read-from-minibuffer))
  :actions  '((?V "Revert commit(s)" magit-revert)
              (?v "Revert changes"   magit-revert-no-commit))
  :sequence-actions '((?V "Continue" magit-sequencer-continue)
                      (?s "Skip"     magit-sequencer-skip)
                      (?a "Abort"    magit-sequencer-abort))
  :sequence-predicate 'magit-sequencer-in-progress-p)

;;;###autoload
(defun magit-revert (commit &optional args)
  (interactive (magit-sequencer-read-args 'revert "Revert commit"))
  (magit-assert-one-parent commit "revert")
  (magit-run-git "revert" args commit))

;;;###autoload
(defun magit-revert-no-commit (commit &optional args)
  (interactive (magit-sequencer-read-args 'revert "Revert changes"))
  (magit-assert-one-parent commit "revert")
  (magit-run-git "revert" "--no-commit" args commit))

;;;###autoload
(defun magit-sequencer-continue ()
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (if (magit-anything-unstaged-p)
          (error "Cannot continue due to unstaged changes")
        (magit-run-git-sequencer
         (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--continue"))
    (error "No cherry-pick or revert in progress")))

;;;###autoload
(defun magit-sequencer-skip ()
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (magit-run-git-sequencer
       (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--skip")
    (error "No cherry-pick or revert in progress")))

;;;###autoload
(defun magit-sequencer-abort ()
  (interactive)
  (if (magit-sequencer-in-progress-p)
      (magit-run-git-sequencer
       (if (magit-revert-in-progress-p) "revert" "cherry-pick") "--abort")
    (error "No cherry-pick or revert in progress")))

(defun magit-sequencer-read-args (command prompt)
  (let ((selection (magit-current-sections 'commit)))
    (list (if (or current-prefix-arg (not selection))
              (if (eq command 'cherry-pick)
                  (magit-read-other-branch-or-commit prompt)
                (magit-read-branch-or-commit prompt))
            (setq selection (mapcar 'magit-section-value selection))
            (if (eq command 'cherry-pick)
                (nreverse selection)
              selection))
          magit-current-popup-args)))

(defun magit-sequencer-in-progress-p ()
  (or (magit-cherry-pick-in-progress-p)
      (magit-revert-in-progress-p)))

(defun magit-cherry-pick-in-progress-p ()
  (file-regular-p (magit-git-dir "CHERRY_PICK_HEAD")))

(defun magit-revert-in-progress-p ()
  (file-regular-p (magit-git-dir "REVERT_HEAD")))

(defun magit-insert-sequencer-sequence ()
  (-when-let
      (heading (or (and (magit-cherry-pick-in-progress-p) "Cherry Picking:")
                   (and (magit-revert-in-progress-p)      "Reverting:")))
    (magit-insert-section (sequence)
      (magit-insert-heading heading)
      (let* ((lines (nreverse
                     (magit-file-lines (magit-git-dir "sequencer/todo"))))
             (stop (car (last lines))))
        (dolist (line lines)
          (when (string-match "^pick \\([^ ]+\\) \\(.*\\)$" line)
            (magit-bind-match-strings (hash msg) line
              (magit-insert-section (commit hash)
                (insert (if (eq line stop) "stop " "pick ")
                        (propertize hash 'face 'magit-hash))
                (insert " " msg "\n"))))))
      (insert "\n"))))

;;;; Patch

(magit-define-popup magit-am-popup
  "Popup console for mailbox commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-am"
  :switches '((?3 "Fall back on 3way merge"           "--3way")
              (?s "Add Signed-off-by lines"           "--signoff")
              (?c "Remove text before scissors line"  "--scissors")
              (?k "Inhibit removal of email cruft"    "--keep")
              (?b "Limit removal of email cruft"      "--keep-non-patch")
              (?d "Use author date as committer date"
                  "--committer-date-is-author-date")
              (?D "Use committer date as author date" "--ignore-date"))
  :options  '((?p "Remove leading slashes from paths" "-p" read-number))
  :actions  '((?w "Apply patches" magit-am-apply-patches)
              (?m "Apply maildir" magit-am-apply-maildir))
  :default-arguments '("--3way")
  :default-actions 'magit-am-apply-patches
  :sequence-actions '((?w "Continue" magit-am-continue)
                      (?s "Skip"     magit-am-skip)
                      (?a "Abort"    magit-am-abort))
  :sequence-predicate 'magit-am-in-progress-p)

;;;###autoload
(defun magit-am-apply-patches (&optional files args)
  (interactive
   (let ((selection (magit-current-sections 'file)))
     (list (if (or current-prefix-arg (not selection))
               (list (read-file-name "Apply patch(es): "
                                     nil (car (last selection))))
             (nreverse (mapcar 'magit-section-value selection)))
           magit-current-popup-args)))
  (magit-run-git-sequencer "am" args "--" (mapcar 'expand-file-name files)))

;;;###autoload
(defun magit-am-apply-maildir (&optional maildir args)
  (interactive (list (read-file-name "Apply mbox or Maildir: ")
                     magit-current-popup-args))
  (magit-run-git-sequencer "am" args (expand-file-name maildir)))

;;;###autoload
(defun magit-am-continue ()
  (interactive)
  (if (magit-am-in-progress-p)
      (if (magit-anything-unstaged-p)
          (error "Cannot continue due to unstaged changes")
        (magit-run-git-sequencer "am" "--continue"))
    (user-error "Not applying any patches")))

;;;###autoload
(defun magit-am-skip ()
  (interactive)
  (if (magit-am-in-progress-p)
      (magit-run-git-sequencer "am" "--skip")
    (user-error "Not applying any patches")))

;;;###autoload
(defun magit-am-abort ()
  (interactive)
  (if (magit-am-in-progress-p)
      (magit-run-git "am" "--abort")
    (user-error "Not applying any patches")))

(defun magit-am-in-progress-p ()
  (file-exists-p (magit-git-dir "rebase-apply/applying")))

(defun magit-insert-am-sequence ()
  (when (file-exists-p (magit-git-dir "rebase-apply/applying"))
    (let (msg file hash)
      (magit-insert-section (sequence)
        (magit-insert-heading "Applying patches:")
        (magit-insert-rebase-apply-sequence)
        (insert ?\n)))))

;;;; Reset

;;;###autoload
(defun magit-reset-index (commit)
  "Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT revers to the
head this effectivley unstages all changes.
\n(git reset --mixed COMMIT)"
  (interactive (list (magit-read-branch-or-commit "Reset index to")))
  (magit-run-git "reset" commit "--"))

;;;###autoload
(defun magit-reset (commit &optional hard)
  "Reset the head and index to COMMIT, but not the working tree.
With a prefix argument also reset the working tree.
\n(git reset --mixed|--hard COMMIT)"
  (interactive (list (magit-read-branch-or-commit
                      (if current-prefix-arg
                          "Hard reset to"
                        "Reset head to"))
                     current-prefix-arg))
  (unless hard
    (magit-maybe-save-head-message commit))
  (magit-run-git "reset" (if hard "--hard" "--mixed") commit))

;;;###autoload
(defun magit-reset-head (commit)
  "Reset the head and index to COMMIT, but not the working tree.
\n(git reset --mixed COMMIT)"
  (interactive (list (magit-read-branch-or-commit "Reset head to")))
  (magit-maybe-save-head-message commit)
  (magit-run-git "reset" "--mixed" commit))

;;;###autoload
(defun magit-reset-soft (commit)
  "Reset the head to COMMIT, but not the index and working tree.
\n(git reset --soft REVISION)"
  (interactive (list (magit-read-branch-or-commit "Soft reset to")))
  (magit-maybe-save-head-message commit)
  (magit-run-git "reset" "--soft" commit))

;;;###autoload
(defun magit-reset-hard (commit)
  "Reset the head, index, and working tree to COMMIT.
\n(git reset --hard REVISION)"
  (interactive (list (magit-read-branch-or-commit "Hard reset to")))
  (magit-run-git "reset" "--hard" commit))

(defun magit-maybe-save-head-message (commit)
  (when (equal (magit-rev-parse commit)
               (magit-rev-parse "HEAD~"))
    (with-temp-buffer
      (magit-git-insert "log" "-1" "--format=%B" "HEAD")
      (when git-commit-major-mode
        (funcall git-commit-major-mode))
      (git-commit-setup-font-lock)
      (git-commit-save-message))))

;;; Transfer
;;;; Remotes

(magit-define-popup magit-remote-popup
  "Popup console for remote commands."
  'magit-popups
  :man-page "git-remote"
  :actions  '((?a "Add"    magit-remote-add)
              (?r "Rename" magit-remote-rename)
              (?k "Remove" magit-remote-remove)))

;;;###autoload
(defun magit-remote-add (remote url)
  "Add the REMOTE and fetch it.
\n(git remote add -f REMOTE URL)."
  (interactive (list (magit-read-string "Remote name")
                     (magit-read-string "Remote url")))
  (magit-run-git-async "remote" "add" "-f" remote url))

;;;###autoload
(defun magit-remote-remove (remote)
  "Delete the REMOTE.
\n(git remote rm REMOTE)."
  (interactive (list (magit-read-remote "Delete remote")))
  (magit-run-git "remote" "rm" remote))

;;;###autoload
(defun magit-remote-rename (old new)
  "Rename remote OLD to NEW.
\n(git remote rename OLD NEW)."
  (interactive
   (let  ((remote (magit-read-remote "Rename remote")))
     (list remote (magit-read-string (format "Rename remote '%s' to" remote)))))
  (unless (string= old new)
    (magit-run-git "remote" "rename" old new)))

;;;; Fetch

(magit-define-popup magit-fetch-popup
  "Popup console for fetch commands."
  'magit-popups
  :man-page "git-fetch"
  :switches '((?p "Prune"   "--prune"))
  :actions  '((?f "Current" magit-fetch-current)
              (?o "Other"   magit-fetch)
              (?a "All"     magit-remote-update))
  :default-action 'magit-fetch-current)

;;;###autoload
(defun magit-fetch (remote &optional args)
  "Fetch from REMOTE."
  (interactive (list (magit-read-remote "Fetch remote")
                     magit-current-popup-args))
  (magit-run-git-async "fetch" remote args))

;;;###autoload
(defun magit-fetch-current (&optional args)
  "Fetch for the default remote.
If there is no default remote, ask for one."
  (interactive (list magit-current-popup-args))
  (magit-fetch (or (magit-get-current-remote)
                   (magit-read-remote "Fetch remote"))
               args))

;;;###autoload
(defun magit-remote-update ()
  "Update all remotes."
  (interactive)
  (magit-run-git-async "remote" "update" magit-current-popup-args))

;;;; Pull

(magit-define-popup magit-pull-popup
  "Popup console for pull commands."
  'magit-popups
  :man-page "git-pull"
  :switches '((?f "Force"  "--force")
              (?r "Rebase" "--rebase"))
  :actions  '((?F "Pull"   magit-pull))
  :default-action 'magit-pull)

;;;###autoload
(defun magit-pull ()
  "Pull changes from a remote repository.

If there is no default remote, the user is prompted for one and
the choosen values is saved.  If there is no default merge
branch, the user is prompted for one and the choosen values is
saved.

With a prefix argument, the default remote is not used and the
user is prompted for a remote.  With two prefix arguments, the
default merge branch is not used and the user is prompted for
a merge branch.  Values entered by the user because of prefix
arguments are not saved."
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (branch-remote (magit-get-current-remote))
         (branch-merge (magit-get "branch" branch "merge"))
         (branch-merge-name (and branch-merge
                                 (save-match-data
                                   (string-match "^refs/heads/\\(.+\\)"
                                                 branch-merge)
                                   (match-string 1 branch-merge))))
         (choose-remote (>= (prefix-numeric-value current-prefix-arg) 4))
         (choose-branch (>= (prefix-numeric-value current-prefix-arg) 16))
         (remote-needed (or choose-remote
                            (not branch-remote)))
         (branch-needed (or choose-branch
                            (not branch-merge-name)))
         (chosen-branch-remote
          (if remote-needed
              (magit-read-remote "Pull from remote" branch-remote)
            branch-remote))
         (chosen-branch-merge-name
          (if branch-needed
              (magit-read-remote-branch (format "Pull branch from remote %s"
                                                chosen-branch-remote)
                                        chosen-branch-remote)
            branch-merge-name)))
    (when (and (not branch-remote)
               (not choose-remote))
      (magit-set chosen-branch-remote "branch" branch "remote"))
    (when (and (not branch-merge-name)
               (not choose-branch))
      (magit-set (format "%s" chosen-branch-merge-name)
                 "branch" branch "merge"))
    (magit-run-git-async
     "pull" magit-current-popup-args
     (and choose-remote chosen-branch-remote)
     (and (or choose-remote choose-branch)
          (list (format "refs/heads/%s:refs/remotes/%s/%s"
                        chosen-branch-merge-name
                        chosen-branch-remote
                        chosen-branch-merge-name))))))

;;;; Push

(magit-define-popup magit-push-popup
  "Popup console for push commands."
  'magit-popups
  :man-page "git-push"
  :switches '((?f "Force"         "--force")
              (?h "Disable hooks" "--no-verify")
              (?d "Dry run"       "-n")
              (?u "Set upstream"  "-u"))
  :actions  '((?P "Push"          magit-push)
              (?t "Push tags"     magit-push-tags))
  :default-action 'magit-push)

;;;###autoload
(defun magit-push-tags (remote &optional args)
  "Push all tags to a remote repository.
If only one remote exists, push to that.  Otherwise prompt for a
remote, offering the remote configured for the current branch as
default."
  (interactive (let ((remotes (magit-list-remotes)))
                 (list (if (= (length remotes) 1)
                           (car remotes)
                         (magit-read-remote "Push tags to remote"))
                       magit-current-popup-args)))
  (magit-run-git-async "push" remote "--tags" args))

;;;###autoload
(defun magit-push (arg)
  "Push the current branch to a remote repository.

With a single prefix argument ask the user what remote to push
to.  With two or more prefix arguments also ask the user the
name of the remote branch to push to.

Otherwise use the remote and branch as configured using the
Git variables `branch.<name>.remote' and `branch.<name>.merge'.
If the former is undefined ask the user.  If the latter is
undefined push without specifing the remote branch explicitly.

Also see option `magit-set-upstream-on-push'."
  (interactive "P")
  (let* ((branch (or (magit-get-current-branch)
                     (user-error "Don't push a detached head.  That's gross")))
         (auto-remote (magit-get-current-remote))
         (used-remote (if (or arg (not auto-remote))
                          (magit-read-remote
                           (format "Push %s to remote" branch) auto-remote)
                        auto-remote))
         (auto-branch (and (equal used-remote auto-remote)
                           (magit-get "branch" branch "merge")))
         (used-branch (if (>= (prefix-numeric-value arg) 16)
                          (magit-read-remote-branch
                           (format "Push %s as branch" branch)
                           used-remote auto-branch)
                        auto-branch)))
    (cond ;; Pushing to what's already configured.
          ((and auto-branch
                (equal auto-branch used-branch)
                (equal auto-remote used-remote)))
          ;; Setting upstream because of magit-current-popup-args.
          ((member "-u" magit-current-popup-args))
          ;; Two prefix arguments; ignore magit-set-upstream-on-push.
          ((>= (prefix-numeric-value arg) 16)
           (and (yes-or-no-p "Set upstream while pushing? ")
                (setq magit-current-popup-args
                      (cons "-u" magit-current-popup-args))))
          ;; Else honor magit-set-upstream-on-push.
          ((eq magit-set-upstream-on-push 'refuse)
           (user-error "Not pushing since no upstream has been set."))
          ((or (eq magit-set-upstream-on-push 'dontask)
               (and (eq magit-set-upstream-on-push t)
                    (yes-or-no-p "Set upstream while pushing? ")))
           (setq magit-current-popup-args (cons "-u" magit-current-popup-args))))
    (magit-run-git-async
     "push" "-v" used-remote
     (if used-branch (format "%s:%s" branch used-branch) branch)
     magit-current-popup-args)))

;;; Miscellaneous
;;;; Tag

(magit-define-popup magit-tag-popup
  "Popup console for tag commands."
  'magit-popups
  :man-page "git-tag"
  :switches '((?a "Annotate" "--annotate")
              (?s "Sign"     "--sign")
              (?f "Force"    "--force"))
  :actions  '((?t "Create"   magit-tag)
              (?k "Delete"   magit-tag-delete)
              (?p "Prune"    magit-tag-prune))
  :default-action 'magit-tag)

;;;###autoload
(defun magit-tag (name rev &optional args)
  "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\n(git tag [--annotate] NAME REV)"
  (interactive (list (magit-read-tag "Tag name")
                     (magit-read-branch-or-commit "Place tag on")
                     (let ((args magit-current-popup-args))
                       (when current-prefix-arg
                         (add-to-list 'args "--annotate"))
                       args)))
  (magit-run-git-with-editor "tag" args name rev))

;;;###autoload
(defun magit-tag-delete (tags)
  "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.
\n(git tag -d TAGS)"
  (interactive
   (let ((tags (magit-current-sections 'tag)))
     (if (> (length tags) 1)
         (if (magit-confirm 'delete-tags "Delete %i tags" tags)
             (list tags)
           (user-error "Abort"))
       (list (magit-read-tag "Delete tag" t)))))
  (magit-run-git "tag" "-d" tags))

(defun magit-tag-prune (tags remote-tags remote)
  "Offer to delete tags missing locally from REMOTE, and vice versa."
  (interactive
   (let* ((remote (magit-read-remote "Prune tags using remote"))
          (tags   (magit-list-tags))
          (rtags  (prog2 (message "Determining remote tags...")
                      (magit-list-remote-tags remote)
                    (message "Determining remote tags...done")))
          (ltags  (-difference tags rtags))
          (rtags  (-difference rtags tags)))
     (unless (or ltags rtags)
       (message "Same tags exist locally and remotely"))
     (when ltags
       (unless (if (> (length ltags) 1)
                   (magit-confirm t "Delete %i tags from remote" ltags)
                 (magit-confirm t (format "Delete %s from remote" (car ltags))))
         (setq ltags nil)))
     (when rtags
       (unless (if (> (length rtags) 1)
                   (magit-confirm t "Delete %i tags locally" rtags)
                 (magit-confirm t (format "Delete %s locally" (car rtags))))
         (setq rtags nil)))
     (list ltags rtags remote)))
  (when tags
    (magit-run-git "tag" "-d" tags))
  (when remote-tags
    (magit-run-git-async "push" remote (--map (concat ":" it) remote-tags))))

;;;; Notes

(magit-define-popup magit-notes-popup
  "Popup console for notes commands."
  'magit-popups 'magit-popup-sequence-mode
  :man-page "git-tag"
  :switches '((?n "Dry run"          "--dry-run"))
  :options  '((?r "Manipulate ref"   "--ref="      magit-notes-popup-read-ref)
              (?s "Merge strategy"   "--strategy=" read-from-minibuffer))
  :actions  '((?T "Edit"             magit-notes-edit)
              (?r "Remove"           magit-notes-remove)
              (?m "Merge"            magit-notes-merge)
              (?p "Prune"            magit-notes-prune)
              (?s "Set ref"          magit-notes-set-ref)
              (?S "Set display refs" magit-notes-set-display-refs))
  :sequence-actions '((?c "Commit merge" magit-notes-merge-commit)
                      (?a "Abort merge"  magit-notes-merge-abort))
  :sequence-predicate 'magit-notes-merging-p
  :default-action 'magit-notes-edit)

(defun magit-notes-edit (commit &optional ref)
  (interactive (magit-notes-read-args "Edit notes"))
  (magit-server-visit-args 'commit t)
  (magit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "edit" commit))

(defun magit-notes-remove (commit &optional ref)
  (interactive (magit-notes-read-args "Remove notes"))
  (magit-run-git-with-editor "notes" "remove" commit))

(defun magit-notes-merge (ref)
  (interactive (list (magit-read-string "Merge reference")))
  (magit-run-git-with-editor "notes" "merge" ref))

(defun magit-notes-merge-commit ()
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--commit"))

(defun magit-notes-merge-abort ()
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--abort"))

(defun magit-notes-prune (&optional dry-run)
  (interactive (list (and (member "--dry-run" magit-current-popup-args) t)))
  (when dry-run
    (magit-process))
  (magit-run-git-with-editor "notes" "prune" (and dry-run "--dry-run")))

(defun magit-notes-set-ref (ref &optional global)
  (interactive
   (list (magit-completing-read "Set notes ref"
                                (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                                nil nil
                                (--when-let (magit-get "core.notesRef")
                                  (if (string-match "^refs/notes/\\(.+\\)" it)
                                      (match-string 1 it)
                                    it)))
         current-prefix-arg))
  (if ref
      (magit-run-git "config" (and global "--global") "core.notesRef"
                     (if (string-prefix-p "refs/" ref)
                         ref
                       (concat "refs/notes/" ref)))
    (magit-run-git "config" "--unset" "core.notesRef")))

(defun magit-notes-set-display-refs (refs &optional global)
  (interactive
   (list (magit-completing-read "Set additional notes ref(s)"
                                (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                                nil nil
                                (mapconcat #'identity
                                           (magit-get-all "notes.displayRef")
                                           ":"))
         current-prefix-arg))
  (when (and refs (atom refs))
    (setq refs (split-string refs ":")))
  (when global
    (setq global "--global"))
  (magit-git-success "config" "--unset-all" global "notes.displayRef")
  (dolist (ref refs)
    (magit-call-git "config" "--add" global "notes.displayRef" ref))
  (magit-refresh))

(defun magit-notes-read-args (prompt)
 (list (magit-read-branch-or-commit prompt)
       (--when-let (--first (string-match "^--ref=\\(.+\\)" it)
                            magit-current-popup-args)
         (match-string 1 it))))

(defun magit-notes-popup-read-ref (prompt &optional initial-input)
  (magit-completing-read prompt (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                         nil nil initial-input))

(defun magit-notes-merging-p ()
  (let ((dir (magit-git-dir "NOTES_MERGE_WORKTREE")))
    (and (file-directory-p dir)
         (directory-files dir nil "^[^.]"))))

;;;; Stash

(magit-define-popup magit-stash-popup
  "Popup console for stash commands."
  'magit-popups
  :man-page "git-stash"
  :switches '((?k "Don't stash index"       "--keep-index")
              (?i "Reinstate stashed index" "--index")
              (?u "Include untracked files" "--include-untracked")
              (?a "Include all files"       "--all"))
  :actions  '((?z "Save"           magit-stash)
              (?s "Snapshot"       magit-stash-snapshot)
              (?p "Pop"            magit-stash-pop)
              (?k "Drop"           magit-stash-drop)
              (?Z "Save index"     magit-stash-index)
              (?S "Snapshot index" magit-stash-snapshot)
              (?a "Apply"          magit-stash-apply)
              (?b "Branch"         magit-stash-branch)
              (?v "View"           magit-diff-stash))
  :default-arguments '("--index")
  :default-action 'magit-stash
  :max-action-columns 4)

;;;###autoload
(defun magit-stash (description &optional args)
  "Create new stash of working tree and staging area named DESCRIPTION.
Working tree and staging area revert to the current `HEAD'.
With prefix argument, changes in staging area are kept.
\n(git stash save [ARGS] DESCRIPTION)"
  (interactive (list (read-string "Stash message: ")
                     (magit-current-popup-args :not "--index")))
  (magit-run-git "stash" "save" args "--" description))

;;;###autoload
(defun magit-stash-snapshot (&optional args)
  "Create new stash of working tree and staging area; keep changes in place.
\n(git stash save [ARGS] \"Snapshot...\";
 git stash apply stash@{0})"
  (interactive (list (magit-current-popup-args :not "--index")))
  (magit-call-git "stash" "save" args (magit-stash-format-snapshot-message))
  (magit-stash-apply "stash@{0}" "--index"))

(defun magit-stash-index (message &optional snapshot)
  "Create a new stash of the index only."
  (interactive (list (read-string "Stash message: ")))
  (magit-git-string "stash" "save" "--keep-index" (concat "(" message ")"))
  (let ((both (magit-rev-parse "refs/stash")))
    (message "Saved working directory and index state in %s" both)
    (magit-call-git "stash" "drop")
    (magit-call-git "stash" "save" message)
    (if snapshot
        (magit-run-git "stash" "pop" "--index" both)
      (with-temp-buffer
        (magit-git-insert "diff" (concat "stash@{0}.." both))
        (magit-run-git-with-input nil "apply"))
      (magit-refresh))))

(defun magit-stash-index-snapshot ()
  "Create a new stash of the index only; keep changes in place."
  (interactive)
  (magit-stash-index (magit-stash-format-snapshot-message) t))

(defun magit-stash-apply (stash &optional args)
  "Apply a stash on top of the current working tree state.
\n(git stash apply [ARGS] stash@{N})"
  (interactive (list (magit-read-stash "Apply stash" t)
                     (if magit-current-popup
                         (magit-current-popup-args :only "--index")
                       (--first (equal it "--index")
                                magit-stash-popup-defaults))))
  (if (or magit-current-popup (not (member "--index" args)))
      (magit-run-git "stash" "apply" args stash)
    (unless (magit-git-success "stash" "apply" args stash)
      (magit-run-git "stash" "apply" (remove "--index" args) stash))))

(defun magit-stash-pop (stash &optional args)
  "Apply a stash on top of working tree state and remove from stash list.
\n(git stash pop [ARGS] stash@{N})"
  (interactive (list (magit-read-stash "Pop stash" t)
                     (if magit-current-popup
                         (magit-current-popup-args :only "--index")
                       (--first (equal it "--index")
                                magit-stash-popup-defaults))))
  (magit-run-git "stash" "pop" args stash))

(defun magit-stash-drop (stash)
  "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes.
\n(git stash drop stash@{N})"
  (interactive
   (if (use-region-p)
       (let ((stashes (magit-section-region-siblings 'magit-section-value)))
         (when (magit-confirm 'drop-stashes "Drop %i stashes" stashes)
           (deactivate-mark t)
           (list stashes)))
     (list (magit-read-stash "Drop stash"))))
  (if (listp stash)
      (mapc 'magit-stash-drop (nreverse stash))
    (magit-run-git "stash" "drop" stash)))

(defun magit-stash-branch (stash branchname)
  "Create and checkout a branch from STASH.
\n(git stash branch BRANCHNAME stash@{N})"
  (interactive (list (magit-read-stash  "Branch stash" t)
                     (magit-read-string "Branch name")))
  (magit-run-git "stash" "branch" branchname stash))

(defun magit-stash-format-snapshot-message ()
  (format-time-string magit-stash-snapshot-message-format (current-time)))

(defvar magit-stash-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-diff-stash)
    (define-key map "a"  'magit-stash-apply)
    (define-key map "A"  'magit-stash-pop)
    (define-key map "k"  'magit-stash-drop)
    map)
  "Keymap for `stash' sections.")

(magit-define-section-jumper stashes "Stashes")

(defun magit-insert-stashes ()
  ;; #1427 Set log.date to work around an issue in Git <1.7.10.3.
  (--when-let (magit-git-lines "-c" "log.date=default" "stash" "list")
    (magit-insert-section (stashes)
      (magit-insert-heading "Stashes:")
      (dolist (stash it)
        (string-match "^\\(stash@{\\([0-9]+\\)}\\): \\(.+\\)$" stash)
        (magit-bind-match-strings (stash number message) stash
          (magit-insert-section (stash stash)
            (insert (propertize (format "stash@{%s}" number) 'face 'magit-hash)
                    " " message "\n"))))
      (insert "\n"))))

;;;; Submodules

(magit-define-popup magit-submodule-popup
  "Popup console for submodule commands."
  'magit-popups
  :man-page "git-submodule"
  :actions  '((?a "Add"    magit-submodule-add)
              (?b "Setup"  magit-submodule-setup)
              (?i "Init"   magit-submodule-init)
              (?u "Update" magit-submodule-update)
              (?s "Sync"   magit-submodule-sync)))

;;;###autoload
(defun magit-submodule-add (url &optional path)
  "Add the repository at URL as a submodule.
Optional PATH is the path to the submodule relative to the root
of the superproject. If it is nil then the path is determined
based on URL."
  (interactive
   (let* ((default-directory (magit-toplevel))
          (path (read-file-name
                 "Add submodule: " nil nil nil
                 (magit-section-when [file untracked]
                   (directory-file-name (magit-section-value it))))))
     (when path
       (setq path (file-name-as-directory (expand-file-name path)))
       (when (member path (list "" default-directory))
         (setq path nil)))
     (list (magit-read-string
            "Remote url"
            (and path (magit-git-repo-p path t)
                 (let ((default-directory path))
                   (magit-get "remote"
                              (or (magit-get-current-remote) "origin")
                              "url"))))
           (and path (directory-file-name (file-relative-name path))))))
  (magit-run-git "submodule" "add" url path))

;;;###autoload
(defun magit-submodule-setup ()
  "Clone and register missing submodules and checkout appropriate commits."
  (interactive)
  (magit-submodule-update t))

;;;###autoload
(defun magit-submodule-init ()
  "Register submodules listed in .gitmodules into .git/config."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "init")))

;;;###autoload
(defun magit-submodule-update (&optional init)
  "Clone missing submodules and checkout appropriate commits.
With a prefix argument also register submodules in .git/config."
  (interactive "P")
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "update" (and init "--init"))))

;;;###autoload
(defun magit-submodule-sync ()
  "Update each submodule's remote URL according to .gitmodules."
  (interactive)
  (let ((default-directory (magit-get-top-dir)))
    (magit-run-git-async "submodule" "sync")))

;;;; Dispatch Popup

(magit-define-popup magit-dispatch-popup
  "Popup console for dispatching other popups."
  'magit-popups
  :actions '((?b "Branching"       magit-branch-popup)
             (?B "Bisecting"       magit-bisect-popup)
             (?c "Committing"      magit-commit-popup)
             (?d "Diffing"         magit-diff-popup)
             (?f "Fetching"        magit-fetch-popup)
             (?F "Pulling"         magit-pull-popup)
             (?g "Refresh Buffers" magit-refresh-all)
             (?l "Logging"         magit-log-popup)
             (?m "Merging"         magit-merge-popup)
             (?M "Remoting"        magit-remote-popup)
             (?P "Pushing"         magit-push-popup)
             (?o "Submoduling"     magit-submodule-popup)
             (?r "Rebasing"        magit-rebase-popup)
             (?s "Show Status"     magit-status)
             (?S "Stage all"       magit-stage-modified)
             (?t "Tagging"         magit-tag-popup)
             (?U "Reset Index"     magit-reset-index)
             (?v "Show Commit"     magit-show-commit)
             (?V "Show File"       magit-find-file)
             (?y "Show Refs"       magit-show-refs)
             (?Y "Cherry"          magit-cherry)
             (?z "Stashing"        magit-stash-popup)
             (?! "Running"         magit-run-popup)
             (?$ "Show Process"    magit-display-process)))

;;;; Git Popup

(defvar magit-git-command-history nil)

(magit-define-popup magit-run-popup
  "Popup console for running raw Git commands."
  'magit-popups
  :actions '((?! "Git Subcommand (from root)" magit-git-command-topdir)
             (?: "Git Subcommand (from pwd)" magit-git-command)
             (?g "Git Gui" magit-run-git-gui)
             (?k "Gitk" magit-run-gitk))
  :default-action 'magit-git-command)

;;;###autoload
(defun magit-git-command (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
With a prefix argument run Git in the root of the current
repository.  Non-interactively run Git in DIRECTORY with ARGS."
  (interactive (magit-git-command-read-args))
  (require 'eshell)
  (magit-mode-display-buffer (magit-process-buffer)
                             'magit-process-mode 'pop-to-buffer)
  (goto-char (point-max))
  (let ((default-directory directory))
    (magit-run-git-async
     (with-temp-buffer
       (insert args)
       (mapcar 'eval (eshell-parse-arguments (point-min)
                                             (point-max)))))))

(defun magit-git-command-topdir (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
Run Git in the root of the current repository.
\n(fn)" ; arguments are for internal use
  (interactive (magit-git-command-read-args t))
  (magit-git-command args directory))

(defun magit-git-command-read-args (&optional root)
  (let ((dir (if (or root current-prefix-arg)
                 (or (magit-get-top-dir)
                     (user-error "Not inside a Git repository"))
               default-directory)))
    (list (magit-read-string (format "Git subcommand (in %s)"
                                     (abbreviate-file-name dir))
                             nil 'magit-git-command-history)
          dir)))

;;;; Read Repository

(defun magit-read-top-dir (dir)
  "Ask the user for a Git repository."
  (if (and (not dir) magit-repository-directories)
      (let* ((repos (magit-list-repos-uniquify
                     (--map (cons (file-name-nondirectory it) it)
                            (magit-list-repos))))
             (reply (magit-completing-read "Git repository" repos)))
        (file-name-as-directory
         (or (cdr (assoc reply repos))
             (if (file-directory-p reply)
                 (expand-file-name reply)
               (user-error "Not a repository or a directory: %s" reply)))))
    (file-name-as-directory
     (read-directory-name "Git repository: "
                          (or (magit-get-top-dir) default-directory)))))

(defun magit-list-repos ()
  (--mapcat (magit-list-repos* it magit-repository-directories-depth)
            magit-repository-directories))

(defun magit-list-repos* (directory depth)
  (cond ((file-readable-p (expand-file-name ".git" directory))
         (list directory))
        ((and (> depth 0) (file-accessible-directory-p directory))
         (cl-loop for file in (directory-files directory t "^[^.]" t)
                  when (file-directory-p file)
                  append (magit-list-repos* file (1- depth))))))

(defun magit-list-repos-uniquify (alist)
  (let (result (dict (make-hash-table :test 'equal)))
    (dolist (a (delete-dups alist))
      (puthash (car a) (cons (cdr a) (gethash (car a) dict)) dict))
    (maphash
     (lambda (key value)
       (if (= (length value) 1)
           (push (cons key (car value)) result)
         (setq result
               (append result
                       (magit-list-repos-uniquify
                        (--map (cons (concat
                                      key "\\"
                                      (file-name-nondirectory
                                       (directory-file-name
                                        (substring it 0 (- (length key))))))
                                     it)
                               value))))))
     dict)
    result))

;;;; Various

;;;###autoload
(defun magit-format-patch (range)
  (interactive
   (let ((revs (magit-current-sections 'commit)))
     (setq revs (nreverse (mapcar 'magit-section-value revs)))
     (list (if (or current-prefix-arg (not revs))
               (magit-read-range-or-commit "Format range")
             (concat (car revs) "^.." (car (last revs)))))))
  (magit-run-git "format-patch" range))

(defun magit-copy-as-kill ()
  "Copy the thing at point into the kill ring."
  (interactive)
  (magit-section-when (branch commit mcommit file)
    (kill-new (message "%s" (magit-section-value it)))))

;;; magit.el ends soon

(defconst magit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(magit-define-section-jumper\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt '("magit-insert-section"
                                  "magit-section-case"
                                  "magit-section-when"
                                  "magit-bind-match-strings"
                                  "magit-with-blob") t)
                "\\_>")
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode magit-font-lock-keywords)

(defvar magit-version 'undefined
  "The version of Magit that you're using.
Use the function by the same name instead of this variable.")

(defun magit-version (&optional noerror)
  "The version of Magit that you're using.\n\n\(fn)"
  (interactive)
  (let ((toplib (or load-file-name buffer-file-name)))
    (unless (and toplib
                 (equal (file-name-nondirectory toplib) "magit.el"))
      (setq toplib (locate-library "magit.el")))
    (when toplib
      (let* ((dir (file-name-directory toplib))
             (static (expand-file-name "magit-version.el" dir))
             (gitdir (expand-file-name ".git" dir)))
        (cond ((file-exists-p gitdir)
               (setq magit-version
                     (let ((default-directory dir))
                       (magit-git-string "describe" "--tags" "--dirty")))
               (ignore-errors (delete-file static)))
              ((file-exists-p static)
               (load-file static))
              ((featurep 'package)
               (setq magit-version
                     (or (and (fboundp 'package-desc-vers) ; < 24.3.50
                              (package-version-join
                               (package-desc-vers
                                (cdr (assq 'magit package-alist)))))
                         (and (fboundp 'package-desc-version) ; >= 24.3.50
                              (package-version-join
                               (package-desc-version
                                (cadr (assq 'magit package-alist)))))))))))
    (if (stringp magit-version)
        (when (called-interactively-p 'any)
          (message "magit-%s" magit-version))
      (if noerror
          (progn (setq magit-version 'error)
                 (message "Cannot determine Magit's version"))
        (error "Cannot determine Magit's version")))
    magit-version))

(cl-eval-when (load eval) (magit-version t))

(define-obsolete-variable-alias 'magit-highlight-whitespace 'magit-diff-highlight-whitespace)
(define-obsolete-variable-alias 'magit-highlight-trailing-whitespace 'magit-diff-highlight-trailing)
(define-obsolete-variable-alias 'magit-highlight-indentation 'magit-diff-highlight-indentation)
(define-obsolete-variable-alias 'magit-mode-refresh-buffer-hook 'magit-refresh-buffer-hook)
(define-obsolete-variable-alias 'magit-revert-backup 'magit-apply-backup)
(define-obsolete-variable-alias 'magit-show-child-count 'magit-section-show-child-count)
(define-obsolete-variable-alias 'magit-repo-dirs 'magit-repository-directories)
(define-obsolete-variable-alias 'magit-repo-dirs-depth 'magit-repository-directories-depth)

(provide 'magit)

(require 'magit-blame)
(require 'magit-ediff)
(require 'magit-extras)
(require 'magit-wip)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; magit.el ends here
