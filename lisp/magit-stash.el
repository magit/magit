;;; magit-stash.el --- stash support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2020  The Magit Project Contributors
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

;; Support for Git stashes.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'magit)
(require 'magit-reflog)

;;; Options

(defgroup magit-stash nil
  "List stashes and show stash diffs."
  :group 'magit-modes)

;;;; Diff options

(defcustom magit-stash-sections-hook
  '(magit-insert-stash-notes
    magit-insert-stash-worktree
    magit-insert-stash-index
    magit-insert-stash-untracked)
  "Hook run to insert sections into stash diff buffers."
  :package-version '(magit . "2.1.0")
  :group 'magit-stash
  :type 'hook)

;;;; Log options

(defcustom magit-stashes-margin
  (list (nth 0 magit-log-margin)
        (nth 1 magit-log-margin)
        'magit-log-margin-width nil
        (nth 4 magit-log-margin))
  "Format of the margin in `magit-stashes-mode' buffers.

The value has the form (INIT STYLE WIDTH AUTHOR AUTHOR-WIDTH).

If INIT is non-nil, then the margin is shown initially.
STYLE controls how to format the author or committer date.
  It can be one of `age' (to show the age of the commit),
  `age-abbreviated' (to abbreviate the time unit to a character),
  or a string (suitable for `format-time-string') to show the
  actual date.  Option `magit-log-margin-show-committer-date'
  controls which date is being displayed.
WIDTH controls the width of the margin.  This exists for forward
  compatibility and currently the value should not be changed.
AUTHOR controls whether the name of the author is also shown by
  default.
AUTHOR-WIDTH has to be an integer.  When the name of the author
  is shown, then this specifies how much space is used to do so."
  :package-version '(magit . "2.9.0")
  :group 'magit-stash
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize 'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-stashes-mode))

;;; Commands

;;;###autoload (autoload 'magit-stash "magit-stash" nil t)
(transient-define-prefix magit-stash ()
  "Stash uncommitted changes."
  :man-page "git-stash"
  ["Arguments"
   ("-u" "Also save untracked files" ("-u" "--include-untracked"))
   ("-a" "Also save untracked and ignored files" ("-a" "--all"))]
  [["Stash"
    ("z" "both"          magit-stash-both)
    ("i" "index"         magit-stash-index)
    ("w" "worktree"      magit-stash-worktree)
    ("x" "keeping index" magit-stash-keep-index)]
   ["Snapshot"
    ("Z" "both"          magit-snapshot-both)
    ("I" "index"         magit-snapshot-index)
    ("W" "worktree"      magit-snapshot-worktree)
    ("r" "to wip ref"    magit-wip-commit)]
   ["Use"
    ("a" "Apply"         magit-stash-apply)
    ("p" "Pop"           magit-stash-pop)
    ("k" "Drop"          magit-stash-drop)]
   ["Inspect"
    ("l" "List"          magit-stash-list)
    ("v" "Show"          magit-stash-show)]
   ["Transform"
    ("b" "Branch"        magit-stash-branch)
    ("B" "Branch here"   magit-stash-branch-here)
    ("f" "Format patch"  magit-stash-format-patch)]])

(defun magit-stash-arguments ()
  (transient-args 'magit-stash))

;;;###autoload
(defun magit-stash-both (message &optional include-untracked)
  "Create a stash of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (magit-stash-read-args))
  (magit-stash-save message t t include-untracked t))

;;;###autoload
(defun magit-stash-index (message)
  "Create a stash of the index only.
Unstaged and untracked changes are not stashed.  The stashed
changes are applied in reverse to both the index and the
worktree.  This command can fail when the worktree is not clean.
Applying the resulting stash has the inverse effect."
  (interactive (list (magit-stash-read-message)))
  (magit-stash-save message t nil nil t 'worktree))

;;;###autoload
(defun magit-stash-worktree (message &optional include-untracked)
  "Create a stash of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (magit-stash-read-args))
  (magit-stash-save message nil t include-untracked t 'index))

;;;###autoload
(defun magit-stash-keep-index (message &optional include-untracked)
  "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (magit-stash-read-args))
  (magit-stash-save message t t include-untracked t 'index))

(defun magit-stash-read-args ()
  (list (magit-stash-read-message)
        (magit-stash-read-untracked)))

(defun magit-stash-read-untracked ()
  (let ((prefix (prefix-numeric-value current-prefix-arg))
        (args   (magit-stash-arguments)))
    (cond ((or (= prefix 16) (member "--all" args)) 'all)
          ((or (= prefix  4) (member "--include-untracked" args)) t))))

(defun magit-stash-read-message ()
  (let* ((default (format "On %s: "
                          (or (magit-get-current-branch) "(no branch)")))
         (input (magit-read-string "Stash message" default)))
    (if (equal input default)
        (concat default (magit-rev-format "%h %s"))
      input)))

;;;###autoload
(defun magit-snapshot-both (&optional include-untracked)
  "Create a snapshot of the index and working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (magit-snapshot-read-args))
  (magit-snapshot-save t t include-untracked t))

;;;###autoload
(defun magit-snapshot-index ()
  "Create a snapshot of the index only.
Unstaged and untracked changes are not stashed."
  (interactive)
  (magit-snapshot-save t nil nil t))

;;;###autoload
(defun magit-snapshot-worktree (&optional include-untracked)
  "Create a snapshot of unstaged changes in the working tree.
Untracked files are included according to infix arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (magit-snapshot-read-args))
  (magit-snapshot-save nil t include-untracked t))

(defun magit-snapshot-read-args ()
  (list (magit-stash-read-untracked)))

(defun magit-snapshot-save (index worktree untracked &optional refresh)
  (magit-stash-save (concat "WIP on " (magit-stash-summary))
                    index worktree untracked refresh t))

;;;###autoload
(defun magit-stash-apply (stash)
  "Apply a stash to the working tree.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index."
  (interactive (list (magit-read-stash "Apply stash")))
  (if (= (magit-call-git "stash" "apply" "--index" stash) 0)
      (magit-refresh)
    (magit-run-git "stash" "apply" stash)))

(defun magit-stash-pop (stash)
  "Apply a stash to the working tree and remove it from stash list.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index
and forgo removing the stash."
  (interactive (list (magit-read-stash "Pop stash")))
  (if (= (magit-call-git "stash" "apply" "--index" stash) 0)
      (magit-stash-drop stash)
    (magit-run-git "stash" "apply" stash)))

;;;###autoload
(defun magit-stash-drop (stash)
  "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes."
  (interactive
   (list (--if-let (magit-region-values 'stash)
             (magit-confirm 'drop-stashes nil "Drop %i stashes" nil it)
           (magit-read-stash "Drop stash"))))
  (dolist (stash (if (listp stash)
                     (nreverse (prog1 stash (setq stash (car stash))))
                   (list stash)))
    (message "Deleted refs/%s (was %s)" stash
             (magit-rev-parse "--short" stash))
    (magit-call-git "rev-parse" stash)
    (magit-call-git "reflog" "delete" "--updateref" "--rewrite" stash))
  (when-let ((ref (and (string-match "\\(.+\\)@{[0-9]+}$" stash)
                       (match-string 1 stash))))
    (unless (string-match "^refs/" ref)
      (setq ref (concat "refs/" ref)))
    (unless (magit-rev-verify (concat ref "@{0}"))
      (magit-run-git "update-ref" "-d" ref)))
  (magit-refresh))

;;;###autoload
(defun magit-stash-clear (ref)
  "Remove all stashes saved in REF's reflog by deleting REF."
  (interactive (let ((ref (or (magit-section-value-if 'stashes) "refs/stash")))
                 (magit-confirm t (format "Drop all stashes in %s" ref))
                 (list ref)))
  (magit-run-git "update-ref" "-d" ref))

;;;###autoload
(defun magit-stash-branch (stash branch)
  "Create and checkout a new BRANCH from STASH."
  (interactive (list (magit-read-stash "Branch stash")
                     (magit-read-string-ns "Branch name")))
  (magit-run-git "stash" "branch" branch stash))

;;;###autoload
(defun magit-stash-branch-here (stash branch)
  "Create and checkout a new BRANCH and apply STASH.
The branch is created using `magit-branch-and-checkout', using the
current branch or `HEAD' as the start-point."
  (interactive (list (magit-read-stash "Branch stash")
                     (magit-read-string-ns "Branch name")))
  (let ((inhibit-magit-refresh t))
    (magit-branch-and-checkout branch (or (magit-get-current-branch) "HEAD")))
  (magit-stash-apply stash))

;;;###autoload
(defun magit-stash-format-patch (stash)
  "Create a patch from STASH"
  (interactive (list (magit-read-stash "Create patch from stash")))
  (with-temp-file (magit-rev-format "0001-%f.patch" stash)
    (magit-git-insert "stash" "show" "-p" stash))
  (magit-refresh))

;;; Plumbing

(defun magit-stash-save (message index worktree untracked
                                 &optional refresh keep noerror ref)
  (if (or (and index     (magit-staged-files t))
          (and worktree  (magit-unstaged-files t))
          (and untracked (magit-untracked-files (eq untracked 'all))))
      (magit-with-toplevel
        (magit-stash-store message (or ref "refs/stash")
                           (magit-stash-create message index worktree untracked))
        (if (eq keep 'worktree)
            (with-temp-buffer
              (magit-git-insert "diff" "--cached")
              (magit-run-git-with-input
               "apply" "--reverse" "--cached" "--ignore-space-change" "-")
              (magit-run-git-with-input
               "apply" "--reverse" "--ignore-space-change" "-"))
          (unless (eq keep t)
            (if (eq keep 'index)
                (magit-call-git "checkout" "--" ".")
              (magit-call-git "reset" "--hard" "HEAD" "--"))
            (when untracked
              (magit-call-git "clean" "--force" "-d"
                              (and (eq untracked 'all) "-x")))))
        (when refresh
          (magit-refresh)))
    (unless noerror
      (user-error "No %s changes to save" (cond ((not index)  "unstaged")
                                                ((not worktree) "staged")
                                                (t "local"))))))

(defun magit-stash-store (message ref commit)
  (magit-update-ref ref message commit t))

(defun magit-stash-create (message index worktree untracked)
  (unless (magit-rev-parse "--verify" "HEAD")
    (error "You do not have the initial commit yet"))
  (let ((magit-git-global-arguments (nconc (list "-c" "commit.gpgsign=false")
                                           magit-git-global-arguments))
        (default-directory (magit-toplevel))
        (summary (magit-stash-summary))
        (head "HEAD"))
    (when (and worktree (not index))
      (setq head (or (magit-commit-tree "pre-stash index" nil "HEAD")
                     (error "Cannot save the current index state"))))
    (or (setq index (magit-commit-tree (concat "index on " summary) nil head))
        (error "Cannot save the current index state"))
    (and untracked
         (setq untracked (magit-untracked-files (eq untracked 'all)))
         (setq untracked (magit-with-temp-index nil nil
                           (or (and (magit-update-files untracked)
                                    (magit-commit-tree
                                     (concat "untracked files on " summary)))
                               (error "Cannot save the untracked files")))))
    (magit-with-temp-index index "-m"
      (when worktree
        (or (magit-update-files (magit-git-items "diff" "-z" "--name-only" head))
            (error "Cannot save the current worktree state")))
      (or (magit-commit-tree message nil head index untracked)
          (error "Cannot save the current worktree state")))))

(defun magit-stash-summary ()
  (concat (or (magit-get-current-branch) "(no branch)")
          ": " (magit-rev-format "%h %s")))

;;; Sections

(defvar magit-stashes-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-delete-thing] 'magit-stash-clear)
    map)
  "Keymap for `stashes' section.")

(defvar magit-stash-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing]  'magit-stash-show)
    (define-key map [remap magit-delete-thing] 'magit-stash-drop)
    (define-key map "a"  'magit-stash-apply)
    (define-key map "A"  'magit-stash-pop)
    map)
  "Keymap for `stash' sections.")

(magit-define-section-jumper magit-jump-to-stashes
  "Stashes" stashes "refs/stash")

(cl-defun magit-insert-stashes (&optional (ref   "refs/stash")
                                          (heading "Stashes:"))
  "Insert `stashes' section showing reflog for \"refs/stash\".
If optional REF is non-nil, show reflog for that instead.
If optional HEADING is non-nil, use that as section heading
instead of \"Stashes:\"."
  (let ((verified (magit-rev-verify ref))
        (autostash
         (and (magit-rebase-in-progress-p)
              (magit-file-line
               (magit-git-dir
                (-> (if (file-directory-p (magit-git-dir "rebase-merge"))
                        "rebase-merge/autostash"
                      "rebase-apply/autostash")))))))
    (when (or autostash verified)
      (magit-insert-section (stashes ref)
        (magit-insert-heading heading)
        (when autostash
          (pcase-let ((`(,author ,date ,msg)
                       (split-string
                        (car (magit-git-lines
                              "show" "-q" "--format=%aN%x00%at%x00%s"
                              autostash))
                        "\0")))
            (magit-insert-section (stash autostash)
              (insert (propertize "AUTOSTASH" 'font-lock-face 'magit-hash))
              (insert " " msg "\n")
              (save-excursion
                (backward-char)
                (magit-log-format-margin autostash author date)))))
        (if verified
            (magit-git-wash (apply-partially 'magit-log-wash-log 'stash)
              "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" ref)
          (insert ?\n)
          (save-excursion
            (backward-char)
            (magit-make-margin-overlay)))))))

;;; List Stashes

;;;###autoload
(defun magit-stash-list ()
  "List all stashes in a buffer."
  (interactive)
  (magit-stashes-setup-buffer))

(define-derived-mode magit-stashes-mode magit-reflog-mode "Magit Stashes"
  "Mode for looking at lists of stashes."
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer))

(defun magit-stashes-setup-buffer ()
  (magit-setup-buffer #'magit-stashes-mode nil
    (magit-buffer-refname "refs/stash")))

(defun magit-stashes-refresh-buffer ()
  (magit-insert-section (stashesbuf)
    (magit-insert-heading (if (equal magit-buffer-refname "refs/stash")
                              "Stashes:"
                            (format "Stashes [%s]:" magit-buffer-refname)))
    (magit-git-wash (apply-partially 'magit-log-wash-log 'stash)
      "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" magit-buffer-refname)))

(cl-defmethod magit-buffer-value (&context (major-mode magit-stashes-mode))
  magit-buffer-refname)

(defvar magit--update-stash-buffer nil)

(defun magit-stashes-maybe-update-stash-buffer (&optional _)
  "When moving in the stashes buffer, update the stash buffer.
If there is no stash buffer in the same frame, then do nothing."
  (when (derived-mode-p 'magit-stashes-mode)
    (magit--maybe-update-stash-buffer)))

(defun magit--maybe-update-stash-buffer ()
  (when-let ((stash  (magit-section-value-if 'stash))
             (buffer (magit-get-mode-buffer 'magit-stash-mode nil t)))
    (if magit--update-stash-buffer
        (setq magit--update-stash-buffer (list stash buffer))
      (setq magit--update-stash-buffer (list stash buffer))
      (run-with-idle-timer
       magit-update-other-window-delay nil
       (let ((args (with-current-buffer buffer
                     (let ((magit-direct-use-buffer-arguments 'selected))
                       (magit-show-commit--arguments)))))
         (lambda ()
           (pcase-let ((`(,stash ,buf) magit--update-stash-buffer))
             (setq magit--update-stash-buffer nil)
             (when (buffer-live-p buf)
               (let ((magit-display-buffer-noselect t))
                 (apply #'magit-stash-show stash args))))
           (setq magit--update-stash-buffer nil)))))))

;;; Show Stash

;;;###autoload
(defun magit-stash-show (stash &optional args files)
  "Show all diffs of a stash in a buffer."
  (interactive (cons (or (and (not current-prefix-arg)
                              (magit-stash-at-point))
                         (magit-read-stash "Show stash"))
                     (pcase-let ((`(,args ,files)
                                  (magit-diff-arguments 'magit-stash-mode)))
                       (list (delete "--stat" args) files))))
  (magit-stash-setup-buffer stash args files))

(define-derived-mode magit-stash-mode magit-diff-mode "Magit Stash"
  "Mode for looking at individual stashes."
  :group 'magit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun magit-stash-setup-buffer (stash args files)
  (magit-setup-buffer #'magit-stash-mode nil
    (magit-buffer-revision stash)
    (magit-buffer-range (format "%s^..%s" stash stash))
    (magit-buffer-diff-args args)
    (magit-buffer-diff-files files)))

(defun magit-stash-refresh-buffer ()
  (magit-set-header-line-format
   (concat (capitalize magit-buffer-revision) " "
           (propertize (magit-rev-format "%s" magit-buffer-revision)
                       'font-lock-face
                       (list :weight 'normal :foreground
                             (face-attribute 'default :foreground)))))
  (setq magit-buffer-revision-hash (magit-rev-parse magit-buffer-revision))
  (magit-insert-section (stash)
    (magit-run-section-hook 'magit-stash-sections-hook)))

(cl-defmethod magit-buffer-value (&context (major-mode magit-stash-mode))
  magit-buffer-revision)

(defun magit-stash-insert-section (commit range message &optional files)
  (magit-insert-section (commit commit)
    (magit-insert-heading message)
    (magit--insert-diff "diff" range "-p" "--no-prefix" magit-buffer-diff-args
                        "--" (or files magit-buffer-diff-files))))

(defun magit-insert-stash-notes ()
  "Insert section showing notes for a stash.
This shows the notes for stash@{N} but not for the other commits
that make up the stash."
  (magit-insert-section section (note)
    (magit-insert-heading "Notes")
    (magit-git-insert "notes" "show" magit-buffer-revision)
    (if (= (point)
           (oref section content))
        (magit-cancel-section)
      (insert "\n"))))

(defun magit-insert-stash-index ()
  "Insert section showing staged changes of the stash."
  (magit-stash-insert-section
   (format "%s^2" magit-buffer-revision)
   (format "%s^..%s^2" magit-buffer-revision magit-buffer-revision)
   "Staged"))

(defun magit-insert-stash-worktree ()
  "Insert section showing unstaged changes of the stash."
  (magit-stash-insert-section
   magit-buffer-revision
   (format "%s^2..%s" magit-buffer-revision magit-buffer-revision)
   "Unstaged"))

(defun magit-insert-stash-untracked ()
  "Insert section showing the untracked files commit of the stash."
  (let ((stash magit-buffer-revision)
        (rev (concat magit-buffer-revision "^3")))
    (when (magit-rev-verify rev)
      (magit-stash-insert-section (format "%s^3" stash)
                                  (format "%s^..%s^3" stash stash)
                                  "Untracked files"
                                  (magit-git-items "ls-tree" "-z" "--name-only"
                                                   "-r" "--full-tree" rev)))))

;;; _
(provide 'magit-stash)
;;; magit-stash.el ends here
