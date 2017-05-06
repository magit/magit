;;; magit-stash.el --- stash support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2017  The Magit Project Contributors
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

(require 'magit)

;;; Options

(defgroup magit-stash nil
  "List stashes and show stash diffs."
  :group 'magit-modes)

;;;; Diff options

(defcustom magit-stash-sections-hook
  '(magit-insert-stash-worktree
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
  :group 'magit-stash
  :group 'magit-margin
  :type magit-log-margin--custom-type
  :initialize 'magit-custom-initialize-reset
  :set-after '(magit-log-margin)
  :set (apply-partially #'magit-margin-set-variable 'magit-stashes-mode))

;;; Commands

;;;###autoload (autoload 'magit-stash-popup "magit-stash" nil t)
(magit-define-popup magit-stash-popup
  "Popup console for stash commands."
  :man-page "git-stash"
  :switches '((?u "Also save untracked files" "--include-untracked")
              (?a "Also save untracked and ignored files" "--all"))
  :actions  '((?z "Save"               magit-stash)
              (?Z "Snapshot"           magit-snapshot)
              (?p "Pop"                magit-stash-pop)
              (?i "Save index"         magit-stash-index)
              (?I "Snapshot index"     magit-snapshot-index)
              (?a "Apply"              magit-stash-apply)
              (?w "Save worktree"      magit-stash-worktree)
              (?W "Snapshot worktree"  magit-snapshot-worktree)
              (?l "List"               magit-stash-list)
              (?x "Save keeping index" magit-stash-keep-index)
              (?r "Snapshot to wipref" magit-wip-commit)
              (?v "Show"               magit-stash-show)
              (?b "Branch"             magit-stash-branch)
              (?k "Drop"               magit-stash-drop) nil
              (?f "Format patch"       magit-stash-format-patch))
  :default-action 'magit-stash
  :max-action-columns 3)

;;;###autoload
(defun magit-stash (message &optional include-untracked)
  "Create a stash of the index and working tree.
Untracked files are included according to popup arguments.
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
Untracked files are included according to popup arguments.
One prefix argument is equivalent to `--include-untracked'
while two prefix arguments are equivalent to `--all'."
  (interactive (magit-stash-read-args))
  (magit-stash-save message nil t include-untracked t 'index))

;;;###autoload
(defun magit-stash-keep-index (message &optional include-untracked)
  "Create a stash of the index and working tree, keeping index intact.
Untracked files are included according to popup arguments.
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
(defun magit-snapshot (&optional include-untracked)
  "Create a snapshot of the index and working tree.
Untracked files are included according to popup arguments.
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
Untracked files are included according to popup arguments.
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
  (interactive (list (magit-read-stash "Apply stash" t)))
  (if (= (magit-call-git "stash" "apply" "--index" stash) 0)
      (magit-refresh)
    (magit-run-git "stash" "apply" stash)))

(defun magit-stash-pop (stash)
  "Apply a stash to the working tree and remove it from stash list.
Try to preserve the stash index.  If that fails because there
are staged changes, apply without preserving the stash index
and forgo removing the stash."
  (interactive (list (magit-read-stash "Apply pop" t)))
  (if (= (magit-call-git "stash" "apply" "--index" stash) 0)
      (magit-stash-drop stash)
    (magit-run-git "stash" "apply" stash)))

;;;###autoload
(defun magit-stash-drop (stash)
  "Remove a stash from the stash list.
When the region is active offer to drop all contained stashes."
  (interactive (list (--if-let (magit-region-values 'stash)
                         (or (magit-confirm t nil "Drop %i stashes" it)
                             (user-error "Abort"))
                       (magit-read-stash "Drop stash"))))
  (dolist (stash (if (listp stash)
                     (nreverse (prog1 stash (setq stash (car stash))))
                   (list stash)))
    (message "Deleted refs/%s (was %s)" stash
             (magit-rev-parse "--short" stash))
    (magit-call-git "rev-parse" stash)
    (magit-call-git "reflog" "delete" "--updateref" "--rewrite" stash))
  (-when-let (ref (and (string-match "\\(.+\\)@{[0-9]+}$" stash)
                       (match-string 1 stash)))
    (unless (string-match "^refs/" ref)
      (setq ref (concat "refs/" ref)))
    (unless (magit-rev-verify (concat ref "@{0}"))
      (magit-run-git "update-ref" "-d" ref)))
  (magit-refresh))

;;;###autoload
(defun magit-stash-clear (ref)
  "Remove all stashes saved in REF's reflog by deleting REF."
  (interactive
   (let ((ref (or (magit-section-when 'stashes) "refs/stash")))
     (if (magit-confirm t (format "Drop all stashes in %s" ref))
         (list ref)
       (user-error "Abort"))))
  (magit-run-git "update-ref" "-d" ref))

;;;###autoload
(defun magit-stash-branch (stash branch)
  "Create and checkout a new BRANCH from STASH."
  (interactive (list (magit-read-stash "Branch stash" t)
                     (magit-read-string-ns "Branch name")))
  (magit-run-git "stash" "branch" branch stash))

;;;###autoload
(defun magit-stash-format-patch (stash)
  "Create a patch from STASH"
  (interactive (list (magit-read-stash "Create patch from stash" t)))
  (with-temp-file (magit-rev-format "0001-%f.patch" stash)
    (magit-git-insert "stash" "show" "-p" stash))
  (magit-refresh))

;;; Plumbing

(defun magit-stash-save (message index worktree untracked
                                 &optional refresh keep noerror ref)
  (if (or (and index     (magit-staged-files t))
          (and worktree  (magit-modified-files t))
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
              (magit-call-git "reset" "--hard" "HEAD"))
            (when untracked
              (magit-call-git "clean" "-f" (and (eq untracked 'all) "-x")))))
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
  (when (magit-rev-verify ref)
    (magit-insert-section (stashes ref (not magit-status-expand-stashes))
      (magit-insert-heading heading)
      (magit-git-wash (apply-partially 'magit-log-wash-log 'stash)
        "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" ref))))

;;; List Stashes

;;;###autoload
(defun magit-stash-list ()
  "List all stashes in a buffer."
  (interactive)
  (magit-mode-setup #'magit-stashes-mode "refs/stash"))

(define-derived-mode magit-stashes-mode magit-reflog-mode "Magit Stashes"
  "Mode for looking at lists of stashes."
  :group 'magit-log
  (hack-dir-local-variables-non-file-buffer))

(cl-defun magit-stashes-refresh-buffer (ref)
  (magit-insert-section (stashesbuf)
    (magit-insert-heading (if (equal ref "refs/stash")
                              "Stashes:"
                            (format "Stashes [%s]:" ref)))
    (magit-git-wash (apply-partially 'magit-log-wash-log 'stash)
      "reflog" "--format=%gd%x00%aN%x00%at%x00%gs" ref)))

;;; Show Stash

;;;###autoload
(defun magit-stash-show (stash &optional args files)
  "Show all diffs of a stash in a buffer."
  (interactive (cons (or (and (not current-prefix-arg)
                              (magit-stash-at-point))
                         (magit-read-stash "Show stash"))
                     (-let [(args files) (magit-diff-arguments)]
                       (list (delete "--stat" args) files))))
  (magit-mode-setup #'magit-stash-mode stash nil args files))

(define-derived-mode magit-stash-mode magit-diff-mode "Magit Stash"
  "Mode for looking at individual stashes."
  :group 'magit-diff
  (hack-dir-local-variables-non-file-buffer))

(defun magit-stash-refresh-buffer (stash _const _args _files)
  (setq header-line-format
        (concat
         "\s" (propertize (capitalize stash) 'face 'magit-section-heading)
         "\s" (magit-rev-format "%s" stash)))
  (setq magit-buffer-revision-hash (magit-rev-parse stash))
  (magit-insert-section (stash)
    (run-hooks 'magit-stash-sections-hook)))

(defun magit-stash-insert-section (commit range message &optional files)
  (magit-insert-section (commit commit)
    (magit-insert-heading message)
    (magit-git-wash #'magit-diff-wash-diffs
      "diff" range "-p" "--no-prefix"
      (nth 2 magit-refresh-args)
      "--" (or files (nth 3 magit-refresh-args)))))

(defun magit-insert-stash-index ()
  "Insert section showing staged changes of the stash."
  (let ((stash (car magit-refresh-args)))
    (magit-stash-insert-section (format "%s^2" stash)
                                (format "%s^..%s^2" stash stash)
                                "Staged")))

(defun magit-insert-stash-worktree ()
  "Insert section showing unstaged changes of the stash."
  (let ((stash (car magit-refresh-args)))
    (magit-stash-insert-section stash
                                (format "%s^2..%s" stash stash)
                                "Unstaged")))

(defun magit-insert-stash-untracked ()
  "Insert section showing the untracked files commit of the stash."
  (let ((stash (car magit-refresh-args))
        (rev   (concat (car magit-refresh-args) "^3")))
    (when (magit-rev-verify rev)
      (magit-stash-insert-section (format "%s^3" stash)
                                  (format "%s^..%s^3" stash stash)
                                  "Untracked files"
                                  (magit-git-items "ls-tree" "-z" "--name-only"
                                                   "-r" "--full-tree" rev)))))

(provide 'magit-stash)
;;; magit-stash.el ends here
