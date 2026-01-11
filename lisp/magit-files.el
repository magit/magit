;;; magit-files.el --- Finding files  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2026 The Magit Project Contributors

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

;; This library implements support for finding blobs, staged files,
;; and Git configuration files.  It also implements modes useful in
;; buffers visiting files and blobs, and the commands used by those
;; modes.

;;; Code:

(require 'magit)

;;; Find Blob

(defvar magit-find-file-hook nil)
(add-hook 'magit-find-file-hook #'magit-blob-mode)

;;;###autoload
(defun magit-find-file (rev file)
  "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file"))
  (magit-find-file--internal rev file #'pop-to-buffer-same-window))

;;;###autoload
(defun magit-find-file-other-window (rev file)
  "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file in other window"))
  (magit-find-file--internal rev file #'switch-to-buffer-other-window))

;;;###autoload
(defun magit-find-file-other-frame (rev file)
  "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file in other frame"))
  (magit-find-file--internal rev file #'switch-to-buffer-other-frame))

(defun magit-find-file-read-args (prompt)
  (let ((pseudo-revs '("{worktree}" "{index}")))
    (let ((rev (magit-completing-read "Find file from revision"
                                      (append pseudo-revs
                                              (magit-list-refnames nil t))
                                      nil 'any nil 'magit-revision-history
                                      (or (magit-branch-or-commit-at-point)
                                          (magit-get-current-branch)))))
      (list rev
            (magit-read-file-from-rev (if (member rev pseudo-revs) "HEAD" rev)
                                      prompt)))))

(defun magit-find-file--internal (rev file fn)
  (let ((buf (magit-find-file-noselect rev file))
        line col)
    (when-let ((visited-file (magit-file-relative-name)))
      (setq line (line-number-at-pos))
      (setq col (current-column))
      (cond
        ((not (equal visited-file file)))
        ((equal magit-buffer-revision rev))
        ((equal rev "{worktree}")
         (setq line (magit-diff-visit--offset file magit-buffer-revision line)))
        ((equal rev "{index}")
         (setq line (magit-diff-visit--offset file nil line)))
        (magit-buffer-revision
         (setq line (magit-diff-visit--offset
                     file (concat magit-buffer-revision ".." rev) line)))
        ((setq line (magit-diff-visit--offset file (list "-R" rev) line)))))
    (funcall fn buf)
    (when line
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)))
    buf))

(defun magit-find-file-noselect (rev file &optional revert)
  "Read FILE from REV into a buffer and return the buffer.
REV is a revision or one of \"{worktree}\" or \"{index}\".  FILE must
be relative to the top directory of the repository.  Non-nil REVERT
means to revert the buffer.  If `ask-revert', then only after asking.
A non-nil value for REVERT is ignored if REV is \"{worktree}\"."
  (setq file (expand-file-name file))
  (cond-let*
    ((equal rev "{worktree}")
     (let ((revert-without-query
            (if (and$ (find-buffer-visiting file)
                      (buffer-local-value 'auto-revert-mode $))
                (cons "." revert-without-query)
              revert-without-query)))
       (find-file-noselect file)))
    ([defdir (file-name-directory file)]
     [topdir (magit-toplevel defdir)]
     (setq rev (magit--abbrev-if-hash rev))
     (with-current-buffer (magit-get-revision-buffer-create
                           rev
                           (file-relative-name file topdir))
       (when (or (not magit-buffer-file-name)
                 (if (eq revert 'ask-revert)
                     (y-or-n-p (format "%s already exists; revert it? "
                                       (buffer-name))))
                 revert)
         (setq magit-buffer-revision rev)
         (setq magit-buffer-file-name file)
         (setq default-directory (if (file-exists-p defdir) defdir topdir))
         (setq-local revert-buffer-function #'magit-revert-rev-file-buffer)
         (revert-buffer t t)
         (run-hooks (if (equal rev "{index}")
                        'magit-find-index-hook
                      'magit-find-file-hook)))
       (current-buffer)))
    ((error "%s isn't inside a Git repository" file))))

(defun magit-get-revision-buffer-create (rev file)
  (magit-get-revision-buffer rev file t))

(defun magit-get-revision-buffer (rev file &optional create)
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun magit-revert-rev-file-buffer (_ignore-auto noconfirm)
  (when (or noconfirm
            (and (not (buffer-modified-p))
                 (catch 'found
                   (dolist (regexp revert-without-query)
                     (when (string-match regexp magit-buffer-file-name)
                       (throw 'found t)))))
            (yes-or-no-p (format "Revert buffer from Git %s? "
                                 (if (equal magit-buffer-revision "{index}")
                                     "index"
                                   (concat "revision " magit-buffer-revision)))))
    (let* ((inhibit-read-only t)
           (default-directory (magit-toplevel))
           (file (file-relative-name magit-buffer-file-name))
           (coding-system-for-read (or coding-system-for-read 'undecided)))
      (erase-buffer)
      (magit-git-insert "cat-file" "-p"
                        (if (equal magit-buffer-revision "{index}")
                            (concat ":" file)
                          (concat magit-buffer-revision ":" file)))
      (setq buffer-file-coding-system last-coding-system-used))
    (let ((buffer-file-name magit-buffer-file-name)
          (after-change-major-mode-hook
           (seq-difference after-change-major-mode-hook
                           '(global-diff-hl-mode-enable-in-buffer ; Emacs >= 30
                             global-diff-hl-mode-enable-in-buffers ; Emacs < 30
                             eglot--maybe-activate-editing-mode)
                           #'eq)))
      ;; We want `normal-mode' to respect nil `enable-local-variables'.
      ;; The FIND-FILE argument wasn't designed for our use case, so we
      ;; have to use this strange invocation to achieve that.
      (normal-mode (not enable-local-variables)))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(define-advice lsp (:around (fn &rest args) magit-find-file)
  "Do nothing when visiting blob using `magit-find-file' and similar.
See also https://github.com/doomemacs/doomemacs/pull/6309."
  (unless magit-buffer-revision
    (apply fn args)))

;;; Find Index

(defvar magit-find-index-hook nil)
(add-hook 'magit-find-index-hook #'magit-blob-mode)

(defun magit-find-file-index-noselect (file &optional revert)
  "Read FILE from the index into a buffer and return the buffer.
FILE must to be relative to the top directory of the repository."
  (magit-find-file-noselect "{index}" file (or revert 'ask-revert)))

(defun magit-update-index ()
  "Update the index with the contents of the current buffer.
The current buffer has to be visiting a file in the index, which
is done using `magit-find-index-noselect'."
  (interactive)
  (let ((file (magit-file-relative-name)))
    (unless (equal magit-buffer-revision "{index}")
      (user-error "%s isn't visiting the index" file))
    (if (y-or-n-p (format "Update index with contents of %s?" (buffer-name)))
        (let ((index (make-temp-name
                      (expand-file-name "magit-update-index-" (magit-gitdir))))
              (buffer (current-buffer)))
          (magit-run-before-change-functions file "un-/stage")
          (unwind-protect
              (progn
                (let ((coding-system-for-write buffer-file-coding-system))
                  (with-temp-file index
                    (insert-buffer-substring buffer)))
                (magit-with-toplevel
                  (magit-call-git
                   "update-index" "--cacheinfo"
                   (substring (magit-git-string "ls-files" "-s" file)
                              0 6)
                   (magit-git-string "hash-object" "-t" "blob" "-w"
                                     (concat "--path=" file)
                                     "--" (magit-convert-filename-for-git index))
                   file)))
            (ignore-errors (delete-file index)))
          (set-buffer-modified-p nil)
          (magit-run-after-apply-functions file "un-/stage"))
      (message "Abort")))
  (when-let ((buffer (magit-get-mode-buffer 'magit-status-mode)))
    (with-current-buffer buffer
      (magit-refresh)))
  t)

;;; Find Config File

(defun magit-find-git-config-file (filename &optional wildcards)
  "Edit a file located in the current repository's git directory.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file', except that it temporarily
binds `default-directory' to the actual git directory, while
reading the FILENAME."
  (interactive
    (let ((default-directory (magit-gitdir)))
      (find-file-read-args "Find file: "
                           (confirm-nonexistent-file-or-buffer))))
  (find-file filename wildcards))

(defun magit-find-git-config-file-other-window (filename &optional wildcards)
  "Edit a file located in the current repo's git directory, in another window.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-window', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
    (let ((default-directory (magit-gitdir)))
      (find-file-read-args "Find file in other window: "
                           (confirm-nonexistent-file-or-buffer))))
  (find-file-other-window filename wildcards))

(defun magit-find-git-config-file-other-frame (filename &optional wildcards)
  "Edit a file located in the current repo's git directory, in another frame.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-frame', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
    (let ((default-directory (magit-gitdir)))
      (find-file-read-args "Find file in other frame: "
                           (confirm-nonexistent-file-or-buffer))))
  (find-file-other-frame filename wildcards))

;;; File Dispatch

;;;###autoload(autoload 'magit-file-dispatch "magit" nil t)
(transient-define-prefix magit-file-dispatch ()
  "Invoke a Magit command that acts on the visited file.
When invoked outside a file-visiting buffer, then fall back
to `magit-dispatch'."
  :info-manual "(magit) Minor Mode for Buffers Visiting Files"
  [:if magit-file-relative-name
   ["File actions"
    ("  s" "Stage"    magit-file-stage :if-not-derived dired-mode)
    ("  s" "Stage"    magit-dired-stage :if-derived dired-mode)
    ("  u" "Unstage"  magit-file-unstage :if-not-derived dired-mode)
    ("  u" "Unstage"  magit-dired-unstage :if-derived dired-mode)
    (", x" "Untrack"  magit-file-untrack)
    (", r" "Rename"   magit-file-rename)
    (", k" "Delete"   magit-file-delete)
    (", c" "Checkout" magit-file-checkout)]
   ["Inspect"
    ("D" "Diff..."    magit-diff)
    ("d" "Diff"       magit-diff-buffer-file)]
   [""
    ("L" "Log..."     magit-log)
    ("l" "Log"        magit-log-buffer-file :if-not-derived dired-mode)
    ("l" "Log"        magit-dired-log :if-derived dired-mode)
    ("t" "Trace"      magit-log-trace-definition)
    ("M" "Merged"     magit-log-merged :level 7)]
   [""
    ("B" "Blame..."   magit-blame)
    ("b" "Blame"      magit-blame-addition)
    ("r" "...removal" magit-blame-removal)
    ("f" "...reverse" magit-blame-reverse)
    ("m" "Blame echo" magit-blame-echo)
    ("q" "Quit blame" magit-blame-quit)]
   ["Navigate"
    ("p" "Prev blob"   magit-blob-previous)
    ("n" "Next blob"   magit-blob-next)
    ("v" "Goto blob"   magit-find-file)
    ("V" "Goto file"   magit-blob-visit-file)
    ("g" "Goto status" magit-status-here)
    ("G" "Goto magit"  magit-display-repository-buffer)]
   ["More actions"
    ("c" "Commit"     magit-commit)
    ("e" "Edit line"  magit-edit-line-commit)]]
  [:if-not magit-file-relative-name
   ["File actions"
    ("s" "Stage"    magit-stage-files)
    ("u" "Unstage"  magit-unstage-files)
    ("x" "Untrack"  magit-file-untrack)
    ("r" "Rename"   magit-file-rename)
    ("k" "Delete"   magit-file-delete)
    ("c" "Checkout" magit-file-checkout)]
   ["Navigate"
    ("g" "Goto status" magit-status-here :if-not-mode magit-status-mode)
    ("G" "Goto magit"  magit-display-repository-buffer)]])

;;; Blob Mode

(defvar-keymap magit-blob-mode-map
  :doc "Keymap for `magit-blob-mode'."
  "p" #'magit-blob-previous
  "n" #'magit-blob-next
  "b" #'magit-blame-addition
  "r" #'magit-blame-removal
  "f" #'magit-blame-reverse
  "q" #'magit-bury-or-kill-buffer)

(define-minor-mode magit-blob-mode
  "Enable some Magit features in blob-visiting buffers.

Currently this only adds the following key bindings.
\n\\{magit-blob-mode-map}"
  :package-version '(magit . "2.3.0"))

(defun magit-bury-buffer (&optional kill-buffer)
  "Bury the current buffer, or with a prefix argument kill it."
  (interactive "P")
  (if kill-buffer (kill-buffer) (bury-buffer)))

(defun magit-bury-or-kill-buffer (&optional bury-buffer)
  "Bury the current buffer if displayed in multiple windows, else kill it.
With a prefix argument only bury the buffer even if it is only displayed
in a single window."
  (interactive "P")
  (if (or bury-buffer (cdr (get-buffer-window-list nil nil t)))
      (bury-buffer)
    (kill-buffer)))

(defun magit-kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer))

(transient-define-suffix magit-blob-previous ()
  "Visit the previous blob which modified the current file."
  :inapt-if-not (##and$ (magit-buffer-file-name)
                        (magit-blob-ancestor (magit-buffer-revision) $))
  (interactive)
  (cond-let
    [[rev  (or magit-buffer-revision "{worktree}")]
     [file (magit-buffer-file-name)]]
    ((not file)
     (user-error "Buffer isn't visiting a file or blob"))
    ([prev (magit-blob-ancestor rev file)]
     (apply #'magit-blob-visit prev))
    ((user-error "You have reached the beginning of time"))))

(transient-define-suffix magit-blob-next ()
  "Visit the next blob which modified the current file."
  :inapt-if-nil 'magit-buffer-file-name
  (interactive)
  (cond-let
    [[rev  (or magit-buffer-revision "{worktree}")]
     [file (magit-buffer-file-name)]]
    ((not file)
     (user-error "Buffer isn't visiting a file or blob"))
    ([next (magit-blob-successor rev file)]
     (apply #'magit-blob-visit next))
    ((user-error "You have reached the end of time"))))

;;;###autoload
(defun magit-blob-visit-file ()
  "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree."
  (interactive)
  (if-let ((file (magit-file-relative-name)))
      (magit-find-file--internal "{worktree}" file #'pop-to-buffer-same-window)
    (user-error "Not visiting a blob")))

(defun magit-blob-visit (rev file)
  (magit-find-file rev file)
  (unless (member rev '("{worktree}" "{index}"))
    (apply #'message "%s (%s %s ago)"
           (magit-rev-format "%s" rev)
           (magit--age (magit-rev-format "%ct" rev)))))

(defun magit-blob-ancestor (rev file)
  (pcase rev
    ((and "{worktree}" (guard (magit-anything-staged-p nil file)))
     (list "{index}" file))
    ((or "{worktree}" "{index}")
     (list (magit-rev-abbrev "HEAD") file))
    (_ (nth (if rev 1 0)
            (magit-with-toplevel
              (seq-partition
               (magit-git-lines "log" "-2" "--format=%h" "--name-only"
                                "--follow" (or rev "HEAD") "--" file)
               2))))))

(defun magit-blob-successor (rev file)
  (pcase rev
    ("{worktree}" nil)
    ("{index}" (list "{worktree}" file))
    (_ (let ((lines (magit-with-toplevel
                      (magit-git-lines "log" "--format=%h" "--name-only"
                                       "--follow" "HEAD" "--" file))))
         (catch 'found
           (while lines
             (if (equal (nth 2 lines) rev)
                 (throw 'found (list (nth 0 lines) (nth 1 lines)))
               (setq lines (nthcdr 2 lines))))
           (list (if (magit-anything-staged-p nil file) "{index}" "{worktree}")
                 file))))))

;;; File Commands

;;;###autoload
(defun magit-file-stage ()
  "Stage all changes to the file being visited in the current buffer."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (magit-with-toplevel
    (magit-stage-1 (and (magit-file-ignored-p buffer-file-name)
                        (if (y-or-n-p "Visited file is ignored; stage anyway?")
                            "--force"
                          (user-error "Abort")))
                   (list (magit-file-relative-name)))))

;;;###autoload
(defun magit-file-unstage ()
  "Unstage all changes to the file being visited in the current buffer."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (magit-with-toplevel
    (magit-unstage-1 (list (magit-file-relative-name)))))

;;;###autoload
(defun magit-file-untrack (files &optional force)
  "Untrack the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
staged as well as unstaged changes."
  (interactive (list (or (if-let ((files (magit-region-values 'file t)))
                             (if (magit-file-tracked-p (car files))
                                 (magit-confirm-files 'untrack files "Untrack")
                               (user-error "Already untracked"))
                           (list (magit-read-tracked-file "Untrack file"))))
                     current-prefix-arg))
  (magit-with-toplevel
    (magit-run-git "rm" "--cached" (and force "--force") "--" files)))

;;;###autoload
(defun magit-file-rename (file newname)
  "Rename or move FILE to NEWNAME.
NEWNAME may be a file or directory name.  If FILE isn't tracked in
Git, fallback to using `rename-file'."
  (interactive
    (let* ((file (magit-read-file "Rename file"))
           (path (expand-file-name file (magit-toplevel))))
      (list path (expand-file-name
                  (read-file-name (format "Move %s to destination: " file)
                                  (file-name-directory path))))))
  (let ((oldbuf (get-file-buffer file))
        (dstdir (file-name-directory newname))
        (dstfile (if (directory-name-p newname)
                     (concat newname (file-name-nondirectory file))
                   newname)))
    (when (and oldbuf (buffer-modified-p oldbuf))
      (user-error "Save %s before moving it" file))
    (when (file-exists-p dstfile)
      (user-error "%s already exists" dstfile))
    (unless (file-exists-p dstdir)
      (user-error "Destination directory %s does not exist" dstdir))
    (if (magit-file-tracked-p file)
        (magit-call-git "mv"
                        (magit-convert-filename-for-git file)
                        (magit-convert-filename-for-git newname))
      (rename-file file newname current-prefix-arg))
    (when oldbuf
      (with-current-buffer oldbuf
        (let ((buffer-read-only buffer-read-only))
          (set-visited-file-name dstfile nil t))
        (if (fboundp 'vc-refresh-state)
            (vc-refresh-state)
          (with-no-warnings
            (vc-find-file-hook))))))
  (magit-refresh))

;;;###autoload
(defun magit-file-delete (files &optional force)
  "Delete the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
uncommitted changes.  When the files aren't being tracked in
Git, then fallback to using `delete-file'."
  (interactive (list (if-let ((files (magit-region-values 'file t)))
                         (magit-confirm-files 'delete files "Delete")
                       (list (magit-read-file "Delete file")))
                     current-prefix-arg))
  (if (magit-file-tracked-p (car files))
      (magit-call-git "rm" (and force "--force") "--" files)
    (let ((topdir (magit-toplevel)))
      (dolist (file files)
        (delete-file (expand-file-name file topdir) t))))
  (magit-refresh))

;;;###autoload
(defun magit-file-checkout (rev file)
  "Checkout FILE from REV."
  (interactive
    (let ((rev (magit-read-branch-or-commit
                "Checkout from revision" magit-buffer-revision)))
      (list rev (magit-read-file-from-rev rev "Checkout file" nil t))))
  (magit-with-toplevel
    (magit-run-git "checkout" rev "--" file)))

;;; Read File

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (rev prompt &optional default include-dirs)
  (let ((files (magit-revision-files rev)))
    (when include-dirs
      (setq files (sort (nconc files (magit-revision-directories rev))
                        #'string<)))
    (magit-completing-read
     prompt files nil t nil 'magit-read-file-hist
     (car (member (or default (magit-current-file)) files)))))

(defun magit-read-file (prompt &optional tracked-only)
  (magit-with-toplevel
    (let ((choices (nconc (magit-list-files)
                          (and (not tracked-only)
                               (magit-untracked-files)))))
      (magit-completing-read
       prompt choices nil t nil nil
       (car (member (or (magit-section-value-if '(file submodule))
                        (magit-file-relative-name nil tracked-only))
                    choices))))))

(defun magit-read-tracked-file (prompt)
  (magit-read-file prompt t))

(defun magit-read-unmerged-file (&optional prompt)
  (let ((current  (magit-current-file))
        (unmerged (magit-unmerged-files)))
    (unless unmerged
      (user-error "There are no unresolved conflicts"))
    (magit-completing-read (or prompt "Resolve file")
                           unmerged nil t nil nil
                           (car (member current unmerged)))))

(defun magit-read-file-choice (prompt files &optional error default)
  "Read file from FILES.

If FILES has only one member, return that instead of prompting.
If FILES has no members, give a user error.  ERROR can be given
to provide a more informative error.

If DEFAULT is non-nil, use this as the default value instead of
`magit-current-file'."
  (pcase (length files)
    (0 (user-error (or error "No file choices")))
    (1 (car files))
    (_ (magit-completing-read
        prompt files nil t nil 'magit-read-file-hist
        (car (member (or default (magit-current-file)) files))))))

(defun magit-read-changed-file (rev-or-range prompt &optional default)
  (magit-read-file-choice
   prompt
   (magit-changed-files rev-or-range)
   default
   (concat "No file changed in " rev-or-range)))

;;; _

(define-obsolete-function-alias 'magit-stage-buffer-file
  'magit-file-stage "Magit 4.3.2")

(define-obsolete-function-alias 'magit-unstage-buffer-file
  'magit-file-unstage "Magit 4.3.2")

(define-obsolete-function-alias 'magit-find-file-noselect-1
  'magit-find-file-noselect "Magit 4.4.0")

(provide 'magit-files)
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
;;; magit-files.el ends here
