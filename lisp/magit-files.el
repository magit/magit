;;; magit-files.el --- finding files  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
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
Switch to a buffer visiting blob REV:FILE,
creating one if none already exists."
  (interactive (magit-find-file-read-args "Find file"))
  (switch-to-buffer (magit-find-file-noselect rev file)))

;;;###autoload
(defun magit-find-file-other-window (rev file)
  "View FILE from REV, in another window.
Like `magit-find-file', but create a new window or reuse an
existing one."
  (interactive (magit-find-file-read-args "Find file in other window"))
  (switch-to-buffer-other-window (magit-find-file-noselect rev file)))

(defun magit-find-file-read-args (prompt)
  (let  ((rev (magit-read-branch-or-commit "Find file from revision")))
    (list rev (magit-read-file-from-rev rev prompt))))

(defun magit-find-file-noselect (rev file)
  "Read FILE from REV into a buffer and return the buffer.
FILE must be relative to the top directory of the repository."
  (magit-find-file-noselect-1 rev file 'magit-find-file-hook))

(defun magit-find-file-noselect-1 (rev file hookvar &optional revert)
  "Read FILE from REV into a buffer and return the buffer.
FILE must be relative to the top directory of the repository.
An empty REV stands for index."
  (let ((topdir (magit-toplevel)))
    (when (file-name-absolute-p file)
      (setq file (file-relative-name file topdir)))
    (with-current-buffer (magit-get-revision-buffer-create rev file)
      (when (or (not magit-buffer-file-name)
                (if (eq revert 'ask-revert)
                    (y-or-n-p (format "%s already exists; revert it? "
                                      (buffer-name))))
                revert)
        (setq magit-buffer-revision
              (if (string= rev "") "{index}" (magit-rev-format "%H" rev)))
        (setq magit-buffer-refname rev)
        (setq magit-buffer-file-name (expand-file-name file topdir))
        (setq default-directory
              (let ((dir (file-name-directory magit-buffer-file-name)))
                (if (file-exists-p dir) dir topdir)))
        (setq-local revert-buffer-function #'magit-revert-rev-file-buffer)
        (revert-buffer t t)
        (run-hooks hookvar))
      (current-buffer))))

(defun magit-get-revision-buffer-create (rev file)
  (magit-get-revision-buffer rev file t))

(defun magit-get-revision-buffer (rev file &optional create)
  (funcall (if create 'get-buffer-create 'get-buffer)
           (format "%s.~%s~" file (if (equal rev "") "index"
                                    (subst-char-in-string ?/ ?_ rev)))))

(defun magit-revert-rev-file-buffer (_ignore-auto noconfirm)
  (when (or noconfirm
            (and (not (buffer-modified-p))
                 (catch 'found
                   (dolist (regexp revert-without-query)
                     (when (string-match regexp magit-buffer-file-name)
                       (throw 'found t)))))
            (yes-or-no-p (format "Revert buffer from git %s? "
                                 (if (equal magit-buffer-refname "") "{index}"
                                   (concat "revision " magit-buffer-refname)))))
    (let* ((inhibit-read-only t)
           (default-directory (magit-toplevel))
           (file (file-relative-name magit-buffer-file-name))
           (coding-system-for-read (or coding-system-for-read 'undecided)))
      (erase-buffer)
      (magit-git-insert "cat-file" "-p" (concat magit-buffer-refname ":" file))
      (setq buffer-file-coding-system last-coding-system-used))
    (let ((buffer-file-name magit-buffer-file-name)
          (after-change-major-mode-hook
           (remq 'global-diff-hl-mode-enable-in-buffers
                 after-change-major-mode-hook)))
      (normal-mode t))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

;;; Find Index

(defvar magit-find-index-hook nil)

(defun magit-find-file-index-noselect (file &optional revert)
  "Read FILE from the index into a buffer and return the buffer.
FILE must to be relative to the top directory of the repository."
  (magit-find-file-noselect-1 "" file 'magit-find-index-hook
                              (or revert 'ask-revert)))

(defun magit-update-index ()
  "Update the index with the contents of the current buffer.
The current buffer has to be visiting a file in the index, which
is done using `magit-find-index-noselect'."
  (interactive)
  (let ((file (magit-file-relative-name)))
    (unless (equal magit-buffer-refname "")
      (user-error "%s isn't visiting the index" file))
    (if (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
        (let ((index (make-temp-file "index"))
              (buffer (current-buffer)))
          (when magit-wip-before-change-mode
            (magit-wip-commit-before-change (list file) " before un-/stage"))
          (let ((coding-system-for-write buffer-file-coding-system))
            (with-temp-file index
              (insert-buffer-substring buffer)))
          (magit-with-toplevel
            (magit-call-git "update-index" "--cacheinfo"
                            (substring (magit-git-string "ls-files" "-s" file)
                                       0 6)
                            (magit-git-string "hash-object" "-t" "blob" "-w"
                                              (concat "--path=" file)
                                              "--" index)
                            file))
          (set-buffer-modified-p nil)
          (when magit-wip-after-apply-mode
            (magit-wip-commit-after-apply (list file) " after un-/stage")))
      (message "Abort")))
  (--when-let (magit-mode-get-buffer 'magit-status-mode)
    (with-current-buffer it (magit-refresh)))
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
   (let ((default-directory (magit-git-dir)))
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file filename wildcards))

(defun magit-find-git-config-file-other-window (filename &optional wildcards)
  "Edit a file located in the current repository's git directory, in another window.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-window', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
   (let ((default-directory (magit-git-dir)))
     (find-file-read-args "Find file in other window: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file-other-window filename wildcards))

(defun magit-find-git-config-file-other-frame (filename &optional wildcards)
  "Edit a file located in the current repository's git directory, in another frame.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-frame', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
   (let ((default-directory (magit-git-dir)))
     (find-file-read-args "Find file in other frame: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file-other-frame filename wildcards))

;;; File Mode

(defvar magit-file-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-xg"    'magit-status)
    (define-key map "\C-x\M-g" 'magit-dispatch-popup)
    (define-key map "\C-c\M-g" 'magit-file-popup)
    map)
  "Keymap for `magit-file-mode'.")

;;;###autoload (autoload 'magit-file-popup "magit" nil t)
(magit-define-popup magit-file-popup
  "Popup console for Magit commands in file-visiting buffers."
  :actions '((?s "Stage"     magit-stage-file)
             (?D "Diff..."   magit-diff-buffer-file-popup)
             (?L "Log..."    magit-log-buffer-file-popup)
             (?B "Blame..."  magit-blame-popup) nil
             (?u "Unstage"   magit-unstage-file)
             (?d "Diff"      magit-diff-buffer-file)
             (?l "Log"       magit-log-buffer-file)
             (?b "Blame"     magit-blame)
             (?p "Prev blob" magit-blob-previous)
             (?c "Commit"    magit-commit-popup) nil nil
             (?r (lambda ()
                   (with-current-buffer magit-pre-popup-buffer
                     (and (not buffer-file-name)
                          (propertize "...reverse" 'face 'default))))
                 magit-blame-reverse)
             (?n "Next blob" magit-blob-next))
  :max-action-columns 5)

(defvar magit-file-mode-lighter "")

(define-minor-mode magit-file-mode
  "Enable some Magit features in a file-visiting buffer.

Currently this only adds the following key bindings.
\n\\{magit-file-mode-map}"
  :package-version '(magit . "2.2.0")
  :lighter magit-file-mode-lighter
  :keymap  magit-file-mode-map)

(defun magit-file-mode-turn-on ()
  (and buffer-file-name
       (ignore-errors (magit-inside-worktree-p))
       (magit-file-mode)))

;;;###autoload
(define-globalized-minor-mode global-magit-file-mode
  magit-file-mode magit-file-mode-turn-on
  :package-version '(magit . "2.2.0")
  :link '(info-link "(magit)Minor Mode for Buffers Visiting Files")
  :group 'magit-essentials
  :group 'magit-modes)

;;; Blob Mode

(defvar magit-blob-mode-map
  (let ((map (make-sparse-keymap)))
    (cond ((featurep 'jkl)
           (define-key map "i" 'magit-blob-previous)
           (define-key map "k" 'magit-blob-next)
           (define-key map "j" 'magit-blame)
           (define-key map "l" 'magit-blame-reverse))
          (t
           (define-key map "p" 'magit-blob-previous)
           (define-key map "n" 'magit-blob-next)
           (define-key map "b" 'magit-blame)
           (define-key map "f" 'magit-blame-reverse)))
    (define-key map "q" 'magit-kill-this-buffer)
    map)
  "Keymap for `magit-blob-mode'.")

(define-minor-mode magit-blob-mode
  "Enable some Magit features in blob-visiting buffers.

Currently this only adds the following key bindings.
\n\\{magit-blob-mode-map}"
  :package-version '(magit . "2.3.0"))

(defun magit-blob-next ()
  "Visit the next blob which modified the current file."
  (interactive)
  (if magit-buffer-file-name
      (magit-blob-visit (or (magit-blob-successor magit-buffer-revision
                                                  magit-buffer-file-name)
                            magit-buffer-file-name)
                        (line-number-at-pos))
    (if (buffer-file-name (buffer-base-buffer))
        (user-error "You have reached the end of time")
      (user-error "Buffer isn't visiting a file or blob"))))

(defun magit-blob-previous ()
  "Visit the previous blob which modified the current file."
  (interactive)
  (-if-let (file (or magit-buffer-file-name
                     (buffer-file-name (buffer-base-buffer))))
      (--if-let (magit-blob-ancestor magit-buffer-revision file)
          (magit-blob-visit it (line-number-at-pos))
        (user-error "You have reached the beginning of time"))
    (user-error "Buffer isn't visiting a file or blob")))

(defun magit-blob-visit (blob-or-file line)
  (if (stringp blob-or-file)
      (find-file blob-or-file)
    (-let [(rev file) blob-or-file]
      (magit-find-file rev file)
      (apply #'message "%s (%s %s ago)"
             (magit-rev-format "%s" rev)
             (magit--age (magit-rev-format "%ct" rev)))))
  (goto-char (point-min))
  (forward-line (1- line)))

(defun magit-blob-ancestor (rev file)
  (let ((lines (magit-with-toplevel
                 (magit-git-lines "log" "-2" "--format=%H" "--name-only"
                                  "--follow" (or rev "HEAD") "--" file))))
    (if rev (cddr lines) (butlast lines 2))))

(defun magit-blob-successor (rev file)
  (let ((lines (magit-with-toplevel
                 (magit-git-lines "log" "--format=%H" "--name-only" "--follow"
                                  "HEAD" "--" file))))
    (catch 'found
      (while lines
        (if (equal (nth 2 lines) rev)
            (throw 'found (list (nth 0 lines) (nth 1 lines)))
          (setq lines (nthcdr 2 lines)))))))

;;; File Commands

(defun magit-file-rename (file newname)
  "Rename the FILE to NEWNAME.
If FILE isn't tracked in Git, fallback to using `rename-file'."
  (interactive
   (let* ((file (magit-read-file "Rename file"))
          (newname (read-file-name (format "Rename %s to file: " file))))
     (list (expand-file-name file (magit-toplevel))
           (expand-file-name newname))))
  (if (magit-file-tracked-p file)
      (let ((oldbuf (get-file-buffer file)))
        (when (and oldbuf (buffer-modified-p oldbuf))
          (user-error "Save %s before moving it" file))
        (when (file-exists-p newname)
          (user-error "%s already exists" newname))
        (magit-run-git "mv" file newname)
        (when oldbuf
          (with-current-buffer oldbuf
            (let ((buffer-read-only buffer-read-only))
              (set-visited-file-name newname))
            (if (fboundp 'vc-refresh-state)
                (vc-refresh-state)
              (with-no-warnings
                (vc-find-file-hook))))))
    (rename-file file newname current-prefix-arg)
    (magit-refresh)))

(defun magit-file-untrack (files &optional force)
  "Untrack the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
staged as well as unstaged changes."
  (interactive (list (or (--if-let (magit-region-values 'file t)
                             (progn
                               (or (magit-file-tracked-p (car it))
                                   (user-error "Already untracked"))
                               (or (magit-confirm-files 'untrack it "Untrack")
                                   (user-error "Abort")))
                           (list (magit-read-tracked-file "Untrack file"))))
                     current-prefix-arg))
  (magit-run-git "rm" "--cached" (and force "--force") "--" files))

(defun magit-file-delete (files &optional force)
  "Delete the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
uncommitted changes.  When the files aren't being tracked in
Git, then fallback to using `delete-file'."
  (interactive (list (--if-let (magit-region-values 'file t)
                         (or (magit-confirm-files 'delete it "Delete")
                             (user-error "Abort"))
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
     (list rev (magit-read-file-from-rev rev "Checkout file"))))
  (magit-with-toplevel
    (magit-run-git "checkout" rev "--" file)))

;;; Read File

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (rev prompt &optional default)
  (let ((files (magit-revision-files rev)))
    (magit-completing-read
     prompt files nil t nil 'magit-read-file-hist
     (car (member (or default (magit-current-file)) files)))))

(defun magit-read-file (prompt &optional tracked-only)
  (let ((choices (nconc (magit-list-files)
                        (unless tracked-only (magit-untracked-files)))))
    (magit-completing-read prompt choices nil t nil nil
                           (car (member (or (magit-section-when (file submodule))
                                            (magit-file-relative-name
                                             nil tracked-only))
                                        choices)))))

(defun magit-read-tracked-file (prompt)
  (magit-read-file prompt t))

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

(defun magit-read-files (prompt initial-contents)
  (mapconcat 'identity
             (completing-read-multiple (or prompt "File,s: ")
                                       (magit-list-files)
                                       nil nil initial-contents) ","))

;;; Patch File

(defcustom magit-patch-save-arguments '(exclude "--stat")
  "Arguments used by `magit-patch-save-arguments' (which see)"
  :package-version '(magit . "2.12.0")
  :group 'magit-diff
  :type '(choice (const :tag "use buffer arguments" buffer)
                 (cons :tag "use buffer arguments except"
                       (const :format "" exclude)
                       (repeat :format "%v%i\n"
                               (string :tag "Argument")))
                 (repeat :tag "use constant arguments"
                         (string :tag "Argument"))))

(magit-define-popup magit-patch-apply-popup
  "Popup console for applying a patch file."
  :man-page "git-apply"
  :switches '((?i "Also apply to index"     "--index")
              (?c "Only apply to index"     "--cached")
              (?3 "Fall back on 3way merge" "--3way"))
  :actions  '((?a "Apply patch" magit-patch-apply))
  :default-action 'magit-patch-apply)

(defun magit-patch-apply (file &rest args)
  "Apply the patch file FILE."
  (interactive (list (expand-file-name
                      (read-file-name "Apply patch: "
                                      default-directory nil nil
                                      (--when-let (magit-file-at-point)
                                        (file-relative-name it))))
                     (magit-patch-apply-arguments)))
  (magit-run-git "apply" args "--" file))

(defun magit-patch-save (file &optional arg)
  "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used."
  (interactive (list (read-file-name "Write patch file: " default-directory)
                     current-prefix-arg))
  (unless (derived-mode-p 'magit-diff-mode)
    (user-error "Only diff buffers can be saved as patches"))
  (pcase-let ((`(,rev ,const ,args ,files) magit-refresh-args))
    (when (derived-mode-p 'magit-revision-mode)
      (setq rev (format "%s~..%s" rev rev)))
    (cond ((eq magit-patch-save-arguments 'buffer)
           (when arg
             (setq args nil)))
          ((eq (car-safe magit-patch-save-arguments) 'exclude)
           (unless arg
             (setq args (-difference args (cdr magit-patch-save-arguments)))))
          ((not arg)
           (setq args magit-patch-save-arguments)))
    (with-temp-file file
      (magit-git-insert "diff" rev "-p" const args "--" files)))
  (magit-refresh))

(provide 'magit-files)
;;; magit-files.el ends here
