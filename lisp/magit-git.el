;;; magit-git.el --- Git functionality

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

;; This library implements wrappers for various Git plumbing commands.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'magit-utils)
(require 'magit-section)

(declare-function magit-process-buffer 'magit-process)
(declare-function magit-process-insert-section 'magit-process)
(defvar magit-process-error-message-re)
(defvar magit-refresh-args) ; from `magit-mode' for `magit-current-file'

(defvar magit-tramp-process-environment nil)

(require 'crm)

;;; Options

;; For now this is shared between `magit-process' and `magit-git'.
(defgroup magit-process nil
  "Git and other external processes used by Magit."
  :group 'magit)

(defcustom magit-git-executable
  ;; Git might be installed in a different location on a remote, so
  ;; it is better not to use the full path to the executable, except
  ;; on Window were we would otherwise end up using one one of the
  ;; wrappers "cmd/git.exe" or "cmd/git.cmd", which are much slower
  ;; than using "bin/git.exe" directly.
  (or (and (eq system-type 'windows-nt)
           (--when-let (executable-find "git.exe")
             (let ((alt (directory-file-name (file-name-directory it))))
               (if (and (equal (file-name-nondirectory alt) "cmd")
                        (setq alt (expand-file-name
                                   (convert-standard-filename "bin/git.exe")
                                   (file-name-directory alt)))
                        (file-executable-p alt))
                   alt
                 it))))
      "git")
  "The Git executable used by Magit."
  :group 'magit-process
  :type 'string)

(defcustom magit-git-global-arguments
  '("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true")
  "Global git arguments.

The arguments set here are used every time the git executable is
run as a subprocess.  They are placed right after the executable
itself and before the git command - as in `git HERE... COMMAND
REST'.  See the manpage `git(1)' for valid arguments.

Be careful what you add here, especially if you are using Tramp
to connect to servers with ancient Git versions.  Never remove
anything that is part of the default value, unless you really
know what you are doing.  And think very hard before adding
something; it will be used every time Magit runs Git for any
purpose."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type '(repeat string))

(define-obsolete-variable-alias 'magit-git-standard-options
  'magit-git-global-arguments "2.1.0")

(defcustom magit-git-debug nil
  "Whether to enable additional reporting of Git errors.

Magit basically calls Git for one of these two reasons: for
side-effects or to do something with its standard output.

When Git is run for side-effects then its output, including error
messages, go into the process buffer which is shown when using \
\\<magit-status-mode-map>\\[magit-process].

When Git's output is consumed in some way, then it would be too
expensive to also insert it into this buffer, but when this
option is non-nil and Git returns with a non-zero exit status,
then at least its standard error is inserted into this buffer."
  :group 'magit
  :group 'magit-process
  :type 'boolean)

(defcustom magit-ref-namespaces
  '(("^@$"                       magit-head nil)
    ("^refs/tags/\\(.+\\)"       magit-tag nil)
    ("^refs/heads/\\(.+\\)"      magit-branch-local nil)
    ("^refs/remotes/\\(.+\\)"    magit-branch-remote nil)
    ("^refs/bisect/\\(bad\\)"    magit-bisect-bad nil)
    ("^refs/bisect/\\(skip.*\\)" magit-bisect-skip nil)
    ("^refs/bisect/\\(good.*\\)" magit-bisect-good nil)
    ("^refs/stash$"              magit-refname-stash nil)
    ("^refs/wip/\\(.+\\)"        magit-refname-wip nil)
    ("^\\(bad\\):"               magit-bisect-bad nil)
    ("^\\(skip\\):"              magit-bisect-skip nil)
    ("^\\(good\\):"              magit-bisect-good nil)
    ("\\(.+\\)"                  magit-refname nil))
  "How different refs should be formatted for display.

Each entry controls how a certain type of ref is displayed, and
has the form (REGEXP FACE FORMATTER).  REGEXP is a regular
expression used to match full refs.  The first entry whose REGEXP
matches the reference is used.  The first regexp submatch becomes
the \"label\" that represents the ref and is propertized with
font FONT.  If FORMATTER is non-nil it should be a function that
takes two arguments, the full ref and the face.  It is supposed
to return a propertized label that represents the ref."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type '(repeat
          (list regexp
                face
                (choice (const :tag "first submatch is label" nil)
                        (function :tag "format using function")))))

;;; Git

(defun magit-process-git-arguments (args)
  "Prepare ARGS for a function that invokes Git.

Magit has many specialized functions for running Git; they all
pass arguments through this function before handing them to Git,
to do the following.

* Flatten ARGS, removing nil arguments.
* Prepend `magit-git-global-arguments' to ARGS.
* Quote arguments as required when using Powershell together
  with Cygwin Git.  See #816."
  (setq args (-flatten args))
  (when (and (eq system-type 'windows-nt)
             (let ((case-fold-search t))
               (string-match-p "cygwin" magit-git-executable)))
    (setq args (--map (replace-regexp-in-string
                       "{\\([0-9]+\\)}" "\\\\{\\1\\\\}" it)
                      args)))
  (append magit-git-global-arguments args))

(defun magit-git-exit-code (&rest args)
  "Execute Git with ARGS, returning its exit code."
  (apply #'process-file magit-git-executable nil nil nil
         (magit-process-git-arguments args)))

(defun magit-git-success (&rest args)
  "Execute Git with ARGS, returning t if its exit code is 0."
  (= (magit-git-exit-code args) 0))

(defun magit-git-failure (&rest args)
  "Execute Git with ARGS, returning t if its exit code is 1."
  (= (magit-git-exit-code args) 1))

(defun magit-git-str (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string.  Like `magit-git-string' but
ignore `magit-git-debug'."
  (with-temp-buffer
    (apply #'process-file magit-git-executable nil (list t nil) nil
           (magit-process-git-arguments args))
    (unless (bobp)
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun magit-git-true (&rest args)
  "Execute Git with ARGS, returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (equal (magit-git-str args) "true"))

(defun magit-git-false (&rest args)
  "Execute Git with ARGS, returning t if it prints \"false\".
Return t if the first (and usually only) output line is the
string \"false\", otherwise return nil."
  (equal (magit-git-str args) "false"))

(defun magit-git-insert (&rest args)
  "Execute Git with ARGS, inserting its output at point.
If Git exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (setq args (magit-process-git-arguments args))
  (if magit-git-debug
      (let (log)
        (unwind-protect
            (progn
              (setq log (make-temp-file "magit-stderr"))
              (delete-file log)
              (let ((exit (apply #'process-file magit-git-executable
                                 nil (list t log) nil args)))
                (when (> exit 0)
                  (let ((msg "Git failed"))
                    (when (file-exists-p log)
                      (setq msg (with-temp-buffer
                                  (insert-file-contents log)
                                  (goto-char (point-max))
                                  (and (re-search-backward
                                        magit-process-error-message-re nil t)
                                       (match-string 1))))
                      (let ((magit-git-debug nil))
                        (with-current-buffer (magit-process-buffer nil t)
                          (magit-process-insert-section default-directory
                                                        magit-git-executable
                                                        args exit log))))
                    (message "%s" msg)))
                exit))
          (ignore-errors (delete-file log))))
    (apply #'process-file magit-git-executable nil (list t nil) nil args)))

(defun magit-git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (with-temp-buffer
    (apply #'magit-git-insert args)
    (unless (bobp)
      (goto-char (point-min))
      (buffer-substring-no-properties (point) (line-end-position)))))

(defun magit-git-lines (&rest args)
  "Execute Git with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted.

If Git exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (with-temp-buffer
    (apply #'magit-git-insert args)
    (split-string (buffer-string) "\n" t)))

(defun magit-git-items (&rest args)
  "Execute Git with ARGS, returning its null-separated output as a list.
Empty items anywhere in the output are omitted.

If Git exits with a non-zero exit status, then report show a
message and add a section in the respective process buffer."
  (with-temp-buffer
    (apply #'magit-git-insert args)
    (split-string (buffer-string) "\0" t)))

(defun magit-git-wash (washer &rest args)
  "Execute Git with ARGS, inserting washed output at point.
Actually first insert the raw output at point.  If there is no
output call `magit-cancel-section'.  Otherwise temporarily narrow
the buffer to the inserted text, move to its beginning, and then
call function WASHER with no argument."
  (declare (indent 1))
  (let ((beg (point)))
    (magit-git-insert args)
    (if (= (point) beg)
        (magit-cancel-section)
      (unless (bolp)
        (insert "\n"))
      (save-restriction
        (narrow-to-region beg (point))
        (goto-char beg)
        (funcall washer args))
      (when (or (= (point) beg)
                (= (point) (1+ beg)))
        (magit-cancel-section)))))

;;; Files

(defmacro magit--with-safe-default-directory (file &rest body)
  (declare (indent 1))
  `(catch 'unsafe-default-dir
     (let ((default-directory
             (let ((file ,file))
               (file-name-as-directory (if file
                                           (expand-file-name file)
                                         default-directory)))))
       (while (not (file-accessible-directory-p default-directory))
         (when (string-equal default-directory "/")
           (throw 'unsafe-default-dir nil))
         (setq default-directory
               (file-name-directory
                (directory-file-name default-directory))))
       ,@body)))

(defun magit-git-dir (&optional path)
  "Return absolute path to the GIT_DIR for the current repository.
If optional PATH is non-nil it has to be a path relative to the
GIT_DIR and its absolute path is returned."
  (magit--with-safe-default-directory nil
    (--when-let (magit-rev-parse-safe "--git-dir")
      (setq it (file-name-as-directory (magit-expand-git-file-name it)))
      (if path (expand-file-name (convert-standard-filename path) it) it))))

(defun magit-toplevel (&optional file strict)
  (magit--with-safe-default-directory file
    (-if-let (cdup (magit-rev-parse-safe "--show-cdup"))
        (magit-expand-git-file-name
         (file-name-as-directory (expand-file-name cdup)))
      (unless strict
        (-when-let (gitdir (magit-git-dir))
          (if (magit-bare-repo-p)
              gitdir
            (file-name-directory (directory-file-name gitdir))))))))

(defmacro magit-with-toplevel (&rest body)
  (declare (indent defun))
  (let ((toplevel (cl-gensym "toplevel")))
    `(let ((,toplevel (magit-toplevel)))
       (if ,toplevel
           (let ((default-directory ,toplevel))
             ,@body)
         (error "Not inside a Git repository: %s" default-directory)))))

(defun magit-inside-gitdir-p ()
  "Return t if `default-directory' is below a repository directory."
  (magit-rev-parse-p "--is-inside-git-dir"))

(defun magit-inside-worktree-p ()
  "Return t if `default-directory' is below the work tree of a repository."
  (magit-rev-parse-p "--is-inside-work-tree"))

(defun magit-bare-repo-p ()
  "Return t if the current repository is bare."
  (magit-rev-parse-p "--is-bare-repository"))

(defun magit-git-repo-p (directory &optional non-bare)
  "Return t if DIRECTORY is a Git repository.
When optional NON-BARE is non-nil also return nil if DIRECTORY is
a bare repositories."
  (or (file-regular-p (expand-file-name ".git" directory))
      (file-directory-p (expand-file-name ".git" directory))
      (and (not non-bare)
           (file-regular-p (expand-file-name "HEAD" directory))
           (file-directory-p (expand-file-name "refs" directory))
           (file-directory-p (expand-file-name "objects" directory)))))

(defvar-local magit-buffer-revision  nil)
(defvar-local magit-buffer-refname   nil)
(defvar-local magit-buffer-file-name nil)
(put 'magit-buffer-revision  'permanent-local t)
(put 'magit-buffer-refname   'permanent-local t)
(put 'magit-buffer-file-name 'permanent-local t)

(defun magit-file-relative-name (&optional file)
  "Return the path of FILE relative to the repository root.
If optional FILE is nil or omitted return the relative path of
the file being visited in the current buffer, if any, else nil.
If the file is not inside a Git repository then return nil."
  (unless file
    (with-current-buffer (or (buffer-base-buffer)
                             (current-buffer))
      (setq file (or magit-buffer-file-name buffer-file-name))))
  (when file
    (--when-let (magit-toplevel file)
      (file-relative-name file it))))

(defun magit-file-tracked-p (file)
  (magit-git-success "ls-files" "--error-unmatch" file))

(defun magit-list-files (&rest args)
  (apply #'magit-git-items "ls-files" "-z" "--full-name" args))

(defun magit-tracked-files ()
  (magit-list-files "--cached"))

(defun magit-untracked-files (&optional all)
  (magit-list-files "--other" (unless all "--exclude-standard")))

(defun magit-modified-files (&optional nomodules)
  (magit-git-items "diff-files" "-z" "--name-only"
                   (and nomodules "--ignore-submodules")))

(defun magit-staged-files (&optional nomodules)
  (magit-git-items "diff-index" "-z" "--name-only" "--cached"
                   (and nomodules "--ignore-submodules")
                   (magit-headish)))

(defun magit-staged-binary-files ()
  (--mapcat (and (string-match "^-\t-\t\\(.+\\)" it)
                 (list (match-string 1 it)))
            (magit-git-items "diff" "-z" "--cached"
                             "--numstat" "--ignore-submodules")))

(defun magit-unmerged-files ()
  (magit-git-items "diff-files" "-z" "--name-only" "--diff-filter=U"))

(defun magit-revision-files (rev)
  (let ((default-directory (magit-toplevel)))
    (magit-git-items "ls-tree" "-z" "-r" "--name-only" rev)))

(defun magit-changed-files (rev-or-range &optional other-rev)
  "Return list of files the have changed between two revisions.
If OTHER-REV is non-nil, REV-OR-RANGE should be a revision, not a
range.  Otherwise, it can be any revision or range accepted by
\"git diff\" (i.e., <rev>, <revA>..<revB>, or <revA>...<revB>)."
  (let ((default-directory (magit-toplevel)))
    (magit-git-items "diff" "-z" "--name-only" rev-or-range other-rev)))

(defun magit-renamed-files (revA revB)
  (--map (cons (nth 1 it) (nth 2 it))
         (-partition 3 (magit-git-items
                        "diff-tree" "-r" "--diff-filter=R" "-z" "-M"
                        revA revB))))

(defun magit-file-status (&rest args)
  (with-temp-buffer
    (save-excursion (magit-git-insert "status" "-z" args))
    (let ((pos (point)) status)
      (while (> (skip-chars-forward "[:print:]") 0)
        (let ((x (char-after     pos))
              (y (char-after (1+ pos)))
              (file (buffer-substring (+ pos 3) (point))))
          (forward-char)
          (if (memq x '(?R ?C))
              (progn
                (setq pos (point))
                (skip-chars-forward "[:print:]")
                (push (list file (buffer-substring pos (point)) x y) status)
                (forward-char))
            (push (list file nil x y) status)))
        (setq pos (point)))
      status)))

(defun magit-expand-git-file-name (filename)
  (unless (file-name-absolute-p filename)
    (setq filename (expand-file-name filename)))
  (if (and (eq system-type 'windows-nt) ; together with cygwin git, see #1318
           (string-match "^/\\(cygdrive/\\)?\\([a-z]\\)/\\(.*\\)" filename))
      (concat (match-string 2 filename) ":/"
              (match-string 3 filename))
    filename))

(defun magit-decode-git-path (path)
  (if (eq (aref path 0) ?\")
      (string-as-multibyte (read path))
    path))

(defun magit-file-at-point ()
  (magit-section-case
    (file (magit-section-value it))
    (hunk (magit-section-parent-value it))))

(defun magit-current-file ()
  (or (magit-file-relative-name)
      (magit-file-at-point)
      (and (derived-mode-p 'magit-log-mode)
           (nth 3 magit-refresh-args))))

;;; Predicates

(defun magit-no-commit-p ()
  "Return t if there is no commit in the current Git repository."
  (not (magit-rev-verify "HEAD")))

(defun magit-anything-staged-p (&optional ignore-submodules &rest files)
  "Return t if there are any staged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (magit-git-failure "diff" "--quiet" "--cached"
                     (and ignore-submodules "--ignore-submodules")
                     "--" files))

(defun magit-anything-unstaged-p (&optional ignore-submodules &rest files)
  "Return t if there are any unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (magit-git-failure "diff" "--quiet"
                     (and ignore-submodules "--ignore-submodules")
                     "--" files))

(defun magit-anything-modified-p (&optional ignore-submodules &rest files)
  "Return t if there are any staged or unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (or (apply 'magit-anything-staged-p   ignore-submodules files)
      (apply 'magit-anything-unstaged-p ignore-submodules files)))

(defun magit-anything-unmerged-p (&rest files)
  "Return t if there are any merge conflicts.
If optional FILES is non-nil, then only conflicts in those files
are considered."
  (and (magit-git-string "ls-files" "--unmerged" files) t))

;;; Revisions and References

(defun magit-rev-parse (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output return nil."
  (apply #'magit-git-string "rev-parse" args))

(defun magit-rev-parse-safe (&rest args)
  "Execute `git rev-parse ARGS', returning first line of output.
If there is no output return nil.  Like `magit-rev-parse' but
ignore `magit-git-debug'."
  (apply #'magit-git-str "rev-parse" args))

(defun magit-rev-parse-p (&rest args)
  "Execute `git rev-parse ARGS', returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (magit-git-true "rev-parse" args))

(defun magit-rev-verify (rev)
  (magit-rev-parse-safe "--verify" rev))

(defun magit-rev-equal (a b)
  (magit-git-success "diff" "--quiet" a b))

(defun magit-rev-head-p (rev)
  (or (equal rev "HEAD")
      (and rev
           (not (string-match-p "\\.\\." rev))
           (equal (magit-rev-parse rev)
                  (magit-rev-parse "HEAD")))))

(defun magit-rev-name (rev &optional pattern)
  (magit-git-string "name-rev" "--name-only" "--no-undefined"
                    (and pattern (concat "--refs=" pattern))
                    rev))

(defun magit-rev-branch (rev)
  (--when-let (magit-rev-name rev "refs/heads/*")
    (unless (string-match-p "~" it) it)))

(defun magit-get-shortname (rev)
  (let ((fn (apply-partially 'magit-git-string "name-rev"
                             "--name-only" "--no-undefined" rev)))
    (setq rev (or (funcall fn "--refs=refs/tags/*")
                  (funcall fn "--refs=refs/heads/*")
                  (funcall fn "--refs=refs/remotes/*" "--always")))
    (if (and (string-match "^\\(?:tags\\|remotes\\)/\\(.+\\)" rev)
             (magit-ref-fullname (match-string 1 rev)))
        (match-string 1 rev)
      rev)))

(defun magit-ref-fullname (name)
  (magit-rev-parse "--symbolic-full-name" name))

(defun magit-ref-exists-p (ref)
  (magit-git-success "show-ref" "--verify" ref))

(defun magit-headish ()
  "Return \"HEAD\" or if that doesn't exist the hash of the empty tree."
  (if (magit-no-commit-p)
      (magit-git-string "mktree")
    "HEAD"))

(defun magit-branch-at-point ()
  (magit-section-when branch))

(defun magit-local-branch-at-point ()
  (magit-section-when branch
    (let ((branch (magit-section-value it)))
      (when (member branch (magit-list-local-branch-names))
        branch))))

(defun magit-commit-at-point ()
  (or (magit-section-when commit)
      (and (derived-mode-p 'magit-revision-mode)
           (car (last magit-refresh-args 2)))))

(defun magit-branch-or-commit-at-point ()
  (or (magit-section-case
        (branch (magit-section-value it))
        (commit (magit-get-shortname (magit-section-value it))))
      (and (derived-mode-p 'magit-revision-mode)
           (car (last magit-refresh-args 2)))))


(defun magit-tag-at-point ()
  (magit-section-when tag))

(defun magit-stash-at-point ()
  (magit-section-when stash))

(defun magit-remote-at-point ()
  (magit-section-case
    (remote (magit-section-value it))
    (branch (magit-section-parent-value it))))

(defun magit-get-current-branch ()
  "Return the refname of the currently checked out branch.
Return nil if no branch is currently checked out."
  (magit-git-string "symbolic-ref" "--short" "HEAD"))

(defun magit-get-previous-branch ()
  "Return the refname of the previously checked out branch.
Return nil if no branch can be found in the `HEAD' reflog
which is different from the current branch and still exists."
  (let ((current (magit-get-current-branch))
        (i 1) prev)
    (while (and (setq prev (magit-rev-verify (format "@{-%i}" i)))
                (or (not (setq prev (magit-rev-branch prev)))
                    (equal prev current)))
      (cl-incf i))
    prev))

(cl-defun magit-get-tracked-ref
    (&optional (branch (magit-get-current-branch)))
  (when branch
    (let ((remote (magit-get "branch" branch "remote"))
          (merge  (magit-get "branch" branch "merge")))
      (when (and remote merge)
        (cond ((string-equal remote ".") merge)
              ((string-prefix-p "refs/heads/" merge)
               (concat "refs/remotes/" remote "/" (substring merge 11))))))))

(cl-defun magit-get-tracked-branch
    (&optional (branch (magit-get-current-branch)))
  (when branch
    (let ((remote (magit-get "branch" branch "remote"))
          (merge  (magit-get "branch" branch "merge")))
      (when (and remote merge (string-prefix-p "refs/heads/" merge))
        (setq merge (substring merge 11))
        (if (string-equal remote ".")
            merge
          (concat remote "/" merge))))))

(defun magit-get-remote (&optional branch)
  (when (or branch (setq branch (magit-get-current-branch)))
    (let ((remote (magit-get "branch" branch "remote")))
      (unless (equal remote ".")
        remote))))

(defun magit-get-remote-branch (&optional branch)
  (-when-let (local (or branch (magit-get-current-branch)))
    (let ((remote (magit-get-remote local))
          (branch (magit-get "branch" local "merge")))
      (when (and remote branch (string-match "^refs/heads/\\(.+\\)" branch))
        (cons remote (match-string 1 branch))))))

(defun magit-get-current-tag (&optional rev with-distance)
  "Return the closest tag reachable from REV.

If optional REV is nil then default to \"HEAD\".
If optional WITH-DISTANCE is non-nil then return (TAG COMMITS),
if it is `dirty' return (TAG COMMIT DIRTY). COMMITS is the number
of commits in \"HEAD\" but not in TAG and DIRTY is t if there are
uncommitted changes, nil otherwise."
  (--when-let (magit-git-str "describe" "--long" "--tags"
                             (and (eq with-distance 'dirty) "--dirty") rev)
    (save-match-data
      (string-match
       "\\(.+\\)-\\(?:0[0-9]*\\|\\([0-9]+\\)\\)-g[0-9a-z]+\\(-dirty\\)?$" it)
      (if with-distance
          `(,(match-string 1 it)
            ,(string-to-number (or (match-string 2 it) "0"))
            ,@(and (match-string 3 it) (list t)))
        (match-string 1 it)))))

(defun magit-get-next-tag (&optional rev with-distance)
  "Return the closest tag from which REV is reachable.

If optional REV is nil then default to \"HEAD\".
If no such tag can be found or if the distance is 0 (in which
case it is the current tag, not the next) return nil instead.
If optional WITH-DISTANCE is non-nil then return (TAG COMMITS)
where COMMITS is the number of commits in TAG but not in REV."
  (--when-let (magit-git-string "describe" "--contains" rev)
    (save-match-data
      (when (string-match "^[^^~]+" it)
        (setq it (match-string 0 it))
        (unless (equal it (magit-get-current-tag rev))
          (if with-distance
              (list it (car (magit-rev-diff-count it rev)))
            it))))))

(defvar magit-list-refs-namespaces
  '("refs/heads" "refs/remotes" "refs/tags" "refs/pull"))

(defun magit-list-refs (&rest args)
  (magit-git-lines "for-each-ref" "--format=%(refname)"
                   (or args magit-list-refs-namespaces)))

(defun magit-list-branches ()
  (magit-list-refs "refs/heads" "refs/remotes"))

(defun magit-list-local-branches ()
  (magit-list-refs "refs/heads"))

(defun magit-list-remote-branches (&optional remote)
  (magit-list-refs (concat "refs/remotes/" remote)))

(defun magit-list-containing-branches (&optional commit)
  (--map (substring it 2) (magit-git-lines "branch" "--contains" commit)))

(defun magit-list-merged-branches (&optional commit)
  (--map (substring it 2) (magit-git-lines "branch" "--merged" commit)))

(defun magit-list-unmerged-branches (&optional commit)
  (--map (substring it 2) (magit-git-lines "branch" "--no-merged" commit)))

(defun magit-list-unmerged-to-upstream-branches ()
  (--filter (-when-let (upstream (magit-get-tracked-branch it))
              (member it (magit-list-unmerged-branches upstream)))
            (magit-list-local-branch-names)))

(defun magit-list-refnames (&rest args)
  (magit-git-lines "for-each-ref" "--format=%(refname:short)"
                   (or args magit-list-refs-namespaces)))

(defun magit-list-branch-names ()
  (magit-list-refnames "refs/heads" "refs/remotes"))

(defun magit-list-local-branch-names ()
  (magit-list-refnames "refs/heads"))

(defun magit-list-remote-branch-names (&optional remote relative)
  (if (and remote relative)
      (let ((regexp (format "^refs/remotes/%s/\\(.+\\)" remote)))
        (--mapcat (when (string-match regexp it)
                    (list (match-string 1 it)))
                  (magit-list-remote-branches remote)))
    (magit-list-refnames (concat "refs/remotes/" remote))))

(defun magit-format-refs (format &rest args)
  (let ((lines (magit-git-lines
                "for-each-ref" (concat "--format=" format)
                (or args (list "refs/heads" "refs/remotes" "refs/tags")))))
    (if (string-match-p "\f" format)
        (--map (split-string it "\f") lines)
      lines)))

(defun magit-list-remotes ()
  (magit-git-lines "remote"))

(defun magit-list-tags ()
  (magit-git-lines "tag"))

(defun magit-list-notes-refnames ()
  (--map (substring it 6) (magit-list-refnames "refs/notes")))

(defun magit-list-remote-tags (remote)
  (--map (substring it 51)
         (--filter (not (string-match-p "\\^{}$" it))
                   (magit-git-lines "ls-remote" "--tags" remote))))

(defun magit-get-submodules ()
  (--mapcat (and (string-match "^160000 [0-9a-z]\\{40\\} 0\t\\(.+\\)$" it)
                 (list (match-string 1 it)))
            (magit-git-items "ls-files" "-z" "--stage")))

(defun magit-branch-p (string)
  (and (or (member string (magit-list-branches))
           (member string (magit-list-branch-names))) t))

(defun magit-remote-p (string)
  (car (member string (magit-list-remotes))))

(defun magit-rev-diff-count (a b)
  "Return the commits in A but not B and vice versa.
Return a list of two integers: (A>B B>A)."
  (mapcar 'string-to-number
          (split-string (magit-git-string "rev-list"
                                          "--count" "--left-right"
                                          (concat a "..." b))
                        "\t")))

(defun magit-abbrev-length ()
  (string-to-number (or (magit-get "core.abbrev") "7")))

(defun magit-abbrev-arg ()
  (format "--abbrev=%d" (magit-abbrev-length)))

(defun magit-commit-parents (commit)
  (--when-let (magit-git-string "rev-list" "-1" "--parents" commit)
    (cdr (split-string it))))

(defun magit-assert-one-parent (commit command)
  (when (> (length (magit-commit-parents commit)) 1)
    (user-error "Cannot %s a merge commit" command)))

(defun magit-patch-id (rev)
  (with-temp-buffer
    (process-file shell-file-name nil '(t nil) nil shell-command-switch
                  (let ((exec (shell-quote-argument magit-git-executable)))
                    (format "%s diff-tree -u %s | %s patch-id" exec rev exec)))
    (car (split-string (buffer-string)))))

(defun magit-reflog-enable (ref &optional stashish)
  (let ((oldrev  (magit-rev-verify ref))
        (logfile (magit-git-dir (concat "logs/" ref))))
    (unless (file-exists-p logfile)
      (when oldrev
        (magit-git-success "update-ref" "-d" ref oldrev))
      (make-directory (file-name-directory logfile) t)
      (with-temp-file logfile)
      (when (and oldrev (not stashish))
        (magit-git-success "update-ref" "-m" "enable reflog" ref oldrev "")))))

(defun magit-rev-format (format &optional rev)
  "Return output of `git show -s --format=FORMAT [REV]'."
  (magit-git-string "show" "-s" (concat "--format=" format) rev))

(defun magit-format-rev-summary (rev)
  (--when-let (magit-rev-format "%h %s" rev)
    (string-match " " it)
    (put-text-property 0 (match-beginning 0) 'face 'magit-hash it)
    it))

(defun magit-format-ref-label (ref &optional head)
  (cl-destructuring-bind (re face fn)
      (--first (string-match (car it) ref) magit-ref-namespaces)
    (if fn
        (funcall fn ref face)
      (propertize (or (match-string 1 ref) ref)
                  'face (if (equal ref head) 'magit-branch-current face)))))

(defun magit-format-ref-labels (string)
  (save-match-data
    (let ((regexp "\\(, \\|tag: \\| -> \\|[()]\\)") head names)
      (if (and (derived-mode-p 'magit-log-mode)
               (member "--simplify-by-decoration" (nth 2 magit-refresh-args)))
          (let ((branches (magit-list-local-branch-names))
                (re (format "^%s/.+" (regexp-opt (magit-list-remotes)))))
            (setq names
                  (--map (cond ((string-equal it "HEAD")     it)
                               ((string-prefix-p "refs/" it) it)
                               ((member it branches) (concat "refs/heads/" it))
                               ((string-match re it) (concat "refs/remotes/" it))
                               (t                    (concat "refs/" it)))
                         (split-string
                          (replace-regexp-in-string "tag: " "refs/tags/" string)
                          regexp t))))
        (setq names (split-string string regexp t)))
      (when (member "HEAD" names)
        (setq head  (magit-git-string "symbolic-ref" "HEAD")
              names (cons (or head "@") (delete head (delete "HEAD" names)))))
      (mapconcat (lambda (it) (magit-format-ref-label it head)) names " "))))

(defmacro magit-with-blob (commit file &rest body)
  (declare (indent 2)
           (debug (form form body)))
  `(with-temp-buffer
     (let ((buffer-file-name ,file))
       (save-excursion
         (magit-git-insert "cat-file" "-p"
                           (concat ,commit ":" buffer-file-name)))
       (decode-coding-inserted-region
        (point-min) (point-max) buffer-file-name t nil nil t)
       ,@body)))

(defmacro magit-with-temp-index (tree &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((file (cl-gensym "file")))
    `(let ((,file (magit-git-dir (make-temp-name "index.magit."))))
       (setq ,file (or (file-remote-p ,file 'localname) ,file))
       (unwind-protect
           (progn ,@(--when-let tree
                      `((or (magit-git-success
                             "read-tree" ,it (concat "--index-output=" ,file))
                            (error "Cannot read tree %s" ,it))))
                  (if (file-remote-p default-directory)
                      (let ((magit-tramp-process-environment
                             (setenv-internal magit-tramp-process-environment
                                              "GIT_INDEX_FILE" ,file t)))
                        ,@body)
                    (let ((process-environment process-environment))
                      (setenv "GIT_INDEX_FILE" ,file)
                      ,@body)))
         (ignore-errors
           (delete-file (concat (file-remote-p default-directory) ,file)))))))

(defun magit-commit-tree (message &optional tree &rest parents)
  (magit-git-string "commit-tree" "-m" message
                    (--mapcat (list "-p" it) (delq nil parents))
                    (or tree (magit-git-string "write-tree"))))

(defun magit-commit-worktree (message &rest other-parents)
  (magit-with-temp-index "HEAD"
    (and (magit-update-files (magit-modified-files))
         (apply #'magit-commit-tree message nil "HEAD" other-parents))))

(defun magit-update-files (files)
  (magit-git-success "update-index" "--add" "--remove" "--" files))

(defconst magit-range-re
  (concat "\\`\\([^ \t]*[^.]\\)?"       ; revA
          "\\(\\.\\.\\.?\\)"            ; range marker
          "\\([^.][^ \t]*\\)?\\'"))     ; revB

;;; Completion

(defvar magit-revision-history nil)

(defun magit-read-branch (prompt &optional default)
  (magit-completing-read prompt (magit-list-branch-names)
                         nil t nil 'magit-revision-history
                         (or (magit-branch-at-point)
                             default (magit-get-current-branch))))

(defun magit-read-branch-or-commit (prompt &optional secondary-default)
  (or (magit-completing-read prompt (cons "HEAD" (magit-list-refnames))
                             nil nil nil 'magit-revision-history
                             (or (magit-branch-or-commit-at-point)
                                 secondary-default
                                 (magit-get-current-branch)))
      (user-error "Nothing selected")))

(defun magit-read-range-or-commit (prompt &optional secondary-default)
  (magit-read-range
   prompt
   (or (--when-let (magit-region-values 'commit 'branch)
         (deactivate-mark)
         (concat (car (last it)) ".." (car it)))
       (magit-branch-or-commit-at-point)
       secondary-default
       (magit-get-current-branch))))

(defun magit-read-range (prompt &optional default)
  (let* ((choose-completion-string-functions
          '(crm--choose-completion-string))
         (minibuffer-completion-table #'crm--collection-fn)
         (minibuffer-completion-confirm t)
         (crm-completion-table (magit-list-refnames))
         (crm-separator "\\.\\.\\.?")
         (input (read-from-minibuffer
                 (concat prompt (and default (format " (%s)" default)) ": ")
                 nil crm-local-completion-map
                 nil 'magit-revision-history
                 default)))
    (when (string-equal input "")
      (or (setq input default)
          (user-error "Nothing selected")))
    input))

(defun magit-read-remote-branch
    (prompt &optional remote default local-branch require-match)
  (when (consp default)
    (setq default (concat (car default) "/" (cdr default))))
  (let ((branch (magit-completing-read
                 prompt
                 (nconc (and local-branch
                             (if remote
                                 (concat remote "/" local-branch)
                               (--map (concat it "/" local-branch)
                                      (magit-list-remotes))))
                        (magit-list-remote-branch-names remote t))
                 nil require-match nil 'magit-revision-history default)))
    (string-match "^\\([^/]+\\)/\\(.+\\)" branch)
    (cons (match-string 1 branch)
          (match-string 2 branch))))

(defun magit-read-local-branch (prompt &optional secondary-default)
  (magit-completing-read prompt (magit-list-local-branch-names)
                         nil t nil 'magit-revision-history
                         (or (magit-branch-at-point)
                             secondary-default
                             (magit-get-current-branch))))

(defun magit-read-local-branch-or-ref (prompt &optional secondary-default)
  (magit-completing-read prompt (nconc (magit-list-local-branch-names)
                                       (magit-list-refs "refs/"))
                         nil t nil 'magit-revision-history
                         (or (magit-branch-at-point)
                             secondary-default
                             (magit-get-current-branch))))

(defun magit-read-other-branch (prompt &optional exclude secondary-default)
  (let* ((current (magit-get-current-branch))
         (atpoint (magit-branch-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default (magit-get-previous-branch))))
    (magit-completing-read prompt (delete exclude (magit-list-branch-names))
                           nil t nil 'magit-revision-history default)))

(defun magit-read-other-branch-or-commit
    (prompt &optional exclude secondary-default)
  (let* ((current (magit-get-current-branch))
         (atpoint (magit-branch-or-commit-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default (magit-get-previous-branch))))
    (or (magit-completing-read prompt (delete exclude (magit-list-refnames))
                               nil nil nil 'magit-revision-history default)
        (user-error "Nothing selected"))))

(defun magit-read-tag (prompt &optional require-match)
  (magit-completing-read prompt (magit-list-tags) nil
                         require-match nil 'magit-revision-history
                         (magit-tag-at-point)))

(defun magit-read-stash (prompt &optional use-at-point)
  (let ((atpoint (magit-stash-at-point)))
    (or (and use-at-point atpoint)
        (let ((stashes (magit-git-lines "stash" "list" "--format=%gd")))
          (magit-completing-read prompt stashes nil t nil nil
                                 (or atpoint (car stashes)))))))

(defun magit-read-remote (prompt &optional default use-only)
  (let ((remotes (magit-list-remotes)))
    (if (and use-only (= (length remotes) 1))
        (car remotes)
      (magit-completing-read prompt remotes
                             nil t nil nil
                             (or default
                                 (magit-remote-at-point)
                                 (magit-get-remote))))))

;;; Variables

(defun magit-get (&rest keys)
  "Return the value of Git config entry specified by KEYS."
  (magit-git-str "config" (mapconcat 'identity keys ".")))

(defun magit-get-all (&rest keys)
  "Return all values of the Git config entry specified by KEYS."
  (let ((magit-git-debug nil))
    (magit-git-lines "config" "--get-all" (mapconcat 'identity keys "."))))

(defun magit-get-boolean (&rest keys)
  "Return the boolean value of Git config entry specified by KEYS."
  (magit-git-true "config" "--bool" (mapconcat 'identity keys ".")))

(defun magit-set (val &rest keys)
  "Set Git config settings specified by KEYS to VAL."
  (if val
      (magit-git-string "config" (mapconcat 'identity keys ".") val)
    (magit-git-string "config" "--unset" (mapconcat 'identity keys "."))))

;;; magit-git.el ends soon
(provide 'magit-git)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-git.el ends here
