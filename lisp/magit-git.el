;;; magit-git.el --- Git functionality  -*- lexical-binding: t -*-

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

;; This library implements wrappers for various Git plumbing commands.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'magit-utils)
(require 'magit-section)

(declare-function magit-maybe-make-margin-overlay 'magit-log)
(declare-function magit-process-buffer 'magit-process)
(declare-function magit-process-file 'magit-process)
(declare-function magit-process-insert-section 'magit-process)
(defvar magit-process-error-message-re)
(defvar magit-refresh-args) ; from `magit-mode' for `magit-current-file'
(defvar magit-branch-prefer-remote-upstream)

(defvar magit-tramp-process-environment nil)

;;; Options

;; For now this is shared between `magit-process' and `magit-git'.
(defgroup magit-process nil
  "Git and other external processes used by Magit."
  :group 'magit)

(defvar magit-git-environment
  (list (format "INSIDE_EMACS=%s,magit" emacs-version))
  "Prepended to `process-environment' while running git.")

(defcustom magit-git-executable
  ;; Git might be installed in a different location on a remote, so
  ;; it is better not to use the full path to the executable, except
  ;; on Window were we would otherwise end up using one one of the
  ;; wrappers "cmd/git.exe" or "cmd/git.cmd", which are much slower
  ;; than using "bin/git.exe" directly.
  (or (and (eq system-type 'windows-nt)
           (--when-let (executable-find "git")
             (or (ignore-errors
                   ;; Git for Windows 2.x provides cygpath so we can
                   ;; ask it for native paths.  Using an upper case
                   ;; alias makes this fail on 1.x (which is good,
                   ;; because we would not want to end up using some
                   ;; other cygpath).
                   (prog1 (car
                           (process-lines
                            it "-c"
                            "alias.X=!x() { which \"$1\" | cygpath -mf -; }; x"
                            "X" "git"))
                     (setq magit-git-environment
                           (cons (concat "PATH="
                                         (car (process-lines
                                               it "-c"
                                               "alias.P=!cygpath -wp \"$PATH\""
                                               "P")))
                                 magit-git-environment))))
                 ;; For 1.x, we search for bin/ next to cmd/.
                 (let ((alt (directory-file-name (file-name-directory it))))
                   (if (and (equal (file-name-nondirectory alt) "cmd")
                            (setq alt (expand-file-name
                                       (convert-standard-filename "bin/git.exe")
                                       (file-name-directory alt)))
                            (file-executable-p alt))
                       alt
                     it)))))
      "git")
  "The Git executable used by Magit."
  :group 'magit-process
  :type 'string)

(defcustom magit-git-global-arguments
  `("--no-pager" "--literal-pathspecs" "-c" "core.preloadindex=true"
    ,@(and (eq system-type 'windows-nt)
           (list "-c" "i18n.logOutputEncoding=UTF-8")))
  "Global Git arguments.

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
  :package-version '(magit . "2.9.0")
  :group 'magit-git-arguments
  :group 'magit-process
  :type '(repeat string))

(defvar magit-git-debug nil
  "Whether to enable additional reporting of git errors.

Magit basically calls git for one of these two reasons: for
side-effects or to do something with its standard output.

When git is run for side-effects then its output, including error
messages, go into the process buffer which is shown when using \
\\<magit-status-mode-map>\\[magit-process].

When git's output is consumed in some way, then it would be too
expensive to also insert it into this buffer, but when this
option is non-nil and git returns with a non-zero exit status,
then at least its standard error is inserted into this buffer.

This is only intended for debugging purposes.  Do not enable this
permanently, that would negatively affect performance.")

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
  "How refs are formatted for display.

Each entry controls how a certain type of ref is displayed, and
has the form (REGEXP FACE FORMATTER).  REGEXP is a regular
expression used to match full refs.  The first entry whose REGEXP
matches the reference is used.  The first regexp submatch becomes
the \"label\" that represents the ref and is propertized with
font FONT.  If FORMATTER is non-nil it should be a function that
takes two arguments, the full ref and the face.  It is supposed
to return a propertized label that represents the ref."
  :package-version '(magit . "2.1.0")
  :group 'magit-miscellanous
  :type '(repeat
          (list regexp
                face
                (choice (const :tag "first submatch is label" nil)
                        (function :tag "format using function")))))

(defcustom magit-prefer-remote-upstream nil
  "Whether to favor remote branches when reading the upstream branch.

This controls whether commands that read a branch from the user
and then set it as the upstream branch, offer a local or a remote
branch as default completion candidate, when they have the choice.

This affects all commands that use `magit-read-upstream-branch'
or `magit-read-starting-point', which includes all commands that
change the upstream and many which create new branches."
  :package-version '(magit . "2.4.2")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-list-refs-sortby nil
  "How to sort the ref collection in the prompt.

This affects commands that read a ref.  More specifically, it
controls the order of refs returned by `magit-list-refs', which
is called by functions like `magit-list-branch-names' to generate
the collection of refs.  By default, refs are sorted according to
their full refname (i.e., 'refs/...').

Any value accepted by the `--sort' flag of `git for-each-ref' can
be used.  For example, \"-creatordate\" places refs with more
recent committer or tagger dates earlier in the list.  A list of
strings can also be given in order to pass multiple sort keys to
`git for-each-ref'.

Note that, depending on the completion framework you use, this
may not be sufficient to change the order in which the refs are
displayed.  It only controls the order of the collection passed
to `magit-completing-read' or, for commands that support reading
multiple strings, `read-from-minibuffer'.  The completion
framework ultimately determines how the collection is displayed."
  :package-version '(magit . "2.10.4")
  :group 'magit-miscellanous
  :type '(choice string (repeat string)))

;;; Git

(defvar magit--refresh-cache nil)

(defmacro magit--with-refresh-cache (key &rest body)
  (declare (indent 1) (debug (form body)))
  (let ((k (cl-gensym)))
    `(if magit--refresh-cache
         (let ((,k ,key))
           (--if-let (assoc ,k (cdr magit--refresh-cache))
               (progn (cl-incf (caar magit--refresh-cache))
                      (cdr it))
             (cl-incf (cdar magit--refresh-cache))
             (let ((value ,(macroexp-progn body)))
               (push (cons ,k value)
                     (cdr magit--refresh-cache))
               value)))
       ,@body)))

(defmacro magit-with-editor (&rest body)
  "Like `with-editor' but let-bind some more variables."
  (declare (indent 0) (debug (body)))
  `(let ((magit-process-popup-time -1)
         ;; The user may have customized `shell-file-name' to
         ;; something which results in `w32-shell-dos-semantics' nil
         ;; (which changes the quoting style used by
         ;; `shell-quote-argument'), but Git for Windows expects shell
         ;; quoting in the dos style.
         (shell-file-name (if (and (eq system-type 'windows-nt)
                                   ;; If we have Cygwin mount points
                                   ;; the git flavor is cygwin, so dos
                                   ;; shell quoting is probably wrong.
                                   (not magit-cygwin-mount-points))
                              "cmdproxy"
                            shell-file-name)))
     (with-editor "GIT_EDITOR"
       ,@body)))

(defun magit-process-git-arguments (args)
  "Prepare ARGS for a function that invokes Git.

Magit has many specialized functions for running Git; they all
pass arguments through this function before handing them to Git,
to do the following.

* Flatten ARGS, removing nil arguments.
* Prepend `magit-git-global-arguments' to ARGS."
  (append magit-git-global-arguments (-flatten args)))

(defun magit-git-exit-code (&rest args)
  "Execute Git with ARGS, returning its exit code."
  (apply #'magit-process-file magit-git-executable nil nil nil
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
  (setq args (-flatten args))
  (magit--with-refresh-cache (cons default-directory args)
    (with-temp-buffer
      (apply #'magit-process-file magit-git-executable nil (list t nil) nil
             (magit-process-git-arguments args))
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

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
If Git exits with a non-zero exit status, then show a message and
add a section in the respective process buffer."
  (setq args (magit-process-git-arguments args))
  (if magit-git-debug
      (let (log)
        (unwind-protect
            (progn
              (setq log (make-temp-file "magit-stderr"))
              (delete-file log)
              (let ((exit (apply #'magit-process-file magit-git-executable
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
                        (with-current-buffer (magit-process-buffer t)
                          (magit-process-insert-section default-directory
                                                        magit-git-executable
                                                        args exit log))))
                    (message "%s" msg)))
                exit))
          (ignore-errors (delete-file log))))
    (apply #'magit-process-file magit-git-executable
           nil (list t nil) nil args)))

(defun magit-git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (setq args (-flatten args))
  (magit--with-refresh-cache (cons default-directory args)
    (with-temp-buffer
      (apply #'magit-git-insert args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

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
call function WASHER with ARGS as its sole argument."
  (declare (indent 1))
  (let ((beg (point)))
    (setq args (-flatten args))
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
        (magit-cancel-section))
      (magit-maybe-make-margin-overlay))))

(defun magit-git-version (&optional raw)
  (--when-let (let (magit-git-global-arguments)
                (ignore-errors (substring (magit-git-string "version") 12)))
    (if raw it (and (string-match "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" it)
                    (match-string 1 it)))))

;;; Files

(defun magit--safe-default-directory (&optional file)
  (catch 'unsafe-default-dir
    (let ((dir (file-name-as-directory
                (expand-file-name (or file default-directory))))
          (previous nil))
      (while (not (magit-file-accessible-directory-p dir))
        (setq dir (file-name-directory (directory-file-name dir)))
        (when (equal dir previous)
          (throw 'unsafe-default-dir nil))
        (setq previous dir))
      dir)))

(defmacro magit--with-safe-default-directory (file &rest body)
  (declare (indent 1) (debug (form body)))
  `(-when-let (default-directory (magit--safe-default-directory ,file))
     ,@body))

(defun magit-git-dir (&optional path)
  "Return absolute path to the control directory of the current repository.

All symlinks are followed.  If optional PATH is non-nil, then
it has to be a path relative to the control directory and its
absolute path is returned."
  (magit--with-refresh-cache (list default-directory 'magit-git-dir path)
    (magit--with-safe-default-directory nil
      (--when-let (magit-rev-parse-safe "--git-dir")
        (setq it (file-name-as-directory (magit-expand-git-file-name it)))
        (if path (expand-file-name (convert-standard-filename path) it) it)))))

(defvar magit--separated-gitdirs nil)

(defun magit--record-separated-gitdir ()
  (let ((topdir (magit-toplevel))
        (gitdir (magit-git-dir)))
    ;; We want to delete the entry for `topdir' here, rather than within
    ;; (unless ...), in case a `--separate-git-dir' repository was switched to
    ;; the standard structure (i.e., "topdir/.git/").
    (setq magit--separated-gitdirs (cl-delete topdir
                                            magit--separated-gitdirs
                                            :key #'car :test #'equal))
    (unless (equal (file-name-as-directory (expand-file-name ".git" topdir))
                   gitdir)
      (push (cons topdir gitdir) magit--separated-gitdirs))))

(defun magit-toplevel (&optional directory)
  "Return the absolute path to the toplevel of the current repository.

From within the working tree or control directory of a repository
return the absolute path to the toplevel directory of the working
tree.  As a special case, from within a bare repository return
the control directory instead.  When called outside a repository
then return nil.

When optional DIRECTORY is non-nil then return the toplevel for
that directory instead of the one for `default-directory'.

Try to respect the option `find-file-visit-truename', i.e.  when
the value of that option is nil, then avoid needlessly returning
the truename.  When a symlink to a sub-directory of the working
tree is involved, or when called from within a sub-directory of
the gitdir or from the toplevel of a gitdir, which itself is not
located within the working tree, then it is not possible to avoid
returning the truename."
  (magit--with-refresh-cache
      (cons (or directory default-directory) 'magit-toplevel)
    (magit--with-safe-default-directory directory
      (-if-let (topdir (magit-rev-parse-safe "--show-toplevel"))
          (let (updir)
            (setq topdir (magit-expand-git-file-name topdir))
            (if (and
                 ;; Always honor these settings.
                 (not find-file-visit-truename)
                 (not (getenv "GIT_WORK_TREE"))
                 ;; `--show-cdup' is the relative path to the toplevel
                 ;; from `(file-truename default-directory)'.  Here we
                 ;; pretend it is relative to `default-directory', and
                 ;; go to that directory.  Then we check whether
                 ;; `--show-toplevel' still returns the same value and
                 ;; whether `--show-cdup' now is the empty string.  If
                 ;; both is the case, then we are at the toplevel of
                 ;; the same working tree, but also avoided needlessly
                 ;; following any symlinks.
                 (progn
                   (setq updir (file-name-as-directory
                                (magit-rev-parse-safe "--show-cdup")))
                   (setq updir (if (file-name-absolute-p updir)
                                   (concat (file-remote-p default-directory) updir)
                                 (expand-file-name updir)))
                   (let ((default-directory updir))
                     (and (string-equal (magit-rev-parse-safe "--show-cdup") "")
                          (--when-let (magit-rev-parse-safe "--show-toplevel")
                            (string-equal (magit-expand-git-file-name it)
                                          topdir))))))
                updir
              (concat (file-remote-p default-directory)
                      (file-name-as-directory topdir))))
        (-when-let (gitdir (magit-rev-parse-safe "--git-dir"))
          (setq gitdir (file-name-as-directory
                        (if (file-name-absolute-p gitdir)
                            ;; We might have followed a symlink.
                            (concat (file-remote-p default-directory)
                                    (magit-expand-git-file-name gitdir))
                          (expand-file-name gitdir))))
          (if (magit-bare-repo-p)
              gitdir
            (let* ((link (expand-file-name "gitdir" gitdir))
                   (wtree (and (file-exists-p link)
                               (magit-file-line link))))
              (cond
               ((and wtree
                     ;; Ignore .git/gitdir files that result from a
                     ;; Git bug.  See #2364.
                     (not (equal wtree ".git")))
                ;; Return the linked working tree.
                (file-name-directory wtree))
               ;; The working directory may not be the parent directory of
               ;; .git if it was set up with `git init --separate-git-dir'.
               ;; See #2955.
               ((car (rassoc gitdir magit--separated-gitdirs)))
               (t
                ;; Step outside the control directory to enter the working tree.
                (file-name-directory (directory-file-name gitdir)))))))))))

(defmacro magit-with-toplevel (&rest body)
  (declare (indent defun) (debug (body)))
  (let ((toplevel (cl-gensym "toplevel")))
    `(let ((,toplevel (magit-toplevel)))
       (if ,toplevel
           (let ((default-directory ,toplevel))
             ,@body)
         (error "Not inside a Git repository: %s" default-directory)))))

(defun magit-inside-gitdir-p ()
  "Return t if `default-directory' is below a repository directory."
  ;; This does not work if the gitdir is not located inside the
  ;; working tree: (magit-rev-parse-p "--is-inside-git-dir").
  (-when-let (gitdir (magit-git-dir))
    (file-in-directory-p default-directory gitdir)))

(defun magit-inside-worktree-p ()
  "Return t if `default-directory' is below the work tree of a repository."
  (magit-rev-parse-p "--is-inside-work-tree"))

(defun magit-bare-repo-p ()
  "Return t if the current repository is bare."
  (magit-rev-parse-p "--is-bare-repository"))

(defun magit-git-repo-p (directory &optional non-bare)
  "Return t if DIRECTORY is a Git repository.
When optional NON-BARE is non-nil also return nil if DIRECTORY is
a bare repository."
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

(defun magit-file-relative-name (&optional file tracked)
  "Return the path of FILE relative to the repository root.

If optional FILE is nil or omitted return the relative path of
the file being visited in the current buffer, if any, else nil.
If the file is not inside a Git repository then return nil.

If TRACKED is non-nil, return the path only if it matches a
tracked file."
  (unless file
    (with-current-buffer (or (buffer-base-buffer)
                             (current-buffer))
      (setq file (or magit-buffer-file-name buffer-file-name))))
  (when (and file (or (not tracked)
                      (magit-file-tracked-p (file-relative-name file))))
    (--when-let (magit-toplevel
                 (magit--safe-default-directory
                  (directory-file-name (file-name-directory file))))
      (file-relative-name file it))))

(defun magit-file-tracked-p (file)
  (magit-git-success "ls-files" "--error-unmatch" file))

(defun magit-list-files (&rest args)
  (apply #'magit-git-items "ls-files" "-z" "--full-name" args))

(defun magit-tracked-files ()
  (magit-list-files "--cached"))

(defun magit-untracked-files (&optional all files)
  (magit-list-files "--other" (unless all "--exclude-standard") "--" files))

(defun magit-modified-files (&optional nomodules)
  (magit-git-items "diff-files" "-z" "--name-only"
                   (and nomodules "--ignore-submodules")))

(defun magit-staged-files (&optional nomodules files)
  (magit-git-items "diff-index" "-z" "--name-only" "--cached"
                   (and nomodules "--ignore-submodules")
                   (magit-headish) "--" files))

(defun magit-unstaged-files (&optional nomodules files)
  (magit-git-items "diff-index" "-z" "--name-only"
                   (and nomodules "--ignore-submodules")
                   (magit-headish) "--" files))

(defun magit-staged-binary-files ()
  (--mapcat (and (string-match "^-\t-\t\\(.+\\)" it)
                 (list (match-string 1 it)))
            (magit-git-items "diff" "-z" "--cached"
                             "--numstat" "--ignore-submodules")))

(defun magit-unmerged-files ()
  (magit-git-items "diff-files" "-z" "--name-only" "--diff-filter=U"))

(defun magit-revision-files (rev)
  (magit-with-toplevel
    (magit-git-items "ls-tree" "-z" "-r" "--name-only" rev)))

(defun magit-changed-files (rev-or-range &optional other-rev)
  "Return list of files the have changed between two revisions.
If OTHER-REV is non-nil, REV-OR-RANGE should be a revision, not a
range.  Otherwise, it can be any revision or range accepted by
\"git diff\" (i.e., <rev>, <revA>..<revB>, or <revA>...<revB>)."
  (magit-with-toplevel
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

(defcustom magit-cygwin-mount-points
  (when (eq system-type 'windows-nt)
    (cl-sort (--map (if (string-match "^\\(.*\\) on \\(.*\\) type" it)
                        (cons (file-name-as-directory (match-string 2 it))
                              (file-name-as-directory (match-string 1 it)))
                      (lwarn '(magit) :error
                             "Failed to parse Cygwin mount: %S" it))
                    ;; If --exec-path is not a native Windows path,
                    ;; then we probably have a cygwin git.
                    (let ((process-environment
                           (append magit-git-environment process-environment)))
                      (and (not (string-match-p
                                 "\\`[a-zA-Z]:"
                                 (car (process-lines
                                       magit-git-executable "--exec-path"))))
                           (ignore-errors (process-lines "mount")))))
             #'> :key (-lambda ((cyg . _win)) (length cyg))))
  "Alist of (CYGWIN . WIN32) directory names.
Sorted from longest to shortest CYGWIN name."
  :package-version '(magit . "2.3.0")
  :group 'magit-process
  :type '(alist :key-type string :value-type directory))

(defun magit-expand-git-file-name (filename)
  (unless (file-name-absolute-p filename)
    (setq filename (expand-file-name filename)))
  (-if-let ((cyg . win)
            (cl-assoc filename magit-cygwin-mount-points
                      :test (lambda (f cyg) (string-prefix-p cyg f))))
      (concat win (substring filename (length cyg)))
    filename))

(defun magit-convert-filename-for-git (filename)
  "Convert FILENAME so that it can be passed to git.
1. If it's a remote filename, then remove the remote part.
2. Expand \"~/\", git isn't a shell and does not understand it.
3. Deal with an `windows-nt' Emacs vs. Cygwin Git incompatibility."
  (if (file-name-absolute-p filename)
      (-if-let ((cyg . win)
                (cl-rassoc filename magit-cygwin-mount-points
                           :test (lambda (f win) (string-prefix-p win f))))
          (concat cyg (substring filename (length win)))
        (or (file-remote-p filename 'localname)
            filename))
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
           (car (nth 2 magit-refresh-args)))))

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

(defun magit-rev-verify-commit (rev)
  "Return full hash for REV if it names an existing commit."
  (magit-rev-verify (concat rev "^{commit}")))

(defun magit-rev-equal (a b)
  (magit-git-success "diff" "--quiet" a b))

(defun magit-rev-eq (a b)
  (equal (magit-rev-verify a)
         (magit-rev-verify b)))

(defun magit-rev-ancestor-p (a b)
  "Return non-nil if commit A is an ancestor of commit B."
  (magit-git-success "merge-base" "--is-ancestor" a b))

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

(defun magit-name-branch (rev &optional lax)
  (or (magit-name-local-branch rev)
      (magit-name-remote-branch rev)
      (and lax (or (magit-name-local-branch rev t)
                   (magit-name-remote-branch rev t)))))

(defun magit-name-local-branch (rev &optional lax)
  (--when-let (magit-git-string "name-rev" "--name-only" "--no-undefined"
                                "--refs=refs/heads/*" rev)
    (and (or lax (not (string-match-p "[~^]" it))) it)))

(defun magit-name-remote-branch (rev &optional lax)
  (--when-let (magit-git-string "name-rev" "--name-only" "--no-undefined"
                                "--refs=refs/remotes/*" rev)
    (and (or lax (not (string-match-p "[~^]" it)))
         (substring it 8))))

(defun magit-name-tag (rev &optional lax)
  (--when-let (magit-git-string "name-rev" "--name-only" "--no-undefined"
                                "--refs=refs/tags/*" rev)
    (and (or lax (not (string-match-p "[~^]" it)))
         (substring it 5))))

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
  (magit-section-case
    (branch (magit-section-value it))
    (commit (magit-name-branch (magit-section-value it)))))

(defun magit-local-branch-at-point ()
  (magit-section-case
    (branch (let ((branch (magit-section-value it)))
              (when (member branch (magit-list-local-branch-names))
                branch)))
    (commit (magit-name-local-branch (magit-section-value it)))))

(defun magit-remote-branch-at-point ()
  (magit-section-case
    (branch (let ((branch (magit-section-value it)))
              (when (member branch (magit-list-remote-branch-names))
                branch)))
    (commit (magit-name-remote-branch (magit-section-value it)))))

(defun magit-commit-at-point ()
  (or (magit-section-when commit)
      (and (derived-mode-p 'magit-revision-mode)
           (car magit-refresh-args))))

(defun magit-branch-or-commit-at-point ()
  (or (magit-section-case
        (branch (magit-section-value it))
        (commit (let ((rev (magit-section-value it)))
                  (or (magit-get-shortname rev) rev)))
        (tag    (magit-section-value it)))
      (and (derived-mode-p 'magit-revision-mode)
           (car magit-refresh-args))))

(defun magit-tag-at-point ()
  (magit-section-case
    (tag    (magit-section-value it))
    (commit (magit-name-tag (magit-section-value it)))))

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

(defun magit-get-upstream-ref (&optional branch)
  (and (or branch (setq branch (magit-get-current-branch)))
       (let ((remote (magit-get "branch" branch "remote"))
             (merge  (magit-get "branch" branch "merge")))
         (when (and remote merge)
           (cond ((string-equal remote ".") merge)
                 ((string-prefix-p "refs/heads/" merge)
                  (concat "refs/remotes/" remote "/" (substring merge 11))))))))

(defun magit-get-upstream-branch (&optional branch verify)
  (and (or branch (setq branch (magit-get-current-branch)))
       (-when-let* ((remote (magit-get "branch" branch "remote"))
                    (merge  (magit-get "branch" branch "merge")))
         (and (string-prefix-p "refs/heads/" merge)
              (let* ((upstream (substring merge 11))
                     (upstream
                      (if (string-equal remote ".")
                          (propertize upstream 'face 'magit-branch-local)
                        (propertize (concat remote "/" upstream)
                                    'face 'magit-branch-remote))))
                (and (or (not verify)
                         (magit-rev-verify upstream))
                     upstream))))))

(defun magit-get-indirect-upstream-branch (branch &optional force)
  (let ((remote (magit-get "branch" branch "remote")))
    (and remote (not (equal remote "."))
         ;; The user has opted in...
         (or force
             (--any (if (magit-git-success "check-ref-format" "--branch" it)
                        (equal it branch)
                      (string-match-p it branch))
                    magit-branch-prefer-remote-upstream))
         ;; and local BRANCH tracks a remote branch...
         (let ((upstream (magit-get-upstream-branch branch)))
           ;; whose upstream...
           (and upstream
                ;; has the same name as BRANCH and...
                (equal (substring upstream (1+ (length remote))) branch)
                ;; and can be fast-forwarded to BRANCH.
                (magit-rev-ancestor-p upstream branch)
                upstream)))))

(defun magit-get-upstream-remote (&optional branch)
  (and (or branch (setq branch (magit-get-current-branch)))
       (magit-get "branch" branch "remote")))

(defun magit-get-push-remote (&optional branch)
  (or (and (or branch (setq branch (magit-get-current-branch)))
           (magit-get "branch" branch "pushRemote"))
      (magit-get "remote.pushDefault")))

(defun magit-get-push-branch (&optional branch verify)
  (and (or branch (setq branch (magit-get-current-branch)))
       (-when-let* ((remote (magit-get-push-remote branch))
                    (push-branch (concat remote "/" branch)))
         (and (or (not verify)
                  (magit-rev-verify push-branch))
              push-branch))))

(defun magit-get-@{push}-branch (&optional branch)
  (let ((ref (magit-rev-parse "--symbolic-full-name"
                              (concat branch "@{push}"))))
    (when (and ref (string-prefix-p "refs/remotes/" ref))
      (substring ref 13))))

(defun magit-get-remote (&optional branch)
  (when (or branch (setq branch (magit-get-current-branch)))
    (let ((remote (magit-get "branch" branch "remote")))
      (unless (equal remote ".")
        remote))))

(defun magit-split-branch-name (branch)
  (cond ((member branch (magit-list-local-branch-names))
         (cons "." branch))
        ((string-match "/" branch)
         (let ((remote (substring branch 0 (match-beginning 0))))
           (if (save-match-data (member remote (magit-list-remotes)))
               (cons remote (substring branch (match-end 0)))
             (error "Invalid branch name %s" branch))))))

(defun magit-get-current-tag (&optional rev with-distance)
  "Return the closest tag reachable from REV.

If optional REV is nil then default to `HEAD'.
If optional WITH-DISTANCE is non-nil then return (TAG COMMITS),
if it is `dirty' return (TAG COMMIT DIRTY). COMMITS is the number
of commits in `HEAD' but not in TAG and DIRTY is t if there are
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

If optional REV is nil then default to `HEAD'.
If no such tag can be found or if the distance is 0 (in which
case it is the current tag, not the next) return nil instead.
If optional WITH-DISTANCE is non-nil then return (TAG COMMITS)
where COMMITS is the number of commits in TAG but not in REV."
  (--when-let (magit-git-str "describe" "--contains" (or rev "HEAD"))
    (save-match-data
      (when (string-match "^[^^~]+" it)
        (setq it (match-string 0 it))
        (unless (equal it (magit-get-current-tag rev))
          (if with-distance
              (list it (car (magit-rev-diff-count it rev)))
            it))))))

(defvar magit-list-refs-namespaces
  '("refs/heads" "refs/remotes" "refs/tags" "refs/pull"))

(defun magit-list-refs (&optional namespaces format sortby)
  "Return list of references.

When NAMESPACES is non-nil, list refs from these namespaces
rather than those from `magit-list-refs-namespaces'.

FORMAT is passed to the `--format' flag of `git for-each-ref' and
defaults to \"%(refname)\".

SORTBY is a key or list of keys to pass to the `--sort' flag of
`git for-each-ref'.  When nil, use `magit-list-refs-sortby'"
  (magit-git-lines "for-each-ref"
                   (concat "--format=" (or format "%(refname)"))
                   (--map (concat "--sort=" it)
                          (pcase (or sortby magit-list-refs-sortby)
                            ((and val (pred stringp)) (list val))
                            ((and val (pred listp)) val)))
                   (or namespaces magit-list-refs-namespaces)))

(defun magit-list-branches ()
  (magit-list-refs (list "refs/heads" "refs/remotes")))

(defun magit-list-local-branches ()
  (magit-list-refs "refs/heads"))

(defun magit-list-remote-branches (&optional remote)
  (magit-list-refs (concat "refs/remotes/" remote)))

(defun magit-list-containing-branches (&optional commit)
  (--filter (not (string-match-p "\\`(HEAD" it))
            (--map (substring it 2)
                   (magit-git-lines "branch" "--contains" commit))))

(defun magit-list-merged-branches (&optional commit)
  (--filter (not (string-match-p "\\`(HEAD" it))
            (--map (substring it 2)
                   (magit-git-lines "branch" "--merged" commit))))

(defun magit-list-unmerged-branches (&optional commit)
  (--filter (not (string-match-p "\\`(HEAD" it))
            (--map (substring it 2)
                   (magit-git-lines "branch" "--no-merged" commit))))

(defun magit-list-unmerged-to-upstream-branches ()
  (--filter (-when-let (upstream (magit-get-upstream-branch it))
              (member it (magit-list-unmerged-branches upstream)))
            (magit-list-local-branch-names)))

(defun magit-list-refnames (&optional namespaces)
  (magit-list-refs namespaces "%(refname:short)"))

(defun magit-list-branch-names ()
  (magit-list-refnames (list "refs/heads" "refs/remotes")))

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

(defun magit-remote-list-tags (remote)
  (--keep (and (not (string-match-p "\\^{}$" it))
               (substring it 51))
          (magit-git-lines "ls-remote" "--tags" remote)))

(defun magit-remote-list-branches (remote)
  (--keep (and (not (string-match-p "\\^{}$" it))
               (substring it 52))
          (magit-git-lines "ls-remote" "--heads" remote)))

(defun magit-remote-list-refs (remote)
  (--keep (and (not (string-match-p "\\^{}$" it))
               (substring it 41))
          (magit-git-lines "ls-remote" remote)))

(defun magit-get-submodules ()
  (--mapcat (and (string-match "^160000 [0-9a-z]\\{40\\} 0\t\\(.+\\)$" it)
                 (list (match-string 1 it)))
            (magit-git-items "ls-files" "-z" "--stage")))

(defun magit-list-worktrees ()
  (let (worktrees worktree)
    (dolist (line (let ((magit-git-global-arguments
                         ;; KLUDGE At least in v2.8.3 this triggers a segfault.
                         (remove "--no-pager" magit-git-global-arguments)))
                    (magit-git-lines "worktree" "list" "--porcelain")))
      (cond ((string-prefix-p "worktree" line)
             (push (setq worktree (list (substring line 9) nil nil nil))
                   worktrees))
            ((string-equal line "bare")
             (let* ((default-directory (car worktree))
                    (wt (and (not (magit-get-boolean "core.bare"))
                             (magit-get "core.worktree"))))
               (if (and wt (file-exists-p (expand-file-name wt)))
                   (progn (setf (nth 0 worktree) (expand-file-name wt))
                          (setf (nth 2 worktree) (magit-rev-parse "HEAD"))
                          (setf (nth 3 worktree) (magit-get-current-branch)))
                 (setf (nth 1 worktree) t))))
            ((string-prefix-p "HEAD" line)
             (setf (nth 2 worktree) (substring line 5)))
            ((string-prefix-p "branch" line)
             (setf (nth 3 worktree) (substring line 18)))
            ((string-equal line "detached"))))
    (nreverse worktrees)))

(defun magit-ref-p (rev)
  (or (car (member rev (magit-list-refs)))
      (car (member rev (magit-list-refnames)))))

(defun magit-branch-p (rev)
  (or (car (member rev (magit-list-branches)))
      (car (member rev (magit-list-branch-names)))))

(defun magit-local-branch-p (rev)
  (or (car (member rev (magit-list-local-branches)))
      (car (member rev (magit-list-local-branch-names)))))

(defun magit-remote-branch-p (rev)
  (or (car (member rev (magit-list-remote-branches)))
      (car (member rev (magit-list-remote-branch-names)))))

(defun magit-branch-set-face (branch)
  (propertize branch 'face (if (magit-local-branch-p branch)
                               'magit-branch-local
                             'magit-branch-remote)))

(defun magit-tag-p (rev)
  (car (member rev (magit-list-tags))))

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
  (--if-let (magit-get "core.abbrev")
      (string-to-number it)
    ;; Guess the length git will be using based on an example
    ;; abbreviation.  Actually HEAD's abbreviation might be an
    ;; outlier, so use the shorter of the abbreviations for two
    ;; commits.  When a commit does not exist, then fall back
    ;; to the default of 7.  See #3034.
    (min (--if-let (magit-rev-parse "--short" "HEAD")  (length it) 7)
         (--if-let (magit-rev-parse "--short" "HEAD~") (length it) 7))))

(defun magit-abbrev-arg (&optional arg)
  (format "--%s=%d" (or arg "abbrev") (magit-abbrev-length)))

(defun magit-rev-abbrev (rev)
  (magit-rev-parse (magit-abbrev-arg "short") rev))

(defun magit-commit-children (commit &optional args)
  (-map #'car
        (--filter (member commit (cdr it))
                  (--map (split-string it " ")
                         (magit-git-lines
                          "log" "--format=%H %P"
                          (or args (list "--branches" "--tags" "--remotes"))
                          "--not" commit)))))

(defun magit-commit-parents (commit)
  (--when-let (magit-git-string "rev-list" "-1" "--parents" commit)
    (cdr (split-string it))))

(defun magit-assert-one-parent (commit command)
  (when (> (length (magit-commit-parents commit)) 1)
    (user-error "Cannot %s a merge commit" command)))

(defun magit-patch-id (rev)
  (with-temp-buffer
    (magit-process-file
     shell-file-name nil '(t nil) nil shell-command-switch
     (let ((exec (shell-quote-argument magit-git-executable)))
       (format "%s diff-tree -u %s | %s patch-id" exec rev exec)))
    (car (split-string (buffer-string)))))

(defun magit-rev-format (format &optional rev args)
  (let ((str (magit-git-string "show" "--no-patch"
                               (concat "--format=" format) args
                               (if rev (concat rev "^{commit}") "HEAD") "--")))
    (unless (string-equal str "")
      str)))

(defun magit-rev-insert-format (format &optional rev args)
  (magit-git-insert "show" "--no-patch"
                    (concat "--format=" format) args
                    (if rev (concat rev "^{commit}") "HEAD") "--"))

(defun magit-format-rev-summary (rev)
  (--when-let (magit-rev-format "%h %s" rev)
    (string-match " " it)
    (put-text-property 0 (match-beginning 0) 'face 'magit-hash it)
    it))

(defun magit-format-ref-label (ref &optional head)
  (-let [(_re face fn)
         (--first (string-match (car it) ref) magit-ref-namespaces)]
    (if fn
        (funcall fn ref face)
      (propertize (or (match-string 1 ref) ref)
                  'face (if (equal ref head) 'magit-branch-current face)))))

(defun magit-format-ref-labels (string)
  (save-match-data
    (let ((regexp "\\(, \\|tag: \\| -> \\|[()]\\)") head names)
      (if (and (derived-mode-p 'magit-log-mode)
               (member "--simplify-by-decoration" (cadr magit-refresh-args)))
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

(defun magit-object-type (object)
  (magit-git-string "cat-file" "-t" object))

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

(defmacro magit-with-temp-index (tree arg &rest body)
  (declare (indent 2) (debug (form form body)))
  (let ((file (cl-gensym "file")))
    `(let ((magit--refresh-cache nil)
           (,file (magit-convert-filename-for-git
                   (make-temp-name (magit-git-dir "index.magit.")))))
       (unwind-protect
           (progn (--when-let ,tree
                    (or (magit-git-success "read-tree" ,arg it
                                           (concat "--index-output=" ,file))
                        (error "Cannot read tree %s" it)))
                  (if (file-remote-p default-directory)
                      (let ((magit-tramp-process-environment
                             (cons (concat "GIT_INDEX_FILE=" ,file)
                                   magit-tramp-process-environment)))
                        ,@body)
                    (let ((process-environment
                           (cons (concat "GIT_INDEX_FILE=" ,file)
                                 process-environment)))
                      ,@body)))
         (ignore-errors
           (delete-file (concat (file-remote-p default-directory) ,file)))))))

(defun magit-commit-tree (message &optional tree &rest parents)
  (magit-git-string "commit-tree" "--no-gpg-sign" "-m" message
                    (--mapcat (list "-p" it) (delq nil parents))
                    (or tree (magit-git-string "write-tree"))))

(defun magit-commit-worktree (message &optional arg &rest other-parents)
  (magit-with-temp-index "HEAD" arg
    (and (magit-update-files (magit-modified-files))
         (apply #'magit-commit-tree message nil "HEAD" other-parents))))

(defun magit-update-files (files)
  (magit-git-success "update-index" "--add" "--remove" "--" files))

(defun magit-update-ref (ref message rev &optional stashish)
  (or (if (not (version< (magit-git-version) "2.6.0"))
          (magit-git-success "update-ref" "--create-reflog"
                             "-m" message ref rev
                             (or (magit-rev-verify ref) ""))
        ;; `--create-reflog' didn't exist before v2.6.0
        (let ((oldrev  (magit-rev-verify ref))
              (logfile (magit-git-dir (concat "logs/" ref))))
          (unless (file-exists-p logfile)
            (when oldrev
              (magit-git-success "update-ref" "-d" ref oldrev))
            (make-directory (file-name-directory logfile) t)
            (with-temp-file logfile)
            (when (and oldrev (not stashish))
              (magit-git-success "update-ref" "-m" "enable reflog"
                                 ref oldrev ""))))
        (magit-git-success "update-ref" "-m" message ref rev
                           (or (magit-rev-verify ref) "")))
      (error "Cannot update %s with %s" ref rev)))

(defconst magit-range-re
  (concat "\\`\\([^ \t]*[^.]\\)?"       ; revA
          "\\(\\.\\.\\.?\\)"            ; range marker
          "\\([^.][^ \t]*\\)?\\'"))     ; revB

(defun magit-split-range (range)
  (when (string-match magit-range-re range)
    (let ((beg (or (match-string 1 range) "HEAD"))
          (end (or (match-string 3 range) "HEAD")))
      (cons (if (string-equal (match-string 2 range) "...")
                (magit-git-string "merge-base" beg end)
              beg)
            end))))

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
  (magit-completing-read-multiple prompt
                                  (magit-list-refnames)
                                  "\\.\\.\\.?"
                                  default 'magit-revision-history))

(defun magit-read-remote-branch
    (prompt &optional remote default local-branch require-match)
  (let ((choice (magit-completing-read
                 prompt
                 (-union (and local-branch
                              (if remote
                                  (concat remote "/" local-branch)
                                (--map (concat it "/" local-branch)
                                       (magit-list-remotes))))
                         (magit-list-remote-branch-names remote t))
                 nil require-match nil 'magit-revision-history default)))
    (if (or remote (string-match "\\`\\([^/]+\\)/\\(.+\\)" choice))
        choice
      (user-error "`%s' doesn't have the form REMOTE/BRANCH" choice))))

(defun magit-read-refspec (prompt remote)
  (magit-completing-read prompt
                         (prog2 (message "Determining available refs...")
                             (magit-remote-list-refs remote)
                           (message "Determining available refs...done"))))

(defun magit-read-local-branch (prompt &optional secondary-default)
  (magit-completing-read prompt (magit-list-local-branch-names)
                         nil t nil 'magit-revision-history
                         (or (magit-local-branch-at-point)
                             secondary-default
                             (magit-get-current-branch))))

(defun magit-read-local-branch-or-commit (prompt)
  (let ((branches (magit-list-local-branch-names))
        (commit (magit-commit-at-point)))
    (or (magit-completing-read prompt
                               (if commit (cons commit branches) branches)
                               nil nil nil 'magit-revision-history
                               (or (magit-local-branch-at-point) commit))
                     (user-error "Nothing selected"))))

(defun magit-read-local-branch-or-ref (prompt &optional secondary-default)
  (magit-completing-read prompt (nconc (magit-list-local-branch-names)
                                       (magit-list-refs "refs/"))
                         nil t nil 'magit-revision-history
                         (or (magit-local-branch-at-point)
                             secondary-default
                             (magit-get-current-branch))))

(defun magit-read-other-branch
    (prompt &optional exclude secondary-default no-require-match)
  (let* ((current (magit-get-current-branch))
         (atpoint (magit-branch-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default
                      (magit-get-previous-branch))))
    (magit-completing-read prompt (delete exclude (magit-list-branch-names))
                           nil (not no-require-match)
                           nil 'magit-revision-history default)))

(defun magit-read-other-branch-or-commit
    (prompt &optional exclude secondary-default)
  (let* ((current (magit-get-current-branch))
         (atpoint (magit-branch-or-commit-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      secondary-default
                      (magit-get-previous-branch))))
    (or (magit-completing-read prompt (delete exclude (magit-list-refnames))
                               nil nil nil 'magit-revision-history default)
        (user-error "Nothing selected"))))

(cl-defun magit-read-upstream-branch
    (&optional (branch (magit-get-current-branch)) prompt)
  (magit-completing-read
   (or prompt (format "Change upstream of %s to" branch))
   (-union (--map (concat it "/" branch)
                  (magit-list-remotes))
           (delete branch (magit-list-branch-names)))
   nil nil nil 'magit-revision-history
   (or (let ((r (magit-remote-branch-at-point))
             (l (magit-branch-at-point)))
         (when (and l (equal l branch))
           (setq l nil))
         (if magit-prefer-remote-upstream (or r l) (or l r)))
       (let ((r (magit-branch-p "origin/master"))
             (l (and (not (equal branch "master"))
                     (magit-branch-p "master"))))
         (if magit-prefer-remote-upstream (or r l) (or l r)))
       (let ((previous (magit-get-previous-branch)))
         (and (not (equal previous branch)) previous)))))

(defun magit-read-starting-point (prompt)
  (or (magit-completing-read (concat prompt " starting at")
                             (cons "HEAD" (magit-list-refnames))
                             nil nil nil 'magit-revision-history
                             (magit--default-starting-point))
      (user-error "Nothing selected")))

(defun magit--default-starting-point ()
  (or (let ((r (magit-remote-branch-at-point))
            (l (magit-local-branch-at-point)))
        (if magit-prefer-remote-upstream (or r l) (or l r)))
      (magit-commit-at-point)
      (magit-stash-at-point)
      (magit-get-current-branch)))

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

(defun magit-read-remote-or-url (prompt &optional default)
  (magit-completing-read prompt
                         (nconc (magit-list-remotes)
                                (list "https://" "git://" "git@"))
                         nil nil nil nil
                         (or default
                             (magit-remote-at-point)
                             (magit-get-remote))))

(defun magit-read-module-path (prompt)
  (magit-completing-read prompt (magit-get-submodules)))

;;; Variables

(defun magit-config-get-from-cached-list (key)
  (gethash
   ;; `git config --list' downcases first and last components of the key.
   (--> key
        (replace-regexp-in-string "\\`[^.]+" #'downcase it t t)
        (replace-regexp-in-string "[^.]+\\'" #'downcase it t t))
   (magit--with-refresh-cache (list 'config (magit-toplevel))
     (let ((configs (make-hash-table :test 'equal)))
       (dolist (conf (magit-git-items "config" "--list" "-z"))
         (let* ((nl-pos (cl-position ?\n conf))
                (key (substring conf 0 nl-pos))
                (val (if nl-pos (substring conf (1+ nl-pos)) "")))
           (puthash key (nconc (gethash key configs) (list val)) configs)))
       configs))))

(defun magit-get (&rest keys)
  "Return the value of Git config entry specified by KEYS."
  (car (last (apply 'magit-get-all keys))))

(defun magit-get-all (&rest keys)
  "Return all values of the Git config entry specified by KEYS."
  (let ((magit-git-debug nil)
        (key (mapconcat 'identity keys ".")))
    (if magit--refresh-cache
        (magit-config-get-from-cached-list key)
      (magit-git-items "config" "-z" "--get-all" key))))

(defun magit-get-boolean (&rest keys)
  "Return the boolean value of Git config entry specified by KEYS."
  (let ((key (mapconcat 'identity keys ".")))
    (if magit--refresh-cache
        (equal "true" (car (magit-config-get-from-cached-list key)))
      (magit-git-true "config" "--bool" key))))

(defun magit-set (val &rest keys)
  "Set Git config settings specified by KEYS to VAL."
  (let ((key (mapconcat 'identity keys ".")))
    (if val
        (magit-git-success "config" key val)
      (magit-git-success "config" "--unset" key))
    val))

(gv-define-setter magit-get (val &rest keys)
  `(magit-set ,val ,@keys))

(provide 'magit-git)
;;; magit-git.el ends here
