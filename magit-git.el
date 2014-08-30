;;; magit-git.el --- Git functionality

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

(require 'cl-lib)
(require 'dash)

(require 'magit-utils)
(require 'magit-section)

(defvar magit-refresh-args) ; from `magit-mode' for `magit-current-file'

(require 'tramp) ; for `magit-expand-git-file-name'

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

(defcustom magit-git-standard-options
  '("--no-pager" "-c" "core.preloadindex=true")
  "Standard options when running Git.
Be careful what you add here, especially if you are using
tramp to connect to servers with ancient Git versions."
  :group 'magit-process
  :type '(repeat string))

(defcustom magit-ref-namespaces
  '(("^\\(HEAD\\)$"              magit-head nil)
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
to return a propertized label that represents the ref.

Currently this variable is only used in logs and the branch
manager but it will be used in more places in the future."
  :package-version '(magit . "2.1.0")
  :group 'magit-modes
  :type '(repeat
          (list regexp
                face
                (choice (const :tag "first submatch is label" nil)
                        (function :tag "format using function")))))

;;; Git

(defun magit-process-git-arguments (args)
  (setq args (-flatten args))
  ;; Kludge for Powershell in combination with Cygwin Git, see #816.
  (when (and (eq system-type 'windows-nt)
             (let ((case-fold-search t))
               (string-match-p "cygwin" magit-git-executable)))
    (setq args (--map (replace-regexp-in-string
                       "{\\([0-9]+\\)}" "\\\\{\\1\\\\}" it)
                      args)))
  (append magit-git-standard-options args))

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

(defun magit-git-string (&rest args)
  "Execute Git with ARGS, returning the first line of its output.
If there is no output return nil.  If the output begins with a
newline return an empty string."
  (with-temp-buffer
    (apply #'process-file magit-git-executable nil (list t nil) nil
           (magit-process-git-arguments args))
    (unless (= (point-min) (point-max))
      (goto-char (point-min))
      (buffer-substring-no-properties
       (line-beginning-position)
       (line-end-position)))))

(defun magit-git-true (&rest args)
  "Execute Git with ARGS, returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (equal (magit-git-string args) "true"))

(defun magit-git-false (&rest args)
  "Execute Git with ARGS, returning t if it prints \"false\".
Return t if the first (and usually only) output line is the
string \"false\", otherwise return nil."
  (equal (magit-git-string args) "false"))

(defun magit-git-insert (&rest args)
  "Execute Git with ARGS, inserting its output at point."
  (apply #'process-file magit-git-executable nil (list t nil) nil
         (magit-process-git-arguments args)))

(defun magit-git-lines (&rest args)
  "Execute Git with ARGS, returning its output as a list of lines.
Empty lines anywhere in the output are omitted."
  (with-temp-buffer
    (apply #'process-file magit-git-executable nil (list t nil) nil
           (magit-process-git-arguments args))
    (split-string (buffer-string) "\n" 'omit-nulls)))

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

(defun magit-git-dir (&optional path)
  "Return absolute path to the GIT_DIR for the current repository.
If optional PATH is non-nil it has to be a path relative to the
GIT_DIR and its absolute path is returned"
  (--when-let (magit-rev-parse "--git-dir")
    (setq it (file-name-as-directory (magit-expand-git-file-name it)))
    (if path (expand-file-name (convert-standard-filename path) it) it)))

(defun magit-toplevel ()
  "Return the top-level directory for the current repository.

Determine the repository which contains `default-directory' in
its work tree and return the absolute path to its top-level
directory.  Otherwise return nil."
  (--when-let (magit-rev-parse "--show-toplevel")
    (file-name-as-directory (magit-expand-git-file-name it))))

(defun magit-get-top-dir (&optional directory)
  "Return the top-level directory for the current repository.

Determine the repository which contains `default-directory' in
either its work tree or Git control directory and return the
absolute path to its top-level directory.  If there is no top
directory, because the repository is bare, return the control
directory instead.

If optional DIRECTORY is non-nil then return the top directory
of the repository that contains that instead.  DIRECTORY has to
be an existing directory."
  (setq directory (if directory
                      (file-name-as-directory
                       (expand-file-name directory))
                    default-directory))
  (unless (file-directory-p directory)
    (error "%s isn't an existing directory" directory))
  (let ((default-directory directory))
    (or (magit-toplevel)
        (-when-let (gitdir (magit-git-dir))
          (if (magit-bare-repo-p)
              gitdir
            (file-name-directory (directory-file-name gitdir)))))))

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
    (setq file (file-truename file))
    (--when-let (magit-get-top-dir (file-name-directory file))
      (substring file (length it)))))

(defun magit-file-tracked-p (file)
  (magit-git-success "ls-files" "--error-unmatch" file))

(defun magit-tracked-files ()
  (magit-git-lines "ls-files" "--full-name"))

(defun magit-untracked-files ()
  (magit-git-lines "ls-files" "--full-name" "--other" "--exclude-standard"))

(defun magit-modified-files ()
  (magit-git-lines "diff-files" "--name-only"))

(defun magit-staged-files ()
  (magit-git-lines "diff-index" "--name-only" (magit-headish)))

(defun magit-unmerged-files ()
  (magit-git-lines "diff-files" "--name-only" "--diff-filter=U"))

(defun magit-revision-files (rev)
  (magit-git-lines "ls-tree" "-r" "--name-only" rev))

(defun magit-file-status (&optional file status)
  (if file
      (cdr (--first (or (string-equal (car it) file)
                        (string-match-p (format " -> %s$" (regexp-quote file))
                                        (car it)))
                    (or status (magit-file-status))))
    (--map (list (substring it 3) (aref it 0) (aref it 1))
           (magit-git-lines "status" "--porcelain" "-u" "--ignored"))))

(defun magit-expand-git-file-name (filename)
  (when (tramp-tramp-file-p default-directory)
    (setq filename (file-relative-name filename
                                       (with-parsed-tramp-file-name
                                           default-directory nil
                                         localname))))
  (expand-file-name filename))

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
  (not (magit-git-string "rev-list" "-1" "HEAD")))

(defun magit-anything-staged-p (&rest files)
  "Return t if there are any staged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (magit-git-failure "diff" "--quiet" "--cached" "--" files))

(defun magit-anything-unstaged-p (&rest files)
  "Return t if there are any unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (magit-git-failure "diff" "--quiet" "--" files))

(defun magit-anything-modified-p (&rest files)
  "Return t if there are any staged or unstaged changes.
If optional FILES is non-nil, then only changes to those files
are considered."
  (or (apply 'magit-anything-staged-p files)
      (apply 'magit-anything-unstaged-p files)))

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

(defun magit-rev-parse-p (&rest args)
  "Execute `git rev-parse ARGS', returning t if it prints \"true\".
Return t if the first (and usually only) output line is the
string \"true\", otherwise return nil."
  (magit-git-true "rev-parse" args))

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

(defun magit-commit-at-point ()
  (magit-section-when commit))

(defun magit-branch-or-commit-at-point ()
  (magit-section-case
    (branch (magit-section-value it))
    (commit (magit-get-shortname (magit-section-value it)))))

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
Return nil if the previously checked out branch no longer exists."
  (let ((current (magit-get-current-branch))
        (i 1) prev)
    (while (and (setq prev (magit-rev-parse "--verify" (format "@{-%i}" i)))
                (setq prev (magit-get-shortname prev))
                (equal prev current))
      (cl-incf i))
    prev))

(defun magit-get-tracked-branch (&optional branch qualified)
  "Return the name of the tracking branch the local branch name BRANCH.

If optional QUALIFIED is non-nil return the full branch path,
otherwise try to shorten it to a name (which may fail)."
  (unless branch
    (setq branch (magit-get-current-branch)))
  (when branch
    (let ((remote (magit-get "branch" branch "remote"))
          (merge  (magit-get "branch" branch "merge")))
      (when (and (not merge)
                 (not (equal remote ".")))
        (setq merge branch))
      (when (and remote merge)
        (if (string= remote ".")
            (cond (qualified merge)
                  ((string-match "^refs/heads/" merge)
                   (substring merge 11))
                  ((string-match "^refs/" merge)
                   merge))
          (let* ((fetch (--map (split-string it "[+:]" t)
                               (magit-get-all "remote" remote "fetch")))
                 (match (cadr (assoc merge fetch))))
            (unless match
              (let* ((prefix (nreverse (split-string merge "/")))
                     (unique (list (car prefix))))
                (setq prefix (cdr prefix))
                (setq fetch
                      (--mapcat (cl-destructuring-bind (from to) it
                                  (setq from (nreverse (split-string from "/")))
                                  (when (equal (car from) "*")
                                    (list (list (cdr from) to))))
                                fetch))
                (while (and prefix (not match))
                  (if (setq match (cadr (assoc prefix fetch)))
                      (setq match (concat (substring match 0 -1)
                                          (mapconcat 'identity unique "/")))
                    (push (car prefix) unique)
                    (setq prefix (cdr prefix))))))
            (cond ((not match) nil)
                  (qualified match)
                  ((string-match "^refs/remotes/" match)
                   (substring match 13))
                  (t match))))))))

(defun magit-get-current-remote ()
  "Return the remote configured for the current branch.
If HEAD is detached, or the current branch doesn't track
any branch or tracks another local branch, return nil."
  (-when-let (branch (magit-get-current-branch))
    (let ((remote (magit-get "branch" branch "remote")))
      (unless (equal remote ".")
        remote))))

(defun magit-get-current-tag (&optional with-distance)
  "Return the closest tag reachable from \"HEAD\".

If optional WITH-DISTANCE is non-nil then return (TAG COMMITS),
if it is `dirty' return (TAG COMMIT DIRTY). COMMITS is the number
of commits in \"HEAD\" but not in TAG and DIRTY is t if there are
uncommitted changes, nil otherwise."
  (--when-let (magit-git-string "describe" "--long" "--tags"
                                (and (eq with-distance 'dirty) "--dirty"))
    (save-match-data
      (string-match
       "\\(.+\\)-\\(?:0[0-9]*\\|\\([0-9]+\\)\\)-g[0-9a-z]+\\(-dirty\\)?$" it)
      (if with-distance
          (list (match-string 1 it)
                (string-to-number (or (match-string 2 it) "0"))
                (and (match-string 3 it) t))
        (match-string 1 it)))))

(defun magit-get-next-tag (&optional with-distance)
  "Return the closest tag from which \"HEAD\" is reachable.

If no such tag can be found or if the distance is 0 (in which
case it is the current tag, not the next) return nil instead.

If optional WITH-DISTANCE is non-nil then return (TAG COMMITS)
where COMMITS is the number of commits in TAG but not in \"HEAD\"."
  (--when-let (magit-git-string "describe" "--contains" "HEAD")
    (save-match-data
      (when (string-match "^[^^~]+" it)
        (setq it (match-string 0 it))
        (unless (equal it (magit-get-current-tag))
          (if with-distance
              (list it (car (magit-rev-diff-count it "HEAD")))
            it))))))

(defun magit-list-refs (&rest args)
  (magit-git-lines
   "for-each-ref" "--format=%(refname)"
   (or args (list "refs/heads" "refs/remotes" "refs/tags"))))

(defun magit-list-branches ()
  (magit-list-refs "refs/heads" "refs/remotes"))

(defun magit-list-local-branches ()
  (magit-list-refs "refs/heads"))

(defun magit-list-remote-branches (&optional remote)
  (magit-list-refs (concat "refs/remotes/" remote)))

(defun magit-list-refnames (&rest args)
  (magit-git-lines
   "for-each-ref" "--format=%(refname:short)"
   (or args (list "refs/heads" "refs/remotes" "refs/tags"))))

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

(defun magit-branch-p (string)
  (and (or (member string (magit-list-branches))
           (member string (magit-list-branch-names))) t))

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
  (cdr (split-string (magit-git-string "rev-list" "-1" "--parents" commit))))

(defun magit-assert-one-parent (commit command)
  (when (> (length (magit-commit-parents commit)) 1)
    (user-error "Cannot %s a merge commit" command)))

(defun magit-reflog-enable (ref)
  (let ((logfile (magit-git-dir (concat "logs/" ref))))
    (unless (file-exists-p logfile)
      (make-directory (file-name-directory logfile) t)
      (with-temp-file logfile))))

(defun magit-rev-format (format &optional rev)
  "Return first line of `git log -1 --format=format:FORMAT [REV]'.
Execute Git, returning the first line of its output.  If there is
no output return nil."
  (magit-git-string "log" "-1" (concat "--format=format:" format) rev))

(defun magit-format-rev-summary (rev)
  (--when-let (magit-rev-format "%h %s" rev)
    (string-match " " it)
    (put-text-property 0 (match-beginning 0) 'face 'magit-hash it)
    it))

(defun magit-format-ref-label (ref)
  (cl-destructuring-bind (re face fn)
      (--first (string-match (car it) ref) magit-ref-namespaces)
    (if fn
        (funcall fn ref face)
      (propertize (or (match-string 1 ref) ref) 'face face))))

(defun magit-format-ref-labels (string)
  (save-match-data
    (mapconcat 'magit-format-ref-label
               (split-string string "\\(tag: \\|[(), ]\\)" t) " ")))

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

;;; Completion

(defvar magit-revision-history nil)

(defun magit-read-branch (prompt &optional default)
  (magit-completing-read prompt (magit-list-branch-names)
                         nil t nil 'magit-revision-history
                         (or (magit-branch-at-point)
                             default (magit-get-current-branch))))

(defun magit-read-branch-or-commit (prompt &optional default)
  (or (magit-completing-read prompt (magit-list-refnames)
                             nil nil nil 'magit-revision-history
                             (or (magit-branch-or-commit-at-point)
                                 default (magit-get-current-branch)))
      (user-error "Nothing selected")))

(defalias 'magit-read-range-or-commit #'magit-read-branch-or-commit)

(defun magit-read-remote-branch (prompt remote &optional default)
  (magit-completing-read prompt (magit-list-remote-branch-names remote t)
                         nil nil nil 'magit-revision-history default))

(defun magit-read-local-branch (prompt &optional default)
  (magit-completing-read prompt (magit-list-local-branch-names)
                         nil t nil 'magit-revision-history
                         (or (magit-branch-at-point)
                             default (magit-get-current-branch))))

(defun magit-read-other-branch (prompt &optional exclude default)
  (let* ((current (magit-get-current-branch))
         (atpoint (magit-branch-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      default (magit-get-previous-branch))))
    (magit-completing-read prompt (delete exclude (magit-list-branch-names))
                           nil t nil 'magit-revision-history default)))

(defun magit-read-other-branch-or-commit (prompt &optional exclude default)
  (let* ((current (magit-get-current-branch))
         (atpoint (magit-branch-or-commit-at-point))
         (exclude (or exclude current))
         (default (or (and (not (equal atpoint exclude)) atpoint)
                      (and (not (equal current exclude)) current)
                      default (magit-get-previous-branch))))
    (or (magit-completing-read prompt (delete exclude (magit-list-refnames))
                               nil nil nil 'magit-revision-history default)
        (user-error "Nothing selected"))))

(defun magit-read-tag (prompt &optional require-match)
  (magit-completing-read prompt (magit-git-lines "tag") nil
                         require-match nil 'magit-revision-history))

(defun magit-read-stash (prompt &optional use-at-point)
  (let ((atpoint (magit-stash-at-point)))
    (or (and use-at-point atpoint)
        (magit-completing-read prompt
                               (--map (car (split-string it ":"))
                                      (magit-git-lines "stash" "list"))
                               nil t nil nil atpoint))))

(defun magit-read-remote (prompt &optional default)
  (magit-completing-read prompt (magit-git-lines "remote")
                         nil t nil nil
                         (or default
                             (magit-remote-at-point)
                             (magit-get-current-remote))))

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (revision prompt &optional default)
  (unless revision
    (setq revision "HEAD"))
  (unless default
    (setq default (magit-current-file)))
  (let* ((default-directory (magit-get-top-dir))
         (files (magit-revision-files revision)))
    (when (and default (not (member default files)))
      (setq default nil))
    (magit-completing-read prompt files
                           nil 'require-match
                           nil 'magit-read-file-hist default)))

;;; Variables

(defun magit-get (&rest keys)
  "Return the value of Git config entry specified by KEYS."
  (magit-git-string "config" (mapconcat 'identity keys ".")))

(defun magit-get-all (&rest keys)
  "Return all values of the Git config entry specified by KEYS."
  (magit-git-lines "config" "--get-all" (mapconcat 'identity keys ".")))

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
