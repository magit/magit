;;; magit.el --- A Git porcelain inside Emacs

;; Copyright (C) 2008-2015  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Former-Maintainers:
;;	Nicolas Dudebout  <nicolas.dudebout@gatech.edu>
;;	Peter J. Weisberg <pj@irregularexpressions.net>
;;	Phil Jackson      <phil@shellarchive.co.uk>
;;	Rémi Vanicat      <vanicat@debian.org>
;;	Yann Hodique      <yann.hodique@gmail.com>

;; Package-Requires: ((emacs "24.4") (dash "2.10.0") (with-editor "2.1.0") (git-commit "2.1.0") (magit-popup "2.1.0"))
;; Keywords: git tools vc
;; Homepage: https://github.com/magit/magit

;; Magit requires at least GNU Emacs 24.4 and Git 1.9.4.

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

;; Magit is an interface to the version control system Git,
;; implemented as an Emacs package.  Magit aspires to be a complete
;; Git porcelain.  While we cannot (yet) claim, that Magit wraps and
;; improves upon each and every Git command, it is complete enough to
;; allow even experienced Git users to perform almost all of their
;; daily version control tasks directly from within Emacs.  While many
;; fine Git clients exist, only Magit and Git itself deserve to be
;; called porcelains.

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'with-editor)
(require 'git-commit)
(require 'magit-core)
(require 'magit-diff)
(require 'magit-apply)
(require 'magit-log)

(require 'format-spec)
(require 'package nil t) ; used in `magit-version'

(eval-when-compile (require 'dired-x))
(declare-function dired-jump 'dired-x)
(eval-when-compile (require 'eshell))
(declare-function eshell-parse-arguments 'eshell)
(eval-when-compile (require 'message))
(declare-function message-goto-body 'message)

;;; Options
;;;; Status Mode

(defgroup magit-status nil
  "Inspect and manipulate Git repositories."
  :group 'magit-modes)

(defcustom magit-status-mode-hook nil
  "Hook run after entering Magit-Status mode."
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-headers-hook
  '(magit-insert-head-header
    magit-insert-upstream-header
    magit-insert-tags-header)
  "Hook run to insert headers into the status buffer.

This hook is run by `magit-insert-status-headers', which in turn
has to be a member of `magit-insert-status-sections' to be used
at all."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'hook
  :options '(magit-insert-repo-header
             magit-insert-remote-header
             magit-insert-head-header
             magit-insert-upstream-header
             magit-insert-tags-header))

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
  "Hook run to insert sections into a status buffer."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-refresh-hook nil
  "Hook run after a status buffer has been refreshed."
  :package-version '(magit . "2.1.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-status-buffer-switch-function 'pop-to-buffer
  "Function used by `magit-status' to switch to a status buffer.

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

;;;; Refs Mode

(defgroup magit-refs nil
  "Inspect and manipulate Git branches and tags."
  :group 'magit-modes)

(defcustom magit-refs-mode-hook nil
  "Hook run after entering Magit-Refs mode."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :type 'hook)

(defcustom magit-refs-sections-hook
  '(magit-insert-branch-description
    magit-insert-local-branches
    magit-insert-remote-branches
    magit-insert-tags)
  "Hook run to insert sections into a references buffer."
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

(defcustom magit-refs-show-commit-count nil
  "Whether to show commit counts in Magit-Refs mode buffers.

all    Show counts for branches and tags.
branch Show counts for branches only.
nil    Never show counts."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :safe (lambda (val) (memq val '(all branch nil)))
  :type '(choice (const all    :tag "For branches and tags")
                 (const branch :tag "For branches only")
                 (const nil    :tag "Never")))
(put 'magit-refs-show-commit-count 'safe-local-variable 'symbolp)
(put 'magit-refs-show-commit-count 'permanent-local t)

(defcustom magit-refs-show-margin 'branch
  "Whether to initially show the margin in refs buffers.

When non-nil the committer name and date are initially displayed
in the margin of refs buffers.  The margin can be shown or hidden
in the current buffer using the command `magit-toggle-margin'."
  :package-version '(magit . "2.1.0")
  :group 'magit-refs
  :safe (lambda (val) (memq val '(all branch nil)))
  :type '(choice (const all    :tag "For branches and tags")
                 (const branch :tag "For branches only")
                 (const nil    :tag "Never")))

;;;; Miscellaneous

(defcustom magit-repository-directories nil
  "Directories containing Git repositories.
Magit checks these directories for Git repositories and offers
them as choices when `magit-status' is used with a prefix
argument."
  :group 'magit
  :type '(repeat string))

(defcustom magit-repository-directories-depth 3
  "The maximum depth to look for Git repositories.
When looking for a Git repository below the directories in
`magit-repository-directories', only descend this many levels
deep."
  :group 'magit
  :type 'integer)

;;;; Faces

(defface magit-header-line
  '((t :inherit magit-section-heading))
  "Face for the `header-line'."
  :group 'magit-faces)

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

(defface magit-tag
  '((((class color) (background light)) :foreground "Goldenrod4")
    (((class color) (background  dark)) :foreground "LightGoldenrod2"))
  "Face for tag labels shown in log buffer."
  :group 'magit-faces)

(defface magit-branch-remote
  '((((class color) (background light)) :foreground "DarkOliveGreen4")
    (((class color) (background  dark)) :foreground "DarkSeaGreen2"))
  "Face for remote branch head labels shown in log buffer."
  :group 'magit-faces)

(defface magit-branch-local
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for local branches."
  :group 'magit-faces)

(defface magit-branch-current
  '((((class color) (background light)) :inherit magit-branch-local :box t)
    (((class color) (background  dark)) :inherit magit-branch-local :box t))
  "Face for current branch."
  :group 'magit-faces)

(defface magit-head
  '((((class color) (background light)) :inherit magit-branch-local)
    (((class color) (background  dark)) :inherit magit-branch-local))
  "Face for the symbolic ref \"HEAD\"."
  :group 'magit-faces)

(defface magit-refname
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
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

(defface magit-cherry-unmatched
  '((t :foreground "cyan"))
  "Face for unmatched cherry commits."
  :group 'magit-faces)

(defface magit-cherry-equivalent
  '((t :foreground "magenta"))
  "Face for equivalent cherry commits."
  :group 'magit-faces)

(defface magit-filename
  '((t :weight normal))
  "Face for filenames."
  :group 'magit-faces)

;;; Inspect
;;;; Status Mode

(defvar magit-status-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "jz" 'magit-jump-to-stashes)
    (define-key map "jt" 'magit-jump-to-tracked)
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
\\<magit-hunk-section-map>\
Type \\[magit-diff-visit-file] to visit the change at point.
Type \\[magit-stage] to stage the change at point; \\[magit-unstage] to unstage.
\\<magit-status-mode-map>\
Type \\[magit-commit-popup] to create a commit.
\n\\{magit-status-mode-map}"
  :group 'magit-status
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload
(defun magit-status (&optional directory)
  "Show the status of the current Git repository in a buffer.
With a prefix argument prompt for a repository to be shown.
With two prefix arguments prompt for an arbitrary directory.
If that directory isn't the root of an existing repository
then offer to initialize it as a new repository."
  (interactive
   (list (and (or current-prefix-arg (not (magit-toplevel)))
              (magit-read-repository
               (>= (prefix-numeric-value current-prefix-arg) 16)))))
  (if directory
      (let ((toplevel (magit-toplevel directory)))
        (setq directory (file-name-as-directory (expand-file-name directory)))
        (if (and toplevel (string-equal directory toplevel))
            (magit-status-internal directory)
          (when (y-or-n-p
                 (if toplevel
                     (format "%s is a repository.  Create another in %s? "
                             toplevel directory)
                   (format "Create repository in %s? " directory)))
            (magit-init directory))))
    (magit-status-internal default-directory)))

(put 'magit-status 'interactive-only 'magit-status-internal)

(defun magit-status-internal (directory &optional switch-function)
  (let ((default-directory (file-name-as-directory
                            (expand-file-name directory))))
    (magit-mode-setup magit-status-buffer-name-format
                      (or switch-function
                          magit-status-buffer-switch-function)
                      #'magit-status-mode
                      #'magit-status-refresh-buffer)))

(defun ido-enter-magit-status ()
  "Drop into `magit-status' from file switching.

To make this command available use something like:

  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-completion-map
                (kbd \"C-x g\") 'ido-enter-magit-status)))"
  (interactive)
  (with-no-warnings ; FIXME these are internal variables
    (setq ido-exit 'fallback fallback 'magit-status))
  (exit-minibuffer))

(defun magit-status-refresh-buffer ()
  (magit-git-exit-code "update-index" "--refresh")
  (magit-insert-section (status)
    (run-hooks 'magit-status-sections-hook))
  (run-hooks 'magit-status-refresh-hook))

(defun magit-insert-status-headers ()
  "Insert header sections appropriate for `magit-status-mode' buffers.
The sections are inserted by running the functions on the hook
`magit-status-headers-hook'."
  (if (magit-rev-verify "HEAD")
      (magit-insert-headers magit-status-headers-hook)
    (insert "In the beginning there was darkness\n\n")))

(defun magit-insert-repo-header ()
  "Insert a header line showing the path to the repository top-level."
  (let ((topdir (magit-toplevel)))
    (magit-insert-section (repo topdir)
      (magit-insert (format "%-10s%s\n" "Repo: "
                            (abbreviate-file-name topdir))))))

(defun magit-insert-remote-header ()
  "Insert a header line about the remote of the current branch."
  (-when-let (remote (magit-get-remote))
    (magit-insert-section (remote remote)
      (magit-insert
       (concat (format "%-10s" "Remote: ")
               (propertize remote 'face 'magit-branch-remote) " "
               (magit-get "remote" remote "url") "\n")))))

(cl-defun magit-insert-head-header
    (&optional (branch (magit-get-current-branch)))
  "Insert a header line about the `HEAD' commit."
  (let ((output (magit-rev-format "%h %s" "HEAD")))
    (string-match "^\\([^ ]+\\) \\(.*\\)" output)
    (magit-bind-match-strings (hash msg) output
      (magit-insert-section it (branch (or branch hash))
        (unless branch
          (setf (magit-section-type it) 'commit))
        (magit-insert
         (concat
          (format "%-10s" "Head: ")
          (propertize hash 'face 'magit-hash) " "
          (and branch
               (concat (propertize branch 'face 'magit-branch-local) " "))
          msg "\n"))))))

(cl-defun magit-insert-upstream-header
    (&optional (branch   (magit-get-current-branch))
               (upstream (magit-get-tracked-branch branch)))
  "Insert a header line about the upstream branch and its tip."
  (-when-let (string (and upstream (magit-rev-format "%h %s" upstream)))
    (string-match "^\\([^ ]+\\) \\(.*\\)" string)
    (magit-bind-match-strings (hash msg) string
      (magit-insert-section (branch upstream)
        (magit-insert
         (concat
          (format "%-10s" "Upstream: ")
          (if hash (propertize hash 'face 'magit-hash) "missing") " "
          (and (magit-get-boolean "branch" branch "rebase") "onto ")
          (propertize upstream 'face
                      (if (string= (magit-get "branch" branch "remote") ".")
                          'magit-branch-local
                        'magit-branch-remote))
          " " msg "\n"))))))

(defun magit-insert-tags-header ()
  "Insert a header line about the current and/or next tag."
  (let* ((this-tag (magit-get-current-tag nil t))
         (next-tag (magit-get-next-tag nil t))
         (this-cnt (cadr this-tag))
         (next-cnt (cadr next-tag))
         (this-tag (car this-tag))
         (next-tag (car next-tag))
         (both-tags (and this-tag next-tag t)))
    (when (or this-tag next-tag)
      (magit-insert-section (tag (or this-tag next-tag))
        (magit-insert
         (concat
          (format "%-10s" (if both-tags "Tags: " "Tag: "))
          (and this-tag (magit-format-status-tag-sentence this-tag this-cnt nil))
          (and both-tags ", ")
          (and next-tag (magit-format-status-tag-sentence next-tag next-cnt t))
          "\n"))))))

(defun magit-format-status-tag-sentence (tag count next)
  (concat (propertize tag 'face 'magit-tag)
          (and (> count 0)
               (format " (%s)"
                       (propertize (format "%s" count) 'face
                                   (if next 'magit-tag 'magit-branch-local))))))

(defun magit-insert-user-header ()
  "Insert a header line about the current user."
  (let ((name  (magit-get "user.name"))
        (email (magit-get "user.email")))
    (when (and name email)
      (magit-insert-section (user name)
        (magit-insert
         (concat (format "%-10s" "User: ")
                 (propertize name 'face 'magit-log-author)
                 " <" email ">" "\n"))))))

(magit-define-section-jumper tracked "Tracked files")

(defun magit-insert-tracked-files ()
  "Insert a tree of tracked files."
  (-when-let (files (magit-list-files))
    (magit-insert-section (tracked nil t)
      (magit-insert-heading "Tracked files:")
      (magit-insert-un/tracked-files-1 files nil)
      (insert ?\n))))

(magit-define-section-jumper untracked "Untracked files")

(defvar magit-untracked-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "k"  'magit-discard)
    (define-key map "s"  'magit-stage)
    map)
  "Keymap for the `untracked' section.")

(defun magit-insert-untracked-files ()
  "Maybe insert a list or tree of untracked files.
Do so depending on the value of `status.showUntrackedFiles'."
  (let ((show (or (magit-get "status.showUntrackedFiles") "normal")))
    (unless (equal show "no")
      (if (equal show "all")
          (-when-let (files (magit-untracked-files))
            (magit-insert-section (untracked)
              (magit-insert-heading "Untracked files:")
              (magit-insert-un/tracked-files-1 files nil)
              (insert ?\n)))
        (-when-let
            (files (--mapcat (and (eq (aref it 0) ??)
                                  (list (substring it 3)))
                             (magit-git-items "status" "-z" "--porcelain")))
          (magit-insert-section (untracked)
            (magit-insert-heading "Untracked files:")
            (dolist (file files)
              (magit-insert-section (file file)
                (insert (propertize file 'face 'magit-filename) ?\n))))
          (insert ?\n))))))

(defun magit-insert-un/tracked-files-1 (files directory)
  (while (and files (string-prefix-p (or directory "") (car files)))
    (let ((dir (file-name-directory (car files))))
      (if (equal dir directory)
          (let ((file (pop files)))
            (magit-insert-section (file file)
              (insert (propertize file 'face 'magit-filename) ?\n)))
        (magit-insert-section (file dir t)
          (insert (propertize dir 'file 'magit-filename) ?\n)
          (magit-insert-heading)
          (setq files (magit-insert-un/tracked-files-1 files dir))))))
  files)

;;;; Refs Mode

(defvar magit-refs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-mode-map)
    (define-key map "\C-y" 'magit-refs-set-show-commit-count)
    map)
  "Keymap for `magit-refs-mode'.")

(define-derived-mode magit-refs-mode magit-mode "Magit Refs"
  "Mode which lists and compares references.
This mode is documented in info node `(magit)Branches and Remotes'.

\\<magit-refs-mode-map>\
Type \\[magit-refresh] to refresh the current buffer.
Type \\[magit-branch-popup] to see available branch commands.
Type \\[magit-show-commit] or \\[magit-diff-show-or-scroll-up]\
 to visit the commit at point.
Type \\[magit-merge-popup] to merge the branch or commit at point.
Type \\[magit-cherry-pick] to cherry-pick the commit at point.
Type \\[magit-reset-head] to reset HEAD to the commit at point.
\n\\{magit-refs-mode-map}"
  :group 'magit-modes
  (hack-dir-local-variables-non-file-buffer))

;;;###autoload (autoload 'magit-show-refs-popup "magit" nil t)
(magit-define-popup magit-show-refs-popup
  "Popup console for `magit-show-refs'."
  'magit-refs
  :man-page "git-branch"
  :switches '((?m "Merged to HEAD"            "--merged")
              (?M "Merged to master"          "--merged=master")
              (?n "Not merged to HEAD"        "--no-merged")
              (?N "Not merged to master"      "--no-merged=master"))
  :options  '((?c "Contains"   "--contains="  magit-read-branch-or-commit)
              (?m "Merged"     "--merged="    magit-read-branch-or-commit)
              (?n "Not merged" "--no-merged=" magit-read-branch-or-commit))
  :actions  '((?y "Show refs, comparing them with HEAD"
                  magit-show-refs-head)
              (?c "Show refs, comparing them with current branch"
                  magit-show-refs-current)
              (?o "Show refs, comparing them with other branch"
                  magit-show-refs))
  :default-action 'magit-show-refs-head
  :use-prefix 'popup)

;;;###autoload
(defun magit-show-refs-head (&optional args)
  "List and compare references in a dedicated buffer.
Refs are compared with `HEAD'."
  (interactive (list (magit-show-refs-arguments)))
  (magit-show-refs nil args))

;;;###autoload
(defun magit-show-refs-current (&optional args)
  "List and compare references in a dedicated buffer.
Refs are compared with the current branch or `HEAD' if
it is detached."
  (interactive (list (magit-show-refs-arguments)))
  (magit-show-refs (magit-get-current-branch) args))

;;;###autoload
(defun magit-show-refs (&optional ref args)
  "List and compare references in a dedicated buffer.
Refs are compared with a branch read form the user."
  (interactive (list (magit-read-other-branch "Compare with")
                     (magit-show-refs-arguments)))
  (magit-mode-setup magit-refs-buffer-name-format nil
                    #'magit-refs-mode
                    #'magit-refs-refresh-buffer ref args))

(defun magit-refs-refresh-buffer (&rest ignore)
  (setq magit-set-buffer-margin-refresh (not magit-show-margin))
  (unless (magit-rev-verify (or (car magit-refresh-args) "HEAD"))
    (setq magit-refs-show-commit-count nil))
  (magit-insert-section (branchbuf)
    (run-hooks 'magit-refs-sections-hook)))

(defconst magit-refs-branch-line-re
  (concat "^"
          "\\(?:[ \\*]\\) "
          "\\(?1:([^)]+)\\|[^ ]+?\\)"       ; branch
          "\\(?: +\\)"
          "\\(?2:[0-9a-fA-F]+\\) "          ; sha1
          "\\(?:\\["
          "\\(?4:[^:]+\\)"                  ; upstream
          "\\(?:: \\(?:"
          "\\(?7:gone\\)\\|"                ; gone
          "\\(?:ahead \\(?5:[0-9]+\\)\\)?"  ; ahead
          "\\(?:, \\)?"
          "\\(?:behind \\(?6:[0-9]+\\)\\)?" ; behind
          "\\)\\)?"
          "\\] \\)?"
          "\\(?3:.*\\)"))                   ; message

(defvar magit-refs-local-branch-format "%4c %-25n %U%m\n"
  "Format used for local branches in refs buffers.")
(defvar magit-refs-remote-branch-format "%4c %-25n %m\n"
  "Format used for remote branches in refs buffers.")
(defvar magit-refs-tags-format "%4c %-25n %m\n"
  "Format used for tags in refs buffers.")
(defvar magit-refs-indent-cherry-lines 3
  "Indentation of cherries in refs buffers.")

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

(defun magit-refs-set-show-commit-count ()
  "Change for which refs the commit count is shown."
  (interactive)
  (setq-local magit-refs-show-commit-count
              (magit-read-char-case "Show commit counts for " nil
                (?a "[a]ll refs" 'all)
                (?b "[b]ranches only" t)
                (?n "[n]othing" nil)))
  (magit-refresh))

(defun magit-insert-local-branches ()
  "Insert sections showing all local branches."
  (magit-insert-section (local nil)
    (magit-insert-heading "Branches:")
    (let ((current  (magit-get-current-branch))
          (branches (magit-list-local-branch-names)))
      (dolist (line (magit-git-lines "branch" "-vv"
                                     (cadr magit-refresh-args)))
        (string-match magit-refs-branch-line-re line)
        (magit-bind-match-strings
            (branch hash message upstream ahead behind gone) line
          (when (string-match-p "(" branch)
            (setq branch nil))
          (magit-insert-branch
           branch magit-refs-local-branch-format current branches
           'magit-branch-local hash message upstream ahead behind gone))))
    (insert ?\n)))

(defun magit-insert-remote-branches ()
  "Insert sections showing all remote-tracking branches."
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
          (when (string-match magit-refs-branch-line-re line)
            (magit-bind-match-strings (branch hash message) line
              (when (string-match-p (format "^%s/" remote) branch)
                (magit-insert-branch
                 branch magit-refs-remote-branch-format current branches
                 'magit-branch-remote hash message))))))
      (insert ?\n))))

(defun magit-insert-branch (branch format &rest args)
  "For internal use, don't add to a hook."
  (unless magit-refs-show-commit-count
    (setq format (replace-regexp-in-string "%[0-9]\\([cC]\\)" "%1\\1" format t)))
  (if (equal branch "HEAD")
      (magit-insert-section it (commit (magit-rev-parse "HEAD") t)
        (apply #'magit-insert-branch-1 it nil format args))
    (magit-insert-section it (branch branch t)
      (apply #'magit-insert-branch-1 it branch format args))))

(defun magit-insert-branch-1
    (section branch format current branches face
             &optional hash message upstream ahead behind gone)
  "For internal use, don't add to a hook."
  (let* ((head  (or (car magit-refresh-args) current "HEAD"))
         (count (and branch
                     (magit-refs-format-commit-count branch head format)))
         (mark  (and (or (equal branch head)
                         (and (not branch) (equal head "HEAD")))
                     (if (equal branch current)
                         (propertize "@" 'face 'magit-head)
                       (propertize "#" 'face 'magit-tag)))))
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
         (?c . ,(or mark count ""))
         (?C . ,(or mark " "))
         (?h . ,(or (propertize hash 'face 'magit-hash) ""))
         (?m . ,(or message ""))
         (?n . ,(propertize (or branch "(detached)") 'face face))
         (?u . ,(or upstream ""))
         (?U . ,(if upstream
                    (format (propertize "[%s%s] " 'face 'magit-dimmed)
                            upstream
                            (cond
                             (gone
                              (concat ": " (propertize gone 'face 'error)))
                             ((or ahead behind)
                              (concat ": "
                                      (and ahead (format "ahead %s" ahead))
                                      (and ahead behind ", ")
                                      (and behind (format "behind %s" behind))))
                             (t "")))
                  "")))))
    (when magit-show-margin
      (magit-refs-format-margin branch))
    (magit-refs-insert-cherry-commits head branch section)))

(defvar magit-tag-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-show-commit)
    (define-key map "k"  'magit-tag-delete)
    map)
  "Keymap for `tag' sections.")

(defun magit-insert-tags ()
  "Insert sections showing all tags."
  (-when-let (tags (magit-git-lines "tag" "-l" "-n"))
    (magit-insert-section (tags)
      (magit-insert-heading "Tags:")
      (let ((head (or (car magit-refresh-args)
                      (magit-get-current-branch)
                      "HEAD"))
            (format (if magit-refs-show-commit-count
                        magit-refs-tags-format
                      (replace-regexp-in-string
                       "%[0-9]\\([cC]\\)" "%1\\1" magit-refs-tags-format t))))
        (dolist (tag (nreverse tags))
          (string-match "^\\([^ \t]+\\)[ \t]+\\([^ \t\n].*\\)?" tag)
          (let* ((message (match-string 2 tag))
                 (tag     (match-string 1 tag))
                 (count   (magit-refs-format-commit-count
                           tag head format t)))
            (magit-insert-section section (tag tag t)
              (magit-insert-heading
               (format-spec format
                            `((?n . ,(propertize tag 'face 'magit-tag))
                              (?c . ,(or count ""))
                              (?m . ,(or message "")))))
              (when (and magit-show-margin
                         (eq magit-refs-show-margin 'all))
                (magit-refs-format-margin (concat tag "^{commit}")))
              (magit-refs-insert-cherry-commits head tag section)))))
      (insert ?\n))))

(defun magit-refs-insert-cherry-commits (head ref section)
  (if (magit-section-hidden section)
      (setf (magit-section-washer section)
            (apply-partially #'magit-refs-insert-cherry-commits-1
                             head ref section))
    (magit-refs-insert-cherry-commits-1 head ref section)))

(defun magit-refs-insert-cherry-commits-1 (head ref section)
  (let ((start (point)))
    (magit-git-wash (apply-partially 'magit-log-wash-log 'cherry)
      "cherry" "-v" "--abbrev" head ref magit-refresh-args)
    (unless (= (point) start)
      (insert (propertize "\n" 'magit-section section)))))

(defun magit-refs-format-commit-count (ref head format &optional tag-p)
  (and (string-match-p "%-?[0-9]+c" format)
       (if tag-p
           (eq magit-refs-show-commit-count 'all)
         magit-refs-show-commit-count)
       (let ((count (cadr (magit-rev-diff-count head ref))))
	 (and (> count 0)
	      (propertize (number-to-string count) 'face 'magit-dimmed)))))

(defun magit-refs-format-margin (commit)
  (save-excursion
    (goto-char (line-beginning-position 0))
    (let ((line (magit-rev-format "%ct%cN" commit)))
      (magit-format-log-margin (substring line 10)
                               (substring line 0 10)))))

;;;; Files

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

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (rev prompt &optional default)
  (let ((files (magit-revision-files rev)))
    (magit-completing-read
     prompt files nil t nil 'magit-read-file-hist
     (car (member (or default (magit-current-file)) files)))))

(defun magit-read-changed-file (rev-or-range prompt &optional default)
  (let ((files (magit-changed-files rev-or-range)))
    (magit-completing-read
     prompt files nil t nil 'magit-read-file-hist
     (car (member (or default (magit-current-file)) files)))))

(defun magit-get-revision-buffer (rev file &optional create)
  (funcall (if create 'get-buffer-create 'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun magit-get-revision-buffer-create (rev file)
  (magit-get-revision-buffer rev file t))

(defun magit-find-file-noselect (rev file)
  "Read FILE from REV into a buffer and return the buffer."
  (or (magit-get-revision-buffer rev file)
      (with-current-buffer (magit-get-revision-buffer-create rev file)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (magit-git-insert "cat-file" "-p" (concat rev ":" file)))
        (setq magit-buffer-revision  (magit-rev-format "%H" rev)
              magit-buffer-refname   rev
              magit-buffer-file-name (expand-file-name file (magit-toplevel)))
        (let ((buffer-file-name magit-buffer-file-name))
          (normal-mode t))
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)
        (goto-char (point-min))
        (run-hooks 'magit-find-file-hook)
        (current-buffer))))

(defun magit-find-file-index-noselect (file &optional revert)
  "Read FILE from the index into a buffer and return the buffer."
  (let* ((bufname (concat file ".~{index}~"))
         (origbuf (get-buffer bufname)))
    (with-current-buffer (get-buffer-create bufname)
      (when (or (not origbuf) revert
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
            magit-buffer-file-name (expand-file-name file (magit-toplevel)))
      (let ((buffer-file-name magit-buffer-file-name))
        (normal-mode t))
      (setq buffer-read-only t)
      (set-buffer-modified-p nil)
      (goto-char (point-min))
      (run-hooks 'magit-find-index-hook)
      (current-buffer))))

(defun magit-update-index ()
  "Update the index with the contents of the current buffer.
The current buffer has to be visiting a file in the index, which
is done using `magit-find-index-noselect'."
  (interactive)
  (let ((file (magit-file-relative-name)))
    (unless (equal magit-buffer-refname "{index}")
      (user-error "%s isn't visiting the index" file))
    (if (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
        (let ((index (make-temp-file "index"))
              (buffer (current-buffer)))
          (with-temp-file index
            (insert-buffer-substring buffer))
          (magit-call-git "update-index" "--cacheinfo"
                          (substring (magit-git-string "ls-files" "-s" file) 0 6)
                          (magit-git-string "hash-object" "-t" "blob" "-w"
                                            (concat "--path=" file)
                                            "--" index)
                          file)
          (set-buffer-modified-p nil))
      (message "Abort")))
  (--when-let (magit-mode-get-buffer
               magit-status-buffer-name-format 'magit-status-mode)
    (with-current-buffer it (magit-refresh)))
  t)

;;;###autoload
(defun magit-dired-jump (&optional other-window)
  "Visit file at point using Dired.
With a prefix argument, visit in other window.  If there
is no file at point then instead visit `default-directory'."
  (interactive "P")
  (dired-jump other-window (--if-let (magit-file-at-point)
                               (expand-file-name it)
                             default-directory)))

;;; Manipulate
;;;; Init

;;;###autoload
(defun magit-init (directory)
  "Initialize a Git repository, then show its status.

If the directory is below an existing repository, then the user
has to confirm that a new one should be created inside.  If the
directory is the root of the existing repository, then the user
has to confirm that it should be reinitialized.

Non-interactively DIRECTORY is (re-)initialized unconditionally."
  (interactive
   (let ((directory (file-name-as-directory
                     (expand-file-name
                      (read-directory-name "Create repository in: ")))))
     (-when-let (toplevel (magit-toplevel directory))
       (setq toplevel (expand-file-name toplevel))
       (unless (y-or-n-p (if (string-equal toplevel directory)
                             (format "Reinitialize existing repository %s? "
                                     directory)
                           (format "%s is a repository.  Create another in %s? "
                                   toplevel directory)))
         (user-error "Abort")))
     (list directory)))
  (make-directory directory t)
  ;; `git init' does not understand the meaning of "~"!
  (magit-call-git "init" (expand-file-name directory))
  (magit-status-internal directory))

;;;; Branch

;;;###autoload (autoload 'magit-branch-popup "magit" nil t)
(magit-define-popup magit-branch-popup
  "Popup console for branch commands."
  'magit-commands
  :man-page "git-branch"
  :switches '((?t "Set upstream configuration" "--track"))
  :actions  '((?b "Checkout"          magit-checkout)
              (?u "Set upstream"      magit-branch-set-upstream)
              (?k "Delete"            magit-branch-delete)
              (?c "Create"            magit-branch)
              (?U "Unset upstream"    magit-branch-unset-upstream)
              (?r "Rename"            magit-branch-rename)
              (?B "Create & Checkout" magit-branch-and-checkout)
              (?e "Set description"   magit-branch-edit-description))
  :default-arguments '("--track")
  :default-action 'magit-checkout
  :max-action-columns 3)

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
  (magit-run-git-no-revert "branch" args branch start-point))

;;;###autoload
(defun magit-branch-and-checkout (branch start-point &optional args)
  "Create and checkout BRANCH at branch or revision START-POINT.
\n(git checkout [ARGS] -b BRANCH START-POINT)."
  (interactive (magit-branch-read-args "Create and checkout branch"
                                       (magit-stash-at-point)))
  (if (string-match-p "^stash@{[0-9]+}$" start-point)
      (magit-run-git "stash" "branch" branch start-point)
    (magit-run-git "checkout" args "-b" branch start-point)))

(defun magit-branch-read-args (prompt &optional secondary-default)
  (let* ((args (magit-branch-arguments))
         (start (magit-read-branch-or-commit (concat prompt " starting at")
                                             secondary-default))
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
(defun magit-branch-delete (branches &optional force)
  "Delete one or multiple branches.
If the region marks multiple branches, then offer to delete
those, otherwise prompt for a single branch to be deleted,
defaulting to the branch at point."
  (interactive
   (let ((branches (magit-region-values 'branch))
         (force current-prefix-arg))
     (if (if (> (length branches) 1)
             (magit-confirm t nil "Delete %i branches" branches)
           (setq branches
                 (list (magit-read-branch (if current-prefix-arg
                                              "Force delete branch"
                                            "Delete branch")
                                          (magit-get-previous-branch)))))
         (unless force
           (--when-let (-intersection
                        (-union (magit-list-unmerged-branches)
                                (magit-list-unmerged-to-upstream-branches))
                        branches)
             (if (magit-confirm 'delete-unmerged-branch
                   "Delete unmerged branch %s"
                   "Delete %i unmerged branches" it)
                 (setq force t)
               (or (setq branches (-difference branches it))
                   (user-error "Abort")))))
       (user-error "Abort"))
     (list branches force)))
  (let* ((refs (-map #'magit-ref-fullname branches))
         (ambiguous (--filter (not it) refs)))
    (when ambiguous
      (user-error
       "%s ambiguous.  Please cleanup using git directly."
       (let ((len (length ambiguous)))
         (cond
          ((= len 1)
           (format "%s is" (--first (not (magit-ref-fullname it)) branches)))
          ((= len (length refs))
           (format "These %s names are" len))
          (t
           (format "%s of these names are" len))))))
    (cond
     ((string-match "^refs/remotes/\\([^/]+\\)" (car refs))
      (let* ((remote (match-string 1 (car refs)))
             (offset (1+ (length remote))))
        (magit-run-git-async
         "push" remote (--map (concat ":" (substring it offset)) branches))))
     ((> (length branches) 1)
      (magit-run-git "branch" (if force "-D" "-d")
                     (delete (magit-get-current-branch) branches)))
     (t ; And now for something completely different.
      (let* ((branch (car branches))
             (prompt (format "Branch %s is checked out.  " branch)))
        (when (equal branch (magit-get-current-branch))
          (pcase (if (or (equal branch "master")
                         (not (magit-rev-verify "master")))
                     (magit-read-char-case prompt nil
                       (?d "[d]etach HEAD & delete" 'detach)
                       (?a "[a]bort"                'abort))
                   (magit-read-char-case prompt nil
                     (?d "[d]etach HEAD & delete"     'detach)
                     (?c "[c]heckout master & delete" 'master)
                     (?a "[a]bort"                    'abort)))
            (`detach (magit-call-git "checkout" "--detach"))
            (`master (magit-call-git "checkout" "master"))
            (`abort  (user-error "Abort")))
          (setq force t))
        (magit-run-git "branch" (if force "-D" "-d") branch))))))

(put 'magit-branch-delete 'interactive-only t)

;;;###autoload
(defun magit-branch-set-upstream (branch upstream)
  "Change the UPSTREAM branch of BRANCH."
  (interactive
   (let  ((branch (magit-read-local-branch "Change upstream of branch")))
     (list branch (magit-completing-read
                   "Change upstream to branch"
                   (delete branch (magit-list-branch-names))
                   nil nil nil 'magit-revision-history))))
  (magit-run-git-no-revert "branch" (concat "--set-upstream-to=" upstream)
                           branch))

;;;###autoload
(defun magit-branch-unset-upstream (branch)
  "Unset the upstream branch of BRANCH."
  (interactive (list (magit-read-local-branch "Unset upstream of branch")))
  (magit-run-git-no-revert "branch" "--unset-upstream" branch))

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
    (magit-run-git-no-revert "branch" (if force "-M" "-m") old new)))

;;;###autoload
(defun magit-branch-edit-description (branch)
  "Edit the description of BRANCH."
  (interactive (list (magit-read-local-branch "Edit branch description")))
  (magit-run-git-with-editor "branch" "--edit-description"))

(defun magit-insert-branch-description ()
  "Insert header containing the description of the current branch.
Insert a header line with the name and description of the
current branch.  The description is taken from the Git variable
`branch.<NAME>.description'; if that is undefined then no header
line is inserted at all."
  (let ((branch (magit-get-current-branch)))
    (--when-let (magit-git-lines
                 "config" (format "branch.%s.description" branch))
      (magit-insert-section (branchdesc branch t)
        (magit-insert-heading branch ": " (car it))
        (insert (mapconcat 'identity (cdr it) "\n"))
        (insert "\n\n")))))

;;;; Merge

;;;###autoload (autoload 'magit-merge-popup "magit" nil t)
(magit-define-popup magit-merge-popup
  "Popup console for merge commands."
  'magit-commands 'magit-popup-sequence-mode
  :man-page "git-merge"
  :switches '((?f "Fast-forward only" "--ff-only")
              (?n "No fast-forward"   "--no-ff")
              (?s "Squash"            "--squash"))
  :options  '((?s "Strategy" "--strategy=" read-from-minibuffer))
  :actions  '((?m "Merge"                  magit-merge)
              (?e "Merge and edit message" magit-merge-editmsg)
              (?p "Preview merge"          magit-merge-preview)
              (?n "Merge but don't commit" magit-merge-nocommit))
  :sequence-actions   '((?m "Commit merge" magit-commit)
                        (?a "Abort merge"  magit-merge-abort))
  :sequence-predicate 'magit-merge-state
  :default-action 'magit-merge
  :max-action-columns 2)

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
                     (magit-merge-arguments)
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
                     (magit-merge-arguments)))
  (magit-merge-assert)
  (with-editor "GIT_EDITOR"
    (let ((magit-process-popup-time -1))
      (magit-run-git-async "merge" "--edit" args rev))))

;;;###autoload
(defun magit-merge-nocommit (rev &optional args)
  "Merge commit REV into the current branch; pretending it failed.
Pretend the merge failed to give the user the opportunity to
inspect the merge and change the commit message.
\n(git merge --no-commit [ARGS] rev)"
  (interactive (list (magit-read-other-branch-or-commit "Merge")
                     (magit-merge-arguments)))
  (magit-merge-assert)
  (magit-run-git "merge" "--no-commit" args rev))

;;;###autoload
(defun magit-merge-preview (rev)
  "Preview result of merging REV into the current branch."
  (interactive (list (magit-read-other-branch-or-commit "Preview merge")))
  (magit-mode-setup magit-diff-buffer-name-format
                    magit-diff-switch-buffer-function
                    #'magit-diff-mode
                    #'magit-merge-refresh-preview-buffer rev))

(defun magit-merge-refresh-preview-buffer (rev)
  (magit-insert-section (diffbuf)
    (let* ((branch (magit-get-current-branch))
           (head (or branch (magit-rev-verify "HEAD"))))
      (magit-insert-heading (format "Preview merge of %s into %s"
                                    rev (or branch "HEAD")))
      (magit-git-wash #'magit-diff-wash-diffs
        "merge-tree" (magit-git-string "merge-base" head rev) head rev))))

;;;###autoload
(defun magit-merge-abort ()
  "Abort the current merge operation.
\n(git merge --abort)"
  (interactive)
  (if (file-exists-p (magit-git-dir "MERGE_HEAD"))
      (when (magit-confirm 'abort-merge)
        (magit-run-git-async "merge" "--abort"))
    (user-error "No merge in progress")))

(defun magit-checkout-stage (file arg)
  "During a conflict checkout and stage side, or restore conflict."
  (interactive
   (let ((file (magit-completing-read "Checkout file"
                                      (magit-tracked-files) nil nil nil
                                      'magit-read-file-hist
                                      (magit-current-file))))
     (cond ((member file (magit-unmerged-files))
            (list file (magit-checkout-read-stage file)))
           ((yes-or-no-p (format "Restore conflicts in %s? " file))
            (list file "--merge"))
           (t
            (user-error "Quit")))))
  (pcase (cons arg (cddr (car (magit-file-status file))))
    ((or `("--ours"   ?D ,_)
         `("--theirs" ,_ ?D))
     (magit-run-git "rm" "--" file))
    (_ (if (equal arg "--merge")
           ;; This fails if the file was deleted on one
           ;; side.  And we cannot do anything about it.
           (magit-run-git "checkout" "--merge" "--" file)
         (magit-call-git "checkout" arg "--" file)
         (magit-run-git "add" "-u" "--" file)))))

(defun magit-merge-state ()
  (file-exists-p (magit-git-dir "MERGE_HEAD")))

(defun magit-merge-assert ()
  (or (not (magit-anything-modified-p))
      (magit-confirm 'merge-dirty
        "Merging with dirty worktree is risky.  Continue")
      (user-error "Abort")))

(defun magit-checkout-read-stage (file)
  (magit-read-char-case (format "For %s checkout: " file) t
    (?o "[o]ur stage"   "--ours")
    (?t "[t]heir stage" "--theirs")
    (?c "[c]onflict"    "--merge")))

(defun magit-insert-merge-log ()
  "Insert section for the on-going merge.
Display the heads that are being merged.
If no merge is in progress, do nothing."
  (-when-let (heads (mapcar 'magit-get-shortname
                            (magit-file-lines (magit-git-dir "MERGE_HEAD"))))
    (magit-insert-section (commit (car heads))
      (magit-insert-heading
        (format "Merging %s:" (mapconcat 'identity heads ", ")))
      (magit-insert-log
       (concat (magit-git-string "merge-base" "--octopus" "HEAD" (car heads))
               ".." (car heads))
       (let ((args magit-log-section-arguments))
         (unless (member "--decorate=full" magit-log-section-arguments)
           (push "--decorate=full" args))
         args)))))

;;;; Reset

;;;###autoload
(defun magit-reset-index (commit)
  "Reset the index to COMMIT.
Keep the head and working tree as-is, so if COMMIT refers to the
head this effectivley unstages all changes.
\n(git reset COMMIT)"
  (interactive (list (magit-read-branch-or-commit "Reset index to")))
  (magit-reset-internal nil commit))

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
  (magit-reset-internal (if hard "--hard" "--mixed") commit))

;;;###autoload
(defun magit-reset-head (commit)
  "Reset the head and index to COMMIT, but not the working tree.
\n(git reset --mixed COMMIT)"
  (interactive (list (magit-read-branch-or-commit "Reset head to")))
  (magit-reset-internal "--mixed" commit))

;;;###autoload
(defun magit-reset-soft (commit)
  "Reset the head to COMMIT, but not the index and working tree.
\n(git reset --soft REVISION)"
  (interactive (list (magit-read-branch-or-commit "Soft reset to")))
  (magit-reset-internal "--soft" commit))

;;;###autoload
(defun magit-reset-hard (commit)
  "Reset the head, index, and working tree to COMMIT.
\n(git reset --hard REVISION)"
  (interactive (list (magit-read-branch-or-commit "Hard reset to")))
  (magit-reset-internal "--hard" commit))

(defun magit-reset-internal (arg commit)
  (when (and (not (member arg '("--hard" nil)))
             (equal (magit-rev-parse commit)
                    (magit-rev-parse "HEAD~")))
    (with-temp-buffer
      (magit-git-insert "show" "-s" "--format=%B" "HEAD")
      (when git-commit-major-mode
        (funcall git-commit-major-mode))
      (git-commit-setup-font-lock)
      (git-commit-save-message)))
  (magit-wip-commit-before-change nil " before reset")
  (magit-run-git "reset" arg commit "--"))

;;;; Files

(defun magit-file-rename (file newname)
  "Rename the FILE to NEWNAME.
If FILE isn't tracked in Git fallback to using `rename-file'."
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
            (vc-find-file-hook))))
    (rename-file file newname current-prefix-arg)
    (magit-refresh)))

(defun magit-file-untrack (file)
  "Untrack FILE.
Stop tracking FILE in Git but do not remove it from the working
tree."
  (interactive (list (magit-read-tracked-file "Untrack file")))
  (magit-run-git "rm" "--cached" "--" file))

(defun magit-file-delete (file &optional force)
  "Delete FILE.
With a prefix argument FORCE do so even when FILE has uncommitted
changes.

If FILE isn't tracked in Git fallback to using `delete-file'."
  (interactive (list (magit-read-file "Delete file")))
  (if (magit-file-tracked-p file)
      (magit-run-git "rm" (and force "--force") "--" file)
    (delete-file (expand-file-name file (magit-toplevel)) t)
    (magit-refresh)))

(defun magit-read-tracked-file (prompt)
  (magit-read-file prompt t))

(defun magit-read-file (prompt &optional tracked-only)
  (let ((choices (nconc (magit-list-files)
                        (unless tracked-only (magit-untracked-files)))))
    (magit-completing-read prompt choices nil t nil nil
                           (car (member (or (magit-section-when (file))
                                            (magit-file-relative-name))
                                        choices)))))

(defun magit-read-files (prompt initial-contents)
  (mapconcat 'identity
             (completing-read-multiple (or prompt "File,s: ")
                                       (magit-list-files)
                                       nil nil initial-contents) ","))

;;; Miscellaneous
;;;; Tag

;;;###autoload (autoload 'magit-tag-popup "magit" nil t)
(magit-define-popup magit-tag-popup
  "Popup console for tag commands."
  'magit-commands
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
                     (let ((args (magit-tag-arguments)))
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
  (interactive (list (--if-let (magit-region-values 'tag)
                         (magit-confirm t nil "Delete %i tags" it)
                       (magit-read-tag "Delete tag" t))))
  (magit-run-git-no-revert "tag" "-d" tags))

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
     (unless (magit-confirm t "Delete %s locally"
               "Delete %i tags locally" ltags)
       (setq ltags nil))
     (unless (magit-confirm t "Delete %s from remote"
               "Delete %i tags from remote" rtags)
       (setq rtags nil))
     (list ltags rtags remote)))
  (when tags
    (magit-call-git "tag" "-d" tags))
  (when remote-tags
    (magit-run-git-async-no-revert
     "push" remote (--map (concat ":" it) remote-tags))))

;;;; Notes

;;;###autoload (autoload 'magit-notes-popup "magit" nil t)
(magit-define-popup magit-notes-popup
  "Popup console for notes commands."
  'magit-commands 'magit-popup-sequence-mode
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
  "Edit the note attached to COMMIT.
REF is the notes ref used to store the notes.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (magit-notes-read-args "Edit notes"))
  (magit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "edit" commit))

(defun magit-notes-remove (commit &optional ref)
  "Remove the note attached to COMMIT.
REF is the notes ref from which the note is removed.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (magit-notes-read-args "Remove notes"))
  (magit-run-git-with-editor "notes" "remove" commit))

(defun magit-notes-merge (ref)
  "Merge the notes ref REF into the current notes ref.

The current notes ref is the value of Git variable
`core.notesRef' or \"refs/notes/commits\" if that is undefined.

When there are conflict, then they have to resolved in the
temporary worktree \".git/NOTES_MERGE_WORKTREE\".  When
done use `magit-notes-merge-commit' to finish.  To abort
use `magit-notes-merge-abort'."
  (interactive (list (magit-read-string "Merge reference")))
  (magit-run-git-with-editor "notes" "merge" ref))

(defun magit-notes-merge-commit ()
  "Commit the current notes ref merge.
Also see `magit-notes-merge'."
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--commit"))

(defun magit-notes-merge-abort ()
  "Abort the current notes ref merge.
Also see `magit-notes-merge'."
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--abort"))

(defun magit-notes-prune (&optional dry-run)
  "Remove notes about unreachable commits."
  (interactive (list (and (member "--dry-run" (magit-notes-arguments)) t)))
  (when dry-run
    (magit-process))
  (magit-run-git-with-editor "notes" "prune" (and dry-run "--dry-run")))

(defun magit-notes-set-ref (ref &optional global)
  "Set the current notes ref to REF.
The ref is made current by setting the value of the Git variable
`core.notesRef'.  With a prefix argument GLOBAL change the global
value, else the value in the current repository.  When this is
undefined, then \"refs/notes/commit\" is used.

Other `magit-notes-*' commands, as well as the sub-commands
of Git's `note' command, default to operate on that ref."
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
      (magit-run-git-no-revert "config" (and global "--global") "core.notesRef"
                               (if (string-prefix-p "refs/" ref)
                                   ref
                                 (concat "refs/notes/" ref)))
    (magit-run-git-no-revert "config" (and global "--global")
                             "--unset" "core.notesRef")))

(defun magit-notes-set-display-refs (refs &optional global)
  "Set notes refs to be display in addition to \"core.notesRef\".
REFS is a colon separated list of notes refs.  The values are
stored in the Git variable `notes.displayRef'.  With a prefix
argument GLOBAL change the global values, else the values in
the current repository."
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
  (let ((inhibit-magit-revert t))
    (magit-refresh)))

(defun magit-notes-read-args (prompt)
 (list (magit-read-branch-or-commit prompt)
       (--when-let (--first (string-match "^--ref=\\(.+\\)" it)
                            (magit-notes-arguments))
         (match-string 1 it))))

(defun magit-notes-popup-read-ref (prompt &optional initial-input)
  (magit-completing-read prompt (nconc (list "refs/" "refs/notes/")
                                       (magit-list-notes-refnames))
                         nil nil initial-input))

(defun magit-notes-merging-p ()
  (let ((dir (magit-git-dir "NOTES_MERGE_WORKTREE")))
    (and (file-directory-p dir)
         (directory-files dir nil "^[^.]"))))

;;;; Submodules

;;;###autoload (autoload 'magit-submodule-popup "magit" nil t)
(magit-define-popup magit-submodule-popup
  "Popup console for submodule commands."
  'magit-commands nil nil
  :man-page "git-submodule"
  :actions  '((?a "Add"    magit-submodule-add)
              (?b "Setup"  magit-submodule-setup)
              (?i "Init"   magit-submodule-init)
              (?u "Update" magit-submodule-update)
              (?s "Sync"   magit-submodule-sync)
              (?f "Fetch"  magit-submodule-fetch)))

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
                   (magit-get "remote" (or (magit-get-remote) "origin")
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
  "Register submodules listed in \".gitmodules\" into \".git/config\"."
  (interactive)
  (magit-with-toplevel
    (magit-run-git-async "submodule" "init")))

;;;###autoload
(defun magit-submodule-update (&optional init)
  "Clone missing submodules and checkout appropriate commits.
With a prefix argument also register submodules in \".git/config\"."
  (interactive "P")
  (magit-with-toplevel
    (magit-run-git-async "submodule" "update" (and init "--init"))))

;;;###autoload
(defun magit-submodule-sync ()
  "Update each submodule's remote URL according to \".gitmodules\"."
  (interactive)
  (magit-with-toplevel
    (magit-run-git-async "submodule" "sync")))

;;;###autoload
(defun magit-submodule-fetch (&optional all)
  "Fetch all submodules.
With a prefix argument fetch all remotes."
  (interactive "P")
  (magit-with-toplevel
    (magit-run-git-async "submodule" "foreach"
                         (format "git fetch %s || true" (if all "--all" "")))))

;;;; Dispatch Popup

;;;###autoload (autoload 'magit-dispatch-popup "magit" nil t)
(magit-define-popup magit-dispatch-popup
  "Popup console for dispatching other popups."
  'magit-commands nil nil
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
             (?w "Apply patches"   magit-am-popup)
             (?W "Format patches"  magit-patch-popup)
             (?s "Show Status"     magit-status)
             (?S "Stage all"       magit-stage-modified)
             (?t "Tagging"         magit-tag-popup)
             (?U "Reset Index"     magit-reset-index)
             (?v "Show Commit"     magit-show-commit)
             (?V "Show File"       magit-find-file)
             (?y "Show Refs"       magit-show-refs-popup)
             (?Y "Cherry"          magit-cherry)
             (?z "Stashing"        magit-stash-popup)
             (?! "Running"         magit-run-popup)
             (?$ "Show Process"    magit-process)))

;;;; Git Popup

(defvar magit-git-command-history nil)

;;;###autoload (autoload 'magit-run-popup "magit" nil t)
(magit-define-popup magit-run-popup
  "Popup console for running raw Git commands."
  'magit-commands nil nil
  :actions '((?! "Git Subcommand (from root)" magit-git-command-topdir)
             (?: "Git Subcommand (from pwd)" magit-git-command)
             (?g "Git Gui" magit-run-git-gui)
             (?k "Gitk" magit-run-gitk))
  :default-action 'magit-git-command)

;;;###autoload
(defun magit-git-command (args directory)
  "Execute a Git subcommand asynchronously, displaying the output.
With a prefix argument run Git in the root of the current
repository, otherwise in `default-directory'.

Non-interactively run Git in DIRECTORY with ARGS."
  (interactive (magit-git-command-read-args))
  (require 'eshell)
  (magit-mode-display-buffer (magit-process-buffer nil t)
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
Run Git in the top-level directory of the current repository.
\n(fn)" ; arguments are for internal use
  (interactive (magit-git-command-read-args t))
  (magit-git-command args directory))

(defun magit-git-command-read-args (&optional root)
  (let ((dir (if (or root current-prefix-arg)
                 (or (magit-toplevel)
                     (user-error "Not inside a Git repository"))
               default-directory)))
    (list (magit-read-string (format "Git subcommand (in %s)"
                                     (abbreviate-file-name dir))
                             nil 'magit-git-command-history)
          dir)))

;;;; Read Repository

(defun magit-read-repository (&optional read-directory-name)
  "Read a Git repository in the minibuffer, with completion.

The completion choices are the basenames of top-levels of
repositories found in the directories specified by option
`magit-repository-directories'.  In case of name conflicts
the basenames are prefixed with the name of the respective
parent directories.  The returned value is the actual path
to the selected repository.

With prefix argument simply read a directory name using
`read-directory-name'."
  (if (and (not read-directory-name) magit-repository-directories)
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
                          (or (magit-toplevel) default-directory)))))

(defun magit-list-repos ()
  (--mapcat (magit-list-repos-1 it magit-repository-directories-depth)
            magit-repository-directories))

(defun magit-list-repos-1 (directory depth)
  (cond ((file-readable-p (expand-file-name ".git" directory))
         (list directory))
        ((and (> depth 0) (file-accessible-directory-p directory))
         (--mapcat (when (file-directory-p it)
                     (magit-list-repos-1 it (1- depth)))
                   (directory-files directory t "^[^.]" t)))))

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

(defun magit-copy-as-kill ()
  "Save the value of the current section to the kill ring.
For commits save the full hash.  For branches do so only when
a prefix argument is used, otherwise save the branch name.
When the region is active, then behave like `kill-ring-save'."
  (interactive)
  (if (region-active-p)
      (copy-region-as-kill (mark) (point) 'region)
    (-when-let (section (magit-current-section))
      (let ((value (magit-section-value section)))
        (magit-section-case
          (branch (when current-prefix-arg
                    (setq value (magit-rev-parse value))))
          (commit (setq value (magit-rev-parse value)))
          (module-commit (let ((default-directory
                                 (file-name-as-directory
                                  (expand-file-name
                                   (magit-section-parent-value section)
                                   (magit-toplevel)))))
                           (setq value (magit-rev-parse value))))
          (t value))
        (kill-new (message "%s" value))))))

(defun magit-copy-buffer-thing-as-kill ()
  "Save the thing displayed in the current buffer to the kill ring.
When the region is active, then behave like `kill-ring-save'."
  (interactive)
  (if (region-active-p)
      (copy-region-as-kill (mark) (point) 'region)
    (--when-let (cond ((derived-mode-p 'magit-revision-mode)
                       (magit-rev-parse (car magit-refresh-args)))
                      ((derived-mode-p 'magit-diff-mode
                                       'magit-cherry-mode
                                       'magit-reflog-mode
                                       'magit-refs-mode
                                       'magit-stash-mode)
                       (car magit-refresh-args))
                      ((derived-mode-p 'magit-log-mode)
                       (if magit-log-select-pick-function
                           (car magit-refresh-args)
                         (cadr magit-refresh-args)))
                      ((derived-mode-p 'magit-status-mode)
                       (or (magit-get-current-branch) "HEAD"))
                      ((derived-mode-p 'magit-stashes-mode)
                       "refs/stash")
                      (t (magit-copy-as-kill)))
      (kill-new (message "%s" it)))))

;;; magit.el ends soon

(defconst magit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(magit-define-section-jumper\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt '("magit-insert-section"
                                  "magit-section-case"
                                  "magit-section-when"
                                  "magit-bind-match-strings"
                                  "magit-with-temp-index"
                                  "magit-with-blob"
                                  "magit-with-toplevel") t)
                "\\_>")
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode magit-font-lock-keywords)

(defvar magit-version 'undefined
  "The version of Magit that you're using.
Use the function by the same name instead of this variable.")

(defun magit-version ()
  "Return the version of Magit currently in use.
When called interactive also show the used versions of Magit,
Git, and Emacs in the echo area."
  (interactive)
  (let ((magit-git-global-arguments nil)
        (toplib (or load-file-name buffer-file-name)))
    (unless (and toplib
                 (equal (file-name-nondirectory toplib) "magit.el"))
      (setq toplib (locate-library "magit.el")))
    (when toplib
      (let* ((dir (file-name-directory toplib))
             (static (expand-file-name "magit-version.el" dir))
             (gitdir (expand-file-name
                      ".git" (file-name-directory (directory-file-name dir)))))
        (cond ((file-exists-p gitdir)
               (setq magit-version
                     (let ((default-directory dir))
                       (magit-git-string "describe" "--tags" "--dirty")))
               (unless noninteractive
                 (ignore-errors (delete-file static))))
              ((file-exists-p static)
               (load-file static))
              ((featurep 'package)
               (ignore-errors
                 (--when-let (assq 'magit package-alist)
                   (setq magit-version
                         (and (fboundp 'package-desc-version)
                              (package-version-join
                               (package-desc-version (cadr it)))))))))))
    (if (stringp magit-version)
        (when (called-interactively-p 'any)
          (message "Magit %s, Git %s, Emacs %s"
                   magit-version
                   (ignore-errors (substring (magit-git-string "version") 12))
                   emacs-version))
      (setq magit-version 'error)
      (message "Cannot determine Magit's version"))
    magit-version))

(defun magit-startup-asserts ()
  (let* ((magit-git-global-arguments nil)
         (version (ignore-errors (substring (magit-git-string "version") 12))))
    (when version
      (when (string-match "^\\([0-9]+\\.[0-9]+\\.[0-9]+\\)" version)
        (setq version (match-string 1 version)))
      (when (version< version "1.9.4")
        (display-warning 'magit (format "\
Magit requires Git >= 1.9.4, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.

If you use Tramp to work inside remote Git repositories, then you
have to make sure a suitable Git is used on the remote machines
too.\n" version) :error))))
  (when (version< emacs-version "24.4")
    (display-warning 'magit (format "\
Magit requires Emacs >= 24.4, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.\n" emacs-version) :error))
  (--each '((magit-log-edit  . git-commit)
            (git-commit-mode . git-commit)
            (git-rebase-mode . git-rebase))
    (when (or (featurep (car it)) (locate-library (symbol-name (car it))))
      (display-warning 'magit (format "%s has to be removed

Magit is no longer compatible with the library `%s',
which was used in earlier releases.  Please remove it, so that
Magit can use the successor `%s' without the obsolete
library getting in the way.  Then restart Emacs.\n"
                                      (car it)  (car it) (cdr it)) :error))))

(provide 'magit)

(cl-eval-when (load eval)
  (require 'magit-sequence)
  (require 'magit-commit)
  (require 'magit-remote)
  (require 'magit-bisect)
  (require 'magit-stash)
  (require 'magit-blame)
  (unless (load "magit-autoloads" t t)
    (require 'magit-ediff)
    (require 'magit-extras)
    (require 'git-rebase)))

(if after-init-time
    (progn (magit-startup-asserts)
           (magit-version))
  (add-hook 'after-init-hook #'magit-startup-asserts t)
  (add-hook 'after-init-hook #'magit-version t))

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; magit.el ends here
