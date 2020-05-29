;;; magit.el --- A Git porcelain inside Emacs  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2008-2020  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;;	Kyle Meyer        <kyle@kyleam.com>
;;	Noam Postavsky    <npostavs@users.sourceforge.net>
;; Former-Maintainers:
;;	Nicolas Dudebout  <nicolas.dudebout@gatech.edu>
;;	Peter J. Weisberg <pj@irregularexpressions.net>
;;	Phil Jackson      <phil@shellarchive.co.uk>
;;	Rémi Vanicat      <vanicat@debian.org>
;;	Yann Hodique      <yann.hodique@gmail.com>

;; Keywords: git tools vc
;; Homepage: https://github.com/magit/magit

;; Magit requires at least GNU Emacs 25.1 and Git 2.2.0.

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

(require 'subr-x)

(require 'with-editor)
(require 'git-commit)
(require 'magit-core)
(require 'magit-diff)
(require 'magit-log)
(require 'magit-wip)
(require 'magit-apply)
(require 'magit-repos)

(require 'format-spec)
(require 'package nil t) ; used in `magit-version'

(defconst magit--minimal-git "2.2.0")
(defconst magit--minimal-emacs "25.1")

;;; Faces

(defface magit-header-line
  '((t :inherit magit-section-heading))
  "Face for the `header-line' in some Magit modes.
Note that some modes, such as `magit-log-select-mode', have their
own faces for the `header-line', or for parts of the
`header-line'."
  :group 'magit-faces)

(defface magit-header-line-key
  '((t :inherit font-lock-builtin-face))
  "Face for keys in the `header-line'."
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

(defface magit-branch-remote-head
  '((((class color) (background light)) :inherit magit-branch-remote :box t)
    (((class color) (background  dark)) :inherit magit-branch-remote :box t))
  "Face for current branch."
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

(defface magit-branch-upstream
  '((t :slant italic))
  "Face for upstream branch.
This face is only used in logs and it gets combined
 with `magit-branch-local', `magit-branch-remote'
and/or `magit-branch-remote-head'."
  :group 'magit-faces)

(defface magit-head
  '((((class color) (background light)) :inherit magit-branch-local)
    (((class color) (background  dark)) :inherit magit-branch-local))
  "Face for the symbolic ref `HEAD'."
  :group 'magit-faces)

(defface magit-refname
  '((((class color) (background light)) :foreground "grey30")
    (((class color) (background  dark)) :foreground "grey80"))
  "Face for refnames without a dedicated face."
  :group 'magit-faces)

(defface magit-refname-stash
  '((t :inherit magit-refname))
  "Face for stash refnames."
  :group 'magit-faces)

(defface magit-refname-wip
  '((t :inherit magit-refname))
  "Face for wip refnames."
  :group 'magit-faces)

(defface magit-refname-pullreq
  '((t :inherit magit-refname))
  "Face for pullreq refnames."
  :group 'magit-faces)

(defface magit-keyword
  '((t :inherit font-lock-string-face))
  "Face for parts of commit messages inside brackets."
  :group 'magit-faces)

(defface magit-keyword-squash
  '((t :inherit font-lock-warning-face))
  "Face for squash! and fixup! keywords in commit messages."
  :group 'magit-faces)

(defface magit-signature-good
  '((t :foreground "green"))
  "Face for good signatures."
  :group 'magit-faces)

(defface magit-signature-bad
  '((t :foreground "red" :weight bold))
  "Face for bad signatures."
  :group 'magit-faces)

(defface magit-signature-untrusted
  '((t :foreground "cyan"))
  "Face for good untrusted signatures."
  :group 'magit-faces)

(defface magit-signature-expired
  '((t :foreground "orange"))
  "Face for signatures that have expired."
  :group 'magit-faces)

(defface magit-signature-expired-key
  '((t :inherit magit-signature-expired))
  "Face for signatures made by an expired key."
  :group 'magit-faces)

(defface magit-signature-revoked
  '((t :foreground "violet red"))
  "Face for signatures made by a revoked key."
  :group 'magit-faces)

(defface magit-signature-error
  '((t :foreground "firebrick3"))
  "Face for signatures that cannot be checked (e.g. missing key)."
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

;;; Dispatch Popup

;;;###autoload (autoload 'magit-dispatch "magit" nil t)
(transient-define-prefix magit-dispatch ()
  "Invoke a Magit command from a list of available commands."
  ["Transient and dwim commands"
   [("A" "Apply"          magit-cherry-pick)
    ("b" "Branch"         magit-branch)
    ("B" "Bisect"         magit-bisect)
    ("c" "Commit"         magit-commit)
    ("C" "Clone"          magit-clone)
    ("d" "Diff"           magit-diff)
    ("D" "Diff (change)"  magit-diff-refresh)
    ("e" "Ediff (dwim)"   magit-ediff-dwim)
    ("E" "Ediff"          magit-ediff)]
   [("f" "Fetch"          magit-fetch)
    ("F" "Pull"           magit-pull)
    ("l" "Log"            magit-log)
    ("L" "Log (change)"   magit-log-refresh)
    ("m" "Merge"          magit-merge)
    ("M" "Remote"         magit-remote)
    ("o" "Submodule"      magit-submodule)
    ("O" "Subtree"        magit-subtree)]
   [("P" "Push"           magit-push)
    ("r" "Rebase"         magit-rebase)
    ("t" "Tag"            magit-tag)
    ("T" "Note"           magit-notes)
    ("V" "Revert"         magit-revert)
    ("w" "Apply patches"  magit-am)
    ("W" "Format patches" magit-patch)
    ("X" "Reset"          magit-reset)]
   [("y" "Show Refs"      magit-show-refs)
    ("Y" "Cherries"       magit-cherry)
    ("z" "Stash"          magit-stash)
    ("!" "Run"            magit-run)
    ("%" "Worktree"       magit-worktree)]]
  ["Applying changes"
   :if-derived magit-mode
   [("a" "Apply"          magit-apply)
    ("v" "Reverse"        magit-reverse)
    ("k" "Discard"        magit-discard)]
   [("s" "Stage"          magit-stage)
    ("u" "Unstage"        magit-unstage)]
   [("S" "Stage all"      magit-stage-modified)
    ("U" "Unstage all"    magit-unstage-all)]]
  ["Essential commands"
   :if-derived magit-mode
   ("g" "       refresh current buffer"   magit-refresh)
   ("<tab>" "   toggle section at point"  magit-section-toggle)
   ("<return>" "visit thing at point"     magit-visit-thing)
   ("C-h m" "   show all key bindings"    describe-mode)])

;;; Git Popup

(defcustom magit-shell-command-verbose-prompt t
  "Whether to show the working directory when reading a command.
This affects `magit-git-command', `magit-git-command-topdir',
`magit-shell-command', and `magit-shell-command-topdir'."
  :package-version '(magit . "2.11.0")
  :group 'magit-commands
  :type 'boolean)

(defvar magit-git-command-history nil)

;;;###autoload (autoload 'magit-run "magit" nil t)
(transient-define-prefix magit-run ()
  "Run git or another command, or launch a graphical utility."


  [["Run git subcommand"
    ("!" "in repository root"   magit-git-command-topdir)
    ("p" "in working directory" magit-git-command)]
   ["Run shell command"
    ("s" "in repository root"   magit-shell-command-topdir)
    ("S" "in working directory" magit-shell-command)]
   ["Launch"
    ("k" "gitk"                 magit-run-gitk)
    ("a" "gitk --all"           magit-run-gitk-all)
    ("b" "gitk --branches"      magit-run-gitk-branches)
    ("g" "git gui"              magit-run-git-gui)]])

;;;###autoload
(defun magit-git-command (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

With a prefix argument COMMAND is run in the top-level directory
of the current working tree, otherwise in `default-directory'."
  (interactive (list (magit-read-shell-command nil "git ")))
  (magit--shell-command command))

;;;###autoload
(defun magit-git-command-topdir (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of the current
working tree."
  (interactive (list (magit-read-shell-command t "git ")))
  (magit--shell-command command (magit-toplevel)))

;;;###autoload
(defun magit-shell-command (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  With a
prefix argument COMMAND is run in the top-level directory of
the current working tree, otherwise in `default-directory'."
  (interactive (list (magit-read-shell-command)))
  (magit--shell-command command))

;;;###autoload
(defun magit-shell-command-topdir (command)
  "Execute COMMAND asynchronously; display output.

Interactively, prompt for COMMAND in the minibuffer.  COMMAND
is run in the top-level directory of the current working tree."
  (interactive (list (magit-read-shell-command t)))
  (magit--shell-command command (magit-toplevel)))

(defun magit--shell-command (command &optional directory)
  (let ((default-directory (or directory default-directory))
        (process-environment process-environment))
    (push "GIT_PAGER=cat" process-environment)
    (magit-start-process shell-file-name nil
                         shell-command-switch command))
  (magit-process-buffer))

(defun magit-read-shell-command (&optional toplevel initial-input)
  (let ((default-directory
          (if (or toplevel current-prefix-arg)
              (or (magit-toplevel)
                  (magit--not-inside-repository-error))
            default-directory)))
    (read-shell-command (if magit-shell-command-verbose-prompt
                            (format "Async shell command in %s: "
                                    (abbreviate-file-name default-directory))
                          "Async shell command: ")
                        initial-input 'magit-git-command-history)))

;;; Font-Lock Keywords

(defconst magit-font-lock-keywords
  (eval-when-compile
    `((,(concat "(\\(magit-define-section-jumper\\)\\_>"
                "[ \t'\(]*"
                "\\(\\(?:\\sw\\|\\s_\\)+\\)?")
       (1 'font-lock-keyword-face)
       (2 'font-lock-function-name-face nil t))
      (,(concat "(" (regexp-opt '("magit-insert-section"
                                  "magit-section-case"
                                  "magit-bind-match-strings"
                                  "magit-with-temp-index"
                                  "magit-with-blob"
                                  "magit-with-toplevel") t)
                "\\_>")
       . 1))))

(font-lock-add-keywords 'emacs-lisp-mode magit-font-lock-keywords)

;;; Version

(defvar magit-version 'undefined
  "The version of Magit that you're using.
Use the function by the same name instead of this variable.")

;;;###autoload
(defun magit-version (&optional print-dest)
  "Return the version of Magit currently in use.
If optional argument PRINT-DEST is non-nil, output
stream (interactively, the echo area, or the current buffer with
a prefix argument), also print the used versions of Magit, Git,
and Emacs to it."
  (interactive (list (if current-prefix-arg (current-buffer) t)))
  (let ((magit-git-global-arguments nil)
        (toplib (or load-file-name buffer-file-name))
        debug)
    (unless (and toplib
                 (equal (file-name-nondirectory toplib) "magit.el"))
      (setq toplib (locate-library "magit.el")))
    (setq toplib (and toplib (file-chase-links toplib)))
    (push toplib debug)
    (when toplib
      (let* ((topdir (file-name-directory toplib))
             (gitdir (expand-file-name
                      ".git" (file-name-directory
                              (directory-file-name topdir))))
             (static (locate-library "magit-version.el" nil (list topdir)))
             (static (and static (file-chase-links static))))
        (or (progn
              (push 'repo debug)
              (when (and (file-exists-p gitdir)
                         ;; It is a repo, but is it the Magit repo?
                         (file-exists-p
                          (expand-file-name "../lisp/magit.el" gitdir)))
                (push t debug)
                ;; Inside the repo the version file should only exist
                ;; while running make.
                (when (and static (not noninteractive))
                  (ignore-errors (delete-file static)))
                (setq magit-version
                      (let ((default-directory topdir))
                        (magit-git-string "describe" "--tags" "--dirty")))))
            (progn
              (push 'static debug)
              (when (and static (file-exists-p static))
                (push t debug)
                (load-file static)
                magit-version))
            (when (featurep 'package)
              (push 'elpa debug)
              (ignore-errors
                (--when-let (assq 'magit package-alist)
                  (push t debug)
                  (setq magit-version
                        (and (fboundp 'package-desc-version)
                             (package-version-join
                              (package-desc-version (cadr it))))))))
            (progn
              (push 'dirname debug)
              (let ((dirname (file-name-nondirectory
                              (directory-file-name topdir))))
                (when (string-match "\\`magit-\\([0-9]\\{8\\}\\.[0-9]*\\)"
                                    dirname)
                  (setq magit-version (match-string 1 dirname)))))
            ;; If all else fails, just report the commit hash. It's
            ;; better than nothing and we cannot do better in the case
            ;; of e.g. a shallow clone.
            (progn
              (push 'hash debug)
              ;; Same check as above to see if it's really the Magit repo.
              (when (and (file-exists-p gitdir)
                         (file-exists-p
                          (expand-file-name "../lisp/magit.el" gitdir)))
                (setq magit-version
                      (let ((default-directory topdir))
                        (magit-git-string "rev-parse" "HEAD"))))))))
    (if (stringp magit-version)
        (when print-dest
          (princ (format "Magit %s, Git %s, Emacs %s, %s"
                         (or magit-version "(unknown)")
                         (or (let ((magit-git-debug
                                    (lambda (err)
                                      (display-warning '(magit git)
                                                       err :error))))
                               (magit-git-version t))
                             "(unknown)")
                         emacs-version
                         system-type)
                 print-dest))
      (setq debug (reverse debug))
      (setq magit-version 'error)
      (when magit-version
        (push magit-version debug))
      (unless (equal (getenv "TRAVIS") "true")
        ;; The repository is a sparse clone.
        (message "Cannot determine Magit's version %S" debug)))
    magit-version))

;;; Debugging Tools

(defun magit-debug-git-executable ()
  "Display a buffer with information about `magit-git-executable'.
See info node `(magit)Debugging Tools' for more information."
  (interactive)
  (with-current-buffer (get-buffer-create "*magit-git-debug*")
    (pop-to-buffer (current-buffer))
    (erase-buffer)
    (insert (concat
             (format "magit-git-executable: %S" magit-git-executable)
             (and (not (file-name-absolute-p magit-git-executable))
                  (format " [%S]" (executable-find magit-git-executable)))
             (format " (%s)\n"
                     (let* ((errmsg nil)
                            (magit-git-debug (lambda (err) (setq errmsg err))))
                       (or (magit-git-version t) errmsg)))))
    (insert (format "exec-path: %S\n" exec-path))
    (--when-let (cl-set-difference
                 (-filter #'file-exists-p (remq nil (parse-colon-path
                                                     (getenv "PATH"))))
                 (-filter #'file-exists-p (remq nil exec-path))
                 :test #'file-equal-p)
      (insert (format "  entries in PATH, but not in exec-path: %S\n" it)))
    (dolist (execdir exec-path)
      (insert (format "  %s (%s)\n" execdir (car (file-attributes execdir))))
      (when (file-directory-p execdir)
        (dolist (exec (directory-files
                       execdir t (concat
                                  "\\`git" (regexp-opt exec-suffixes) "\\'")))
          (insert (format "    %s (%s)\n" exec
                          (let* ((magit-git-executable exec)
                                 (errmsg nil)
                                 (magit-git-debug (lambda (err) (setq errmsg err))))
                            (or (magit-git-version t) errmsg)))))))))

;;; Startup Asserts

(defun magit-startup-asserts ()
  (when-let ((val (getenv "GIT_DIR")))
    (setenv "GIT_DIR")
    (message "Magit unset $GIT_DIR (was %S).  See \
https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike" val))
  (when-let ((val (getenv "GIT_WORK_TREE")))
    (setenv "GIT_WORK_TREE")
    (message "Magit unset $GIT_WORK_TREE (was %S).  See \
https://github.com/magit/magit/wiki/Don't-set-$GIT_DIR-and-alike" val))
  (let ((version (magit-git-version)))
    (when (and version
               (version< version magit--minimal-git)
               (not (equal (getenv "TRAVIS") "true")))
      (display-warning 'magit (format "\
Magit requires Git >= %s, you are using %s.

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
too.\n" magit--minimal-git version) :error)))
  (when (version< emacs-version magit--minimal-emacs)
    (display-warning 'magit (format "\
Magit requires Emacs >= %s, you are using %s.

If this comes as a surprise to you, because you do actually have
a newer version installed, then that probably means that the
older version happens to appear earlier on the `$PATH'.  If you
always start Emacs from a shell, then that can be fixed in the
shell's init file.  If you start Emacs by clicking on an icon,
or using some sort of application launcher, then you probably
have to adjust the environment as seen by graphical interface.
For X11 something like ~/.xinitrc should work.\n"
                                    magit--minimal-emacs emacs-version)
                     :error)))

;;; Loading Libraries

(provide 'magit)

(cl-eval-when (load eval)
  (require 'magit-status)
  (require 'magit-refs)
  (require 'magit-files)
  (require 'magit-reset)
  (require 'magit-branch)
  (require 'magit-merge)
  (require 'magit-tag)
  (require 'magit-worktree)
  (require 'magit-notes)
  (require 'magit-sequence)
  (require 'magit-commit)
  (require 'magit-remote)
  (require 'magit-clone)
  (require 'magit-fetch)
  (require 'magit-pull)
  (require 'magit-push)
  (require 'magit-bisect)
  (require 'magit-stash)
  (require 'magit-blame)
  (require 'magit-obsolete)
  (require 'magit-submodule)
  (unless (load "magit-autoloads" t t)
    (require 'magit-patch)
    (require 'magit-subtree)
    (require 'magit-ediff)
    (require 'magit-gitignore)
    (require 'magit-extras)
    (require 'git-rebase)
    (require 'magit-imenu)
    (require 'magit-bookmark)))

(eval-after-load 'bookmark
  '(require 'magit-bookmark))

(if after-init-time
    (progn (magit-startup-asserts)
           (magit-version))
  (add-hook 'after-init-hook #'magit-startup-asserts t)
  (add-hook 'after-init-hook #'magit-version t))

;;; magit.el ends here
