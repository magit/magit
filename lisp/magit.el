;;; magit.el --- A Git porcelain inside Emacs  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2008-2022  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Marius Vollmer <marius.vollmer@gmail.com>
;;      Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;;      Kyle Meyer <kyle@kyleam.com>
;;      Noam Postavsky <npostavs@users.sourceforge.net>
;; Former-Maintainers:
;;      Nicolas Dudebout <nicolas.dudebout@gatech.edu>
;;      Peter J. Weisberg <pj@irregularexpressions.net>
;;      Phil Jackson <phil@shellarchive.co.uk>
;;      Rémi Vanicat <vanicat@debian.org>
;;      Yann Hodique <yann.hodique@gmail.com>

;; Keywords: git tools vc
;; Homepage: https://github.com/magit/magit
;; Package-Requires: ((emacs "25.1") (dash "2.19.1") (git-commit "3.3.0") (magit-section "3.3.0") (transient "0.3.6") (with-editor "3.0.5"))
;; Package-Version: 3.3.0
;; SPDX-License-Identifier: GPL-3.0-or-later

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

;; Magit requires at least GNU Emacs 25.1 and Git 2.2.0.

;;; Commentary:

;; Magit is a text-based Git user interface that puts an unmatched focus
;; on streamlining workflows.  Commands are invoked using short mnemonic
;; key sequences that take the cursor’s position in the highly actionable
;; interface into account to provide context-sensitive behavior.

;; With Magit you can do nearly everything that you can do when using Git
;; on the command-line, but at greater speed and while taking advantage
;; of advanced features that previously seemed too daunting to use on a
;; daily basis.  Many users will find that by using Magit they can become
;; more effective Git user.

;;; Code:

(require 'magit-core)
(require 'magit-diff)
(require 'magit-log)
(require 'magit-wip)
(require 'magit-apply)
(require 'magit-repos)
(require 'git-commit)

(require 'format-spec)
(require 'package nil t) ; used in `magit-version'
(require 'with-editor)

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
  '((((supports (:box t))) :inherit magit-branch-remote :box t)
    (t                     :inherit magit-branch-remote :inverse-video t))
  "Face for current branch."
  :group 'magit-faces)

(defface magit-branch-local
  '((((class color) (background light)) :foreground "SkyBlue4")
    (((class color) (background  dark)) :foreground "LightSkyBlue1"))
  "Face for local branches."
  :group 'magit-faces)

(defface magit-branch-current
  '((((supports (:box t))) :inherit magit-branch-local :box t)
    (t                     :inherit magit-branch-local :inverse-video t))
  "Face for current branch."
  :group 'magit-faces)

(defface magit-branch-upstream
  '((t :slant italic))
  "Face for upstream branch.
This face is only used in logs and it gets combined
 with `magit-branch-local', `magit-branch-remote'
and/or `magit-branch-remote-head'."
  :group 'magit-faces)

(defface magit-branch-warning
  '((t :inherit warning))
  "Face for warning about (missing) branch."
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
  '((t :foreground "medium aquamarine"))
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
  '((t :foreground "light blue"))
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

;;; Global Bindings

;;;###autoload
(define-obsolete-variable-alias 'global-magit-file-mode
  'magit-define-global-key-bindings "Magit 3.0.0")

;;;###autoload
(defcustom magit-define-global-key-bindings t
  "Whether to bind some Magit commands in the global keymap.

If this variable is non-nil, then the following bindings may
be added to the global keymap.  The default is t.

key             binding
---             -------
C-x g           magit-status
C-x M-g         magit-dispatch
C-c M-g         magit-file-dispatch

These bindings may be added when `after-init-hook' is run.
Each binding is added if and only if at that time no other key
is bound to the same command and no other command is bound to
the same key.  In other words we try to avoid adding bindings
that are unnecessary, as well as bindings that conflict with
other bindings.

Adding the above bindings is delayed until `after-init-hook'
is called to allow users to set the variable anywhere in their
init file (without having to make sure to do so before `magit'
is loaded or autoloaded) and to increase the likelihood that
all the potentially conflicting user bindings have already
been added.

To set this variable use either `setq' or the Custom interface.
Do not use the function `customize-set-variable' because doing
that would cause Magit to be loaded immediately when that form
is evaluated (this differs from `custom-set-variables', which
doesn't load the libraries that define the customized variables).

Setting this variable to nil has no effect if that is done after
the key bindings have already been added.

We recommend that you bind \"C-c g\" instead of \"C-c M-g\" to
`magit-file-dispatch'.  The former is a much better binding
but the \"C-c <letter>\" namespace is strictly reserved for
users; preventing Magit from using it by default.

Also see info node `(magit)Commands for Buffers Visiting Files'."
  :package-version '(magit . "3.0.0")
  :group 'magit-essentials
  :type 'boolean)

;;;###autoload
(progn
  (defun magit-maybe-define-global-key-bindings ()
    (when magit-define-global-key-bindings
      (let ((map (current-global-map)))
        (dolist (elt '(("C-x g"   . magit-status)
                       ("C-x M-g" . magit-dispatch)
                       ("C-c M-g" . magit-file-dispatch)))
          (let ((key (kbd (car elt)))
                (def (cdr elt)))
            (unless (or (lookup-key map key)
                        (where-is-internal def (make-sparse-keymap) t))
              (define-key map key def)))))))
  (if after-init-time
      (magit-maybe-define-global-key-bindings)
    (add-hook 'after-init-hook 'magit-maybe-define-global-key-bindings t)))

;;; Dispatch Popup

;;;###autoload (autoload 'magit-dispatch "magit" nil t)
(transient-define-prefix magit-dispatch ()
  "Invoke a Magit command from a list of available commands."
  :info-manual "(magit)Top"
  ["Transient and dwim commands"
   ;; → bound in magit-mode-map or magit-section-mode-map
   ;; ↓ bound below
   [("A" "Apply"          magit-cherry-pick)
    ;; a                  ↓
    ("b" "Branch"         magit-branch)
    ("B" "Bisect"         magit-bisect)
    ("c" "Commit"         magit-commit)
    ("C" "Clone"          magit-clone)
    ("d" "Diff"           magit-diff)
    ("D" "Diff (change)"  magit-diff-refresh)
    ("e" "Ediff (dwim)"   magit-ediff-dwim)
    ("E" "Ediff"          magit-ediff)
    ("f" "Fetch"          magit-fetch)
    ("F" "Pull"           magit-pull)
    ;; g                  ↓
    ;; G                → magit-refresh-all
    ("h" "Help"           magit-help)
    ("H" "Section info"   magit-describe-section :if-derived magit-mode)]
   [("i" "Ignore"         magit-gitignore)
    ("I" "Init"           magit-init)
    ("j" "Jump to section"magit-status-jump  :if-mode     magit-status-mode)
    ("j" "Display status" magit-status-quick :if-not-mode magit-status-mode)
    ("J" "Display buffer" magit-display-repository-buffer)
    ;; k                  ↓
    ;; K                → magit-file-untrack
    ("l" "Log"            magit-log)
    ("L" "Log (change)"   magit-log-refresh)
    ("m" "Merge"          magit-merge)
    ("M" "Remote"         magit-remote)
    ;; n                → magit-section-forward
    ;; N       reserved → forge-dispatch
    ("o" "Submodule"      magit-submodule)
    ("O" "Subtree"        magit-subtree)
    ;; p                → magit-section-backward
    ("P" "Push"           magit-push)
    ;; q                → magit-mode-bury-buffer
    ("Q" "Command"        magit-git-command)]
   [("r" "Rebase"         magit-rebase)
    ;; R                → magit-file-rename
    ;; s                  ↓
    ;; S                  ↓
    ("t" "Tag"            magit-tag)
    ("T" "Note"           magit-notes)
    ;; u                  ↓
    ;; U                  ↓
    ;; v                  ↓
    ("V" "Revert"         magit-revert)
    ("w" "Apply patches"  magit-am)
    ("W" "Format patches" magit-patch)
    ;; x                → magit-reset-quickly
    ("X" "Reset"          magit-reset)
    ("y" "Show Refs"      magit-show-refs)
    ("Y" "Cherries"       magit-cherry)
    ("z" "Stash"          magit-stash)
    ("Z" "Worktree"       magit-worktree)
    ("!" "Run"            magit-run)]]
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
   ("C-x m" "   show all key bindings"    describe-mode)])

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
    (magit--with-connection-local-variables
     (magit-start-process shell-file-name nil
                          shell-command-switch command)))
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
                 (member (file-name-nondirectory toplib)
                         '("magit.el" "magit.el.gz")))
      (let ((load-suffixes (reverse load-suffixes))) ; prefer .el than .elc
        (setq toplib (locate-library "magit"))))
    (setq toplib (and toplib (magit--straight-chase-links toplib)))
    (push toplib debug)
    (when toplib
      (let* ((topdir (file-name-directory toplib))
             (gitdir (expand-file-name
                      ".git" (file-name-directory
                              (directory-file-name topdir))))
             (static (locate-library "magit-version.el" nil (list topdir)))
             (static (and static (magit--straight-chase-links static))))
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
                        (magit-git-string "describe"
                                          "--tags" "--dirty" "--always")))))
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
                (when (string-match "\\`magit-\\([0-9].*\\)" dirname)
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
          (princ (format "Magit %s%s, Git %s, Emacs %s, %s"
                         (or magit-version "(unknown)")
                         (or (and (ignore-errors (version< "2008" magit-version))
                                  (ignore-errors
                                    (require 'lisp-mnt)
                                    (and (fboundp 'lm-header)
                                         (format
                                          " [>= %s]"
                                          (with-temp-buffer
                                            (insert-file-contents
                                             (locate-library "magit.el" t))
                                            (lm-header "Package-Version"))))))
                             "")
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
      (unless (equal (getenv "CI") "true")
        ;; The repository is a sparse clone.
        (message "Cannot determine Magit's version %S" debug)))
    magit-version))

;;; Debugging Tools

(defun magit-debug-git-executable ()
  "Display a buffer with information about `magit-git-executable'.
Also include information about `magit-remote-git-executable'.
See info node `(magit)Debugging Tools' for more information."
  (interactive)
  (with-current-buffer (get-buffer-create "*magit-git-debug*")
    (pop-to-buffer (current-buffer))
    (erase-buffer)
    (insert (format "magit-remote-git-executable: %S\n"
                    magit-remote-git-executable))
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
               (not (equal (getenv "CI") "true")))
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

(with-eval-after-load 'bookmark
  (require 'magit-bookmark))

(if after-init-time
    (progn (magit-startup-asserts)
           (magit-version))
  (add-hook 'after-init-hook #'magit-startup-asserts t)
  (add-hook 'after-init-hook #'magit-version t))

;;; magit.el ends here
