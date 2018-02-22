;;; magit-submodule.el --- submodule support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2018  The Magit Project Contributors
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

;;; Code:

(require 'magit)

(defvar x-stretch-cursor)
(defvar bookmark-make-record-function)

;;; Options

(defcustom magit-module-sections-hook
  '(magit-insert-modules-overview
    magit-insert-modules-unpulled-from-upstream
    magit-insert-modules-unpulled-from-pushremote
    magit-insert-modules-unpushed-to-upstream
    magit-insert-modules-unpushed-to-pushremote)
  "Hook run by `magit-insert-modules'.

That function isn't part of `magit-status-sections-hook's default
value, so you have to add it yourself for this hook to have any
effect."
  :package-version '(magit . "2.11.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-module-sections-nested t
  "Whether `magit-insert-modules' wraps inserted sections.

If this is non-nil, then only a single top-level section
is inserted.  If it is nil, then all sections listed in
`magit-module-sections-hook' become top-level sections."
  :package-version '(magit . "2.11.0")
  :group 'magit-status
  :type 'boolean)

(defcustom magit-submodule-list-mode-hook '(hl-line-mode)
  "Hook run after entering Magit-Submodule-List mode."
  :package-version '(magit . "2.9.0")
  :group 'magit-repolist
  :type 'hook
  :get 'magit-hook-custom-get
  :options '(hl-line-mode))

(defcustom magit-submodule-list-columns
  '(("Path"     25 magit-modulelist-column-path   nil)
    ("Version"  25 magit-repolist-column-version  nil)
    ("Branch"   20 magit-repolist-column-branch   nil)
    ("B<U" 3 magit-repolist-column-unpulled-from-upstream   ((:right-align t)))
    ("B>U" 3 magit-repolist-column-unpushed-to-upstream     ((:right-align t)))
    ("B<P" 3 magit-repolist-column-unpulled-from-pushremote ((:right-align t)))
    ("B>P" 3 magit-repolist-column-unpushed-to-pushremote   ((:right-align t)))
    ("B"   3 magit-repolist-column-branches                 ((:right-align t)))
    ("S"   3 magit-repolist-column-stashes                  ((:right-align t))))
  "List of columns displayed by `magit-list-submodules'.

Each element has the form (HEADER WIDTH FORMAT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  FORMAT is a function that is called with one
argument, the repository identification (usually its basename),
and with `default-directory' bound to the toplevel of its working
tree.  It has to return a string to be inserted or nil.  PROPS is
an alist that supports the keys `:right-align' and `:pad-right'."
  :package-version '(magit . "2.8.0")
  :group 'magit-repolist-mode
  :type `(repeat (list :tag "Column"
                       (string   :tag "Header Label")
                       (integer  :tag "Column Width")
                       (function :tag "Inserter Function")
                       (repeat   :tag "Properties"
                                 (list (choice :tag "Property"
                                               (const :right-align)
                                               (const :pad-right)
                                               (symbol))
                                       (sexp   :tag "Value"))))))

;;; Popup

;;;###autoload (autoload 'magit-submodule-popup "magit-submodule" nil t)
(magit-define-popup magit-submodule-popup
  "Popup console for submodule commands."
  :man-page "git-submodule"
  :switches '((?f "Force"            "--force")
              (?r "Recursive"        "--recursive")
              (?N "Do not fetch"     "--no-fetch")
              (?C "Checkout tip"     "--checkout")
              (?R "Rebase onto tip"  "--rebase")
              (?M "Merge tip"        "--merge")
              (?U "Use upstream tip" "--remote"))
  :actions
  '((?a "Add            git submodule add [--force]"
        magit-submodule-add)
    (?r "Register       git submodule init"
        magit-submodule-register)
    (?p "Populate       git submodule update --init"
        magit-submodule-populate)
    (?u "Update         git submodule update [--force] [--no-fetch]
                     [--remote] [--recursive] [--checkout|--rebase|--merge]"
        magit-submodule-update)
    (?s "Synchronize    git submodule sync [--recursive]"
        magit-submodule-synchronize)
    (?d "Unpopulate     git submodule deinit [--force]"
        magit-submodule-unpopulate)
    nil
    (?l "List all modules"  magit-list-submodules)
    (?f "Fetch all modules" magit-fetch-modules))
  :max-action-columns 1)

(defun magit-submodule-filtered-arguments (&rest filters)
  (--filter (and (member it filters) it)
            (magit-submodule-arguments)))

;;;###autoload
(defun magit-submodule-add (url &optional path name args)
  "Add the repository at URL as a module.

Optional PATH is the path to the module relative to the root of
the superproject.  If it is nil, then the path is determined
based on the URL.  Optional NAME is the name of the module.  If
it is nil, then PATH also becomes the name."
  (interactive
   (magit-with-toplevel
     (let* ((url (magit-read-string-ns "Add submodule (remote url)"))
            (path (let ((read-file-name-function
                         (if (or (eq read-file-name-function 'ido-read-file-name)
                                 (advice-function-member-p
                                  'ido-read-file-name
                                  read-file-name-function))
                             ;; The Ido variant doesn't work properly here.
                             #'read-file-name-default
                           read-file-name-function)))
                    (directory-file-name
                     (file-relative-name
                      (read-directory-name
                       "Add submodules at path: " nil nil nil
                       (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                            (match-string 1 url))))))))
       (list url
             (directory-file-name path)
             (magit-submodule-read-name-for-path path)
             (magit-submodule-filtered-arguments "--force")))))
  (magit-with-toplevel
    (magit-call-git "submodule" "add" (and name (list "--name" name))
                    args "--" url path)
    (unless (version< (magit-git-version) "2.11.0")
      (magit-call-git "submodule" "absorbgitdirs" path))
    (magit-refresh)))

;;;###autoload
(defun magit-submodule-read-name-for-path (path &optional prefer-short)
  (let* ((path (directory-file-name (file-relative-name path)))
         (name (file-name-nondirectory path)))
    (push (if prefer-short path name) minibuffer-history)
    (magit-read-string-ns
     "Submodule name" nil (cons 'minibuffer-history 2)
     (or (--keep (-let [(var val) (split-string it "=")]
                   (and (equal val path)
                        (cadr (split-string var "\\."))))
                 (magit-git-lines "config" "--list" "-f" ".gitmodules"))
         (if prefer-short name path)))))

;;;###autoload
(defun magit-submodule-register (modules)
  "Register MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; This command and the underlying "git submodule init" do NOT
  ;; "initialize" modules.  They merely "register" modules in the
  ;; super-projects $GIT_DIR/config file, the purpose of which is to
  ;; allow users to change such values before actually initializing
  ;; the modules.
  (interactive
   (list (magit-module-confirm "Register" 'magit-module-no-worktree-p)))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "init" "--" modules)))

;;;###autoload
(defun magit-submodule-populate (modules)
  "Create MODULES working directories, checking out the recorded commits.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; This is the command that actually "initializes" modules.
  ;; A module is initialized when it has a working directory,
  ;; a gitlink, and a .gitmodules entry.
  (interactive
   (list (magit-module-confirm "Populate" 'magit-module-no-worktree-p)))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "update" "--init" "--" modules)))

;;;###autoload
(defun magit-submodule-update (modules args)
  "Update MODULES by checking out the recorded commits.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; Unlike `git-submodule's `update' command ours can only update
  ;; "initialized" modules by checking out other commits but not
  ;; "initialize" modules by creating the working directories.
  ;; To do the latter we provide the "setup" command.
  (interactive
   (list (magit-module-confirm "Update" 'magit-module-worktree-p)
         (magit-submodule-filtered-arguments
          "--force" "--remote" "--recursive" "--checkout" "--rebase" "--merge"
          "--no-fetch")))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "update" args "--" modules)))

;;;###autoload
(defun magit-submodule-synchronize (modules args)
  "Synchronize url configuration of MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  (interactive
   (list (magit-module-confirm "Synchronize" 'magit-module-worktree-p)
         (magit-submodule-filtered-arguments "--recursive")))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "sync" args "--" modules)))

;;;###autoload
(defun magit-submodule-unpopulate (modules args)
  "Remove working directories of MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; Even though a package is "uninitialized" (it has no worktree)
  ;; the super-projects $GIT_DIR/config may never-the-less set the
  ;; module's url.  This may happen if you `deinit' and then `init'
  ;; to register (NOT initialize).  Because the purpose of `deinit'
  ;; is to remove the working directory AND to remove the url, this
  ;; command does not limit itself to modules that have no working
  ;; directory.
  (interactive
   (list (magit-module-confirm "Unpopulate")
         (magit-submodule-filtered-arguments "--force")))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "deinit" args "--" modules)))

;;; Sections

;;;###autoload
(defun magit-insert-modules ()
  "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section."
  (-when-let (modules (magit-list-module-paths))
    (if magit-module-sections-nested
        (magit-insert-section section (submodules nil t)
          (magit-insert-heading
            (format "%s (%s)"
                    (propertize "Modules" 'face 'magit-section-heading)
                    (length modules)))
          (if (oref section hidden)
              (oset section washer 'magit--insert-modules)
            (magit--insert-modules)))
      (magit--insert-modules))))

(defun magit--insert-modules (&optional _section)
  (magit-run-section-hook 'magit-module-sections-hook))

;;;###autoload
(defun magit-insert-modules-overview ()
  "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash."
  (-when-let (modules (magit-list-module-paths))
    (magit-insert-section section (submodules nil t)
      (magit-insert-heading
        (format "%s (%s)"
                (propertize "Modules overview" 'face 'magit-section-heading)
                (length modules)))
      (if (oref section hidden)
          (oset section washer 'magit--insert-modules-overview)
        (magit--insert-modules-overview)))))

(defvar magit-modules-overview-align-numbers t)

(defun magit--insert-modules-overview (&optional _section)
  (magit-with-toplevel
    (let* ((modules (magit-list-module-paths))
           (path-format (format "%%-%is "
                                (min (apply 'max (mapcar 'length modules))
                                     (/ (window-width) 2))))
           (branch-format (format "%%-%is " (min 25 (/ (window-width) 3)))))
      (dolist (module modules)
        (let ((default-directory
                (expand-file-name (file-name-as-directory module))))
          (magit-insert-section (submodule module t)
            (insert (propertize (format path-format module)
                                'face 'magit-diff-file-heading))
            (if (not (file-exists-p ".git"))
                (insert "(unpopulated)")
              (insert (format branch-format
                              (--if-let (magit-get-current-branch)
                                  (propertize it 'face 'magit-branch-local)
                                (propertize "(detached)" 'face 'warning))))
              (--if-let (magit-git-string "describe" "--tags")
                  (progn (when (and magit-modules-overview-align-numbers
                                    (string-match-p "\\`[0-9]" it))
                           (insert ?\s))
                         (insert (propertize it 'face 'magit-tag)))
                (--when-let (magit-rev-format "%h")
                  (insert (propertize it 'face 'magit-hash)))))
            (insert ?\n))))))
  (insert ?\n))

(defvar magit-submodules-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap magit-visit-thing] 'magit-list-submodules)
    map)
  "Keymap for `submodules' sections.")

(defvar magit-submodule-section-map
  (let ((map (make-sparse-keymap)))
    (unless (featurep 'jkl)
      (define-key map "\C-j"   'magit-submodule-visit))
    (define-key map [C-return] 'magit-submodule-visit)
    (define-key map [remap magit-visit-thing]  'magit-submodule-visit)
    (define-key map [remap magit-delete-thing] 'magit-submodule-deinit)
    (define-key map "K" 'magit-file-untrack)
    (define-key map "R" 'magit-file-rename)
    map)
  "Keymap for `submodule' sections.")

(defun magit-submodule-visit (module &optional other-window)
  "Visit MODULE by calling `magit-status' on it.
Offer to initialize MODULE if it's not checked out yet.
With a prefix argument, visit in another window."
  (interactive (list (or (magit-section-when submodule)
                         (magit-read-module-path "Visit module"))
                     current-prefix-arg))
  (magit-with-toplevel
    (let ((path (expand-file-name module)))
      (if (and (not (file-exists-p (expand-file-name ".git" module)))
               (not (y-or-n-p (format "Initialize submodule '%s' first?"
                                      module))))
          (when (file-exists-p path)
            (dired-jump other-window (concat path "/.")))
        (magit-run-git-async "submodule" "update" "--init" "--" module)
        (set-process-sentinel
         magit-this-process
         (lambda (process event)
           (let ((magit-process-raise-error t))
             (magit-process-sentinel process event))
           (when (and (eq (process-status      process) 'exit)
                      (=  (process-exit-status process) 0))
             (magit-diff-visit-directory path other-window))))))))

;;;###autoload
(defun magit-insert-modules-unpulled-from-upstream ()
  "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unpulled from @{upstream}"
                              'modules-unpulled-from-upstream
                              "HEAD..@{upstream}"))

;;;###autoload
(defun magit-insert-modules-unpulled-from-pushremote ()
  "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unpulled from ${push}"
                              'modules-unpulled-from-pushremote
                              "HEAD..@{push}"))

;;;###autoload
(defun magit-insert-modules-unpushed-to-upstream ()
  "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unmerged into @{upstream}"
                              'modules-unpushed-to-upstream
                              "@{upstream}..HEAD"))

;;;###autoload
(defun magit-insert-modules-unpushed-to-pushremote ()
  "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unpushed to @{push}"
                              'modules-unpushed-to-pushremote
                              "${push}..HEAD"))

(defun magit--insert-modules-logs (heading type range)
  "For internal use, don't add to a hook."
  (-when-let (modules (magit-list-module-paths))
    (magit-insert-section section ((eval type) nil t)
      (string-match "\\`\\(.+\\) \\([^ ]+\\)\\'" heading)
      (magit-insert-heading
        (concat
         (propertize (match-string 1 heading) 'face 'magit-section-heading) " "
         (propertize (match-string 2 heading) 'face 'magit-branch-remote) ":"))
      (magit-with-toplevel
        (dolist (module modules)
          (when (magit-module-worktree-p module)
            (let ((default-directory
                    (expand-file-name (file-name-as-directory module))))
              (when (magit-file-accessible-directory-p default-directory)
                (magit-insert-section sec (file module t)
                  (magit-insert-heading
                    (concat (propertize module 'face 'magit-diff-file-heading) ":"))
                  (magit-git-wash (apply-partially 'magit-log-wash-log 'module)
                    "-c" "push.default=current" "log" "--oneline" range)
                  (when (> (point)
                           (oref sec content))
                    (delete-char -1))))))))
      (if (> (point)
             (oref section content))
          (insert ?\n)
        (magit-cancel-section)))))

;;; List

;;;###autoload
(defun magit-list-submodules ()
  "Display a list of the current repository's submodules."
  (interactive)
  (magit-display-buffer (magit-mode-get-buffer 'magit-submodule-list-mode t))
  (magit-submodule-list-mode)
  (setq tabulated-list-entries
        (-keep (lambda (module)
                 (let ((default-directory
                         (expand-file-name (file-name-as-directory module))))
                   (and (file-exists-p ".git")
                        (list module
                              (vconcat
                               (--map (or (funcall (nth 2 it) module) "")
                                      magit-submodule-list-columns))))))
               (magit-list-module-paths)))
  (tabulated-list-print))

(defvar magit-submodule-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-repolist-mode-map)
    (define-key map "g" 'magit-list-submodules)
    map)
  "Local keymap for Magit-Submodule-List mode buffers.")

(define-derived-mode magit-submodule-list-mode tabulated-list-mode "Modules"
  "Major mode for browsing a list of Git submodules."
  :group 'magit-repolist-mode
  (setq x-stretch-cursor        nil)
  (setq tabulated-list-padding  0)
  (setq tabulated-list-sort-key (cons "Path" nil))
  (setq tabulated-list-format
        (vconcat (mapcar (-lambda ((title width _fn props))
                           (nconc (list title width t)
                                  (-flatten props)))
                         magit-submodule-list-columns)))
  (tabulated-list-init-header)
  (setq imenu-prev-index-position-function
        #'magit-imenu--submodule-prev-index-position-function)
  (setq imenu-extract-index-name-function
        #'magit-imenu--submodule-extract-index-name-function)
  (setq-local bookmark-make-record-function
              #'magit-bookmark--submodules-make-record))

(defun magit-modulelist-column-path (path)
  "Insert the relative path of the submodule."
  path)

(provide 'magit-submodule)
;;; magit-submodule.el ends here
