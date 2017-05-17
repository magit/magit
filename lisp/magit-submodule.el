;;; magit-submodule.el --- submodule support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2017  The Magit Project Contributors
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

;;; Options

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
    ("L<U" 3 magit-repolist-column-unpulled-from-upstream   ((:right-align t)))
    ("L>U" 3 magit-repolist-column-unpushed-to-upstream     ((:right-align t)))
    ("L<P" 3 magit-repolist-column-unpulled-from-pushremote ((:right-align t)))
    ("L>P" 3 magit-repolist-column-unpushed-to-pushremote   ((:right-align t))))
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

;;; Commands

;;;###autoload (autoload 'magit-submodule-popup "magit-submodule" nil t)
(magit-define-popup magit-submodule-popup
  "Popup console for submodule commands."
  :man-page "git-submodule"
  ;:max-action-columns 2
  :actions  '("Managing"
              (?a "Add"    magit-submodule-add)
              (?s "Setup"  magit-submodule-setup)
              (?d "Deinit" magit-submodule-deinit)
              (?C "Configure..." magit-submodule-config-popup)
              "Updating"
              (?u "Update" magit-submodule-update)
              (?U "Update all" magit-submodule-update-all)
              (?F "Pull" magit-submodule-update-remote)
              (?R "Pull all" magit-submodule-update-all-remote)
              (?f "Fetch all"  magit-submodule-fetch)))

(defun magit-submodule-get-name (module)
  ;; Find submodule.<name>.path = <module> in `.gitmodules'.
  (cadr (split-string
         (car (or (magit-git-items "config" "-z" "-f" ".gitmodules"
                                   "--get-regexp" "^submodule\\..*\\.path$"
                                   (concat "^" (regexp-quote module) "$"))
                  (error "No such submodule `%s'" module)))
         "\n")))

;; TODO(?): add functions to set/see individual variables
(defun magit-submodule-edit-gitsubmodules ()
  (find-file ".gitmodules"))
(defun magit-submodule-edit-config ()
  (find-file ".git/config"))

(magit-define-popup magit-submodule-config-popup
  "Configure submodule related git variables."
  'magit-commands nil nil
  :man-page "git-submodule"
  :actions '((?e "Edit .gitmodules" magit-submodule-edit-gitsubmodules)
             (?E "Edit .git/config" magit-submodule-edit-config)
             (?i "Copy missing settings from .gitmodules to .git/config"
                 magit-subdmodule-init)
             (?s "Update url from .gitmodules to .git/config"
                 magit-submodule-sync)))

;;;###autoload
(defun magit-submodule-add (url &optional path name)
  "Add the repository at URL as a submodule.

Optional PATH is the path to the submodule relative to the root
of the superproject.  If it is nil, then the path is determined
based on the URL.

Optional NAME is the name of the submodule.  If it is nil, then
PATH also becomes the name."
  (interactive
   (magit-with-toplevel
     (let* ((url (magit-read-string-ns "Add submodule (remote url)"))
            (path (let ((read-file-name-function #'read-file-name-default))
                    (directory-file-name
                     (file-relative-name
                      (read-directory-name
                       "Add submodules at path: " nil nil nil
                       (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                            (match-string 1 url))))))))
       (list url
             (directory-file-name path)
             (magit-submodule-read-name-for-path path)))))
  (magit-run-git "submodule" "add" (and name (list "--name" name)) url path))

;;;###autoload
(defun magit-submodule-read-name-for-path (path)
  (setq path (directory-file-name (file-relative-name path)))
  (push (file-name-nondirectory path) minibuffer-history)
  (magit-read-string-ns
   "Submodule name" nil (cons 'minibuffer-history 2)
   (or (--keep (-let [(var val) (split-string it "=")]
                 (and (equal val path)
                      (cadr (split-string var "\\."))))
               (magit-git-lines "config" "--list" "-f" ".gitmodules"))
       path)))

;;;###autoload
(defun magit-submodule-setup ()
  "Clone and register missing submodules and checkout appropriate commits."
  (interactive)
  (magit-with-toplevel
    (--if-let (--filter (not (file-exists-p (expand-file-name ".git" it)))
                        (magit-get-submodules))
        (magit-run-git-async "submodule" "update" "--init" "--" it)
      (message "All submodules already setup"))))

;;;###autoload
(defun magit-submodule-init ()
  "Register submodules listed in \".gitmodules\" into \".git/config\"."
  (interactive)
  (magit-with-toplevel
    (magit-run-git-async "submodule" "init")))

(defun magit-submodule-read-module-path (prompt)
  (or (magit-section-when module)
      (magit-completing-read prompt (magit-get-submodules))))

(defun magit-submodule-update-read-method (&optional prompt)
  (list (magit-read-char-case (or prompt "Update submodules by ") t
          (?c "[c]heckout" 'checkout)
          (?m "[m]erge" 'merge)
          (?r "[r]ebase" 'rebase)
          (?s "[x] reset" 'reset))))

(defun magit-submodule-update-read-args ()
  (let ((module (magit-submodule-read-module-path "Update submodule")))
    (cons module (magit-submodule-update-read-method
                  (format "Update submodule `%s' by " module)))))

;;;###autoload
(defun magit-submodule-update (module method &optional opts)
  "Update MODULE by METHOD to recorded target revision.
METHOD may be `checkout', `merge', `rebase', or `reset'.  When
called from lisp, MODULE may be a list of submodules."
  (interactive (magit-submodule-update-read-args))
  (magit-with-toplevel
    (magit-run-git-async
     (--mapcat (list
                "-c" (format "submodule.%s.update=%s"
                             (magit-submodule-get-name it)
                             (pcase method
                               (`reset "!git reset --keep")
                               (_ (symbol-name method)))))
               (if (listp module) module (list module)))
     "submodule" "update" opts "--" module)))

;;;###autoload
(defun magit-submodule-update-remote (module method)
  "Update MODULE by METHOD to submodule's remote revision.
METHOD may be `checkout', `merge', `rebase', or `reset'."
  (interactive (magit-submodule-update-read-args))
  (magit-submodule-update module method "--remote"))

;;;###autoload
(defun magit-submodule-update-all (method)
  "Update all submodules by METHOD to recorded target revision.
METHOD may be `checkout', `merge', `rebase', or `reset'."
  (interactive (magit-submodule-update-read-method))
  (magit-submodule-update (magit-get-submodules) method nil))

;;;###autoload
(defun magit-submodule-update-all-remote (method)
  "Update all submodules by METHOD to submodule's remote revision.
METHOD may be `checkout', `merge', `rebase', or `reset'."
  (interactive (magit-submodule-update-read-method))
  (magit-submodule-update (magit-get-submodules) method t))

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

;;;###autoload
(defun magit-submodule-deinit (path)
  "Unregister the submodule at PATH."
  (interactive
   (list (magit-completing-read "Deinit module" (magit-get-submodules)
                                nil t nil nil (magit-section-when module))))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "deinit" path)))

;;; Sections

;;;###autoload
(defun magit-insert-submodules ()
  "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags'."
  (-when-let (modules (magit-get-submodules))
    (magit-insert-section (submodules nil t)
      (magit-insert-heading "Modules:")
      (magit-with-toplevel
        (let ((col-format (format "%%-%is " (min 25 (/ (window-width) 3)))))
          (dolist (module modules)
            (let ((default-directory
                    (expand-file-name (file-name-as-directory module))))
              (magit-insert-section (submodule module t)
                (insert (propertize (format col-format module)
                                    'face 'magit-diff-file-heading))
                (if (not (file-exists-p ".git"))
                    (insert "(uninitialized)")
                  (insert (format col-format
                                  (--if-let (magit-get-current-branch)
                                      (propertize it 'face 'magit-branch-local)
                                    (propertize "(detached)" 'face 'warning))))
                  (--when-let (magit-git-string "describe" "--tags")
                    (when (string-match-p "\\`[0-9]" it)
                      (insert ?\s))
                    (insert (propertize it 'face 'magit-tag))))
                (insert ?\n))))))
      (insert ?\n))))

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
                              'magit-get-upstream-ref
                              "HEAD..%s"))

;;;###autoload
(defun magit-insert-modules-unpulled-from-pushremote ()
  "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unpulled from <push-remote>"
                              'modules-unpulled-from-pushremote
                              'magit-get-push-branch
                              "HEAD..%s"))

;;;###autoload
(defun magit-insert-modules-unpushed-to-upstream ()
  "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unmerged into @{upstream}"
                              'modules-unpushed-to-upstream
                              'magit-get-upstream-ref
                              "%s..HEAD"))

;;;###autoload
(defun magit-insert-modules-unpushed-to-pushremote ()
  "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unpushed to <push-remote>"
                              'modules-unpushed-to-pushremote
                              'magit-get-push-branch
                              "%s..HEAD"))

(defun magit-submodule-visit (module &optional other-window)
  "Visit MODULE by calling `magit-status' on it.
Offer to initialize MODULE if it's not checked out yet."
  (interactive (list (or (magit-section-when module)
                         (user-error "No submodule at point"))
                     current-prefix-arg))
  (let ((path (expand-file-name module)))
    (if (or (file-exists-p (expand-file-name ".git" module))
            (not (y-or-n-p (format "Setup submodule '%s' first?"
                                   module))))
        (magit-diff-visit-directory path other-window)
      (magit-submodule-setup module)
      (set-process-sentinel
       (lambda (process event)
         (when (memq (process-status process) '(exit signal))
           (let ((magit-process-raise-error t))
             (magit-process-sentinel process event)))
         (when (and (eq (process-status process) 'exit)
                    (= (process-exit-status process) 0))
           (magit-diff-visit-directory path other-window)))))))

(defvar magit-submodule-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map [C-return] 'magit-submodule-visit)
    (define-key map "\C-j"     'magit-submodule-visit)
    (define-key map [remap magit-visit-thing]  'magit-submodule-visit)
    (define-key map [remap magit-delete-thing] 'magit-submodule-deinit)
    (define-key map "K" 'magit-file-untrack)
    (define-key map "R" 'magit-file-rename)
    map)
  "Keymap for `submodule' sections.")

(defun magit--insert-modules-logs (heading type fn format)
  "For internal use, don't add to a hook."
  (-when-let (modules (magit-get-submodules))
    (magit-insert-section section ((eval type) nil t)
      (string-match "\\`\\(.+\\) \\([^ ]+\\)\\'" heading)
      (magit-insert-heading
        (concat
         (propertize (match-string 1 heading) 'face 'magit-section-heading) " "
         (propertize (match-string 2 heading) 'face 'magit-branch-remote) ":"))
      (magit-with-toplevel
        (dolist (module modules)
          (let ((default-directory
                  (expand-file-name (file-name-as-directory module))))
            (--when-let (and (magit-file-accessible-directory-p default-directory)
                             (funcall fn))
              (magit-insert-section sec (file module t)
                (magit-insert-heading
                  (concat (propertize module 'face 'magit-diff-file-heading) ":"))
                (magit-git-wash (apply-partially 'magit-log-wash-log 'module)
                  "log" "--oneline" (format format it))
                (when (> (point) (magit-section-content sec))
                  (delete-char -1)))))))
      (if (> (point) (magit-section-content section))
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
        (mapcar (lambda (module)
                  (let ((default-directory
                          (expand-file-name (file-name-as-directory module))))
                    (list module
                          (vconcat (--map (or (funcall (nth 2 it) module) "")
                                          magit-submodule-list-columns)))))
                (magit-get-submodules)))
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
  (tabulated-list-init-header))

(defun magit-modulelist-column-path (path)
  "Insert the relative path of the submodule."
  path)

(provide 'magit-submodule)
;;; magit-submodule.el ends here
