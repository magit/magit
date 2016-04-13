;;; magit-submodule.el --- submodule support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2011-2015  The Magit Project Contributors
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

;;; Commands

;;;###autoload (autoload 'magit-submodule-popup "magit-submodule" nil t)
(magit-define-popup magit-submodule-popup
  "Popup console for submodule commands."
  'magit-commands nil nil
  :man-page "git-submodule"
  :actions  '((?a "Add"    magit-submodule-add)
              (?b "Setup"  magit-submodule-setup)
              (?i "Init"   magit-submodule-init)
              (?u "Update" magit-submodule-update)
              (?s "Sync"   magit-submodule-sync)
              (?f "Fetch"  magit-submodule-fetch)
              (?d "Deinit" magit-submodule-deinit)))

;;;###autoload
(defun magit-submodule-add (url &optional path name)
  "Add the repository at URL as a submodule.

Optional PATH is the path to the submodule relative to the root
of the superproject.  If it is nil, then the path is determined
based on URL.

Optional NAME is the name of the submodule.  If it is nil, then
PATH also becomes the name."
  (interactive
   (magit-with-toplevel
     (let ((path (read-file-name
                  "Add submodule: " nil nil nil
                  (magit-section-when [file untracked]
                    (directory-file-name (magit-section-value it))))))
       (when path
         (setq path (file-name-as-directory (expand-file-name path)))
         (when (member path (list "" default-directory))
           (setq path nil)))
       (list (magit-read-string-ns
              "Remote url"
              (and path (magit-git-repo-p path t)
                   (let ((default-directory path))
                     (magit-get "remote" (or (magit-get-remote) "origin")
                                "url"))))
             (and path (directory-file-name (file-relative-name path)))
             (magit-read-string-ns "Name submodule" path)))))
  (magit-run-git "submodule" "add" (and name (list "--name" name)) url path))

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
(defun magit-insert-modules-unpulled-from-upstream ()
  "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits."
  (magit-insert-submodules "Modules unpulled from @{upstream}"
                           'modules-unpulled-from-upstream
                           'magit-get-upstream-ref
                           "HEAD..%s"))

;;;###autoload
(defun magit-insert-modules-unpulled-from-pushremote ()
  "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits."
  (magit-insert-submodules "Modules unpulled from <push-remote>"
                           'modules-unpulled-from-pushremote
                           'magit-get-push-branch
                           "HEAD..%s"))

;;;###autoload
(defun magit-insert-modules-unpushed-to-upstream ()
  "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits."
  (magit-insert-submodules "Modules unmerged into @{upstream}"
                           'modules-unpushed-to-upstream
                           'magit-get-upstream-ref
                           "%s..HEAD"))

;;;###autoload
(defun magit-insert-modules-unpushed-to-pushremote ()
  "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits."
  (magit-insert-submodules "Modules unpushed to <push-remote>"
                           'modules-unpushed-to-pushremote
                           'magit-get-push-branch
                           "%s..HEAD"))

(defun magit-insert-submodules (heading type fn format)
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

;;; magit-submodule.el ends soon

(define-obsolete-function-alias 'magit-insert-unpulled-module-commits
  'magit-insert-modules-unpulled-from-upstream "Magit 2.6.0")
(define-obsolete-function-alias 'magit-insert-unpushed-module-commits
  'magit-insert-modules-unpushed-to-upstream "Magit 2.6.0")

(provide 'magit-submodule)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-submodule.el ends here
