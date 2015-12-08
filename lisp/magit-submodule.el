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
(defun magit-submodule-add (url &optional path)
  "Add the repository at URL as a submodule.
Optional PATH is the path to the submodule relative to the root
of the superproject. If it is nil then the path is determined
based on URL."
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
             (and path (directory-file-name (file-relative-name path)))))))
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
(defun magit-insert-submodule-commits (section range)
  "For internal use, don't add to a hook."
  (if (magit-section-hidden section)
      (setf (magit-section-washer section)
            (apply-partially #'magit-insert-submodule-commits section range))
    (magit-git-wash (apply-partially 'magit-log-wash-log 'module)
      "log" "--oneline" range)
    (when (> (point) (magit-section-content section))
      (delete-char -1))))

;;;###autoload
(defun magit-insert-unpulled-module-commits ()
  "Insert sections for all submodules with unpulled commits.
These sections can be expanded to show the respective commits."
  (-when-let (modules (magit-get-submodules))
    (magit-insert-section section (unpulled-modules)
      (magit-insert-heading "Unpulled modules:")
      (magit-with-toplevel
        (dolist (module modules)
          (let ((default-directory
                  (expand-file-name (file-name-as-directory module))))
            (-when-let (tracked (magit-get-upstream-ref))
              (magit-insert-section sec (file module t)
                (magit-insert-heading
                  (concat (propertize module 'face 'magit-diff-file-heading) ":"))
                (magit-insert-submodule-commits
                 section (concat "HEAD.." tracked)))))))
      (if (> (point) (magit-section-content section))
          (insert ?\n)
        (magit-cancel-section)))))

;;;###autoload
(defun magit-insert-unpushed-module-commits ()
  "Insert sections for all submodules with unpushed commits.
These sections can be expanded to show the respective commits."
  (-when-let (modules (magit-get-submodules))
    (magit-insert-section section (unpushed-modules)
      (magit-insert-heading "Unpushed modules:")
      (magit-with-toplevel
        (dolist (module modules)
          (let ((default-directory
                  (expand-file-name (file-name-as-directory module))))
            (-when-let (tracked (magit-get-upstream-ref))
              (magit-insert-section sec (file module t)
                (magit-insert-heading
                  (concat (propertize module 'face 'magit-diff-file-heading) ":"))
                (magit-insert-submodule-commits
                 section (concat tracked "..HEAD")))))))
      (if (> (point) (magit-section-content section))
          (insert ?\n)
        (magit-cancel-section)))))

;;; magit-submodule.el ends soon
(provide 'magit-submodule)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-submodule.el ends here
