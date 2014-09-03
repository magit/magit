;;; magit-svn.el --- Git-Svn extension for Magit

;; Copyright (C) 2010-2014  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Phil Jackson <phil@shellarchive.co.uk>
;; Keywords: vc tools
;; Package: magit-svn
;; Package-Requires: ((cl-lib "0.3") (magit "2.0.50"))

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

;; This package provides basic support for Git-Svn.
;;
;;   Git-Svn is a Git command that aims to provide bidirectional
;;   operation between a Subversion repository and Git.
;;
;; For information about Git-Svn see its manual page `git-svn(1)'.
;;
;; If you are looking for native SVN support in Emacs, then have a
;; look at `psvn.el' and info node `(emacs)Version Control'.

;; When `magit-svn-mode' is turned on then the unpushed and unpulled
;; commit relative to the Subversion repository are displayed in the
;; status buffer.  Git-Svn commands are available from the the Git-Svn
;; popup on `N'.

;; Typing @kbd{N r} runs @code{git svn rebase}, typing @kbd{N c} runs
;; @code{git svn dcommit} and typing @kbd{N f} runs @code{git svn fetch}.

;; @kbd{N s} will prompt you for a (numeric, Subversion) revision and
;; then search for a corresponding Git sha1 for the commit.  This is
;; limited to the path of the remote Subversion repository.  With a prefix
;; (@kbd{C-u N s} the user will also be prompted for a branch to search
;; in.

;; To enable the mode in a particular repository use:
;;
;;   cd /path/to/repository
;;   git config --add magit.extension svn
;;
;; To enable the mode for all repositories use:
;;
;;   git config --global --add magit.extension svn
;;
;; To enable the mode globally without dropping to a shell:
;;
;;   (add-hook 'magit-mode-hook 'magit-svn-mode)

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'magit)

(declare-function find-lisp-find-files-internal 'find-lisp)

;;; Options

(defgroup magit-svn nil
  "Git-Svn support for Magit."
  :group 'magit-extensions)

(defcustom magit-svn-externals-dir ".git_externals"
  "Directory from repository root that stores cloned SVN externals."
  :group 'magit-svn
  :type 'string)

(defcustom magit-svn-mode-lighter " Svn"
  "Mode-line lighter for Magit-Svn mode."
  :group 'magit-svn
  :type 'string)

;;; Commands

(magit-define-popup magit-svn-popup
  "Key menu for svn."
  'magit
  :man-page "git-svn"
  :switches '((?n "Dry run"         "--dry-run"))
  :actions  '((?r "Rebase"          magit-svn-rebase)
              (?c "DCommit"         magit-svn-dcommit)
              (?f "Fetch"           magit-svn-fetch)
              (?s "Show commit"     magit-svn-show-commit)
              (?B "Create branch"   magit-svn-create-branch)
              (?T "Create tag"      magit-svn-create-tag)
              (?x "Fetch Externals" magit-svn-fetch-externals)))

;;;###autoload
(defun magit-svn-show-commit (rev &optional branch)
  "Show the Git commit for a Svn revision read from the user.
With a prefix argument also read a branch to search in."
  (interactive (list (read-number "SVN revision: ")
                     (and current-prefix-arg
                          (magit-read-local-branch "In branch"))))
  (--if-let (magit-git-string "svn" "find-rev" (format "r%i" rev) branch)
      (magit-show-commit it)
    (user-error "Revision r%s could not be mapped to a commit" rev)))

;;;###autoload
(defun magit-svn-create-branch (name &optional args)
  "Create svn branch NAME."
  (interactive (list (read-string "Branch name: ") magit-current-popup-args))
  (magit-run-git "svn" "branch" args name))

;;;###autoload
(defun magit-svn-create-tag (name &optional args)
  "Create svn tag NAME."
  (interactive (list (read-string "Tag name: ") magit-current-popup-args))
  (magit-run-git "svn" "tag" args name))

;;;###autoload
(defun magit-svn-rebase (&optional args)
  "Run git-svn rebase."
  (interactive (list magit-current-popup-args))
  (magit-run-git-async "svn" "rebase" args))

;;;###autoload
(defun magit-svn-dcommit (&optional args)
  "Run git-svn dcommit."
  (interactive (list magit-current-popup-args))
  (magit-run-git-async "svn" "dcommit" args))

;;;###autoload
(defun magit-svn-fetch ()
  "Run git-svn fetch."
  (interactive)
  (magit-run-git-async "svn" "fetch"))

;;;###autoload
(defun magit-svn-fetch-externals()
  "Fetch and rebase all external repositories.
Loops through all external repositories found
in `magit-svn-external-directories' and runs
`git svn rebase' on each of them."
  (interactive)
  (require 'find-lisp)
  (--if-let (find-lisp-find-files-internal
             (expand-file-name magit-svn-externals-dir)
             (lambda (file dir)
               (string-equal file ".git"))
             'find-lisp-default-directory-predicate)
      (dolist (external it)
        (let ((default-directory (file-name-directory external)))
          (magit-run-git "svn" "rebase")))
    (user-error "No SVN Externals found. Check magit-svn-externals-dir"))
  (magit-refresh))

;;; Utilities

(defun magit-svn-enabled ()
  (magit-get "svn-remote" "svn" "fetch"))

(defun magit-svn-get-url ()
  (magit-git-string "svn" "info" "--url"))

(defun magit-svn-get-rev ()
  (--when-let (--first (string-match "^Revision: \\(.+\\)" it)
                       (magit-git-lines "svn" "info"))
    (match-string 1 it)))

(defun magit-svn-get-ref ()
  (--when-let (--first (string-match "^Remote Branch: \\(.+\\)" it)
                       (magit-git-lines "svn" "rebase" "--dry-run"))
    (match-string 1 it)))

(defun magit-insert-svn-unpulled ()
  (--when-let (magit-svn-get-ref)
    (magit-insert-section (svn-unpulled)
      (magit-insert-heading "Unpulled commits (svn):")
      (magit-insert-log (format "HEAD..%s" it)))))

(defun magit-insert-svn-unpushed ()
  (--when-let (magit-svn-get-ref)
    (magit-insert-section (svn-unpushed)
      (magit-insert-heading "Unpushed commits (svn):")
      (magit-insert-log (format "%s..HEAD" it)))))

(magit-define-section-jumper svn-unpushed "Unpushed commits (svn)")
(magit-define-section-jumper svn-unpulled "Unpulled commits (svn)")

(defun magit-insert-svn-remote ()
  (--when-let (magit-svn-get-rev)
    (magit-insert-section (line)
      (magit-insert (format "%-10s%s from %s\n" "Remote:"
                            (propertize (concat "r" it) 'face 'magit-hash)
                            (magit-svn-get-url))))))

;;; Mode

(easy-menu-define magit-svn-extension-menu
  nil
  "Git-Svn extension menu"
  '("Git-Svn" :visible magit-svn-mode
    ["Create branch" magit-svn-create-branch (magit-svn-enabled)]
    ["Rebase" magit-svn-rebase (magit-svn-enabled)]
    ["Fetch" magit-svn-fetch (magit-svn-enabled)]
    ["Commit" magit-svn-dcommit (magit-svn-enabled)]))

(easy-menu-add-item 'magit-mode-menu '("Extensions") magit-svn-extension-menu)

(defvar magit-svn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "N") 'magit-svn-popup)
    map))

;;;###autoload
(define-minor-mode magit-svn-mode
  "Git-Svn support for Magit."
  :lighter magit-svn-mode-lighter
  :keymap  magit-svn-mode-map
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  (cond
   (magit-svn-mode
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-svn-unpulled
                            'magit-insert-unpulled-commits t t)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-svn-unpushed
                            'magit-insert-unpushed-commits t t)
    (magit-add-section-hook 'magit-status-headers-hook
                            'magit-insert-svn-remote nil t t))
   (t
    (remove-hook 'magit-status-sections-hook 'magit-insert-svn-unpulled t)
    (remove-hook 'magit-status-sections-hook 'magit-insert-svn-unpushed t)
    (remove-hook 'magit-status-headers-hook  'magit-insert-svn-remote t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(custom-add-option 'magit-mode-hook #'magit-svn-mode)

;;; magit-svn.el ends soon

(define-obsolete-function-alias 'turn-on-magit-svn 'magit-svn-mode)

(provide 'magit-svn)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-svn.el ends here
