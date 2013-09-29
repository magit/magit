;;; magit-svn.el --- git-svn plug-in for Magit

;; Copyright (C) 2010-2013  The Magit Project Developers.
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

;; Author: Phil Jackson <phil@shellarchive.co.uk>
;; Keywords: vc tools
;; Package: magit-svn
;; Package-Requires: ((cl-lib "0.3") (magit "1.3.0"))

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
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This plug-in provides git-svn functionality as a separate component
;; of Magit.

;;; Code:

(require 'magit)

(eval-when-compile
  (require 'cl-lib)
  (require 'find-lisp))

(declare-function find-lisp-find-files-internal 'find-lisp)

(defcustom magit-svn-externals-dir ".git_externals"
  "Directory from repository root that stores cloned SVN externals."
  :group 'magit
  :type 'string)

;; git svn commands

;;;###autoload (autoload 'magit-svn-find-rev "magit")
(magit-define-command svn-find-rev (rev &optional branch)
  "Find commit for svn REVISION in BRANCH."
  (interactive
   (list (read-string "SVN revision: ")
         (and current-prefix-arg
              (read-string "In branch: "))))
  (let* ((sha (apply 'magit-git-string
                     `("svn"
                       "find-rev"
                       ,(concat "r" rev)
                       ,@(when branch (list branch))))))
    (if sha
        (magit-show-commit
         (magit-with-section sha 'commit
           (magit-set-section-info sha)
           sha))
      (error "Revision %s could not be mapped to a commit" rev))))

;;;###autoload (autoload 'magit-svn-create-branch "magit")
(magit-define-command svn-create-branch (name)
  "Create svn branch NAME."
  (interactive "sBranch name: ")
  (apply 'magit-run-git "svn" "branch"
         (append magit-custom-options (list name))))

;;;###autoload (autoload 'magit-svn-create-tag "magit")
(magit-define-command svn-create-tag (name)
  "Create svn tag NAME."
  (interactive "sTag name: ")
  (apply 'magit-run-git "svn" "tag"
         (append magit-custom-options (list name))))

;;;###autoload (autoload 'magit-svn-rebase "magit")
(magit-define-command svn-rebase ()
  "Run git-svn rebase."
  (interactive)
  (apply 'magit-run-git-async "svn" "rebase" magit-custom-options))

;;;###autoload (autoload 'magit-svn-dcommit "magit")
(magit-define-command svn-dcommit ()
  "Run git-svn dcommit."
  (interactive)
  (apply 'magit-run-git-async "svn" "dcommit" magit-custom-options))

;;;###autoload (autoload 'magit-svn-remote-update "magit")
(magit-define-command svn-remote-update ()
  "Run git-svn fetch."
  (interactive)
  (when (magit-svn-enabled)
    (magit-run-git-async "svn" "fetch")))

(defun magit-svn-enabled ()
  (not (null (magit-svn-get-ref-info t))))

(defun magit-svn-expand-braces-in-branches (branch)
  (if (not (string-match "\\(.+\\){\\(.+,.+\\)}\\(.*\\):\\(.*\\)\\\*" branch))
      (list branch)
    (let ((prefix (match-string 1 branch))
          (suffix (match-string 3 branch))
          (rhs (match-string 4 branch))
          (pieces (split-string (match-string 2 branch) ",")))
      (mapcar (lambda (p) (concat prefix p suffix ":" rhs p)) pieces))))

(defun magit-svn-get-local-ref (url)
  (let* ((branches (cons (magit-get "svn-remote" "svn" "fetch")
                        (magit-get-all "svn-remote" "svn" "branches")))
         (branches (apply 'nconc
                          (mapcar 'magit-svn-expand-braces-in-branches
                                  branches)))
        (base-url (magit-get "svn-remote" "svn" "url"))
        (result nil))
    (while branches
      (let* ((pats (split-string (pop branches) ":"))
             (src (replace-regexp-in-string "\\*" "\\\\(.*\\\\)" (car pats)))
             (dst (replace-regexp-in-string "\\*" "\\\\1" (cadr pats)))
             (base-url (replace-regexp-in-string "\\+" "\\\\+" base-url))
             (base-url (replace-regexp-in-string "//.+@" "//" base-url))
             (pat1 (concat "^" src "$"))
             (pat2 (cond ((equal src "") (concat "^" base-url "$"))
                         (t (concat "^" base-url "/" src "$")))))
        (cond ((string-match pat1 url)
               (setq result (replace-match dst nil nil url))
               (setq branches nil))
              ((string-match pat2 url)
               (setq result (replace-match dst nil nil url))
               (setq branches nil)))))
    result))

(defvar magit-svn-get-ref-info-cache nil
  "A cache for svn-ref-info.
As `magit-get-svn-ref-info' might be considered a quite
expensive operation a cache is taken so that `magit-status'
doesn't repeatedly call it.")

(defun magit-svn-get-ref-info (&optional use-cache)
  "Gather details about the current git-svn repository.
Return nil if there isn't one.  Keys of the alist are ref-path,
trunk-ref-name and local-ref-name.
If USE-CACHE is non-nil then return the value of
`magit-get-svn-ref-info-cache'."
  (if (and use-cache magit-svn-get-ref-info-cache)
      magit-svn-get-ref-info-cache
    (let* ((fetch (magit-get "svn-remote" "svn" "fetch"))
           (url)
           (revision))
      (when fetch
        (let* ((ref (cadr (split-string fetch ":")))
               (ref-path (file-name-directory ref))
               (trunk-ref-name (file-name-nondirectory ref)))
          (setq-local magit-svn-get-ref-info-cache
                (list
                 (cons 'ref-path ref-path)
                 (cons 'trunk-ref-name trunk-ref-name)
                 ;; get the local ref from the log. This is actually
                 ;; the way that git-svn does it.
                 (cons 'local-ref
                       (with-temp-buffer
                         (magit-git-insert "log" "-1" "--first-parent"
                                           "--grep" "git-svn")
                         (goto-char (point-min))
                         (cond ((re-search-forward
                                 "git-svn-id: \\(.+/.+?\\)@\\([0-9]+\\)" nil t)
                                (setq url (match-string 1)
                                      revision (match-string 2))
                                (magit-svn-get-local-ref url))
                               (t
                                (setq url (magit-get "svn-remote" "svn" "url"))
                                nil))))
                 (cons 'revision revision)
                 (cons 'url url))))))))

(defun magit-svn-get-ref (&optional use-cache)
  "Get the best guess remote ref for the current git-svn based branch.
If USE-CACHE is non nil, use the cached information."
  (let ((info (magit-svn-get-ref-info use-cache)))
    (cdr (assoc 'local-ref info))))

(magit-define-inserter svn-unpulled ()
  (when (magit-svn-enabled)
    (magit-git-section 'svn-unpulled "Unpulled commits (SVN):"
                       (apply-partially 'magit-wash-log 'unique)
                       "log" magit-log-format
                       (magit-diff-abbrev-arg)
                       (format "HEAD..%s" (magit-svn-get-ref t)))))

(magit-define-inserter svn-unpushed ()
  (when (magit-svn-enabled)
    (magit-git-section 'svn-unpushed "Unpushed commits (SVN):"
                       (apply-partially 'magit-wash-log 'unique)
                       "log" magit-log-format
                       (magit-diff-abbrev-arg)
                       (format "%s..HEAD" (magit-svn-get-ref t)))))

(magit-define-section-jumper svn-unpushed  "Unpushed commits (SVN)")

(defun magit-insert-svn-remote-line ()
  (let ((svn-info (magit-svn-get-ref-info)))
    (when svn-info
      (magit-insert-status-line "Remote"
        (concat (cdr (assoc 'url svn-info))
                " @ "
                (cdr (assoc 'revision svn-info)))))))

;;;###autoload
(defun magit-svn-fetch-externals()
  "Loops through all external repos found by `magit-svn-external-directories'
   and runs git svn fetch, and git svn rebase on each of them."
  (interactive)
  (let ((externals (magit-svn-external-directories)))
    (if (not externals)
        (message "No SVN Externals found. Check magit-svn-externals-dir.")
      (dolist (external externals)
        (let ((default-directory (file-name-directory external)))
          (magit-run-git "svn" "fetch")
          (magit-run-git "svn" "rebase")))
      (magit-refresh))))

(defun magit-svn-external-directories()
  "Returns all .git directories within `magit-svn-externals-dir'."
  (require 'find-lisp)
  (find-lisp-find-files-internal (expand-file-name magit-svn-externals-dir)
                                 '(lambda(file dir)
                                    (string-equal file ".git"))
                                 'find-lisp-default-directory-predicate))

(easy-menu-define magit-svn-extension-menu
  nil
  "Git SVN extension menu"
  '("Git SVN"
    :visible magit-svn-mode
    ["Create branch" magit-svn-create-branch (magit-svn-enabled)]
    ["Rebase" magit-svn-rebase (magit-svn-enabled)]
    ["Fetch" magit-svn-remote-update (magit-svn-enabled)]
    ["Commit" magit-svn-dcommit (magit-svn-enabled)]))

(easy-menu-add-item 'magit-mode-menu
                    '("Extensions")
                    magit-svn-extension-menu)

;; add the group and its keys
(progn
  ;; (re-)create the group
  (magit-key-mode-add-group 'svn)

  (magit-key-mode-insert-action 'svn "r" "Rebase" 'magit-svn-rebase)
  (magit-key-mode-insert-action 'svn "c" "DCommit" 'magit-svn-dcommit)
  (magit-key-mode-insert-action 'svn "f" "Fetch" 'magit-svn-remote-update)
  (magit-key-mode-insert-action 'svn "s" "Find rev" 'magit-svn-find-rev)
  (magit-key-mode-insert-action 'svn "B" "Create branch" 'magit-svn-create-branch)
  (magit-key-mode-insert-action 'svn "T" "Create tag" 'magit-svn-create-tag)
  (magit-key-mode-insert-action 'svn "x" "Fetch Externals" 'magit-svn-fetch-externals)
  (magit-key-mode-insert-switch 'svn "-n" "Dry run" "--dry-run")

  ;; generate and bind the menu popup function
  (magit-key-mode-generate 'svn))

(defvar magit-svn-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "N") 'magit-key-mode-popup-svn)
    map))

;;;###autoload
(define-minor-mode magit-svn-mode "SVN support for Magit"
  :lighter " SVN" :require 'magit-svn :keymap 'magit-svn-mode-map
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense with magit"))
  (cond (magit-svn-mode
         (add-hook 'magit-after-insert-unpulled-commits-hook
                   'magit-insert-svn-unpulled nil t)
         (add-hook 'magit-after-insert-unpushed-commits-hook
                   'magit-insert-svn-unpushed nil t)
         (add-hook 'magit-after-insert-status-remote-line-hook
                   'magit-insert-svn-remote-line nil t))
        (t
         (remove-hook 'magit-after-insert-unpulled-commits-hook
                      'magit-insert-svn-unpulled t)
         (remove-hook 'magit-after-insert-unpushed-commits-hook
                      'magit-insert-svn-unpushed t)
         (remove-hook 'magit-after-insert-status-remote-line-hook
                      'magit-insert-svn-remote-line t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(defun turn-on-magit-svn ()
  "Unconditionally turn on `magit-svn-mode'."
  (magit-svn-mode 1))

(provide 'magit-svn)
;;; magit-svn.el ends here
