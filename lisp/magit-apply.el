;;; magit-apply.el --- apply Git diffs

;; Copyright (C) 2010-2015  The Magit Project Contributors
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

;;; Commentary:

;; This library implements commands for applying Git diffs or parts
;; of such a diff.  The supported "apply variants" are apply, stage,
;; unstage, discard, and reverse - more than Git itself knows about,
;; at least at the porcelain level.

;;; Code:

(require 'magit-core)
(require 'magit-diff)
(require 'magit-wip)

;; For `magit-apply'
(declare-function magit-am-popup 'magit-sequence)
;; For `magit-discard-files'
(declare-function magit-checkout-stage 'magit)
(declare-function magit-checkout-read-stage 'magit)

(require 'dired)

;;; Options

(defcustom magit-delete-by-moving-to-trash t
  "Whether Magit uses the system's trash can."
  :package-version '(magit . "2.1.0")
  :group 'magit
  :type 'boolean)

;;; Commands
;;;; Apply

(defun magit-apply (&rest args)
  "Apply the change at point.
With a prefix argument and if necessary, attempt a 3-way merge."
  (interactive (and current-prefix-arg (list "--3way")))
  (--when-let (magit-current-section)
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(,(or `unstaged `staged) ,_)
       (user-error "Change is already in the working tree"))
      (`(untracked file) (magit-am-popup))
      (`(,_      region) (magit-apply-region it args))
      (`(,_        hunk) (magit-apply-hunk it args))
      (`(,_        file) (magit-apply-diff it args)))))

(defun magit-apply-diff (section &rest args)
  (magit-apply-patch section args
                     (concat (magit-diff-file-header section)
                             (buffer-substring (magit-section-content section)
                                               (magit-section-end section)))))

(defun magit-apply-hunk (section &rest args)
  (when (string-match "^diff --cc" (magit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (magit-apply-patch section args
                     (concat (magit-diff-file-header section)
                             (buffer-substring (magit-section-start section)
                                               (magit-section-end section)))))

(defun magit-apply-region (section &rest args)
  (unless (magit-diff-context-p)
    (user-error "Not enough context to apply region.  Increase the context"))
  (when (string-match "^diff --cc" (magit-section-parent-value section))
    (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
  (magit-apply-patch section args
                     (concat (magit-diff-file-header section)
                             (magit-diff-hunk-region-patch section args))))

(defvar magit-apply-inhibit-wip nil)

(defun magit-apply-patch (section args patch)
  (let* ((file (if (eq (magit-section-type section) 'file)
                   (magit-section-value section)
                 (magit-section-parent-value section)))
         (command (symbol-name this-command))
         (command (if (and command (string-match "^magit-\\([^-]+\\)" command))
                      (match-string 1 command)
                    "apply")))
    (when (and magit-wip-before-change-mode (not magit-apply-inhibit-wip))
      (magit-wip-commit-before-change (list file) (concat " before " command)))
    (with-temp-buffer
      (insert patch)
      (magit-run-git-with-input nil
        "apply" args "-p0"
        (unless (magit-diff-context-p) "--unidiff-zero")
        "--ignore-space-change" "-"))
    (when (and magit-wip-after-apply-mode (not magit-apply-inhibit-wip))
      (magit-wip-commit-after-apply (list file) (concat " after " command)))
    (magit-refresh)))

;;;; Stage

(defun magit-stage ()
  "Add the change at point to the staging area."
  (interactive)
  (--when-let (magit-current-section)
    (let ((inhibit-magit-revert t))
      (pcase (list (magit-diff-type) (magit-diff-scope))
        (`(untracked     ,_) (magit-stage-untracked))
        (`(unstaged  region) (magit-apply-region it "--cached"))
        (`(unstaged    hunk) (magit-apply-hunk   it "--cached"))
        (`(unstaged    file) (magit-stage-1 "-u" (list (magit-section-value it))))
        (`(unstaged   files) (magit-stage-1 "-u" (magit-region-values)))
        (`(unstaged    list) (magit-stage-1 "-u"))
        (`(staged        ,_) (user-error "Already staged"))
        (`(committed     ,_) (user-error "Cannot stage committed changes"))
        (`(undefined     ,_) (user-error "Cannot stage this change"))))))

;;;###autoload
(defun magit-stage-file (file)
  "Stage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be staged.  Otherwise stage the file at point without
requiring confirmation."
  (interactive
   (let* ((atpoint (magit-section-when (file)))
          (current (magit-file-relative-name))
          (choices (nconc (magit-modified-files)
                          (magit-untracked-files)))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (magit-completing-read "Stage file" choices
                                      nil t nil nil default)
             default))))
  (let ((default-directory (magit-toplevel)))
    (magit-stage-1 nil (list file))))

;;;###autoload
(defun magit-stage-modified (&optional all)
  "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files.
\('git add --update|--all .')."
  (interactive (progn (unless (or (not (magit-anything-staged-p))
                                  (magit-confirm 'stage-all-changes))
                        (user-error "Abort"))
                      (list current-prefix-arg)))
  (let ((default-directory (magit-toplevel)))
    (magit-stage-1 (if all "--all" "-u"))))

(defun magit-stage-1 (arg &optional files)
  (magit-wip-commit-before-change files " before stage")
  (magit-run-git-no-revert "add" arg (if files (cons "--" files) ".")))

(defun magit-stage-untracked ()
  (let* ((section (magit-current-section))
         (files (pcase (magit-diff-scope)
                  (`file  (list (magit-section-value section)))
                  (`files (magit-region-values))
                  (`list  (magit-untracked-files))))
         plain repos)
    (dolist (file files)
      (if (magit-git-repo-p file t)
          (push file repos)
        (push file plain)))
    (magit-wip-commit-before-change files " before stage")
    (when plain
      (magit-run-git-no-revert "add" "--" plain))
    (dolist (repo repos)
      (let ((inhibit-magit-revert t))
        (save-excursion
          (goto-char (magit-section-start
                      (magit-get-section
                       `((file . ,repo) (untracked) (status)))))
          (call-interactively 'magit-submodule-add))))))

;;;; Unstage

(defun magit-unstage ()
  "Remove the change at point from the staging area."
  (interactive)
  (--when-let (magit-current-section)
    (let ((inhibit-magit-revert t))
      (pcase (list (magit-diff-type) (magit-diff-scope))
        (`(untracked     ,_) (user-error "Cannot unstage untracked changes"))
        (`(unstaged      ,_) (user-error "Already unstaged"))
        (`(staged    region) (magit-apply-region it "--reverse" "--cached"))
        (`(staged      hunk) (magit-apply-hunk   it "--reverse" "--cached"))
        (`(staged      file) (magit-unstage-1 (list (magit-section-value it))))
        (`(staged     files) (magit-unstage-1 (magit-region-values)))
        (`(staged      list) (magit-unstage-all))
        (`(committed     ,_) (user-error "Cannot unstage committed changes"))
        (`(undefined     ,_) (user-error "Cannot unstage this change"))))))

;;;###autoload
(defun magit-unstage-file (file)
  "Unstage all changes to FILE.
With a prefix argument or when there is no file at point ask for
the file to be unstaged.  Otherwise unstage the file at point
without requiring confirmation."
  (interactive
   (let* ((atpoint (magit-section-when (file)))
          (current (magit-file-relative-name))
          (choices (magit-staged-files))
          (default (car (member (or atpoint current) choices))))
     (list (if (or current-prefix-arg (not default))
               (magit-completing-read "Unstage file" choices
                                      nil t nil nil default)
             default))))
  (let ((default-directory (magit-toplevel)))
    (magit-unstage-1 (list file))))

(defun magit-unstage-1 (files)
  (magit-wip-commit-before-change files " before unstage")
  (if (magit-no-commit-p)
      (magit-run-git "rm" "--cached" "--" files)
    (magit-run-git "reset" "HEAD" "--" files)))

;;;###autoload
(defun magit-unstage-all ()
  "Remove all changes from the staging area."
  (interactive)
  (when (or (and (not (magit-anything-unstaged-p))
                 (not (magit-untracked-files)))
            (magit-confirm 'unstage-all-changes))
    (magit-wip-commit-before-change nil " before unstage")
    (magit-run-git "reset" "HEAD" "--")))

;;;; Discard

(defun magit-discard ()
  "Remove the change at point."
  (interactive)
  (--when-let (magit-current-section)
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(committed ,_) (user-error "Cannot discard committed changes"))
      (`(undefined ,_) (user-error "Cannot discard this change"))
      (`(,_      list) (magit-discard-files (magit-section-children it)))
      (`(,_     files) (magit-discard-files (magit-region-sections)))
      (`(,_      file) (magit-discard-files (list it)))
      (_               (magit-discard-apply it)))))

(defun magit-discard-apply (section)
  (let* ((type  (magit-diff-type  section))
         (scope (magit-diff-scope section))
         (fn    (pcase scope
                  (`region 'magit-apply-region)
                  (`hunk   'magit-apply-hunk)
                  (`file   'magit-apply-diff))))
    (when (or (eq scope 'file)
              (magit-confirm 'discard (format "Discard %s" scope)))
      (if (eq type 'unstaged)
          (funcall fn section "--reverse")
        (if (magit-anything-unstaged-p
             nil (if (eq scope 'file)
                     (magit-section-value section)
                   (magit-section-parent-value section)))
            (progn
              (let ((inhibit-magit-refresh t))
                (funcall fn section "--reverse" "--cached")
                (funcall fn section "--reverse"))
              (magit-refresh))
          (funcall fn section "--reverse" "--index"))))))

(defun magit-discard-files (sections)
  (let ((auto-revert-verbose nil)
        (type (magit-diff-type (car sections)))
        (status (magit-file-status))
        files delete resurrect rename discard discard-new resolve)
    (dolist (section sections)
      (let ((file (magit-section-value section)))
        (push file files)
        (pcase (cons (pcase type
                       (`staged ?X)
                       (`unstaged ?Y)
                       (`untracked ?Z))
                     (cddr (assoc file status)))
          (`(?Z)                        (push file delete))
          ((or `(?Z ?? ??) `(?Z ?! ?!)) (push file delete))
          ((or `(?Z ?D ? ) `(,_ ?D ?D)) (push file delete))
          ((or `(,_ ?U ,_) `(,_ ,_ ?U)) (push file resolve))
          (`(,_ ?A ?A)                  (push file resolve))
          (`(?X ?M ,(or ?  ?M ?D)) (push section discard))
          (`(?Y ,_         ?M    ) (push section discard))
          (`(?X ?A         ?M    ) (push file discard-new))
          (`(?X ?C         ?M    ) (push file discard-new))
          (`(?X ?A ,(or ?     ?D)) (push file delete))
          (`(?X ?C ,(or ?     ?D)) (push file delete))
          (`(?X ?D ,(or ?  ?M   )) (push file resurrect))
          (`(?Y ,_            ?D ) (push file resurrect))
          (`(?X ?R ,(or ?  ?M ?D)) (push file rename)))))
    (unwind-protect
        (let ((inhibit-magit-refresh t))
          (magit-wip-commit-before-change files " before discard")
          (when resolve
            (dolist (file (nreverse resolve))
              (magit-checkout-stage file (magit-checkout-read-stage file))))
          (magit-discard-files--resurrect (nreverse resurrect))
          (magit-discard-files--delete    (nreverse delete) status)
          (magit-discard-files--rename    (nreverse rename) status)
          (magit-discard-files--discard   (nreverse discard)
                                          (nreverse discard-new))
          (magit-wip-commit-after-apply files " after discard"))
      (magit-refresh))))

(defun magit-discard-files--resurrect (files)
  (when (magit-confirm-files 'resurrect files)
    (if (eq (magit-diff-type) 'staged)
        (magit-call-git "reset"  "--" files)
      (magit-call-git "checkout" "--" files))))

(defun magit-discard-files--delete (files status)
  (when (if magit-delete-by-moving-to-trash
            (magit-confirm-files 'trash files)
          (magit-confirm-files 'delete files))
    (let ((delete-by-moving-to-trash magit-delete-by-moving-to-trash))
      (dolist (file files)
        (if (memq (magit-diff-type) '(unstaged untracked))
            (with-no-warnings ; #1933
              (dired-delete-file file dired-recursive-deletes
                                 magit-delete-by-moving-to-trash))
          (pcase (nth 3 (assoc file status))
            (?  (delete-file file t)
                (magit-call-git "rm" "--cached" "--" file))
            (?M (let ((temp (magit-git-string "checkout-index" "--temp" file)))
                  (string-match
                   (format "\\(.+?\\)\t%s" (regexp-quote file)) temp)
                  (rename-file (match-string 1 temp)
                               (setq temp (concat file ".~{index}~")))
                  (delete-file temp t))
                (magit-call-git "rm" "--cached" "--force" "--" file))
            (?D (magit-call-git "checkout" "--" file)
                (delete-file file t)
                (magit-call-git "rm" "--cached" "--force" "--" file))))))))

(defun magit-discard-files--rename (files status)
  (when (magit-confirm 'rename "Undo rename %s" "Undo %i renames"
          (mapcar (lambda (file)
                    (setq file (assoc file status))
                    (format "%s -> %s" (cadr file) (car file)))
                  files))
    (dolist (file files)
      (let ((orig (cadr (assoc file status))))
        (if (file-exists-p file)
            (magit-call-git "mv" file orig)
          (magit-call-git "rm" "--cached" "--" file)
          (magit-call-git "reset" "--" orig))))))

(defun magit-discard-files--discard (sections new-files)
  (let ((files (mapcar #'magit-section-value sections)))
    (when (magit-confirm-files
           'discard (append files new-files)
           (format "Discard %s changes in" (magit-diff-type)))
      (if (eq (magit-diff-type (car sections)) 'unstaged)
          (magit-call-git "checkout" "--" files)
        (when new-files
          (magit-call-git "add"   "--" new-files)
          (magit-call-git "reset" "--" new-files))
        (let ((magit-apply-inhibit-wip t))
          (-if-let (binaries (magit-staged-binary-files))
              (let ((text (--filter (not (member (magit-section-value it) binaries))
                                    sections)))
                (cl-destructuring-bind (unsafe safe)
                    (let ((modified (magit-modified-files t)))
                      (--separate (member it modified) binaries))
                  (mapc #'magit-discard-apply text)
                  (when safe
                    (magit-call-git "reset" "--" safe))
                  (user-error
                   (concat "Cannot discard staged changes to binary files, "
                           "which also have unstaged changes.  Unstage instead."))))
            (mapc #'magit-discard-apply sections)))))))

;;;; Reverse

(defun magit-reverse ()
  "Reverse the change at point in the working tree."
  (interactive)
  (--when-let (magit-current-section)
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(untracked ,_) (user-error "Cannot reverse untracked changes"))
      (`(unstaged  ,_) (user-error "Cannot reverse unstaged changes"))
      (`(,_      list) (magit-reverse-files (magit-section-children it)))
      (`(,_     files) (magit-reverse-files (magit-region-sections)))
      (`(,_      file) (magit-reverse-files (list it)))
      (_               (magit-reverse-apply it)))))

(defun magit-reverse-apply (section)
  (let ((scope (magit-diff-scope section)))
    (when (or (eq scope 'file)
              (magit-confirm 'reverse (format "Reverse %s" scope)))
      (funcall (pcase scope
                 (`region 'magit-apply-region)
                 (`hunk   'magit-apply-hunk)
                 (`file   'magit-apply-diff))
               section "--reverse"))))

(defun magit-reverse-files (sections)
  (cl-destructuring-bind (binaries sections)
      (let ((binaries (magit-staged-binary-files)))
        (--separate (member (magit-section-value it) binaries) sections))
    (let ((files (mapcar #'magit-section-value sections)))
      (when (magit-confirm-files 'reverse files)
        (magit-wip-commit-before-change files " before reverse")
        (let ((magit-apply-inhibit-wip t))
          (mapc #'magit-reverse-apply sections))
        (magit-wip-commit-after-apply files " after reverse")))
    (when binaries
      (user-error "Cannot reverse binary files"))))

;;; magit-apply.el ends soon
(provide 'magit-apply)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-apply.el ends here
