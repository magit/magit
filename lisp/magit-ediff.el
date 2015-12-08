;;; magit-ediff.el --- Ediff extension for Magit  -*- lexical-binding: t -*-

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

;; This library provides basic support for Ediff.

;;; Code:

(require 'magit)

(require 'ediff)
(require 'smerge-mode)

(defvar smerge-ediff-buf)
(defvar smerge-ediff-windows)

(defgroup magit-ediff nil
  "Ediff support for Magit."
  :group 'magit-extensions)

(unless (find-lisp-object-file-name 'magit-ediff-quit-hook 'defvar)
  (add-hook 'magit-ediff-quit-hook 'magit-ediff-restore-previous-winconf)
  (add-hook 'magit-ediff-quit-hook 'magit-ediff-cleanup-auxiliary-buffers))
(defcustom magit-ediff-quit-hook
  '(magit-ediff-cleanup-auxiliary-buffers
    magit-ediff-restore-previous-winconf)
  "Hooks to run after finishing Ediff, when that was invoked using Magit.
The hooks are run in the Ediff control buffer.  This is similar
to `ediff-quit-hook' but takes the needs of Magit into account.
The `ediff-quit-hook' is ignored by Ediff sessions which were
invoked using Magit."
  :package-version '(magit . "2.2.0")
  :group 'magit-ediff
  :type 'hook
  :options '(magit-ediff-cleanup-auxiliary-buffers
             magit-ediff-restore-previous-winconf))

(defcustom magit-ediff-dwim-show-on-hunks nil
  "Whether `magit-ediff-dwim' runs show variants on hunks.
If non-nil, `magit-ediff-show-staged' or
`magit-ediff-show-unstaged' are called based on what section the
hunk is in.  Otherwise, `magit-ediff-dwim' runs
`magit-ediff-stage' when point is on an uncommitted hunk."
  :package-version '(magit . "2.2.0")
  :group 'magit-ediff
  :type 'boolean)

(defvar magit-ediff-previous-winconf nil)

;;;###autoload (autoload 'magit-ediff-popup "magit-ediff" nil t)
(magit-define-popup magit-ediff-popup
  "Popup console for ediff commands."
  'magit-diff nil nil
  :actions '((?E "Dwim"          magit-ediff-dwim)
             (?u "Show unstaged" magit-ediff-show-unstaged)
             (?s "Stage"         magit-ediff-stage)
             (?i "Show staged"   magit-ediff-show-staged)
             (?m "Resolve"       magit-ediff-resolve)
             (?w "Show worktree" magit-ediff-show-working-tree)
             (?r "Diff range"    magit-ediff-compare)
             (?c "Show commit"   magit-ediff-show-commit))
  :max-action-columns 2)

;;;###autoload
(defun magit-ediff-resolve (file)
  "Resolve outstanding conflicts in FILE using Ediff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'."
  (interactive
   (let ((current  (magit-current-file))
         (unmerged (magit-unmerged-files)))
     (unless unmerged
       (user-error "There are no unresolved conflicts"))
     (list (magit-completing-read "Resolve file" unmerged nil t nil nil
                                  (car (member current unmerged))))))
  (magit-with-toplevel
    (with-current-buffer (find-file-noselect file)
      (smerge-ediff)
      (setq-local
       ediff-quit-hook
       (lambda ()
         (let ((bufC ediff-buffer-C)
               (bufS smerge-ediff-buf))
           (with-current-buffer bufS
             (when (yes-or-no-p (format "Conflict resolution finished; save %s?"
                                        buffer-file-name))
               (erase-buffer)
               (insert-buffer-substring bufC)
               (save-buffer))))
         (when (buffer-live-p ediff-buffer-A) (kill-buffer ediff-buffer-A))
         (when (buffer-live-p ediff-buffer-B) (kill-buffer ediff-buffer-B))
         (when (buffer-live-p ediff-buffer-C) (kill-buffer ediff-buffer-C))
         (when (buffer-live-p ediff-ancestor-buffer)
           (kill-buffer ediff-ancestor-buffer))
         (let ((magit-ediff-previous-winconf smerge-ediff-windows))
           (run-hooks 'magit-ediff-quit-hook)))))))

;;;###autoload
(defun magit-ediff-stage (file)
  "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository."
  (interactive
   (list (magit-completing-read "Selectively stage file" nil
                                (magit-tracked-files) nil nil nil
                                (magit-current-file))))
  (magit-with-toplevel
    (let* ((conf (current-window-configuration))
           (bufA (magit-get-revision-buffer "HEAD" file))
           (bufB (get-buffer (concat file ".~{index}~")))
           (bufBrw (and bufB (with-current-buffer bufB (not buffer-read-only))))
           (bufC (get-file-buffer file)))
      (ediff-buffers3
       (or bufA (magit-find-file-noselect "HEAD" file))
       (with-current-buffer (magit-find-file-index-noselect file t)
         (setq buffer-read-only nil)
         (current-buffer))
       (or bufC (find-file-noselect file))
       `((lambda ()
           (setq-local
            ediff-quit-hook
            (lambda ()
              (and (buffer-live-p ediff-buffer-B)
                   (buffer-modified-p ediff-buffer-B)
                   (with-current-buffer ediff-buffer-B
                     (magit-update-index)))
              (and (buffer-live-p ediff-buffer-C)
                   (buffer-modified-p ediff-buffer-C)
                   (with-current-buffer ediff-buffer-C
                     (when (y-or-n-p
                            (format "Save file %s? " buffer-file-name))
                       (save-buffer))))
              ,@(unless bufA '((ediff-kill-buffer-carefully ediff-buffer-A)))
              ,@(if bufB
                    (unless bufBrw '((with-current-buffer ediff-buffer-B
                                       (setq buffer-read-only t))))
                  '((ediff-kill-buffer-carefully ediff-buffer-B)))
              ,@(unless bufC '((ediff-kill-buffer-carefully ediff-buffer-C)))
              (let ((magit-ediff-previous-winconf ,conf))
                (run-hooks 'magit-ediff-quit-hook))))))
       'ediff-buffers3))))

;;;###autoload
(defun magit-ediff-compare (revA revB fileA fileB)
  "Compare REVA:FILEA with REVB:FILEB using Ediff.

FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil then this stands for the
working tree state.

If the region is active, use the revisions on the first and last
line of the region.  With a prefix argument, instead of diffing
the revisions, choose a revision to view changes along, starting
at the common ancestor of both revisions (i.e., use a \"...\"
range)."
  (interactive (-let [(revA revB) (magit-ediff-compare--read-revisions
                                   nil current-prefix-arg)]
                 (nconc (list revA revB)
                        (magit-ediff-compare--read-files revA revB))))
  (magit-with-toplevel
    (let ((conf (current-window-configuration))
          (bufA (if revA
                    (magit-get-revision-buffer revA fileA)
                  (get-file-buffer fileA)))
          (bufB (if revB
                    (magit-get-revision-buffer revB fileB)
                  (get-file-buffer fileB))))
      (ediff-buffers
       (or bufA (if revA
                    (magit-find-file-noselect revA fileA)
                  (find-file-noselect fileA)))
       (or bufB (if revB
                    (magit-find-file-noselect revB fileB)
                  (find-file-noselect fileB)))
       `((lambda ()
           (setq-local
            ediff-quit-hook
            (lambda ()
              ,@(unless bufA '((ediff-kill-buffer-carefully ediff-buffer-A)))
              ,@(unless bufB '((ediff-kill-buffer-carefully ediff-buffer-B)))
              (let ((magit-ediff-previous-winconf ,conf))
                (run-hooks 'magit-ediff-quit-hook))))))
       'ediff-revision))))

(defun magit-ediff-compare--read-revisions (&optional arg mbase)
  (let ((input (or arg (magit-diff-read-range-or-commit "Compare range or commit"
                                                        nil mbase)))
        revA revB)
    (if (string-match magit-range-re input)
        (progn (setq revA (or (match-string 1 input) "HEAD")
                     revB (or (match-string 3 input) "HEAD"))
               (when (string= (match-string 2 input) "...")
                 (setq revA (magit-git-string "merge-base" revA revB))))
      (setq revA input))
    (list revA revB)))

(defun magit-ediff-compare--read-files (revA revB &optional fileB)
  (unless fileB
    (setq fileB (magit-read-file-choice
                 (format "File to compare between %s and %s"
                         revA (or revB "the working tree"))
                 (magit-changed-files revA revB)
                 (format "No changed files between %s and %s"
                         revA (or revB "the working tree")))))
  (list (or (car (member fileB (magit-revision-files revA)))
            (cdr (assoc fileB (magit-renamed-files revB revA)))
            (magit-read-file-choice
             (format "File in %s to compare with %s in %s"
                     revA fileB (or revB "the working tree"))
             (magit-changed-files revB revA)
             (format "File in %s to compare with %s in %s"
                     revA fileB (or revB "the working tree"))))
        fileB))

;;;###autoload
(defun magit-ediff-dwim ()
  "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run."
  (interactive)
  (magit-section-case
    (hunk (save-excursion
            (goto-char (magit-section-start (magit-section-parent it)))
            (magit-ediff-dwim)))
    (t
     (let ((range (magit-diff--dwim))
           (file (magit-current-file))
           command revA revB)
       (pcase range
         ((and (guard (not magit-ediff-dwim-show-on-hunks))
               (or `unstaged `staged))
          (setq command (if (magit-anything-unmerged-p)
                            #'magit-ediff-resolve
                          #'magit-ediff-stage)))
         (`unstaged (setq command #'magit-ediff-show-unstaged))
         (`staged (setq command #'magit-ediff-show-staged))
         (`(commit . ,value)
          (setq command #'magit-ediff-show-commit
                revB value))
         ((pred stringp)
          (-let [(a b) (magit-ediff-compare--read-revisions range)]
            (setq command #'magit-ediff-compare
                  revA a
                  revB b)))
         (_
          (when (derived-mode-p 'magit-diff-mode)
            (pcase (magit-diff-type)
              (`committed (-let [(a b) (magit-ediff-compare--read-revisions
                                        (car magit-refresh-args))]
                            (setq revA a revB b)))
              ((guard (not magit-ediff-dwim-show-on-hunks))
               (setq command #'magit-ediff-stage))
              (`unstaged  (setq command #'magit-ediff-show-unstaged))
              (`staged    (setq command #'magit-ediff-show-staged))
              (`undefined (setq command nil))
              (_          (setq command nil))))))
       (cond ((not command)
              (call-interactively
               (magit-read-char-case
                   "Failed to read your mind; do you want to " t
                 (?c "[c]ommit"  'magit-ediff-show-commit)
                 (?r "[r]ange"   'magit-ediff-compare)
                 (?s "[s]tage"   'magit-ediff-stage)
                 (?v "resol[v]e" 'magit-ediff-resolve))))
             ((eq command 'magit-ediff-compare)
              (apply 'magit-ediff-compare revA revB
                     (magit-ediff-compare--read-files revA revB file)))
             ((eq command 'magit-ediff-show-commit)
              (magit-ediff-show-commit revB))
             (file
              (funcall command file))
             (t
              (call-interactively command)))))))

;;;###autoload
(defun magit-ediff-show-staged (file)
  "Show staged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository."
  (interactive
   (list (magit-read-file-choice "Show staged changes for file"
                                 (magit-staged-files)
                                 "No staged files")))
  (let ((conf (current-window-configuration))
        (bufA (magit-get-revision-buffer "HEAD" file))
        (bufB (get-buffer (concat file ".~{index}~"))))
    (ediff-buffers
     (or bufA (magit-find-file-noselect "HEAD" file))
     (or bufB (magit-find-file-index-noselect file t))
     `((lambda ()
         (setq-local
          ediff-quit-hook
          (lambda ()
            ,@(unless bufA '((ediff-kill-buffer-carefully ediff-buffer-A)))
            ,@(unless bufB '((ediff-kill-buffer-carefully ediff-buffer-B)))
            (let ((magit-ediff-previous-winconf ,conf))
              (run-hooks 'magit-ediff-quit-hook))))))
     'ediff-buffers)))

;;;###autoload
(defun magit-ediff-show-unstaged (file)
  "Show unstaged changes using Ediff.

This only allows looking at the changes; to stage, unstage,
and discard changes using Ediff, use `magit-ediff-stage'.

FILE must be relative to the top directory of the repository."
  (interactive
   (list (magit-read-file-choice "Show unstaged changes for file"
                                 (magit-modified-files)
                                 "No unstaged files")))
  (magit-with-toplevel
    (let ((conf (current-window-configuration))
          (bufA (get-buffer (concat file ".~{index}~")))
          (bufB (get-file-buffer file)))
      (ediff-buffers
       (or bufA (magit-find-file-index-noselect file t))
       (or bufB (find-file-noselect file))
       `((lambda ()
           (setq-local
            ediff-quit-hook
            (lambda ()
              ,@(unless bufA '((ediff-kill-buffer-carefully ediff-buffer-A)))
              ,@(unless bufB '((ediff-kill-buffer-carefully ediff-buffer-B)))
              (let ((magit-ediff-previous-winconf ,conf))
                (run-hooks 'magit-ediff-quit-hook))))))
       'ediff-buffers))))

;;;###autoload
(defun magit-ediff-show-working-tree (file)
  "Show changes between HEAD and working tree using Ediff.
FILE must be relative to the top directory of the repository."
  (interactive
   (list (magit-read-file-choice "Show changes in file"
                                 (magit-changed-files "HEAD")
                                 "No changed files")))
  (magit-with-toplevel
    (let ((conf (current-window-configuration))
          (bufA (magit-get-revision-buffer "HEAD" file))
          (bufB (get-file-buffer file)))
      (ediff-buffers
       (or bufA (magit-find-file-noselect "HEAD" file))
       (or bufB (find-file-noselect file))
       `((lambda ()
           (setq-local
            ediff-quit-hook
            (lambda ()
              ,@(unless bufA '((ediff-kill-buffer-carefully ediff-buffer-A)))
              ,@(unless bufB '((ediff-kill-buffer-carefully ediff-buffer-B)))
              (let ((magit-ediff-previous-winconf ,conf))
                (run-hooks 'magit-ediff-quit-hook))))))
       'ediff-buffers))))

;;;###autoload
(defun magit-ediff-show-commit (commit)
  "Show changes introduced by COMMIT using Ediff."
  (interactive (list (magit-read-branch-or-commit "Revision")))
  (let ((revA (concat commit "^"))
        (revB commit))
    (apply #'magit-ediff-compare
           revA revB
           (magit-ediff-compare--read-files revA revB (magit-current-file)))))

(defun magit-ediff-cleanup-auxiliary-buffers ()
  (let* ((ctl-buf ediff-control-buffer)
         (ctl-win (ediff-get-visible-buffer-window ctl-buf))
         (ctl-frm ediff-control-frame)
         (main-frame (cond ((window-live-p ediff-window-A)
                            (window-frame ediff-window-A))
                           ((window-live-p ediff-window-B)
                            (window-frame ediff-window-B)))))
    (ediff-kill-buffer-carefully ediff-diff-buffer)
    (ediff-kill-buffer-carefully ediff-custom-diff-buffer)
    (ediff-kill-buffer-carefully ediff-fine-diff-buffer)
    (ediff-kill-buffer-carefully ediff-tmp-buffer)
    (ediff-kill-buffer-carefully ediff-error-buffer)
    (ediff-kill-buffer-carefully ediff-msg-buffer)
    (ediff-kill-buffer-carefully ediff-debug-buffer)
    (when (boundp 'ediff-patch-diagnostics)
      (ediff-kill-buffer-carefully ediff-patch-diagnostics))
    (cond ((and (ediff-window-display-p)
                (frame-live-p ctl-frm))
           (delete-frame ctl-frm))
          ((window-live-p ctl-win)
           (delete-window ctl-win)))
    (unless (ediff-multiframe-setup-p)
      (ediff-kill-bottom-toolbar))
    (ediff-kill-buffer-carefully ctl-buf)
    (when (frame-live-p main-frame)
      (select-frame main-frame))))

(defun magit-ediff-restore-previous-winconf ()
  (set-window-configuration magit-ediff-previous-winconf))

;;; magit-ediff.el ends soon
(provide 'magit-ediff)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-ediff.el ends here
