;;; magit-ediff.el --- Ediff extension for Magit

;; Copyright (C) 2010-2015  The Magit Project Developers
;;
;; For a full list of contributors, see the AUTHORS.md file
;; at the top-level directory of this distribution and at
;; https://raw.github.com/magit/magit/master/AUTHORS.md

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

;;;###autoload (autoload 'magit-ediff-popup "magit-ediff" nil t)
(magit-define-popup magit-ediff-popup
  "Popup console for ediff commands."
  'magit-diff nil nil
  :actions '((?E "Dwim"    magit-ediff-dwim)
             (?d "Compare" magit-ediff-compare)
             (?m "Resolve" magit-ediff-resolve)
             (?s "Stage"   magit-ediff-stage)))

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
  (with-current-buffer (find-file-noselect file)
    (smerge-ediff)))

;;;###autoload
(defun magit-ediff-stage (file)
  "Stage and unstage changes to FILE using Ediff.
FILE has to be relative to the top directory of the repository."
  (interactive
   (list (magit-completing-read "Selectively stage file" nil
                                (magit-tracked-files) nil nil nil
                                (magit-current-file))))
  (let* ((conf (current-window-configuration))
         (bufA (magit-get-revision-buffer "HEAD" file))
         (bufB (get-buffer (concat file ".~{index}~")))
         (bufBrw (and bufB (with-current-buffer bufB
                             (prog1 (not buffer-read-only)
                               (setq buffer-read-only nil)))))
         (bufC (get-file-buffer file)))
    (ediff-buffers3
     (or bufA (magit-find-file-noselect "HEAD" file))
     (or bufB (magit-find-file-index-noselect  file))
     (or bufC (find-file-noselect file))
     `((lambda ()
         (add-hook
          'ediff-quit-hook
          (lambda ()
            (and (buffer-live-p ediff-buffer-B)
                 (buffer-modified-p ediff-buffer-B)
                 (with-current-buffer ediff-buffer-B
                   (magit-update-index)))
            (and (buffer-live-p ediff-buffer-C)
                 (buffer-modified-p ediff-buffer-C)
                 (with-current-buffer ediff-buffer-C
                   (when (y-or-n-p
                          (format "Save file %s? " (buffer-file-name)))
                     (save-buffer))))
            ,@(unless bufA '((ediff-kill-buffer-carefully ediff-buffer-A)))
            ,@(if bufB
                  (unless bufBrw '((with-current-buffer ediff-buffer-B
                                     (setq buffer-read-only t))))
                '((ediff-kill-buffer-carefully ediff-buffer-B)))
            ,@(unless bufC '((ediff-kill-buffer-carefully ediff-buffer-C)))
            (set-window-configuration ,conf))
          nil t)))      ; and then also run default `ediff-cleanup-mess'
     'ediff-buffers3))) ; no "staging" job exists, use this generic job

;;;###autoload
(defun magit-ediff-compare (revA revB fileA fileB)
  "Compare REVA:FILEA with REVB:FILEB using Ediff.
FILEA and FILEB have to be relative to the top directory of the
repository.  If REVA or REVB is nil then this stands for the
working tree state."
  (interactive (cl-destructuring-bind (range revA revB)
                   (magit-ediff-compare--read-revisions)
                 (nconc (list revA revB)
                        (magit-ediff-compare--read-files range revA revB))))
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
         (add-hook
          'ediff-quit-hook
          (lambda ()
            ,@(unless bufA '((ediff-kill-buffer-carefully ediff-buffer-A)))
            ,@(unless bufB '((ediff-kill-buffer-carefully ediff-buffer-B)))
            (set-window-configuration ,conf))
          nil t)))      ; and then also run default `ediff-cleanup-mess'
     'ediff-revision))) ; this job gets no special handling at all; good

(defun magit-ediff-compare--read-revisions (&optional arg)
  (let ((input (or arg (magit-read-range-or-commit "Compare range or commit")))
        range revA revB)
    (if (string-match
         "^\\([^.]+\\)?\\(?:\\.\\.\\(\\.\\)?\\([^.]+\\)?\\)" input)
        (progn (setq revA (or (match-string 1 input) "HEAD")
                     revB (or (match-string 3 input) "HEAD")
                     range (match-string 0 input))
               (when (match-string 2 input)
                 (setq revA (magit-git-string "merge-base" revA revB))))
      (setq revA (concat input "^")
            revB input))
    (list range revA revB)))

(defun magit-ediff-compare--read-files (range revA revB &optional fileB)
  (unless fileB
    (setq fileB (magit-read-file-from-rev
                 revB (if range
                          (format "In range %s compare file" range)
                        (format "Show changes in %s to file" revB)))))
  (list (or (car (member fileB (magit-revision-files revA)))
            (car (rassoc fileB
                         (cl-mapcan
                          (lambda (elt)
                            (setq elt (split-string (substring elt 97)))
                            (when (nth 2 elt)
                              (list (cons (nth 1 elt) (nth 2 elt)))))
                          (magit-git-items
                           "diff-tree" "-z" "-M" "HEAD^" "HEAD"))))
            (magit-read-file-from-rev
             revA (format "Compare %s:%s with file in %s" revB fileB revA)))
        fileB))

;;;###autoload
(defun magit-ediff-dwim ()
  "Compare, stage, or resolve using Ediff.
This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using Ediff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `magit-ediff-*'
command has to be used explicitly.  If it cannot read the users
mind at all, then it asks the user for a command to run."
  (interactive)
  (magit-section-case
    (hunk (save-excursion
            (goto-char (magit-section-start (magit-section-parent it)))
            (magit-ediff-dwim)))
    (commit (call-interactively 'magit-ediff-compare))
    (t
     (let ((command 'magit-ediff-compare)
           (file (magit-current-file))
           revA revB range)
       (cond (magit-buffer-refname
              (setq revB magit-buffer-refname
                    revA (concat revB "^")))
             ((derived-mode-p 'magit-revision-mode)
              (setq revB (car magit-refresh-args)
                    revA (concat revB "^")))
             ((derived-mode-p 'magit-diff-mode)
              (pcase (magit-diff-type)
                (`committed (cl-destructuring-bind (r a b)
                                (magit-ediff-compare--read-revisions
                                 (car magit-refresh-args))
                              (setq revA a revB b range r)))
                (`undefined (setq command nil))
                (_          (setq command 'magit-ediff-stage))))
             (t
              (magit-section-case
                (commit (setq revA (concat (magit-section-value it) "^")
                              revB (magit-section-value it)))
                (branch (let ((current (magit-get-current-branch))
                              (atpoint (magit-section-value it)))
                          (setq revA current
                                revB (if (eq atpoint current)
                                         (magit-get-tracked-branch)
                                       atpoint)
                                range (concat revA "..." revB))))
                (unpushed (setq revA (magit-get-tracked-branch)
                                revB (magit-get-current-branch)
                                range (concat revA ".." revB)))
                (unpulled (setq revA (magit-get-current-branch)
                                revB (magit-get-tracked-branch)
                                range (concat revA ".." revB)))
                (t (setq command
                         (cond ((not file) nil)
                               ((magit-anything-unmerged-p file)
                                'magit-ediff-resolve)
                               (t 'magit-ediff-stage)))))))
       (cond ((not command)
              (call-interactively
               (magit-read-char-case
                   "Failed to read your mind; do you want to " t
                 (?c "[c]ompare" 'magit-ediff-compare)
                 (?r "[r]esolve" 'magit-ediff-resolve)
                 (?s "[s]tage"   'magit-ediff-stage))))
             ((eq command 'magit-ediff-compare)
              (apply 'magit-ediff-compare revA revB
                     (magit-ediff-compare--read-files range revA revB file)))
             (file
              (funcall command file))
             (t
              (call-interactively command)))))))

;;; magit-ediff.el ends soon
(provide 'magit-ediff)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-ediff.el ends here
