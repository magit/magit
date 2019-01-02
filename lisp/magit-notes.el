;;; magit-notes.el --- notes support  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2019  The Magit Project Contributors
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

;; This library implements support for `git-notes'.

;;; Code:

(require 'magit)

;;; Popup

;;;###autoload (autoload 'magit-notes-popup "magit" nil t)
(magit-define-popup magit-notes-popup
  "Popup console for notes commands."
  :man-page "git-notes"
  :variables '("Configure local settings"
               (?c "core.notesRef"
                   magit-set-core.notesRef
                   magit-format-core.notesRef)
               (?d "notes.displayRef"
                   magit-set-notes.displayRef
                   magit-format-notes.displayRef)
               "Configure global settings"
               (?C "core.notesRef"
                   magit-set-global-core.notesRef
                   magit-format-global-core.notesRef)
               (?D "notes.displayRef"
                   magit-set-global-notes.displayRef
                   magit-format-global-notes.displayRef))
  :switches '("Switch for prune"
              (?n "Dry run"          "--dry-run"))
  :options  '("Option for edit and remove"
              (?r "Manipulate ref"   "--ref=" magit-notes-popup-read-ref)
              "Option for merge"
              (?s "Merge strategy"   "--strategy="))
  :actions  '((?T "Edit"             magit-notes-edit)
              (?r "Remove"           magit-notes-remove)
              (?m "Merge"            magit-notes-merge)
              (?p "Prune"            magit-notes-prune))
  :sequence-actions '((?c "Commit merge" magit-notes-merge-commit)
                      (?a "Abort merge"  magit-notes-merge-abort))
  :sequence-predicate 'magit-notes-merging-p
  :default-action 'magit-notes-edit)

(defun magit-notes-merging-p ()
  (let ((dir (magit-git-dir "NOTES_MERGE_WORKTREE")))
    (and (file-directory-p dir)
         (directory-files dir nil "^[^.]"))))

(defun magit-format-core.notesRef ()
  (magit--format-popup-variable:value "core.notesRef" 22))

(defun magit-format-notes.displayRef ()
  (magit--format-popup-variable:values "notes.displayRef" 22))

(defun magit-format-global-core.notesRef ()
  (magit--format-popup-variable:value "core.notesRef" 22 t))

(defun magit-format-global-notes.displayRef ()
  (magit--format-popup-variable:values "notes.displayRef" 22 t))

;;; Commands

(defun magit-notes-edit (commit &optional ref)
  "Edit the note attached to COMMIT.
REF is the notes ref used to store the notes.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (magit-notes-read-args "Edit notes"))
  (magit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "edit" commit))

(defun magit-notes-remove (commit &optional ref)
  "Remove the note attached to COMMIT.
REF is the notes ref from which the note is removed.

Interactively or when optional REF is nil use the value of Git
variable `core.notesRef' or \"refs/notes/commits\" if that is
undefined."
  (interactive (magit-notes-read-args "Remove notes"))
  (magit-run-git-with-editor "notes" (and ref (concat "--ref=" ref))
                             "remove" commit))

(defun magit-notes-merge (ref)
  "Merge the notes ref REF into the current notes ref.

The current notes ref is the value of Git variable
`core.notesRef' or \"refs/notes/commits\" if that is undefined.

When there are conflicts, then they have to be resolved in the
temporary worktree \".git/NOTES_MERGE_WORKTREE\".  When
done use `magit-notes-merge-commit' to finish.  To abort
use `magit-notes-merge-abort'."
  (interactive (list (magit-read-string-ns "Merge reference")))
  (magit-run-git-with-editor "notes" "merge" ref))

(defun magit-notes-merge-commit ()
  "Commit the current notes ref merge.
Also see `magit-notes-merge'."
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--commit"))

(defun magit-notes-merge-abort ()
  "Abort the current notes ref merge.
Also see `magit-notes-merge'."
  (interactive)
  (magit-run-git-with-editor "notes" "merge" "--abort"))

(defun magit-notes-prune (&optional dry-run)
  "Remove notes about unreachable commits."
  (interactive (list (and (member "--dry-run" (magit-notes-arguments)) t)))
  (when dry-run
    (magit-process-buffer))
  (magit-run-git-with-editor "notes" "prune" (and dry-run "--dry-run")))

(defun magit-set-core.notesRef (ref)
  "Set the local value of `core.notesRef' to REF."
  (interactive (list (magit-notes-read-ref "Set local core.notesRef")))
  (magit-set ref "core.notesRef")
  (magit-with-pre-popup-buffer
    (magit-refresh)))

(defun magit-set-global-core.notesRef (ref)
  "Set the global value of `core.notesRef' to REF."
  (interactive (list (magit-notes-read-ref "Set global core.notesRef")))
  (magit-set ref "--global" "core.notesRef")
  (magit-with-pre-popup-buffer
    (magit-refresh)))

(defun magit-set-notes.displayRef (refs)
  "Set the local values of `notes.displayRef' to REFS."
  (interactive (list (magit-notes-read-refs "Set local notes.displayRef")))
  (magit-set-all refs "notes.displayRef")
  (magit-with-pre-popup-buffer
    (magit-refresh)))

(defun magit-set-global-notes.displayRef (refs)
  "Set the global values of `notes.displayRef' to REFS."
  (interactive (list (magit-notes-read-refs "Set global notes.displayRef")))
  (magit-set-all refs "--global" "notes.displayRef")
  (magit-with-pre-popup-buffer
    (magit-refresh)))

(defun magit-notes-read-ref (prompt)
  (--when-let (magit-completing-read
               prompt (magit-list-notes-refnames) nil nil
               (--when-let (magit-get "core.notesRef")
                 (if (string-prefix-p "refs/notes/" it)
                     (substring it 11)
                   it)))
    (if (string-prefix-p "refs/" it)
        it
      (concat "refs/notes/" it))))

(defun magit-notes-read-refs (prompt)
  (mapcar (lambda (ref)
            (if (string-prefix-p "refs/" ref)
                ref
              (concat "refs/notes/" ref)))
          (completing-read-multiple
           (concat prompt ": ")
           (magit-list-notes-refnames) nil nil
           (mapconcat (lambda (ref)
                        (if (string-prefix-p "refs/notes/" ref)
                            (substring ref 11)
                          ref))
                      (magit-get-all "notes.displayRef")
                      ","))))

(defun magit-notes-read-args (prompt)
 (list (magit-read-branch-or-commit prompt (magit-stash-at-point))
       (--when-let (--first (string-match "^--ref=\\(.+\\)" it)
                            (magit-notes-arguments))
         (match-string 1 it))))

;;; _
(provide 'magit-notes)
;;; magit-notes.el ends here
