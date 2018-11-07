;;; magit-patch.el --- creating and applying patches  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2018  The Magit Project Contributors
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

;; This library implements patch commands.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'magit)

;;; Options

(defcustom magit-patch-save-arguments '(exclude "--stat")
  "Arguments used by `magit-patch-save-arguments' (which see)"
  :package-version '(magit . "2.12.0")
  :group 'magit-diff
  :type '(choice (const :tag "use buffer arguments" buffer)
                 (cons :tag "use buffer arguments except"
                       (const :format "" exclude)
                       (repeat :format "%v%i\n"
                               (string :tag "Argument")))
                 (repeat :tag "use constant arguments"
                         (string :tag "Argument"))))

;;; Commands

;;;###autoload (autoload 'magit-patch-popup "magit-patch" nil t)
(magit-define-popup magit-patch-popup
  "Popup console for patch commands."
  :man-page "git-format-patch"
  :switches '("Switches for formatting patches"
              (?l "Add cover letter" "--cover-letter"))
  :options  '("Options for formatting patches"
              (?f "From"             "--from=")
              (?t "To"               "--to=")
              (?c "CC"               "--cc=")
              (?r "In reply to"      "--in-reply-to=")
              (?P "Subject Prefix"   "--subject-prefix=")
              (?v "Reroll count"     "--reroll-count=")
              (?s "Thread style"     "--thread=")
              (?U "Context lines"    "-U")
              (?M "Detect renames"   "-M")
              (?C "Detect copies"    "-C")
              (?A "Diff algorithm"   "--diff-algorithm="
                  magit-diff-select-algorithm)
              (?o "Output directory" "--output-directory=")
              (?F "Limit to files"   "-- " magit-read-files))
  :actions  '((?p "Format patches"   magit-format-patch)
              (?r "Request pull"     magit-request-pull))
  :default-action 'magit-format-patch)

;;;###autoload
(defun magit-format-patch (range args files)
  "Create patches for the commits in RANGE.
When a single commit is given for RANGE, create a patch for the
changes introduced by that commit (unlike 'git format-patch'
which creates patches for all commits that are reachable from
`HEAD' but not from the specified commit)."
  (interactive
   (cons (if-let ((revs (magit-region-values 'commit t)))
             (concat (car (last revs)) "^.." (car revs))
           (let ((range (magit-read-range-or-commit
                         "Format range or commit")))
             (if (string-match-p "\\.\\." range)
                 range
               (format "%s~..%s" range range))))
         (magit-popup-export-file-args (magit-patch-arguments))))
  (magit-run-git "format-patch" range args "--" files)
  (when (member "--cover-letter" args)
    (find-file
     (expand-file-name
      "0000-cover-letter.patch"
      (let ((topdir (magit-toplevel)))
        (or (--some (and (string-match "--output-directory=\\(.+\\)" it)
                         (expand-file-name (match-string 1 it) topdir))
                    args)
            topdir))))))

;;;###autoload
(defun magit-request-pull (url start end)
  "Request upstream to pull from you public repository.

URL is the url of your publically accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit."
  (interactive
   (list (magit-get "remote" (magit-read-remote "Remote") "url")
         (magit-read-branch-or-commit "Start" (magit-get-upstream-branch))
         (magit-read-branch-or-commit "End")))
  (let ((dir default-directory))
    ;; mu4e changes default-directory
    (compose-mail)
    (setq default-directory dir))
  (message-goto-body)
  (magit-git-insert "request-pull" start url end)
  (set-buffer-modified-p nil))

;;;###autoload (autoload 'magit-patch-apply-popup "magit-patch" nil t)
(magit-define-popup magit-patch-apply-popup
  "Popup console for applying a patch file."
  :man-page "git-apply"
  :switches '((?i "Also apply to index"     "--index")
              (?c "Only apply to index"     "--cached")
              (?3 "Fall back on 3way merge" "--3way"))
  :actions  '((?a "Apply patch" magit-patch-apply))
  :default-action 'magit-patch-apply)

;;;###autoload
(defun magit-patch-apply (file &rest args)
  "Apply the patch file FILE."
  (interactive (list (expand-file-name
                      (read-file-name "Apply patch: "
                                      default-directory nil nil
                                      (--when-let (magit-file-at-point)
                                        (file-relative-name it))))
                     (magit-patch-apply-arguments)))
  (magit-run-git "apply" args "--" (magit-convert-filename-for-git file)))

;;;###autoload
(defun magit-patch-save (file &optional arg)
  "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used."
  (interactive (list (read-file-name "Write patch file: " default-directory)
                     current-prefix-arg))
  (unless (derived-mode-p 'magit-diff-mode)
    (user-error "Only diff buffers can be saved as patches"))
  (pcase-let ((`(,rev ,const ,args ,files) magit-refresh-args))
    (when (derived-mode-p 'magit-revision-mode)
      (setq rev (format "%s~..%s" rev rev)))
    (cond ((eq magit-patch-save-arguments 'buffer)
           (when arg
             (setq args nil)))
          ((eq (car-safe magit-patch-save-arguments) 'exclude)
           (unless arg
             (setq args (-difference args (cdr magit-patch-save-arguments)))))
          ((not arg)
           (setq args magit-patch-save-arguments)))
    (with-temp-file file
      (magit-git-insert "diff" rev "-p" const args "--" files)))
  (magit-refresh))

;;; _
(provide 'magit-patch)
;;; magit-patch.el ends here
