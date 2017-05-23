;;; magit-subtree.el --- subtree support for Magit  -*- lexical-binding: t -*-

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

;;; Popup

;;;###autoload (autoload 'magit-subtree-popup "magit-subtree" nil t)
(magit-define-popup magit-subtree-popup
  "Popup console for subtree commands."
  :man-page "git-subtree"
  :switches '("Switches for add, merge, push, and pull"
              (?s "Squash" "--squash")
              "Switches for split"
              (?i "Ignore joins" "--ignore-joins")
              (?j "Rejoin"       "--rejoin"))
  :options  '("Options"
              (?p "Prefix" "--prefix=" magit-subtree-read-prefix)
              "Options for add, merge, and pull"
              (?m "Message" "--message=")
              "Options for split"
              (?a "Annotate" "--annotate=")
              (?b "Branch"   "--branch=")
              (?o "Onto"     "--onto=" magit-read-branch-or-commit))
  :actions  '((?a "Add"        magit-subtree-add)
              (?m "Merge"      magit-subtree-merge)
              (?p "Push"       magit-subtree-push)
              (?c "Add commit" magit-subtree-add-commit)
              (?f "Pull"       magit-subtree-pull)
              (?s "Split"      magit-subtree-split))
  :max-action-columns 3)

(defun magit-subtree-read-prefix (prompt &optional default)
  (let* ((insert-default-directory nil)
         (topdir (magit-toplevel))
         (prefix (read-directory-name (concat prompt ": ") topdir default)))
    (if (file-name-absolute-p prefix)
        ;; At least `ido-mode's variant is not compatible.
        (if (string-prefix-p topdir prefix)
            (file-relative-name prefix topdir)
          (user-error "%s isn't inside the repository at %s" prefix topdir))
      prefix)))

;;; Commands

(defun magit-subtree-prefix (prompt)
  (--if-let (--first (string-prefix-p "--prefix=" it)
                     (magit-subtree-arguments))
      (substring it 9)
    (magit-subtree-read-prefix prompt)))

(defun magit-subtree-args ()
  (-filter (lambda (arg)
             (if (eq this-command 'magit-subtree-split)
                 (or (equal arg "--ignore-joins")
                     (equal arg "--rejoin")
                     (string-prefix-p "--annotate=" arg)
                     (string-prefix-p "--branch=" arg)
                     (string-prefix-p "--onto=" arg))
               (or (equal arg "--squash")
                   (and (string-prefix-p "--message=" arg)
                        (not (eq this-command 'magit-subtree-push))))))
           (magit-subtree-arguments)))

(defun magit-git-subtree (subcmd prefix &rest args)
  (magit-run-git-async "subtree" subcmd (concat "--prefix=" prefix) args))

;;;###autoload
(defun magit-subtree-add (prefix repository ref args)
  "Add REF from REPOSITORY as a new subtree at PREFIX."
  (interactive
   (cons (magit-subtree-prefix "Add subtree")
         (let ((remote (magit-read-remote-or-url "From repository")))
           (list remote
                 (magit-read-refspec "Ref" remote)
                 (magit-subtree-args)))))
  (magit-git-subtree "add" prefix args repository ref))

;;;###autoload
(defun magit-subtree-add-commit (prefix commit args)
  "Add COMMIT as a new subtree at PREFIX."
  (interactive (list (magit-subtree-prefix "Add subtree")
                     (magit-read-string-ns "Commit")
                     (magit-subtree-args)))
  (magit-git-subtree "add" prefix args commit))

;;;###autoload
(defun magit-subtree-merge (prefix commit args)
  "Merge COMMIT into the PREFIX subtree."
  (interactive (list (magit-subtree-prefix "Merge into subtree")
                     (magit-read-string-ns "Commit")
                     (magit-subtree-args)))
  (magit-git-subtree "merge" prefix args commit))

;;;###autoload
(defun magit-subtree-pull (prefix repository ref args)
  "Pull REF from REPOSITORY into the PREFIX subtree."
  (interactive
   (cons (magit-subtree-prefix "Pull into subtree")
         (let ((remote (magit-read-remote-or-url "From repository")))
           (list remote
                 (magit-read-refspec "Ref" remote)
                 (magit-subtree-args)))))
  (magit-git-subtree "pull" prefix args repository ref))

;;;###autoload
(defun magit-subtree-push (prefix repository ref args)
  "Extract the history of the subtree PREFIX and push it to REF on REPOSITORY."
  (interactive (list (magit-subtree-prefix "Push subtree")
                     (magit-read-remote-or-url "To repository")
                     (magit-read-string-ns "To reference")
                     (magit-subtree-args)))
  (magit-git-subtree "push" prefix args repository ref))

;;;###autoload
(defun magit-subtree-split (prefix commit args)
  "Extract the history of the subtree PREFIX."
  (interactive (list (magit-subtree-prefix "Split subtree")
                     (magit-read-string-ns "Commit")
                     (magit-subtree-args)))
  (magit-git-subtree "split" prefix args commit))

(provide 'magit-subtree)
;;; magit-subtree.el ends here
