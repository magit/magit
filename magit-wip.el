;;; magit-wip.el --- save to wip refs

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

;; This library implements a global minor-mode which saves changes to
;; dedicated work-in-progress refs, whenever the user saves a buffer
;; which visits a file tracked by Git.

;;; Code:

(require 'magit-core)

(require 'format-spec)

;;; Options

(defgroup magit-wip nil
  "Automatically commit work-in-progress to a dedicated ref."
  :group 'magit-extensions)

(defcustom magit-wip-commit-message "autosave %r"
  "Commit message for automatic work-in-progress commits.

The following `format'-like specs are supported:
%r the relative filename of the file being saved,
%a the absolute filename of the file being saved, and
%t the absolute filename of the repository toplevel."
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-save-message "Wrote %a (wip)"
  "Message shown after updating a work-in-progress ref.

The following `format'-like specs are supported:
%r the relative filename of the file being saved,
%a the absolute filename of the file being saved, and
%t the absolute filename of the repository toplevel."
  :group 'magit-wip
  :type '(choice (const :tag "No message" nil)
                 (string :tag "Format")))

(defcustom magit-wip-save-mode-lighter " MWip"
  "Lighter for Magit-Wip-Save mode."
  :group 'magit-wip
  :type 'string)

(defcustom magit-wip-ref-format "refs/wip/%b"
  "Format of work-in-progress refs.

The format string has to begin with \"refs/PREFIX/\"
and end with a `format'-like spec, one of:
%b the short branch name, e.g. \"master\", or
%r the full refname, e.g. \"refs/heads/master\".

When `HEAD' is detached then \"HEAD\" is used for both %b and %r.
The use of %r is recommended but %b is used in the default value
for compatibilty with git-wip (https://github.com/bartman/git-wip)."
  :group 'magit-wip
  :type 'string)

;;; Mode

;;;###autoload
(define-minor-mode magit-wip-save-mode
  "On each save, also commit to a work-in-progress ref.

After saving the buffer this mode also commits the changes to
the work-in-progress ref for the current branch.  Use option
`magit-wip-ref-format' to configure what refname is used.

While this mode can be activated manually it is better to do
so using either

  git config --add magit.extension wip-save

to activate it in individual repositories or

  git config --global --add magit.extension wip-save

to activate it in all repositories.  These settings only take
effect after _also_ turning on `global-magit-wip-save-mode'."
  :lighter magit-wip-save-mode-lighter
  (if magit-wip-save-mode
      (if (and (buffer-file-name)
               (magit-inside-worktree-p))
          (add-hook 'after-save-hook 'magit-wip-save t t)
        (setq magit-wip-save-mode nil)
        (user-error "Need a repository and a file"))
    (remove-hook 'after-save-hook 'magit-wip-save t)))

;;;###autoload
(define-globalized-minor-mode global-magit-wip-save-mode
  magit-wip-save-mode turn-on-magit-wip-save
  :group 'magit-wip)

(defun turn-on-magit-wip-save ()
  "Conditionally turn on Magit-Wip-Save mode.

If the current buffer visits a file tracked in a Git repository,
then turn on `magit-wip-save-mode' provided the `wip-save' Magit
extension has been enabled in that repository."
  (when (and (buffer-file-name)
             (file-directory-p (file-name-directory (buffer-file-name)))
             (magit-inside-worktree-p)
             (magit-file-tracked-p (buffer-file-name))
             (member "wip-save" (magit-get-all "magit.extension")))
    (magit-wip-save-mode 1)))

(defun magit-wip-save (&optional filename wipref)
  "Commit changes to FILENAME in work-in-progress ref WIPREF.
If optional FILENAME is nil or undefined use `buffer-file-name'.
If optional WIPREF is nil or undefined use a ref in accordance
to the current branch and `magit-wip-ref-format'."
  (let* ((filename (or filename (buffer-file-name)))
         (toplevel (magit-toplevel))
         (blobname (file-relative-name filename toplevel))
         (spec   `((?r . ,blobname)
                   (?a . ,filename)
                   (?t . ,toplevel)))
         (ref    (magit-git-string "symbolic-ref" "HEAD"))
         (wipref (or wipref
                     (format-spec
                      magit-wip-ref-format
                      `((?r . ,(or ref "HEAD"))
                        (?b . ,(if ref (substring ref 11) "HEAD"))))))
         (parent (if (and (magit-rev-verify wipref)
                          (equal (magit-git-string "merge-base" wipref ref)
                                 (magit-rev-verify ref)))
                     wipref
                   (or ref "HEAD")))
         (tree   (magit-with-temp-index parent
                   (magit-call-git "add" filename)
                   (magit-git-string "write-tree"))))
    (when (magit-git-failure "diff-tree" "--exit-code" tree parent)
      (magit-reflog-enable wipref)
      (magit-run-git "update-ref" wipref
                     "-m" (concat "magit-wip-save: " blobname)
                     (magit-git-string
                      "commit-tree" tree "-p" parent
                      "-m" (format-spec magit-wip-commit-message spec)))
      (when magit-wip-save-message
        (message (format-spec magit-wip-save-message spec))))))

;;; magit-wip.el ends soon
(provide 'magit-wip)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-wip.el ends here
