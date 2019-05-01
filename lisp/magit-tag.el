;;; magit-tag.el --- tag functionality  -*- lexical-binding: t -*-

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

;; This library implements tag commands.

;;; Code:

(require 'magit)

;;;###autoload (autoload 'magit-tag "magit" nil t)
(define-transient-command magit-tag ()
  "Create or delete a tag."
  :man-page "git-tag"
  ["Arguments"
   ("-f" "Force"    ("-f" "--force"))
   ("-a" "Annotate" ("-a" "--annotate"))
   ("-s" "Sign"     ("-s" "--sign"))
   (magit-tag:--local-user)]
  [["Create"
    ("t"  "tag"     magit-tag-create)
    ("r"  "release" magit-tag-release)]
   ["Do"
    ("k"  "delete"  magit-tag-delete)
    ("p"  "prune"   magit-tag-prune)]])

(defun magit-tag-arguments ()
  (transient-args 'magit-tag))

(define-infix-argument magit-tag:--local-user ()
  :description "Sign as"
  :class 'transient-option
  :shortarg "-u"
  :argument "--local-user="
  :reader 'magit-read-gpg-secret-key
  :history-key 'magit:--gpg-sign)

;;;###autoload
(defun magit-tag-create (name rev &optional args)
  "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\n(git tag [--annotate] NAME REV)"
  (interactive (list (magit-read-tag "Tag name")
                     (magit-read-branch-or-commit "Place tag on")
                     (let ((args (magit-tag-arguments)))
                       (when current-prefix-arg
                         (cl-pushnew "--annotate" args))
                       args)))
  (magit-run-git-with-editor "tag" args name rev))

;;;###autoload
(defun magit-tag-delete (tags)
  "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.
\n(git tag -d TAGS)"
  (interactive (list (--if-let (magit-region-values 'tag)
                         (magit-confirm t nil "Delete %i tags" nil it)
                       (magit-read-tag "Delete tag" t))))
  (magit-run-git "tag" "-d" tags))

;;;###autoload
(defun magit-tag-prune (tags remote-tags remote)
  "Offer to delete tags missing locally from REMOTE, and vice versa."
  (interactive
   (let* ((remote (magit-read-remote "Prune tags using remote"))
          (tags   (magit-list-tags))
          (rtags  (prog2 (message "Determining remote tags...")
                      (magit-remote-list-tags remote)
                    (message "Determining remote tags...done")))
          (ltags  (-difference tags rtags))
          (rtags  (-difference rtags tags)))
     (unless (or ltags rtags)
       (message "Same tags exist locally and remotely"))
     (unless (magit-confirm t
               "Delete %s locally"
               "Delete %i tags locally"
               'noabort ltags)
       (setq ltags nil))
     (unless (magit-confirm t
               "Delete %s from remote"
               "Delete %i tags from remote"
               'noabort rtags)
       (setq rtags nil))
     (list ltags rtags remote)))
  (when tags
    (magit-call-git "tag" "-d" tags))
  (when remote-tags
    (magit-run-git-async "push" remote (--map (concat ":" it) remote-tags))))

(defvar magit-release-tag-regexp "\\`\
\\(?1:\\(?:v\\(?:ersion\\)?\\|r\\(?:elease\\)?\\)?[-_]?\\)?\
\\(?2:[0-9]+\\(?:\\.[0-9]+\\)*\\)\\'"
  "Regexp used to parse release tag names.
The first submatch must match the prefix, if any.
The second submatch must match the version string.")

;;;###autoload
(defun magit-tag-release (tag msg)
  "Create an annotated release tag.

Assume that release tags match `magit-release-tag-regexp'.

First prompt for the name of the new tag using the highest
existing tag as initial input and leaving it to the user to
increment the desired part of the version string.

Then prompt for the message of the new tag.  Base the proposed
tag message on the message of the highest tag, provided that
that contains the corresponding version string and substituting
the new version string for that.  Otherwise propose something
like \"Foo-Bar 1.2.3\", given, for example, a TAG \"v1.2.3\" and a
repository located at something like \"/path/to/foo-bar\".

Then call \"git tag --annotate --sign -m MSG TAG\" to create the,
tag, regardless of whether these arguments are enabled in the
popup.  Finally show the refs buffer to let the user quickly
review the result."
  (interactive
   (save-match-data
     (pcase-let*
         ((`(,pver ,ptag ,pmsg) (car (magit--list-releases)))
          (tag (read-string "Create release tag: " ptag))
          (ver (and (string-match magit-release-tag-regexp tag)
                    (match-string 2 tag)))
          (msg (cond ((and pver (string-match (regexp-quote pver) pmsg))
                      (replace-match ver t t pmsg))
                     ((and ptag (string-match (regexp-quote ptag) pmsg))
                      (replace-match tag t t pmsg))
                     (t (format "%s %s"
                                (capitalize
                                 (file-name-nondirectory
                                  (directory-file-name (magit-toplevel))))
                                ver)))))
       (list tag (read-string (format "Message for %S: " tag) msg)))))
  (let ((proc (magit-run-git-async "tag" "--annotate" "--sign" "-m" msg tag)))
    ;; Allow Emacs to handle pinentry for signing.
    (while (eq (process-status proc) 'run)
      (accept-process-output proc)))
  (magit-refs-setup-buffer nil (magit-show-refs-arguments)))

(defun magit--list-releases ()
  "Return a list of releases.
The list is ordered, beginning with the highest release.
Each release element has the form (VERSION TAG MESSAGE).
`magit-release-tag-regexp' is used to determine whether
a tag qualifies as a release tag."
  (save-match-data
    (mapcar
     #'cdr
     (nreverse
      (cl-sort (cl-mapcan
                (lambda (line)
                  (and (string-match " +" line)
                       (let ((tag (substring line 0 (match-beginning 0)))
                             (msg (substring line (match-end 0))))
                         (and (string-match magit-release-tag-regexp tag)
                              (let ((ver (match-string 2 tag)))
                                (list (list (version-to-list ver)
                                            ver tag msg)))))))
                ;; Cannot rely on "--sort=-version:refname" because
                ;; that gets confused if the version prefix has changed.
                (magit-git-lines "tag" "-n"))
               ;; The inverse of this function does not exist.
               #'version-list-< :key #'car)))))

;;; _
(provide 'magit-tag)
;;; magit-tag.el ends here
