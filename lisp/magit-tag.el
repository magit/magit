;;; magit-tag.el --- tag functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2018  The Magit Project Contributors
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

;;;###autoload (autoload 'magit-tag-popup "magit" nil t)
(magit-define-popup magit-tag-popup
  "Popup console for tag commands."
  :man-page "git-tag"
  :switches '((?a "Annotate" "--annotate")
              (?f "Force"    "--force")
              (?s "Sign"     "--sign"))
  :options  '((?f "Sign"     "--local-user=" magit-read-gpg-secret-key))
  :actions  '((?t "Create"   magit-tag)
              (?k "Delete"   magit-tag-delete)
              (?p "Prune"    magit-tag-prune))
  :default-action 'magit-tag)

;;;###autoload
(defun magit-tag (name rev &optional args)
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

;;;###autoload
(defun magit-tag-release (tag)
  "Create an opinionated release tag.

Assume version tags that match \"\\\\`v?[0-9]\\\\(\\\\.[0-9]\\\\)*\\\\'\".
Prompt for the name of the new tag using the highest existing tag
as initial input and call \"git tag --annotate --sign -m MSG\" TAG,
regardless of whether these arguments are enabled in the popup.
Given a TAG \"v1.2.3\" and a repository \"/path/to/foo-bar\", the
MESSAGE would be \"Foo-Bar 1.2.3\".

Because it is so opinionated, this command is not available from
the tag popup by default."
  (interactive
   (list (read-string "Create tag: "
                      (car (nreverse
                            (cl-sort (magit-list-tags) #'version<
                                     :key (lambda (tag)
                                            (if (string-prefix-p "v" tag)
                                                (substring tag 1)
                                              tag))))))))
  (magit-run-git
   "tag" "--annotate" "--sign"
   "-m" (format "%s %s"
                (capitalize (file-name-nondirectory
                             (directory-file-name (magit-toplevel))))
                (if (string-prefix-p "v" tag)
                    (substring tag 1)
                  tag))
   tag)
  (magit-show-refs))

(provide 'magit-tag)
;;; magit-tag.el ends here
