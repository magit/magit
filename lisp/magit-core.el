;;; magit-core.el --- core functionality  -*- lexical-binding: t -*-

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

;; This library requires several other libraries, so that yet other
;; libraries can just require this one, instead of having to require
;; all the other ones.  In other words this separates the low-level
;; stuff from the rest.  It also defines some Custom groups.

;;; Code:

(require 'magit-utils)
(require 'magit-section)
(require 'magit-git)
(require 'magit-mode)
(require 'magit-margin)
(require 'magit-process)
(require 'magit-autorevert)

(defgroup magit nil
  "Controlling Git from Emacs."
  :link '(url-link "https://magit.vc")
  :link '(info-link "(magit)FAQ")
  :link '(info-link "(magit)")
  :group 'tools)

(defgroup magit-essentials nil
  "Options that every Magit user should briefly think about.

Each of these options falls into one or more of these categories:

* Options that affect Magit's behavior in fundamental ways.
* Options that affect safety.
* Options that affect performance.
* Options that are of a personal nature."
  :link '(info-link "(magit)Essential Settings")
  :group 'magit)

(defgroup magit-miscellaneous nil
  "Miscellanous Magit options."
  :group 'magit)

(defgroup magit-commands nil
  "Options controlling behavior of certain commands."
  :group 'magit)

(defgroup magit-git-arguments nil
  "Options controlling what arguments are passed to Git.

Most of these options can be set using the respective popup,
and it is recommended that you do that because then you can
be certain that Magit supports the arguments that you select.

An option `magit-NAME-argument' specifies the arguments that
are enabled by default by the popup `magit-NAME-popup'."
  :link '(info-link "(magit-popup)Customizing Existing Popups")
  :link '(info-link "(magit-popup)Usage")
  :group 'magit-commands)

(defgroup magit-modes nil
  "Modes used or provided by Magit."
  :group 'magit)

(defgroup magit-buffers nil
  "Options concerning Magit buffers."
  :link '(info-link "(magit)Modes and Buffers")
  :group 'magit)

(defgroup magit-refresh nil
  "Options controlling how Magit buffers are refreshed."
  :link '(info-link "(magit)Automatic Refreshing of Magit Buffers")
  :group 'magit
  :group 'magit-buffers)

(defgroup magit-faces nil
  "Faces used by Magit."
  :group 'magit
  :group 'faces)

(defgroup magit-extensions nil
  "Extensions to Magit."
  :group 'magit)

(custom-add-to-group 'magit-modes   'git-commit        'custom-group)
(custom-add-to-group 'magit-faces   'git-commit-faces  'custom-group)
(custom-add-to-group 'magit-modes   'git-rebase        'custom-group)
(custom-add-to-group 'magit-faces   'git-rebase-faces  'custom-group)
(custom-add-to-group 'magit-process 'with-editor       'custom-group)

(defgroup magit-related nil
  "Options that are relevant to Magit but that are defined elsewhere."
  :link '(custom-group-link vc)
  :link '(custom-group-link smerge)
  :link '(custom-group-link ediff)
  :link '(custom-group-link auto-revert)
  :group 'magit
  :group 'magit-extensions
  :group 'magit-essentials)

(custom-add-to-group 'magit-related     'auto-revert-check-vc-info 'custom-variable)
(custom-add-to-group 'magit-auto-revert 'auto-revert-check-vc-info 'custom-variable)

(custom-add-to-group 'magit-related 'ediff-window-setup-function 'custom-variable)
(custom-add-to-group 'magit-related 'smerge-refine-ignore-whitespace 'custom-variable)
(custom-add-to-group 'magit-related 'vc-follow-symlinks 'custom-variable)

;;; _
(provide 'magit-core)
;;; magit-core.el ends here
