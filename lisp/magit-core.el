;;; magit-core.el --- core functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2010-2016  The Magit Project Contributors
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
(require 'magit-popup)
(require 'magit-process)

(defgroup magit nil
  "Controlling Git from Emacs."
  :group 'tools)

(defgroup magit-commands nil
  "Options controlling behavior of certain commands."
  :group 'magit)

(defgroup magit-modes nil
  "Modes used or provided by Magit."
  :group 'magit)

(defgroup magit-extensions nil
  "Extensions to Magit."
  :group 'magit)

(defgroup magit-faces nil
  "Faces used by Magit."
  :group 'magit
  :group 'faces)

(custom-add-to-group 'magit-modes   'magit-popup       'custom-group)
(custom-add-to-group 'magit-faces   'magit-popup-faces 'custom-group)
(custom-add-to-group 'magit-modes   'git-commit        'custom-group)
(custom-add-to-group 'magit-faces   'git-commit-faces  'custom-group)
(custom-add-to-group 'magit-modes   'git-rebase        'custom-group)
(custom-add-to-group 'magit-faces   'git-rebase-faces  'custom-group)
(custom-add-to-group 'magit-process 'with-editor       'custom-group)

(custom-add-to-group 'magit 'vc-follow-symlinks 'custom-variable)

;;; magit-core.el ends soon
(provide 'magit-core)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-core.el ends here
