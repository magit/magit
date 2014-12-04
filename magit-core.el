;;; magit-core.el --- core functionality

;; Copyright (C) 2010-2014  The Magit Project Developers
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

(defgroup magit-popups nil
  "Command console popups provided by Magit."
  :group 'magit)

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

(custom-add-to-group 'magit-popup  'magit-popups      'custom-group)
(custom-add-to-group 'magit-popups 'magit-popup       'custom-group)
(custom-add-to-group 'magit-modes  'magit-popup       'custom-group)
(custom-add-to-group 'magit-faces  'magit-popup-faces 'custom-group)

(when (featurep 'gitattributes-mode)
  (custom-add-to-group 'magit-modes 'gitattributes-mode 'custom-group))
(when (featurep 'gitconfig-mode)
  (custom-add-to-group 'magit-modes 'gitconfig-mode 'custom-group))
(when (featurep 'gitignore-mode)
  (custom-add-to-group 'magit-modes 'gitignore-mode 'custom-group))

(custom-add-to-group 'magit-modes   'git-commit       'custom-group)
(custom-add-to-group 'magit-faces   'git-commit-faces 'custom-group)
(custom-add-to-group 'magit-modes   'git-rebase       'custom-group)
(custom-add-to-group 'magit-faces   'git-rebase-faces 'custom-group)
(custom-add-to-group 'magit-process 'with-editor      'custom-group)

(custom-add-to-group 'magit 'vc-follow-symlinks 'custom-variable)

;;; magit-core.el ends soon
(provide 'magit-core)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-core.el ends here
