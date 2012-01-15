;;; magit-wip.el --- git-wip plug-in for Magit

;; Copyright (C) 2012  Ryan C. Thompson
;;
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
;; along with Magit.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This plug-in provides special highlighting of git-wip refs in Magit
;; log buffers.

;; Available commands:
;; - `magit-wip-mode': Enable or disable the mode

;; TODO:
;; - Provide an interface for `git wip log'

;;; Code:

(require 'magit)

;;; Customizables:

(define-minor-mode magit-wip-mode
  "Magit support fot git-wip.

Currently, this will just give git-wip refs a special appearance
in Magit log buffers."
  :group 'magit
  :global t
  (magit-wip-setup-namespaces))

(defface magit-log-head-label-wip
  '((((class color) (background light))
     :box t
     :background "Grey95"
     :foreground "LightSkyBlue3")
    (((class color) (background dark))
     :box t
     :background "Grey07"
     :foreground "LightSkyBlue4"))
  "Face for git-wip labels shown in log buffer."
  :group 'magit-faces)

;;; Common code:

(defun magit-log-get-wip-color (suffix)
  (list (concat "(WIP) " suffix)
        'magit-log-head-label-wip))

(defconst magit-wip-refs-namespaces
  '(("wip" magit-log-get-wip-color))
  "A list like `magit-refs-namespaces' but specific to git-wip.")

(defun magit-wip-setup-namespaces ()
  (let ((adder (lambda (ns) (add-to-list 'magit-refs-namespaces ns 'append)))
        (remover (lambda (ns) (setq magit-refs-namespaces (delete ns magit-refs-namespaces)))))
    (mapc (if magit-wip-mode adder remover)
          magit-wip-refs-namespaces)))

(magit-wip-setup-namespaces)

(provide 'magit-wip)
