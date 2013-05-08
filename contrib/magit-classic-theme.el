;;; magit-classic-theme.el --- old-style faces of Magit

;; Copyright (C) 2011  Peter J. Weisberg

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

;; This partial theme restores Magit's default faces to the way they
;; were before 3d0ea1d60fbc0a8135e2146418c0c64c9d05705a.

;;; Code:

(deftheme magit-classic
  "Old-style faces of Magit")

(custom-theme-set-faces
 'magit-classic

 '(magit-header
   ((t)))
 
 '(magit-section-title
   ((t
     :weight bold
     :inherit magit-header)))

 '(magit-branch
   ((t
     :weight bold
     :inherit magit-header)))

 '(magit-diff-file-header
   ((t
     :inherit magit-header)))

 '(magit-diff-hunk-header
   ((t
     :slant italic
     :inherit magit-header)))

 '(magit-diff-add
   ((((class color) (background light))
     :foreground "blue1")
    (((class color) (background dark))
     :foreground "white")))

 '(magit-diff-none
   ((t)))

 '(magit-diff-del
   ((((class color) (background light))
      :foreground "red")
     (((class color) (background dark))
      :foreground "OrangeRed")))

 '(magit-item-highlight
   ((((class color) (background light))
     :background "gray95")
    (((class color) (background dark))
     :background "dim gray")))

 '(magit-item-mark
   ((((class color) (background light))
     :foreground "red")
    (((class color) (background dark))
     :foreground "orange")))
 )

(provide-theme 'magit-classic)
;;; magit-classic-theme.el ends here
