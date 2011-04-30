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