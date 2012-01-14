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

(define-minor-mode magit-wip-mode
  "Magit support fot git-wip.

Currently, this will just give git-wip refs a special appearance
in Magit log buffers."
  :group 'magit
  :global t
  :require 'magit-wip
  (magit-wip-setup-namespaces))

(provide 'magit-wip)
