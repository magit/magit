;;; magit-wanderlust.el --- wanderlust extension for Magit

(require 'magit)

(defun magit-wl-pipe-to-am ()
  "Ask the user for a project in which to apply (via am) the
current email in wl."
  (interactive)
  "Pipe a wanderlust message into git am."
  (let* ((proj (funcall magit-completing-read
                       "Apply to project: "
                       (magit-list-projects)
                       nil t nil nil)))
    (wl-summary-pipe-message-subr
     nil (format "cd '%s' && git am" proj))))

(provide 'magit-wanderlust)
;;; magit-wanderlust.el ends here
