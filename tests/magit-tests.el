(require 'ert)
(eval-when-compile
  (require 'cl))

(eval-when-compile
  (when (null (ignore-errors (require 'mocker)))
    (defmacro* mocker-let (specs &body body)
      (error "Skipping tests, mocker.el is not available"))))

(require 'magit)

(defmacro with-temp-git-repo (repo &rest body)
  (declare (indent 1) (debug t))
  `(let ((,repo (make-temp-file "tmp_git" t)))
     (unwind-protect
         (progn
           (magit-init repo)
           ,@body)
       (delete-directory ,repo t))))

(ert-deftest magit-init-test ()
  (with-temp-git-repo repo
    (should (magit-git-repo-p repo))))

(ert-deftest magit-init-nested ()
  (with-temp-git-repo repo
    (mocker-let
        ((yes-or-no-p (prompt)
                      ((:input-matcher
                        (lambda (p)
                          (string-match "^There is a Git repository" p))
                        :output t))))
      (let ((nested-repo (concat repo "/nested")))
        (make-directory nested-repo)
        (magit-init nested-repo)
        (should (magit-git-repo-p nested-repo))))
    (should (magit-git-repo-p repo))))
