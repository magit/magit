;;; magit-tests.el --- tests for Magit

(require 'cl)
(require 'ert)

(require 'magit)
(require 'magit-blame)

;;; Utilities

(defmacro magit-tests--with-temp-dir (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (gensym)))
    `(let ((,dir (file-name-as-directory (make-temp-file "dir" t))))
       (unwind-protect
           (let ((default-directory ,dir)) ,@body)
         (delete-directory ,dir t)))))

(defmacro magit-tests--with-temp-repo (&rest body)
  (declare (indent 0) (debug t))
  `(magit-tests--with-temp-dir
     (magit-call-git "init" ".")
     ,@body))

(defmacro magit-tests--with-temp-clone (url &rest body)
  (declare (indent 1) (debug t))
  (let ((repo (gensym)))
    `(let ((,repo ,(or url 'default-directory)))
       (magit-tests--with-temp-dir
         (magit-call-git "clone" ,repo ".")
         ,@body))))

(defmacro magit-tests--with-open-file (filename &rest body)
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "*buffer*")))
    `(let (,buffer)
       (unwind-protect
           (progn
             (setq ,buffer (find-file-literally ,filename))
             ,@body)
         (when ,buffer (kill-buffer ,buffer))))))

(defun magit-tests--modify-file (filename)
  (with-temp-file (expand-file-name filename)
    (insert (symbol-name (gensym "content")))))

(defun magit-tests--modify-and-commit (filename)
  (magit-tests--modify-file filename)
  (magit-call-git "add" filename)
  (magit-call-git "-c" "user.name=foo bar"
                  "-c" "user.email=foo@bar.baz"
                  "commit"
                  "-m" (symbol-name (gensym "message"))
                  "--" filename))

(defun magit-tests--head-hash ()
  (magit-git-string
   "rev-parse" (format "--short=%d" magit-sha1-abbrev-length) "HEAD"))

(defun magit-tests--should-have-item-title (title section-path)
  (magit-status default-directory)
  (should (member title
                  (mapcar 'magit-section-title
                          (magit-section-children
                           (magit-find-section section-path
                                               magit-root-section))))))

;;; Tests
;;;; magit.el
;;;;; init

(ert-deftest magit-init ()
  (let* ((top-repo (file-name-as-directory (make-temp-file "top" t)))
         (sub-repo (file-name-as-directory
                    (expand-file-name (make-temp-name "sub") top-repo))))
    (unwind-protect
        (progn
          (magit-init top-repo)
          (should (magit-git-repo-p top-repo))
          (make-directory sub-repo)
          (let ((default-directory sub-repo))
            (flet ((yes-or-no-p (create-sub-repo?) 'yes))
              (magit-init sub-repo)))
          (should (magit-git-repo-p sub-repo))
          (should (magit-git-repo-p top-repo)))
      (delete-directory top-repo t))))

;;;;; status

(ert-deftest magit-status-untracked ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-file "file")
    (magit-tests--should-have-item-title "file" '(untracked))))

(ert-deftest magit-status-staged-all ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-file "file")
    (magit-status default-directory)
    (let ((magit-stage-all-confirm nil))
      (magit-stage-all t))
    (magit-tests--should-have-item-title "file" '(staged))))

(ert-deftest magit-status-unpushed ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-and-commit "file")

    (magit-tests--with-temp-clone default-directory
      (magit-tests--modify-and-commit "file")
      (magit-tests--should-have-item-title
       (magit-tests--head-hash) '(unpushed))

      (magit-tests--modify-and-commit "file")
      (magit-tests--should-have-item-title
       (magit-tests--head-hash) '(unpushed)))))

;;;;; config

(ert-deftest magit-config-get-boolean ()
  (magit-tests--with-temp-repo
    (magit-call-git "config" "a.b" "true")
    (should (magit-get-boolean "a.b"))
    (should (magit-get-boolean "a" "b"))

    (magit-call-git "config" "a.b" "false")
    (should-not (magit-get-boolean "a.b"))
    (should-not (magit-get-boolean "a" "b"))))

;;;; magit-blame.el

(ert-deftest magit-blame-mode ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-and-commit "file")
    (magit-tests--with-open-file "file"
      (should (magit-blame-mode)))))

;;; magit-tests.el ends here
