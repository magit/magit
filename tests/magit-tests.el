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
    (insert (make-temp-name "content"))))

(defun magit-tests--modify-and-commit (filename)
  (magit-tests--modify-file filename)
  (magit-call-git "add" filename)
  (magit-call-git "-c" "user.name=foo bar"
                  "-c" "user.email=foo@bar.baz"
                  "commit"
                  "-m" (symbol-name (gensym "message"))
                  "--" filename))

(defun magit-tests--head-hash ()
  (magit-git-string "rev-parse" "--short" "HEAD"))

(defun magit-tests--should-have-section (path info)
  (magit-status default-directory)
  (should (cl-find info
		   (magit-section-children
		    (magit-find-section (list path) magit-root-section))
		   :key 'magit-section-info :test 'equal)))

;;; Tests
;;;; status

(ert-deftest magit-status-untracked ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-file "file")
    (magit-tests--should-have-section 'untracked "file")))

(ert-deftest magit-status-staged-all ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-file "file")
    (magit-status default-directory)
    (let ((magit-stage-all-confirm nil))
      (magit-stage-all t))
    (magit-tests--should-have-section 'staged "file")))

(ert-deftest magit-status-unpushed ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-and-commit "file")

    (magit-tests--with-temp-clone default-directory
      (magit-tests--modify-and-commit "file")
      (magit-tests--should-have-section
       'unpushed (magit-tests--head-hash))

      (magit-tests--modify-and-commit "file")
      (magit-tests--should-have-section
       'unpushed (magit-tests--head-hash)))))

(ert-deftest magit-get-next-tag ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-and-commit "file")

    (magit-tests--with-temp-clone default-directory
     ;; no tag, return nil
     (should (equal nil (magit-get-next-tag)))
     ;; tag is not annotated, return nil
     (magit-call-git "tag" "FIRST")
     (should (equal "FIRST" (magit-git-string "describe" "--contains" "FIRST")))
     (should (equal nil (magit-get-next-tag)))
     (magit-call-git "tag" "-d" "FIRST")
     )))

;;;; config

(ert-deftest magit-config-get-boolean ()
  (magit-tests--with-temp-repo
    (magit-call-git "config" "a.b" "true")
    (should (magit-get-boolean "a.b"))
    (should (magit-get-boolean "a" "b"))

    (magit-call-git "config" "a.b" "false")
    (should-not (magit-get-boolean "a.b"))
    (should-not (magit-get-boolean "a" "b"))))

;;; magit-tests.el ends here
