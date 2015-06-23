;;; magit-tests.el --- tests for Magit

;; Copyright (C) 2011-2015  The Magit Project Contributors
;;
;; License: GPLv3

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'magit)

;;; Utilities

(defmacro magit-tests--silentio (&rest body)
  ;; Once upon a time there was a dynamic `flet'...
  (declare (indent defun))
  (let ((orig (cl-gensym)))
    `(let ((,orig (symbol-function 'message)))
       (fset 'message (lambda (&rest silentio)))
       (prog1 (progn ,@body)
	 (fset 'message ,orig)))))

(defmacro magit-tests--with-temp-dir (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (cl-gensym)))
    `(let ((,dir (file-name-as-directory (make-temp-file "dir" t))))
       (unwind-protect
           (let ((default-directory ,dir)) ,@body)
         (delete-directory ,dir t)))))

(defmacro magit-tests--with-temp-repo (&rest body)
  (declare (indent 0) (debug t))
  `(magit-tests--with-temp-dir
     (magit-call-git "init" ".")
     (magit-tests--silentio ,@body)))

(defmacro magit-tests--with-temp-clone (url &rest body)
  (declare (indent 1) (debug t))
  (let ((repo (cl-gensym)))
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
                  "-m" (symbol-name (cl-gensym "message"))
                  "--" filename))

(defun magit-tests--should-have-section (type info)
  (magit-status-internal default-directory)
  (message (buffer-string))
  (should (--first (equal (magit-section-value it) info)
                   (magit-section-children
                    (magit-get-section `((,type) (status)))))))

;;; Tests
;;;; status

(ert-deftest magit-status-untracked ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-file "file")
    (magit-tests--modify-file "file with space")
    (magit-tests--modify-file "φιλε")
    (magit-tests--should-have-section 'untracked "file")
    (magit-tests--should-have-section 'untracked "file with space")
    (magit-tests--should-have-section 'untracked "φιλε")))

(ert-deftest magit-status-staged-modified ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-file "file")
    (magit-status-internal default-directory)
    (magit-stage-modified t)
    (magit-tests--should-have-section 'staged "file")))

(ert-deftest magit-status-staged-modified-with-space ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-file "file with space")
    (magit-status-internal default-directory)
    (magit-stage-modified t)
    (magit-tests--should-have-section 'staged "file with space")))

(ert-deftest magit-status-modified ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-and-commit "file")
    (magit-tests--modify-file "file")
    (magit-status-internal default-directory)
    (magit-tests--should-have-section 'unstaged "file")))

(ert-deftest magit-status-modified-with-space ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-and-commit "file with space")
    (magit-tests--modify-file "file with space")
    (magit-status-internal default-directory)
    (magit-tests--should-have-section 'unstaged "file with space")))

(ert-deftest magit-status-unpushed ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-and-commit "file")

    (magit-tests--with-temp-clone default-directory
      (magit-tests--modify-and-commit "file")
      (magit-tests--should-have-section
       'unpushed (magit-rev-parse "--short" "HEAD"))

      (magit-tests--modify-and-commit "file")
      (magit-tests--should-have-section
       'unpushed (magit-rev-parse "--short" "HEAD")))))

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
     (magit-call-git "tag" "-d" "FIRST"))))

;;;; config

(ert-deftest magit-config-get-boolean ()
  (magit-tests--with-temp-repo
    (magit-call-git "config" "a.b" "true")
    (should (magit-get-boolean "a.b"))
    (should (magit-get-boolean "a" "b"))

    (magit-call-git "config" "a.b" "false")
    (should-not (magit-get-boolean "a.b"))
    (should-not (magit-get-boolean "a" "b"))))

;;;; branch and remotes
(ert-deftest magit-list-branch ()
  (magit-tests--with-temp-repo
    (magit-tests--modify-and-commit "file")
    (should (member "master" (magit-list-branch-names)))
    (should (member "master" (magit-list-local-branch-names)))
    (should (null (magit-list-remote-branch-names)))

    (magit-tests--with-temp-clone default-directory
      (should (member "origin/master" (magit-list-branch-names)))
      (should-not (member "origin/master" (magit-list-local-branch-names)))

      (should (member "origin/master" (magit-list-remote-branch-names)))
      (should (member "origin/master" (magit-list-remote-branch-names "origin")))

      (should-not (member "origin/master" (magit-list-remote-branch-names "foo")))
      (should (member "master" (magit-list-remote-branch-names "origin" t)))

      (should-not (member "master" (magit-list-remote-branch-names "foo" t))))))

;;; magit-tests.el ends soon

(defconst magit-tests-font-lock-keywords
  '(("(magit-tests--with-temp-\\(?:clone\\|dir\\|repo\\)\\_>" . 1)))

(font-lock-add-keywords 'emacs-lisp-mode magit-tests-font-lock-keywords)

(provide 'magit-tests)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-tests.el ends here
