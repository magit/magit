(require 'ert)
(eval-when-compile
  (require 'cl))

(eval-when-compile
  (unless (require 'mocker nil t)
    (defmacro* mocker-let (specs &body body)
      (error "Skipping tests, mocker.el is not available"))))

(require 'magit)
(require 'magit-blame)

(defmacro with-temp-git-repo (repo &rest body)
  (declare (indent 1) (debug t))
  `(let* ((,repo (make-temp-file "tmp_git" t))
          (default-directory (concat ,repo "/")))
     (unwind-protect
         (progn
           (magit-init repo)
           ,@body)
       (delete-directory ,repo t))))

(defmacro with-opened-file (file &rest body)
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "*buffer*")))
    `(let (,buffer)
       (unwind-protect
           (progn
             (setq ,buffer (find-file-literally ,file))
             ,@body)
         (when ,buffer (kill-buffer ,buffer))))))

(defun magit-tests-section-has-item-title (title &optional section-path)
  (let ((children (magit-section-children
                   (or (and section-path
                            (magit-find-section section-path
                                                magit-top-section))))))
    (should (member title
                    (mapcar 'magit-section-title children)))))

;;; magit.el tests

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

(ert-deftest magit-init-test-expansion ()
  (let* ((dir "~/plop")
         (exp-dir (file-name-as-directory (expand-file-name dir))))
    (mocker-let
        ;; make sure all steps have the expanded version of dir
        ((magit-get-top-dir (dir)
                            ((:input `(,exp-dir) :output nil)))
         (file-directory-p (dir)
                           ((:input `(,exp-dir) :output t)))
         (magit-run* (args)
                     ((:input `((,magit-git-executable "init"))
                       :output t))))
      (should (magit-init dir)))))

(ert-deftest magit-untracked-file ()
  (let ((dummy-filename "foo"))
    (with-temp-git-repo repo
      (with-temp-buffer
        (write-file (format "%s/%s" repo dummy-filename)))
      (magit-status repo)
      (magit-tests-section-has-item-title dummy-filename '(untracked)))))

(ert-deftest magit-staged-file-from-all ()
  (let ((dummy-filename "foo"))
    (with-temp-git-repo repo
      (with-temp-buffer
        (write-file (format "%s/%s" repo dummy-filename)))
      (magit-status repo)
      (magit-stage-all t)
      (magit-tests-section-has-item-title dummy-filename '(staged)))))

(ert-deftest magit-get-boolean ()
  (with-temp-git-repo repo
    (magit-run* '("git" "config" "core.safecrlf" "true"))
    (should (magit-get-boolean "core.safecrlf"))
    (should (magit-get-boolean "core" "safecrlf"))

    (magit-run* '("git" "config" "core.safecrlf" "false"))
    (should-not (magit-get-boolean "core.safecrlf"))))

;;; magit-blame.el tests

(ert-deftest magit-blame-mode ()
  (let ((dummy-filename "foo"))
    (with-temp-git-repo repo
      (with-temp-buffer
        (insert "dummy content")
        (write-file (format "%s/%s" repo dummy-filename)))
      (magit-status repo)
      (magit-stage-all t)
      (magit-log-edit)
      (insert "dummy message")
      (magit-log-edit-commit)
      (with-opened-file (format "%s/%s" repo dummy-filename)
        (should (magit-blame-mode))))))
