(require 'ert)
(eval-when-compile (require 'cl-lib))

(eval-when-compile
  (unless (require 'mocker nil t)
    (cl-defmacro mocker-let (specs &rest body)
      (error "Skipping tests, mocker.el is not available"))))

(require 'magit)
(require 'magit-blame)

(defmacro with-temp-git-repo (repo &rest body)
  (declare (indent 1) (debug t))
  `(let* ((,repo (make-temp-file "tmp_git" t))
          (default-directory (concat ,repo "/")))
     (unwind-protect
         (progn
           (magit-init ,repo)
           ,@body)
       (delete-directory ,repo t))))

(defmacro with-cloned-git-repo (source-repo dest-repo &rest body)
  (declare (indent 2) (debug t))
  `(let* ((,dest-repo (make-temp-file "tmp_git" t))
          (default-directory (concat ,dest-repo "/")))
     (unwind-protect
         (progn
           (magit-run* (list magit-git-executable "clone" ,source-repo ,dest-repo))
           ,@body)
       (delete-directory ,dest-repo t))))

(defmacro with-opened-file (file &rest body)
  (declare (indent 1) (debug t))
  (let ((buffer (make-symbol "*buffer*")))
    `(let (,buffer)
       (unwind-protect
           (progn
             (setq ,buffer (find-file-literally ,file))
             ,@body)
         (when ,buffer (kill-buffer ,buffer))))))


(defun magit-tests--get-latest-sha1 ()
  (magit-git-string "log"
                    "-n"
                    "1"
                    (format "--abbrev=%s" magit-sha1-abbrev-length)
                    "--format=%h"
                    "HEAD"))

(defun magit-tests--commit-all (msg)
  (magit-stage-all t)
  (magit-log-edit)
  (insert msg)
  (magit-log-edit-commit))

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

(ert-deftest magit-unpushed ()
  (let ((dummy-filename "foo"))
    (with-temp-git-repo repo-server
      (with-temp-buffer
        (insert "1")
        (write-file (format "%s/%s" repo-server dummy-filename)))
      (magit-status repo-server)
      (magit-tests--commit-all "dummy message")

      (with-cloned-git-repo repo-server repo-client
        (with-temp-buffer
          (insert "2")
          (write-file (format "%s/%s" repo-client dummy-filename)))
        (magit-status repo-client)
        (magit-tests--commit-all "an unpushed commit")
        (magit-tests-section-has-item-title (magit-tests--get-latest-sha1) '(unpushed))

        (with-temp-buffer
          (insert "3")
          (write-file (format "%s/%s" repo-client dummy-filename)))
        (magit-status repo-client)
        (magit-tests--commit-all "an unpushed commit #2")
        (magit-tests-section-has-item-title (magit-tests--get-latest-sha1) '(unpushed))))))

(ert-deftest magit-get-boolean ()
  (with-temp-git-repo repo
    (magit-run* '("git" "config" "core.safecrlf" "true"))
    (should (magit-get-boolean "core.safecrlf"))
    (should (magit-get-boolean "core" "safecrlf"))

    (magit-run* '("git" "config" "core.safecrlf" "false"))
    (should-not (magit-get-boolean "core.safecrlf"))))

;;; magit-blame.el tests

;; (ert-deftest magit-blame-mode ()
;;   (let ((dummy-filename "foo"))
;;     (with-temp-git-repo repo
;;       (with-temp-buffer
;;         (insert "dummy content")
;;         (write-file (format "%s/%s" repo dummy-filename)))
;;       (magit-status repo)
;;       (magit-tests--commit-all "dummy message")
;;       (with-opened-file (format "%s/%s" repo dummy-filename)
;;         (should (magit-blame-mode))))))
