;;; magit-tests.el --- tests for Magit

;; Copyright (C) 2011-2015  The Magit Project Contributors
;;
;; License: GPLv3

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ert)

(require 'magit)

(defmacro magit-with-test-directory (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "magit-" t))))
       (condition-case err
           (cl-letf (((symbol-function #'message) (lambda (&rest _))))
             (let ((default-directory ,dir))
               ,@body))
         (error (message "Keeping test directory:\n  %s" ,dir)
                (signal (car err) (cdr err))))
       (delete-directory ,dir t))))

(defmacro magit-with-test-repository (&rest body)
  (declare (indent 0) (debug t))
  `(magit-with-test-directory (magit-git "init" ".") ,@body))

;;; Git

(ert-deftest magit-toplevel ()
  (magit-with-test-directory
    (let ((find-file-visit-truename nil))
      (magit-git "init" "repo")
      ;; magit--with-safe-default-directory
      (should (equal (magit-toplevel "repo/")
                     (magit-toplevel (expand-file-name "repo/"))))
      (should (equal (magit-toplevel "repo")
                     (magit-toplevel (expand-file-name "repo/"))))
      ;; repo
      (make-directory "repo/subdir/subsubdir" t)
      (should (equal (magit-toplevel   "repo/")
                     (expand-file-name "repo/")))
      (should (equal (magit-toplevel   "repo/")
                     (expand-file-name "repo/")))
      (should (equal (magit-toplevel   "repo/subdir/")
                     (expand-file-name "repo/")))
      (should (equal (magit-toplevel   "repo/subdir/subsubdir/")
                     (expand-file-name "repo/")))
      ;; repo-link
      (make-symbolic-link "repo" "repo-link")
      (should (equal (magit-toplevel   "repo-link/")
                     (expand-file-name "repo-link/")))
      (should (equal (magit-toplevel   "repo-link/subdir/")
                     (expand-file-name "repo-link/")))
      (should (equal (magit-toplevel   "repo-link/subdir/subsubdir/")
                     (expand-file-name "repo-link/")))
      ;; *subdir-link
      (make-symbolic-link "repo/subdir"           "subdir-link")
      (make-symbolic-link "repo/subdir/subsubdir" "subsubdir-link")
      (should (equal (magit-toplevel   "subdir-link/")
                     (expand-file-name "repo/")))
      (should (equal (magit-toplevel   "subdir-link/subsubdir/")
                     (expand-file-name "repo/")))
      (should (equal (magit-toplevel   "subsubdir-link")
                     (expand-file-name "repo/")))
      ;; subdir-link-indirect
      (make-symbolic-link "subdir-link" "subdir-link-indirect")
      (should (equal (magit-toplevel   "subdir-link-indirect")
                     (expand-file-name "repo/")))
      ;; wrap/*link
      (magit-git "init" "wrap")
      (make-symbolic-link "../repo"                  "wrap/repo-link")
      (make-symbolic-link "../repo/subdir"           "wrap/subdir-link")
      (make-symbolic-link "../repo/subdir/subsubdir" "wrap/subsubdir-link")
      (should (equal (magit-toplevel   "wrap/repo-link/")
                     (expand-file-name "wrap/repo-link/")))
      (should (equal (magit-toplevel   "wrap/subdir-link")
                     (expand-file-name "repo/")))
      (should (equal (magit-toplevel   "wrap/subsubdir-link")
                     (expand-file-name "repo/")))
      )))

(ert-deftest magit-get-boolean ()
  (magit-with-test-repository
    (magit-git "config" "a.b" "true")
    (should     (magit-get-boolean "a.b"))
    (should     (magit-get-boolean "a" "b"))
    (magit-git "config" "a.b" "false")
    (should-not (magit-get-boolean "a.b"))
    (should-not (magit-get-boolean "a" "b"))))

(ert-deftest magit-get-{current|next}-tag ()
  (magit-with-test-repository
    (magit-git "commit" "-m" "1" "--allow-empty")
    (should (equal (magit-get-current-tag) nil))
    (should (equal (magit-get-next-tag)    nil))
    (magit-git "tag" "1")
    (should (equal (magit-get-current-tag) "1"))
    (should (equal (magit-get-next-tag)    nil))
    (magit-git "commit" "-m" "2" "--allow-empty")
    (magit-git "tag" "2")
    (should (equal (magit-get-current-tag) "2"))
    (should (equal (magit-get-next-tag)    nil))
    (magit-git "commit" "-m" "3" "--allow-empty")
    (should (equal (magit-get-current-tag) "2"))
    (should (equal (magit-get-next-tag)    nil))
    (magit-git "commit" "-m" "4" "--allow-empty")
    (magit-git "tag" "4")
    (magit-git "reset" "HEAD~")
    (should (equal (magit-get-current-tag) "2"))
    (should (equal (magit-get-next-tag)    "4"))))

(ert-deftest magit-list-{|local-|remote-}branch-names ()
  (magit-with-test-repository
    (magit-git "commit" "-m" "init" "--allow-empty")
    (magit-git "update-ref" "refs/remotes/foobar/master" "master")
    (magit-git "update-ref" "refs/remotes/origin/master" "master")
    (should (equal (magit-list-branch-names)
                   (list "master" "foobar/master" "origin/master")))
    (should (equal (magit-list-local-branch-names)
                   (list "master")))
    (should (equal (magit-list-remote-branch-names)
                   (list "foobar/master" "origin/master")))
    (should (equal (magit-list-remote-branch-names "origin")
                   (list "origin/master")))
    (should (equal (magit-list-remote-branch-names "origin" t)
                   (list "master")))))

;;; Status

(defun magit-test-get-section (type info)
  (magit-status-internal default-directory)
  (--first (equal (magit-section-value it) info)
           (magit-section-children
            (magit-get-section `((,type) (status))))))

(ert-deftest magit-status:file-sections ()
  (magit-with-test-repository
    (cl-flet ((modify (file) (with-temp-file file
                               (insert (make-temp-name "content")))))
      (modify "file")
      (modify "file with space")
      (modify "file with äöüéλ")
      (should (magit-test-get-section 'untracked "file"))
      (should (magit-test-get-section 'untracked "file with space"))
      (should (magit-test-get-section 'untracked "file with äöüéλ"))
      (magit-stage-modified t)
      (should (magit-test-get-section 'staged "file"))
      (should (magit-test-get-section 'staged "file with space"))
      (should (magit-test-get-section 'staged "file with äöüéλ"))
      (magit-git "add" ".")
      (modify "file")
      (modify "file with space")
      (modify "file with äöüéλ")
      (should (magit-test-get-section 'unstaged "file"))
      (should (magit-test-get-section 'unstaged "file with space"))
      (should (magit-test-get-section 'unstaged "file with äöüéλ")))))

(ert-deftest magit-status:log-sections ()
  (magit-with-test-repository
    (magit-git "commit" "-m" "common" "--allow-empty")
    (magit-git "commit" "-m" "unpulled" "--allow-empty")
    (magit-git "remote" "add" "origin" "/origin")
    (magit-git "update-ref" "refs/remotes/origin/master" "master")
    (magit-git "branch" "--set-upstream-to=origin/master")
    (magit-git "reset" "--hard" "HEAD~")
    (magit-git "commit" "-m" "unpushed" "--allow-empty")
    (should (magit-test-get-section 'unpulled
                                    (magit-rev-parse "--short" "origin/master")))
    (should (magit-test-get-section 'unpushed
                                    (magit-rev-parse "--short" "master")))))

;;; magit-tests.el ends soon
(provide 'magit-tests)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-tests.el ends here
