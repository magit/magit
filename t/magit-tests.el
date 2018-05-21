;;; magit-tests.el --- tests for Magit

;; Copyright (C) 2011-2018  The Magit Project Contributors
;;
;; License: GPLv3

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ert)
(require 'tramp)
(require 'tramp-sh)

(require 'magit)

(defmacro magit-with-test-directory (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "magit-" t)))
           (process-environment process-environment))
       (push "GIT_AUTHOR_NAME=A U Thor" process-environment)
       (push "GIT_AUTHOR_EMAIL=a.u.thor@example.com" process-environment)
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

(ert-deftest magit--with-safe-default-directory ()
  (magit-with-test-directory
   (let ((find-file-visit-truename nil))
     (should (equal (magit-toplevel "repo/")
                    (magit-toplevel (expand-file-name "repo/"))))
     (should (equal (magit-toplevel "repo")
                    (magit-toplevel (expand-file-name "repo/")))))))

(ert-deftest magit-toplevel:basic ()
  (let ((find-file-visit-truename nil))
    (magit-with-test-directory
     (magit-git "init" "repo")
     (magit-test-magit-toplevel)
     (should (equal (magit-toplevel   "repo/.git/")
                    (expand-file-name "repo/")))
     (should (equal (magit-toplevel   "repo/.git/objects/")
                    (expand-file-name "repo/")))
     (should (equal (magit-toplevel   "repo-link/.git/")
                    (expand-file-name "repo-link/")))
     (should (equal (magit-toplevel   "repo-link/.git/objects/")
                    ;; We could theoretically return "repo-link/"
                    ;; here by going up until `--git-dir' gives us
                    ;; "." .  But that would be a bit risky and Magit
                    ;; never goes there anyway, so it's not worth it.
                    ;; But in the doc-string we say we cannot do it.
                    (expand-file-name "repo/"))))))

(ert-deftest magit-toplevel:tramp ()
  (cl-letf* ((find-file-visit-truename nil)
             ;; Override tramp method so that we don't actually
             ;; require a functioning `sudo'.
             (sudo-method (cdr (assoc "sudo" tramp-methods)))
             ((cdr (assq 'tramp-login-program sudo-method))
              (list shell-file-name))
             ((cdr (assq 'tramp-login-args sudo-method)) nil))
    (magit-with-test-directory
     (setq default-directory
           (concat (format "/sudo:%s@localhost:" (user-login-name))
                   default-directory))
     (magit-git "init" "repo")
     (magit-test-magit-toplevel)
     (should (equal (magit-toplevel   "repo/.git/")
                    (expand-file-name "repo/")))
     (should (equal (magit-toplevel   "repo/.git/objects/")
                    (expand-file-name "repo/")))
     (should (equal (magit-toplevel   "repo-link/.git/")
                    (expand-file-name "repo-link/")))
     (should (equal (magit-toplevel   "repo-link/.git/objects/")
                    (expand-file-name "repo/"))))))

(ert-deftest magit-toplevel:submodule ()
  (let ((find-file-visit-truename nil))
    (magit-with-test-directory
     (magit-git "init" "remote")
     (let ((default-directory (expand-file-name "remote/")))
       (magit-git "commit" "-m" "init" "--allow-empty"))
     (magit-git "init" "super")
     (setq default-directory (expand-file-name "super/"))
     (magit-git "submodule" "add" "../remote" "repo/")
     (magit-test-magit-toplevel)
     (should (equal (magit-toplevel   ".git/modules/repo/")
                    (expand-file-name "repo/")))
     (should (equal (magit-toplevel   ".git/modules/repo/objects/")
                    (expand-file-name "repo/"))))))

(defun magit-test-magit-toplevel ()
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
                 (expand-file-name "repo/"))))

(defun magit-test-magit-get ()
  (should (equal (magit-get-all "a.b") '("val1" "val2")))
  (should (equal (magit-get "a.b") "val2"))
  (let ((default-directory (expand-file-name "../remote/")))
    (should (equal (magit-get "a.b") "remote-value")))
  (should (equal (magit-get "CAM.El.Case.VAR") "value"))
  (should (equal (magit-get "a.b2") "line1\nline2")))

(ert-deftest magit-get ()
  (magit-with-test-directory
   (magit-git "init" "remote")
   (let ((default-directory (expand-file-name "remote/")))
     (magit-git "commit" "-m" "init" "--allow-empty")
     (magit-git "config" "a.b" "remote-value"))
   (magit-git "init" "super")
   (setq default-directory (expand-file-name "super/"))
   ;; Some tricky cases:
   ;; Multiple config values.
   (magit-git "config" "a.b" "val1")
   (magit-git "config" "--add" "a.b" "val2")
   ;; CamelCase variable names.
   (magit-git "config" "Cam.El.Case.Var" "value")
   ;; Values with newlines.
   (magit-git "config" "a.b2" "line1\nline2")
   ;; Config variables in submodules.
   (magit-git "submodule" "add" "../remote" "repo/")

   (magit-test-magit-get)
   (let ((magit--refresh-cache (list (cons 0 0))))
     (magit-test-magit-get))))

(ert-deftest magit-get-boolean ()
  (magit-with-test-repository
   (magit-git "config" "a.b" "true")
   (should     (magit-get-boolean "a.b"))
   (should     (magit-get-boolean "a" "b"))
   (magit-git "config" "a.b" "false")
   (should-not (magit-get-boolean "a.b"))
   (should-not (magit-get-boolean "a" "b"))
   ;; Multiple values, last one wins.
   (magit-git "config" "--add" "a.b" "true")
   (should     (magit-get-boolean "a.b"))
   (let ((magit--refresh-cache (list (cons 0 0))))
     (should    (magit-get-boolean "a.b")))))

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

(ert-deftest magit-process:match-prompt-nil-when-no-match ()
  (should (null (magit-process-match-prompt '("^foo: ?$") "bar: "))))

(ert-deftest magit-process:match-prompt-non-nil-when-match ()
  (should (magit-process-match-prompt '("^foo: ?$") "foo: ")))

(ert-deftest magit-process:match-prompt-match-non-first-prompt ()
  (should (magit-process-match-prompt '("^bar: ?$ " "^foo: ?$") "foo: ")))

(ert-deftest magit-process:match-prompt-suffixes-prompt ()
  (let ((prompts '("^foo: ?$")))
    (should (equal (magit-process-match-prompt prompts "foo:")  "foo: "))
    (should (equal (magit-process-match-prompt prompts "foo: ") "foo: "))))

(ert-deftest magit-process:match-prompt-preserves-match-group ()
  (let* ((prompts '("^foo '\\(?99:.*\\)': ?$"))
         (prompt (magit-process-match-prompt prompts "foo 'bar':")))
    (should (equal prompt "foo 'bar': "))
    (should (equal (match-string 99 "foo 'bar':") "bar"))))

(ert-deftest magit-process:password-prompt ()
  (let ((magit-process-find-password-functions
         (list (lambda (host) (when (string= host "www.host.com") "mypasswd")))))
    (cl-letf (((symbol-function 'process-send-string)
               (lambda (process string) string)))
      (should (string-equal (magit-process-password-prompt
                             nil "Password for 'www.host.com':")
                            "mypasswd\n")))))

;;; Status

(defun magit-test-get-section (list file)
  (magit-status-internal default-directory)
  (--first (equal (oref it value) file)
           (oref (magit-get-section `(,list (status)))
                 children)))

(ert-deftest magit-status:file-sections ()
  (magit-with-test-repository
   (cl-flet ((modify (file) (with-temp-file file
                              (insert (make-temp-name "content")))))
     (modify "file")
     (modify "file with space")
     (modify "file with äöüéλ")
     (should (magit-test-get-section '(untracked) "file"))
     (should (magit-test-get-section '(untracked) "file with space"))
     (should (magit-test-get-section '(untracked) "file with äöüéλ"))
     (magit-stage-modified t)
     (should (magit-test-get-section '(staged) "file"))
     (should (magit-test-get-section '(staged) "file with space"))
     (should (magit-test-get-section '(staged) "file with äöüéλ"))
     (magit-git "add" ".")
     (modify "file")
     (modify "file with space")
     (modify "file with äöüéλ")
     (should (magit-test-get-section '(unstaged) "file"))
     (should (magit-test-get-section '(unstaged) "file with space"))
     (should (magit-test-get-section '(unstaged) "file with äöüéλ")))))

(ert-deftest magit-status:log-sections ()
  (magit-with-test-repository
   (magit-git "commit" "-m" "common" "--allow-empty")
   (magit-git "commit" "-m" "unpulled" "--allow-empty")
   (magit-git "remote" "add" "origin" "/origin")
   (magit-git "update-ref" "refs/remotes/origin/master" "master")
   (magit-git "branch" "--set-upstream-to=origin/master")
   (magit-git "reset" "--hard" "HEAD~")
   (magit-git "commit" "-m" "unpushed" "--allow-empty")
   (should (magit-test-get-section
            '(unpulled . "..@{upstream}")
            (magit-rev-parse "--short" "origin/master")))
   (should (magit-test-get-section
            '(unpushed . "@{upstream}..")
            (magit-rev-parse "--short" "master")))))

;;; PGP signatures

(ert-deftest magit-verify:parse-output ()
  "Test the output parser for gpg --status-fd format used by git
verify-[tag|commit] --raw"

  ;; No data
  (should-not (magit-verify--parse-output nil))

  ;; Corrupted signature
  (should-error (magit-verify--parse-output
                 (list
                  "[GNUPG:] NODATA 4"
                  "[GNUPG:] FAILURE gpg-exit 33554433")))

  ;; Good signature, not expired, key not expired
  (let ((sig (magit-verify--parse-output
              (list
               "[GNUPG:] NEWSIG key@badsig.example.com"
               "[GNUPG:] KEY_CONSIDERED 75741BBB0494201426D349C47A2E132C20E0EAF8 0"
               "[GNUPG:] SIG_ID Ak/49yzkCJ7W0kXLieg5q03MuMY 2018-05-19 1526721941"
               "[GNUPG:] KEY_CONSIDERED 75741BBB0494201426D349C47A2E132C20E0EAF8 0"
               "[GNUPG:] GOODSIG 7A2E132C20E0EAF8 Normal Badsig <key@badsig.example.com>"
               "[GNUPG:] VALIDSIG 75741BBB0494201426D349C47A2E132C20E0EAF8 2018-05-19 1526721941 0 4 0 1 8 01 75741BBB0494201426D349C47A2E132C20E0EAF8"
               "[GNUPG:] TRUST_UNDEFINED 0 pgp"))))
    (should (= 1 (length sig)))
    (setq sig (car sig))
    (should (oref sig sig-validity))
    (should (string= (oref sig key-fingerprint) "75741BBB0494201426D349C47A2E132C20E0EAF8"))
    (should (string= (oref sig key-uid) "key@badsig.example.com"))
    (should (string= (oref sig key-name) "Normal Badsig"))
    (should-not (oref sig key-comment))
    (should (equal (oref sig key-validity) 'undefined))
    (should-not (oref sig key-expired))
    (should-not (oref sig sig-expired)))

  ;; Good EXPIRED signature, key not expired.
  (let ((sig (magit-verify--parse-output
              (list
               "[GNUPG:] NEWSIG key@badsig.example.com"
               "[GNUPG:] KEY_CONSIDERED 75741BBB0494201426D349C47A2E132C20E0EAF8 0"
               "[GNUPG:] SIG_ID iIATwONP94YrWiQVZb2qegtCJUs 2010-01-01 1262305000"
               "[GNUPG:] KEY_CONSIDERED 75741BBB0494201426D349C47A2E132C20E0EAF8 0"
               "[GNUPG:] EXPSIG 7A2E132C20E0EAF8 Normal Badsig <key@badsig.example.com>"
               "[GNUPG:] VALIDSIG 75741BBB0494201426D349C47A2E132C20E0EAF8 2010-01-01 1262305000 1262391400 4 0 1 8 01 75741BBB0494201426D349C47A2E132C20E0EAF8"
               "[GNUPG:] TRUST_UNDEFINED 0 pgp"))))
    (should (= 1 (length sig)))
    (setq sig (car sig))
    (should (oref sig sig-validity))
    (should (string= (oref sig key-fingerprint) "75741BBB0494201426D349C47A2E132C20E0EAF8"))
    (should (string= (oref sig key-uid) "key@badsig.example.com"))
    (should (string= (oref sig key-name) "Normal Badsig"))
    (should-not (oref sig key-comment))
    (should (equal (oref sig key-validity) 'undefined))
    (should-not (oref sig key-expired))
    (should (oref sig sig-expired)))

  ;; Good signature, key EXPIRED
  (let ((sig (magit-verify--parse-output
              (list
               "[GNUPG:] NEWSIG expired@badsig.example.com"
               "[GNUPG:] KEYEXPIRED 1325376017"
               "[GNUPG:] KEY_CONSIDERED 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 0"
               "[GNUPG:] KEYEXPIRED 1325376017"
               "[GNUPG:] SIG_ID T5GuioUtYjuC+u/qR8ROkaapQBc 2010-01-01 1262305000"
               "[GNUPG:] KEYEXPIRED 1325376017"
               "[GNUPG:] KEY_CONSIDERED 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 0"
               "[GNUPG:] EXPKEYSIG FB2B6F167A81BDC5 Expired Badsig <expired@badsig.example.com>"
               "[GNUPG:] VALIDSIG 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 2010-01-01 1262305000 0 4 0 1 8 01 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5"
               "[GNUPG:] VERIFICATION_COMPLIANCE_MODE 23"))))
    (should (= 1 (length sig)))
    (setq sig (car sig))
    (should (oref sig sig-validity))
    (should (string= (oref sig key-fingerprint) "04F19E43F5FD42CB0264605EFB2B6F167A81BDC5"))
    (should (string= (oref sig key-uid) "expired@badsig.example.com"))
    (should (string= (oref sig key-name) "Expired Badsig"))
    (should-not (oref sig key-comment))
    (should-not (oref sig key-validity))
    (should (oref sig key-expired))
    (should-not (oref sig sig-expired)))

  ;; Good EXPIRED signature by EXPIRED key
  (let ((sig (magit-verify--parse-output
              (list
               "[GNUPG:] NEWSIG expired@badsig.example.com"
               "[GNUPG:] KEYEXPIRED 1325376017"
               "[GNUPG:] KEY_CONSIDERED 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 0"
               "[GNUPG:] KEYEXPIRED 1325376017"
               "[GNUPG:] SIG_ID A55c0YZw9Q1luZtywPlBz+eb0Ew 2010-01-01 1262305000"
               "[GNUPG:] KEYEXPIRED 1325376017"
               "[GNUPG:] KEY_CONSIDERED 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 0"
               "[GNUPG:] EXPSIG FB2B6F167A81BDC5 Expired Badsig <expired@badsig.example.com>"
               "[GNUPG:] VALIDSIG 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 2010-01-01 1262305000 1262391400 4 0 1 8 01 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5"
               "[GNUPG:] VERIFICATION_COMPLIANCE_MODE 23"))))
    (should (= 1 (length sig)))
    (setq sig (car sig))
    (should (oref sig sig-validity))
    (should (string= (oref sig key-fingerprint) "04F19E43F5FD42CB0264605EFB2B6F167A81BDC5"))
    (should (string= (oref sig key-uid) "expired@badsig.example.com"))
    (should (string= (oref sig key-name) "Expired Badsig"))
    (should-not (oref sig key-comment))
    (should-not (oref sig key-validity))
    (should (oref sig key-expired))
    (should (oref sig sig-expired))))

;; Bad signature from valid key

;;  Invalid_KEXP_SEXP.txt
"[GNUPG:] NEWSIG expired@badsig.example.com"
"[GNUPG:] KEYEXPIRED 1325376017"
"[GNUPG:] KEY_CONSIDERED 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 0"
"[GNUPG:] KEYEXPIRED 1325376017"
"[GNUPG:] KEYEXPIRED 1325376017"
"[GNUPG:] KEY_CONSIDERED 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 0"
"[GNUPG:] BADSIG FB2B6F167A81BDC5 Expired Badsig <expired@badsig.example.com>"
"[GNUPG:] VERIFICATION_COMPLIANCE_MODE 23"
;; Invalid_KEXP_SOK.txt
"[GNUPG:] NEWSIG expired@badsig.example.com"
"[GNUPG:] KEYEXPIRED 1325376017"
"[GNUPG:] KEY_CONSIDERED 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 0"
"[GNUPG:] KEYEXPIRED 1325376017"
"[GNUPG:] KEYEXPIRED 1325376017"
"[GNUPG:] KEY_CONSIDERED 04F19E43F5FD42CB0264605EFB2B6F167A81BDC5 0"
"[GNUPG:] BADSIG FB2B6F167A81BDC5 Expired Badsig <expired@badsig.example.com>"
"[GNUPG:] VERIFICATION_COMPLIANCE_MODE 23"
;; Invalid_KOK_SEXP.txt
"[GNUPG:] NEWSIG key@badsig.example.com"
"[GNUPG:] KEY_CONSIDERED 75741BBB0494201426D349C47A2E132C20E0EAF8 0"
"[GNUPG:] KEY_CONSIDERED 75741BBB0494201426D349C47A2E132C20E0EAF8 0"
"[GNUPG:] BADSIG 7A2E132C20E0EAF8 Normal Badsig <key@badsig.example.com>"
;; Invalid_kok_sok.txt
"[GNUPG:] NEWSIG key@badsig.example.com"
"[GNUPG:] KEY_CONSIDERED 75741BBB0494201426D349C47A2E132C20E0EAF8 0"
"[GNUPG:] KEY_CONSIDERED 75741BBB0494201426D349C47A2E132C20E0EAF8 0"
"[GNUPG:] BADSIG 7A2E132C20E0EAF8 Normal Badsig <key@badsig.example.com>"
;; Valid_KEXP_SOK.txt
;; Valid_KOK_SEXP.txt
;; Valid_KOK_SOK.txt



;;; magit-tests.el ends soon
(provide 'magit-tests)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-tests.el ends here
