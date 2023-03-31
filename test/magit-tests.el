;;; magit-tests.el --- Tests for Magit  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2008-2023 The Magit Project Contributors

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'ert)
(require 'tramp)
(require 'tramp-sh)

(require 'magit)

(defun magit-test-init-repo (dir &rest args)
  (let ((magit-git-global-arguments
         (nconc (list "-c" "init.defaultBranch=master")
                magit-git-global-arguments)))
    (magit-git "init" args dir)))

(defmacro magit-with-test-directory (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (make-symbol "dir")))
    `(let ((,dir (file-name-as-directory (make-temp-file "magit-" t)))
           (process-environment process-environment)
           (magit-git-global-arguments
            (nconc (list "-c" "protocol.file.allow=always")
                   magit-git-global-arguments)))
       (push "GIT_AUTHOR_NAME=A U Thor" process-environment)
       (push "GIT_AUTHOR_EMAIL=a.u.thor@example.com" process-environment)
       (condition-case err
           (cl-letf (((symbol-function #'message) (lambda (&rest _))))
             (let ((default-directory (file-truename ,dir)))
               ,@body))
         (error (message "Keeping test directory:\n  %s" ,dir)
                (signal (car err) (cdr err))))
       (delete-directory ,dir t))))

(defmacro magit-with-test-repository (&rest body)
  (declare (indent 0) (debug t))
  `(magit-with-test-directory (magit-test-init-repo ".") ,@body))

(defmacro magit-with-bare-test-repository (&rest body)
  (declare (indent 1) (debug t))
  `(magit-with-test-directory (magit-test-init-repo "." "--bare") ,@body))

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
      (magit-test-init-repo "repo")
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

;; FIXME (ert-deftest magit-toplevel:tramp ()
;;   (cl-letf* ((find-file-visit-truename nil)
;;              ;; Override tramp method so that we don't actually
;;              ;; require a functioning `sudo'.
;;              (sudo-method (cdr (assoc "sudo" tramp-methods)))
;;              ((cdr (assq 'tramp-login-program sudo-method))
;;               (list (if (file-executable-p "/bin/sh")
;;                         "/bin/sh"
;;                       shell-file-name)))
;;              ((cdr (assq 'tramp-login-args sudo-method)) nil))
;;     (magit-with-test-directory
;;      (setq default-directory
;;            (concat (format "/sudo:%s@localhost:" (user-login-name))
;;                    default-directory))
;;      (magit-test-init-repo "repo")
;;      (magit-test-magit-toplevel)
;;      (should (equal (magit-toplevel   "repo/.git/")
;;                     (expand-file-name "repo/")))
;;      (should (equal (magit-toplevel   "repo/.git/objects/")
;;                     (expand-file-name "repo/")))
;;      (should (equal (magit-toplevel   "repo-link/.git/")
;;                     (expand-file-name "repo-link/")))
;;      (should (equal (magit-toplevel   "repo-link/.git/objects/")
;;                     (expand-file-name "repo/"))))))

(ert-deftest magit-toplevel:submodule ()
  (let ((find-file-visit-truename nil))
    (magit-with-test-directory
      (magit-test-init-repo "remote")
      (let ((default-directory (expand-file-name "remote/")))
        (magit-git "commit" "-m" "init" "--allow-empty"))
      (magit-test-init-repo "super")
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
  (magit-test-init-repo "wrap")
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
   (magit-test-init-repo "remote")
   (let ((default-directory (expand-file-name "remote/")))
     (magit-git "commit" "-m" "init" "--allow-empty")
     (magit-git "config" "a.b" "remote-value"))
   (magit-test-init-repo "super")
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
               (lambda (_process string) string)))
      (should (string-equal (magit-process-password-prompt
                             nil "Password for 'www.host.com':")
                            "mypasswd\n")))))

(ert-deftest magit-process:password-prompt-observed ()
  (with-temp-buffer
    (cl-letf* ((test-proc (start-process
                           "dummy-proc" (current-buffer)
                           (concat invocation-directory invocation-name)
                           "-Q" "--batch" "--eval" "(read-string \"\")"))
               ((symbol-function 'read-passwd)
                (lambda (_) "mypasswd"))
               (sent-strings nil)
               ((symbol-function 'process-send-string)
                (lambda (_proc string) (push string sent-strings))))
      ;; Don't get stuck when we close the buffer.
      (set-process-query-on-exit-flag test-proc nil)
      ;; Try some example passphrase prompts, reported by users.
      (dolist (prompt '("
Enter passphrase for key '/home/user/.ssh/id_rsa': "
                        ;; Openssh 8.0 sends carriage return.
                        "\
\rEnter passphrase for key '/home/user/.ssh/id_ed25519': "))
        (magit-process-filter test-proc prompt)
        (should (equal (pop sent-strings) "mypasswd\n")))
      (should (null sent-strings)))))

;;; Clone

(ert-deftest magit-clone:--name-to-url-format-defaults ()
  (magit-with-test-repository
   (magit-git "config" "--add" "sourcehut.user" "~shuser")
   (magit-git "config" "--add" "github.user" "ghuser")
   (magit-git "config" "--add" "gitlab.user" "gluser")
   ;; No explicit service
   (should (string-equal (magit-clone--name-to-url "a/b")
                         "git@github.com:a/b.git"))
   (should (string-equal (magit-clone--name-to-url "b")
                         "git@github.com:ghuser/b.git"))
   ;; User in config
   (should (string-equal (magit-clone--name-to-url "gh:b")
                         "git@github.com:ghuser/b.git"))
   (should (string-equal (magit-clone--name-to-url "gl:n")
                         "git@gitlab.com:gluser/n.git"))
   (should (string-equal (magit-clone--name-to-url "sh:l")
                         "git@git.sr.ht:~shuser/l"))
   ;; Explicit user (abbreviated service names)
   (should (string-equal (magit-clone--name-to-url "gh:a/b")
                         "git@github.com:a/b.git"))
   (should (string-equal (magit-clone--name-to-url "gl:t/s")
                         "git@gitlab.com:t/s.git"))
   (should (string-equal (magit-clone--name-to-url "sh:~x/y")
                         "git@git.sr.ht:~x/y"))
   ;; Explicit user (long service names)
   (should (string-equal (magit-clone--name-to-url "github:a1/b1")
                         "git@github.com:a1/b1.git"))
   (should (string-equal (magit-clone--name-to-url "gitlab:t1/s1")
                         "git@gitlab.com:t1/s1.git"))
   (should (string-equal (magit-clone--name-to-url "sourcehut:~x1/y1")
                         "git@git.sr.ht:~x1/y1"))))

(ert-deftest magit-clone:--name-to-url-format-single-string ()
  (let ((magit-clone-url-format "bird@%h:%n.git")
        (magit-clone-name-alist
         '(("\\`\\(?:github:\\|gh:\\)?\\([^:]+\\)\\'" "github.com" "u")
           ("\\`\\(?:gitlab:\\|gl:\\)\\([^:]+\\)\\'" "gitlab.com" "u"))))
    (should (string-equal (magit-clone--name-to-url "gh:a/b")
                          "bird@github.com:a/b.git"))
    (should (string-equal (magit-clone--name-to-url "gl:a/b")
                          "bird@gitlab.com:a/b.git"))
    (should (string-equal (magit-clone--name-to-url "github:c/d")
                          "bird@github.com:c/d.git"))
    (should (string-equal (magit-clone--name-to-url "gitlab:c/d")
                          "bird@gitlab.com:c/d.git"))))

(ert-deftest magit-clone:--name-to-url-format-bad-type-throws-error ()
  (let ((magit-clone-url-format 3))
    (should-error (magit-clone--name-to-url "gh:a/b")
                  :type 'user-error)))

(ert-deftest magit-clone:--name-to-url-format-alist-different-urls-per-hostname ()
  (let ((magit-clone-name-alist
         '(("\\`\\(?:example:\\|ex:\\)\\([^:]+\\)\\'" "git.example.com" "foouser")
           ("\\`\\(?:gh:\\)?\\([^:]+\\)\\'" "github.com" "u")))
        (magit-clone-url-format
         '(("git.example.com" . "cow@%h:~%n")
           (t . "git@%h:%n.git"))))
    (should (string-equal (magit-clone--name-to-url "gh:a/b")
                          "git@github.com:a/b.git"))
    (should (string-equal (magit-clone--name-to-url "ex:a/b")
                          "cow@git.example.com:~a/b"))
    (should (string-equal (magit-clone--name-to-url "example:x/y")
                          "cow@git.example.com:~x/y"))
    (should (string-equal (magit-clone--name-to-url "ex:c")
                          "cow@git.example.com:~foouser/c"))))

(ert-deftest magit-clone:--name-to-url-format-alist-no-fallback-throws-error ()
  (let ((magit-clone-url-format '(("fail.example.com" . "git@%h:~%n"))))
    (should-error (magit-clone--name-to-url "gh:a/b")
                  :type 'user-error)))

;;; Status

(defun magit-test-get-section (list file)
  (magit-status-setup-buffer default-directory)
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

;;; libgit

(ert-deftest magit-in-bare-repo ()
  "Test `magit-bare-repo-p' in a bare repository."
  (magit-with-bare-test-repository
    (should (magit-bare-repo-p))))

(ert-deftest magit-in-non-bare-repo ()
  "Test `magit-bare-repo-p' in a non-bare repository."
  (magit-with-test-repository
    (should-not (magit-bare-repo-p))))

;;; Utils

(ert-deftest magit-utils:add-face-text-property ()
  (let ((str (concat (propertize "ab" 'font-lock-face 'highlight) "cd")))
    (magit--add-face-text-property 0 (length str) 'bold nil str)
    (should (equal (get-text-property 0 'font-lock-face str) '(bold highlight)))
    (should (equal (get-text-property 2 'font-lock-face str) '(bold)))))

(ert-deftest magit-base:ellipsis-default-values ()
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
    (should (equal "…" (magit--ellipsis 'margin)))
    (should (equal "…" (magit--ellipsis))))
  (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
    (should (equal ">" (magit--ellipsis 'margin)))
    (should (equal "..." (magit--ellipsis)))))

(ert-deftest magit-base:ellipsis-customisations-are-respected ()
  (let ((magit-ellipsis '((margin (?· . "!")) (t (?. . ">")))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
      (should (equal "·" (magit--ellipsis 'margin)))
      (should (equal "." (magit--ellipsis))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
      (should (equal "!" (magit--ellipsis 'margin)))
      (should (equal ">" (magit--ellipsis))))))

(ert-deftest magit-base:ellipsis-fancy-nil-defaults-to-universal ()
  (let ((magit-ellipsis '((margin (nil . "...")) (t (nil . "^^^")))))
    (should (equal "..." (magit--ellipsis 'margin)))
    (should (equal "^^^" (magit--ellipsis)))))

(ert-deftest magit-base:ellipsis-legacy-type-allowed ()
  (let ((magit-ellipsis "⋮"))
    (should (equal "⋮" (magit--ellipsis 'margin)))
    (should (equal "⋮" (magit--ellipsis)))))

(ert-deftest magit-base:ellipsis-malformed-customisation-no-default ()
  (let ((magit-ellipsis '((margin (?· . "!")))))
    (should-error (magit--ellipsis)
                  :type 'user-error)))

(ert-deftest magit-base:ellipsis-unknown-use-case-defaults-to-default ()
  (let ((magit-ellipsis '((margin (?· . "!")) (t (?. . ">")))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) t)))
      (should (equal (magit--ellipsis 'foo) (magit--ellipsis))))
    (cl-letf (((symbol-function 'char-displayable-p) (lambda (_) nil)))
      (should (equal (magit--ellipsis 'foo) (magit--ellipsis))))))

;;; magit-tests.el ends soon
(provide 'magit-tests)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-tests.el ends here
