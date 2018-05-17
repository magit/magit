;;; magit-verify.el --- cryptographic signature support for Magit  -*- lexical-binding: t -*-

;; Copyright (C) 2012-2018  The Magit Project Contributors
;;
;; You should have received a copy of the AUTHORS.md file which
;; lists all contributors.  If not, see http://magit.vc/authors.

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; Verifies PGP signatures of commit and tag objects.

;;; Code:

(require 'magit)

(defun magit-verify-commit (id &optional include-invalid)
  "Verify cryptographic signature of commit ID.

Return value is nil if ID is not signed, or a list of the form
\(KEY-FP KEY-UID VALID OWNERTRUST KEY-EXPIRED SIG-EXPIRED), where:

 - KEY-FP is a PGP key fingerprint.
 - KEY-UID is the PGP key's primary UID.
 - VALID is t if and only if the signature is valid.
   Notice that unless INCLUDE-INVALID is non-nil,
   you don't need to verify this value: the function
   would have returned nil if the signature was invalid.
 - OWNERTRUST is either a symbol ('ultimate, 'full,
   'unknown, 'undefined, 'marginal) or nil if the key is NOT
   trusted.
 - KEY-EXPIRED is non-nil if the key or a subkey has expired.
 - SIG-EXPIRED is non-nil if the signature has expired.

By default, this function returns nil for missing or invalid
signature.  If INCLUDE-INVALID is non-nil, however, a list is
always returned even if the signature is invalid.

This function is implemented following GnuPG documentation at:
https://www.gnupg.org/documentation/manuals/gnupg/Automated-signature-checking.html"
  (magit-verify--parse-output (magit-git-lines "verify-commit" "--raw" id)
                              include-invalid))

(defun magit-verify-tag (name &optional include-invalid)
  "Verify cryptographic signature of tag NAME.

The return value has the same format as `magit-verify-tag', which
see."
  (magit-verify--parse-output (magit-git-lines "verify-tag" "--raw" name)
                              include-invalid))

(defun magit-verify--parse-output (lines &optional include-invalid)
  "Internal, DO NOT USE.

The common part of `magit-verify-commit' and
`magit-verify-tag'."

  "Parse LINES as output of git verify-[tag,commit] --raw ...
Returns a possibly empty list of (KEYID OWNERID)."
  (and lines
       (let ((keydata (-non-nil
                       (mapcar
                        (lambda (str)
                          (when (string-match
                                 "^\\[GNUPG:] \\(\\(?:GOOD\\|BAD\\|EXPKEY\\|EXP\\)\\)SIG [[:xdigit:]]+ \\(.+\\)$"
                                 str)
                            (list (match-string 1 str) ; GOOD/BAD/EXP/EXPSIG
                                  (match-string 2 str) ; UID
                                  )))
                        lines)))
             (fingerprint (-non-nil
                           (mapcar
                            (lambda (str)
                              (when (string-match
                                     "^\\[GNUPG:] VALIDSIG \\([[:xdigit:]]+\\) .+$"
                                     str)
                                (match-string 1 str)))
                            lines)))
             (ownertrust (-non-nil
                          (mapcar
                           (lambda (str)
                             (when (string-match
                                    "^\\[GNUPG:] TRUST_\\([[:alpha:]]+\\) .+$"
                                    str)
                               (match-string 1 str)))
                           lines))))
         ;; We should have processed exactly one
         ;; GOODSIG/BADSIG/EXPSIG/EXPKEYSIG line, exactly one VALIDSIG
         ;; line and exactly one TRUST_ line.  If this is not the
         ;; case, panic.
         (unless (= 1 (length keydata))
           (error "Abnormal state 1 in magit-verify--parse-output"))
         (unless (= 1 (length fingerprint))
           (error "Abnormal state 2 in magit-verify--parse-output"))
         (unless (= 1 (length ownertrust))
           (error "Abnormal state 3 in magit-verify--parse-output"))
         (let* ((keydata (car keydata))
                (fingerprint (car fingerprint))
                (ownertrust (car ownertrust))
                (sig-validity (nth 0 keydata))
                (key-uid (nth 1 keydata))
                (valid (and (not (equal "BAD" sig-validity))
                            (not (equal "NEVER" ownertrust)))))
           (and (or valid include-invalid)
                (list fingerprint key-uid valid
                      (pcase (downcase ownertrust)
                        ("ultimate"  'ultimate)
                        ("fully"     'full)
                        ("undefined" 'undefined)
                        ("marginal"  'marginal)
                        ("never"     nil)
                        (_ (error "Unknown owner trust %s" ownertrust)))
                      (equal "EXPKEY" sig-validity)
                      (equal "EXP" sig-validity)))))))

(provide 'magit-verify)
;;; magit-verify.el ends here
