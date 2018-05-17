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

;; Also see
;; https://www.gnupg.org/documentation/manuals/gnupg/Automated-signature-checking.html.

;;; Code:

(require 'magit)

(defun magit-verify-commit (id &optional include-invalid)
  ;; TODO Update me.
  "Verify cryptographic signature of commit ID.

Return a `magit-gpg-signature' object (which see) or if the
commit isn't signed nil.

By default, this function returns nil for missing or invalid
signature.  If INCLUDE-INVALID is non-nil, however, a list is
always returned even if the signature is invalid."
  (with-temp-buffer
    (magit-git-insert "verify-commit" "--raw" id)
    (magit-verify--parse-output include-invalid)))

(defun magit-verify-tag (name &optional include-invalid)
  "Verify cryptographic signature of tag NAME.

The return value has the same format as `magit-verify-tag', which
see."
  (with-temp-buffer
    (magit-git-lines "verify-tag" "--raw" name)
    (magit-verify--parse-output include-invalid)))

(defclass magit-gpg-signature ()
  ;; TODO reasonable order and documentation
  ((sig-validity)
   (key-uid)
   (fingerprint)
   (ownertrust)
   (valid)
   (key-expired)
   (sig-expired))
  "
 - KEY-FP is a PGP key fingerprint.
 - KEY-UID is the PGP key's primary UID.
 - VALID is t if and only if the signature is valid.
   Notice that unless INCLUDE-INVALID is non-nil,
   you don't need to verify this value: the function
   would have returned nil if the signature was invalid.
 - OWNERTRUST is either a symbol (`ultimate', `full',
   `unknown', `undefined', `marginal') or nil if the key is NOT
   trusted.
 - KEY-EXPIRED is non-nil if the key or a subkey has expired.
 - SIG-EXPIRED is non-nil if the signature has expired.")

(defun magit-verify--parse-output (&optional include-invalid)
  (let ((obj (magit-gpg-signature)))
    (with-slots (sig-validity key-uid fingerprint ownertrust
                              valid key-expired sig-expired) obj
      (while (not (eobp))
        ;; TODO Are these in the correct order?
        (cond
         ((looking-at "^\\[GNUPG:] \\(\\(?:GOOD\\|BAD\\|EXPKEY\\|EXP\\)\\)SIG \
[[:xdigit:]]+ \\(.+\\)$")
          (cl-assert (not sig-validity) (buffer-string))
          (setf sig-validity (match-string 1))
          (setf key-uid (match-string 2)))
         ((looking-at "^\\[GNUPG:] VALIDSIG \\([[:xdigit:]]+\\) .+$")
          (cl-assert (not fingerprint) (buffer-string))
          (setf fingerprint (match-string 1)))
         ((looking-at "^\\[GNUPG:] TRUST_\\([[:alpha:]]+\\) .+$")
          (cl-assert (not ownertrust) (buffer-string))
          (setf ownertrust (match-string 1))))
        (forward-line))
      (cl-assert (and sig-validity key-uid fingerprint ownertrust)
                 (buffer-string))
      (setf ownertrust (pcase (downcase ownertrust)
                         ("ultimate"  'ultimate)
                         ("fully"     'full)
                         ("undefined" 'undefined)
                         ("marginal"  'marginal)
                         ("never"     nil)
                         (_ (error "Unknown owner trust %s" ownertrust))))
      (setf valid (and (not (equal "BAD" sig-validity))
                       (not (equal "NEVER" ownertrust))))
      (setf key-expired (equal "EXPKEY" sig-validity))
      (setf sig-expired (equal "EXP" sig-validity))
      (and (or valid include-invalid) obj))))

(provide 'magit-verify)
;;; magit-verify.el ends here
