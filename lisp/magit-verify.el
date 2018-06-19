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

;; Git delegates must of the verification job to GnuPG.  This module
;; is essentially a parser for a subset of the GnuPG's --status-fd
;; formal output syntax, as documented in /doc/DETAILS in GnuPG
;; tarballs.
;;
;;
;; https://www.gnupg.org/documentation/manuals/gnupg/Automated-signature-checking.html.

;;; Code:

(require 'parse-time)
(require 'magit)

;;; High level interface

(defun magit-verify-commit (id &optional
                               ignore-key-expiration
                               ignore-sig-expiration
                               ignore-key-validity
                               ignore-revocation)
  "Verify commit ID.

Return a (possibly empty) list of valid signature's fingerprints.
Due to Git's current limitations, it can be assumed that this
list contains at most one element.

If IGNORE-KEY-EXPIRATION is non-nil, will accept signatures
from expired keys.

If IGNORE-SIG-EXPIRATION is non-nil, will accept expired
signatures.

If IGNORE-KEY-VALIDITY is non-nil, will accept signatures from
keys with ownertrust=never.  Enable if you know what you're
doing.

If IGNORE-REVOCATION is non-nil, will accept signatures from
revoked keys.  Enable if you know what you're doing."
  (-keep (lambda (sig)
           (oref (magit-verify-signature
                  sig
                  ignore-key-expiration
                  ignore-sig-expiration
                  ignore-key-validity
                  ignore-revocation)
                 key-fingerprint))
         (magit-read-commit-signatures id)))

(defun magit-verify-tag (name &optional
                              ignore-key-expiration
                              ignore-sig-expiration
                              ignore-key-validity
                              ignore-revocation)
  "Verify tag NAME.

NAME must be a tag *name*, and cannot be a SHA-1 identifier.
This is because this function actually verifies that the name
stored in the tag object matches the user-provided name, which
git verify-tag doesn't do.  If names don't match, an error will
be signaled.

IGNORE-KEY-EXPIRATION, IGNORE-SIG-EXPIRATION, IGNORE-KEY-VALIDITY
and IGNORE-REVOCATION have the same meaning as in
`magit-verify-commit', which see."
  (-keep (lambda (sig)
           (oref (magit-verify-signature
                  sig
                  ignore-key-expiration
                  ignore-sig-expiration
                  ignore-key-validity
                  ignore-revocation)
                 key-fingerprint))
         (magit-read-tag-signatures name)))

;;; Lower level interface

(defun magit-read-commit-signatures (id)
  "Return PGP signatures of commit ID, regardless of their validity.

In most cases, you'll want to use `magit-verify-commit' instead
of this function, which provides a simpler and safer interface to
the same functionality.

Return a possibly empty list of `magit-pgp-signature' objects, which see."
  (with-temp-buffer
    (magit-process-file magit-git-executable nil t nil
                        "verify-commit" "--raw"
                        (shell-quote-argument id))
    (magit-verify--parse-output
     (split-string (buffer-string) "\n" t))))

(defun magit-read-tag-signatures (name)
  "Return PGP signatures of tag NAME, regardless of their validity.

In most cases, you'll want to use `magit-verify-tag' instead of
this function, which provides a simpler and safer interface to
the same functionality.

NAME must be a tag *name*, and cannot be a SHA-1 identifier.
This is because this function actually verifies that the name
stored in the tag object matches the user-provided name, which
git verify-tag doesn't do.  If names don't match, an error will
be signaled.

Return a possibly empty list of `magit-pgp-signature' objects, which see."
  (let ((realname (magit-git-string "verify-tag" "--format" "%(tag)" name)))
    (unless (string= realname name)
      (error "The tag object referred to by `%s' is actually named `%s'.  \
Maybe you've given `magit-verify-tag' a hash instead of a name, \
or maybe something fishy is going on" name realname)))
  (with-temp-buffer
    (magit-process-file magit-git-executable nil t nil
                        "verify-tag" "--raw"
                        (shell-quote-argument name))
    (magit-verify--parse-output
     (split-string (buffer-string) "\n" t))))

(defun magit-verify-signature (sig &optional
                                   ignore-key-expiration
                                   ignore-sig-expiration
                                   ignore-key-validity
                                   ignore-revocation)
  "Verify signature SIG, a `magit-pgp-signature' object.

If SIG is a valid signature (non-expired, from a non-expired and
non-revoked key, with a non-never ownertrust), return it
unmodified, otherwise return nil."
  (and (not (oref sig error))
       (oref sig sig-validity)
       (or ignore-key-expiration (not (oref sig key-expired)))
       (or ignore-sig-expiration (not (oref sig sig-expired)))
       (or ignore-key-validity   (not (eq 'never (oref sig key-validity))))
       (or ignore-revocation     (not (oref sig key-revoked)))
       sig))

;;; Signature class

(defclass magit-pgp-signature ()
  ((error
    :initform nil
    :type boolean
    :doc "The error state after verification.  See class
    documentation for more details.")
   (sig-validity
    :initform nil
    :type boolean
    :doc "Whether the signature is valid.  This is non-nil if the
    signature could successfully be verified, but doesn't
    guarantee that the signing key is trusted.")
   (sig-creation-date
    :initform nil
    :doc "The date this signature was created.")
   (key-fingerprint
    :initform nil
    :type (or null string)
    :doc "The fingerprint of the signing key.")
   (key-uid
    :initform nil
    :type (or null string)
    :doc "The primary UID of the signing key (e-mail address).")
   (key-name
    :initform nil
    :type (or null string)
    :doc "The name of the primary UID of the signing key.")
   (key-comment
    :initform nil
    :type (or null string)
    :doc "The comment field of the primary UID of the signing key.")
   (key-validity
    :initform nil
    :type (or null symbol)
    :doc "The key's key-validity, as either a symbol (`ultimate',
   `full', `unknown', `undefined', `marginal', `never') or nil if
   unspecified.")
   (key-revoked
    :initform nil
    :type boolean
    :doc "Whether this key is known to be revoked.")
   (key-expiration-date
    :initform nil
    :doc "The key's expiration date as a number of seconds from epoch.")
   (key-expired
    :initform nil
    :type boolean
    :doc "Whether the key has expired.  Notice that an expired
    key doesn't imply that the signature is invalid.")
   (sig-expiration-date
    :initform nil
    :doc "The signature's expiration date as a number of seconds
    from epoch.")
   (sig-expired
    :initform nil
    :type boolean
    :doc "Whether the signature has expired.  Notice that an
    expired signature doesn't imply that the signature is
    invalid."))
  "A PGP signature.

Notice that the presence of such an object doesn't guarantee that
a signature is valid or should be trusted.  Magit provides a
simple verification command, `magit-verify-signature', which you
probably should use before you do anything with an object of this
class.

By the very design of GnuPG, the semantics of this class are
complex.  To manually verify a signature (don't), the following
algorithm should be followed.

Short, minimal, version:

 (and
     (not (oref sig error))                        ; #1
     (oref sig sig-validity)                       ; #2
     (not (oref sig key-revoked))                  ; #3
     (not (equal 'never (oref sig key-validity)))) ; #4

 1. Read the value of the `error' field.

    If nil => Verification was successful, GOTO 2.

    If 'no-pubkey => The public key wasn't in the keyring.

      Fields `key-fingerprint' and `key-uid' indicate the
      identity of the signing key.  Other fields are unusable.

      Signature is INVALID, END.

    If 'unknown-algorithm => GnuPG doesn't support this signature
      algorithm.

      All fields are unusable.

      Signature is INVALID, END.

    If 'no-data => No signature data were found.

      All fields are unusable.

      Signature is INVALID, END.

    If other non-nil value => Unknown error, possible data
                              corruption?

      All fields are unusable.

      Signature is INVALID, END.

 2. Read the value of the `sig-validity' field.

    If t => verification was successful.  GOTO 3.

    If nil => signature was invalid.

      This may indicate that signed data have been tampered with,
      that some corruption happened or a user error.

      Only fields `key-fingerprint', `key-uid', `key-name' and
      `key-comment' are usable.

      Signature is INVALID, END.

 3. Read the value of the `key-revoked' field.

    If nil => Signing key is not revoked, goto 4.

    If  => Signing key was revoked, signature is INVALID, END.

 4. [Optional] Read the value of the `key-validity' field and handle
    accordingly.

    How exactly to manipulate this value is policy-dependant.
    What follows is NOT a recommendation, but the bare minimum
    level of verification:

    If 'never => Signature is INVALID, END.

    Otherwise => goto 5.

 5. Consider the values of `key-expired' and `sig-expired' and
    handle them according to your policy.  It is often
    correct to emit a warning when a key or signature has
    expired without rejecting the signature.")

;;; Internals

(defun magit-verify--extract-uid (line sig)
  ;; Read name and comment from a "*SIG" LINE and `oset' them in SIG.
  (unless (string-match
           ;; "^[A-Z]+SIG  \\(.*\\)\\(?: (<\\(.+\\))\\) <\\(.*\\)$>"
           "^[A-Z]+SIG [[:xdigit:]]+ \\(.*?\\)\\(?: (\\(.*\\))\\)? <\\(.*\\)>$"
           line)
    (error "Error parsing uid from %s" line))
  (oset sig key-name (match-string 1 line))
  (oset sig key-comment (match-string 2 line))
  (oset sig key-uid (match-string 3 line)))

(defun magit-verify--parse-date (str)
  ;; Read STR into a number.
  ;;
  ;; If STR matches ^[[:digit:]]+$, it's treated as a unix timestmap,
  ;; otherwise as an RFC 8601 string.  If nothing works, raise an error."
  (and (not (string= str "0"))
       (or (and (string-match "\\`[[:digit:]]+\\'" str)
                (seconds-to-time (string-to-number str)))
           ;; @FIXME If `parse-iso8601-time-string' fails, it will neither
           ;; return an invalid value nor error, but instead delegate to
           ;; the weird `parse-time-string' which will associate a value to
           ;; every possible input.  This is not what we want, and we need
           ;; a way to error if we can't parse the date.
           (parse-iso8601-time-string str))))

(defun magit-verify--parse-output (lines)
  ;; Parse output from git verify-[tag|commit] --raw (which is
  ;; actually the output of gpg --verify --status-fd) and return a
  ;; possibly empty list of `magit-pgp-signature' object.
  (let ((lines (cl-mapcan (lambda (line)
                            (and (string-prefix-p "[GNUPG:] " line)
                                 (list (substring line 9))))
                          lines))
        (sigs nil)
        (sig nil))
    (message "%s" lines)
    (dolist (line lines nil)
      (let ((fields (split-string line)))
        (pcase (car fields)
        ;; Start reading a new signature
          ("NEWSIG"
           (setq sigs (cons (setq sig (magit-pgp-signature)) sigs))
           (oset sig key-uid (nth 1 fields)))
          ("GOODSIG"
           (oset sig sig-validity t)
           (oset sig sig-expired nil)
           (oset sig key-expired nil)
           (magit-verify--extract-uid line sig))
          ("EXPSIG"
           (oset sig sig-validity t)
           (oset sig sig-expired t)
           (magit-verify--extract-uid line sig))
          ("EXPKEYSIG"
           (oset sig sig-validity t)
           (oset sig key-expired t)
           (magit-verify--extract-uid line sig))
          ("REVKEYSIG"
           (oset sig sig-validity t)
           (oset sig key-revoked t)
           (magit-verify--extract-uid line sig))
          ("ERRSIG"
           (oset sig sig-validity nil)
           (oset sig error (pcase (number-to-string (nth 6 fields))
                             (4 'unknown-algorithm)
                             (9 'no-pubkey)
                             (_ t))))
          ("BADSIG"
           (oset sig sig-validity nil)
           (magit-verify--extract-uid line sig))
          ("VALIDSIG"
           (oset sig key-fingerprint (nth 1 fields))
           (oset sig sig-creation-date (magit-verify--parse-date (nth 3 fields)))
           (oset sig sig-expiration-date (magit-verify--parse-date (nth 4 fields))))
          ("KEYEXPIRED"      (oset sig key-expired t))
          ("TRUST_UNDEFINED" (oset sig key-validity 'undefined))
          ("TRUST_NEVER"     (oset sig key-validity 'never))
          ("TRUST_MARGINAL"  (oset sig key-validity 'marginal))
          ("TRUST_FULLY"     (oset sig key-validity 'fully))
          ("TRUST_ULTIMATE"  (oset sig key-validity 'ultimate))
          ("NO_PUBKEY"       (oset sig error 'no-pubkey))

          ;; Arguments we know we can ignore
          ("SIG_ID")
          ("VERIFICATION_COMPLIANCE_MODE")
          ("KEY_CONSIDERED")

          ;; Error states We may have received more specific
          ;; information earlier, we don't want to overwrite them.
          ("NODATA"
           (unless (member (string-to-number (nth 1 fields)) '(1 2))
             (error "PGP returned abnormal state NODATA %s" (nth 1 fields))))
          ("FAILURE"
           (unless (or (not sig)
                       (oref sig error))
             (oset sig error t)))

          ;; The doc in doc/DETAILS states that: "an application should
          ;; always be willing to ignore unknown keywords that may be
          ;; emitted by future versions of GnuPG."  Yet we really
          ;; shouldn't ignore them *silently*.
          (_ (warn "Unexpected input line in `magit-verify--parse-output': %s" line))
        )))
    sigs))

(provide 'magit-verify)
;;; magit-verify.el ends here
