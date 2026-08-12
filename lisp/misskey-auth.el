;;; misskey-auth.el --- Misskey browser authorization -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Acquire a scoped Misskey API token through MiAuth and persist it through
;; auth-source.  The authorization session is short-lived and never enters an
;; authenticated request body or process argument.

;;; Code:

(require 'auth-source)
(require 'browse-url)
(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'misskey-core)
(require 'misskey-http)

(defconst misskey-auth--permissions '("read:account" "write:notes")
  "Misskey permissions requested by the supported public workflows.")

(defconst misskey-auth--session-regexp
  "\\`[[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-4[[:xdigit:]]\\{3\\}-[89abAB][[:xdigit:]]\\{3\\}-[[:xdigit:]]\\{12\\}\\'"
  "Exact regular expression for a random version 4 UUID.")

(defun misskey-auth--session-id ()
  "Return a fresh random version 4 UUID for one MiAuth session."
  (let ((program (executable-find "uuidgen")))
    (unless program
      (error "The uuidgen executable is required for MiAuth authorization"))
    (with-temp-buffer
      (let ((status (call-process program nil t nil)))
        (unless (and (integerp status) (zerop status))
          (error "The uuidgen executable failed")))
      (let ((session (downcase (string-trim (buffer-string)))))
        (unless (string-match-p misskey-auth--session-regexp session)
          (error "The uuidgen executable did not return a version 4 UUID"))
        session))))

(defun misskey-auth--authorization-url (account session)
  "Return ACCOUNT's MiAuth authorization URL for SESSION."
  (unless (and (stringp session)
               (string-match-p misskey-auth--session-regexp session))
    (error "Invalid MiAuth session ID"))
  (concat
   (misskey--account-origin account) "/miauth/" session "?"
   (url-build-query-string
    `(("name" "misskey.el")
      ("permission" ,(string-join misskey-auth--permissions ","))))))

(defun misskey-auth--request-token (account)
  "Authorize ACCOUNT in a browser and return its new API token."
  (let* ((session (misskey-auth--session-id))
         (authorization-url
          (misskey-auth--authorization-url account session)))
    (browse-url authorization-url)
    (read-string
     "Approve misskey.el in your browser, then press RET here: ")
    (let* ((payload
            (misskey-http--public-read-sync
             (format "miauth/%s/check" session)
             (make-hash-table :test #'equal)
             account))
           (token (and (consp payload) (alist-get 'token payload))))
      (unless (and (eq (alist-get 'ok payload) t)
                   (stringp token)
                   (not (string-empty-p token)))
        (user-error "The Misskey instance did not approve this authorization"))
      token)))

(defun misskey-auth--redact-token (message token)
  "Return MESSAGE with TOKEN replaced by a redaction marker."
  (if (and (stringp token) (not (string-empty-p token)))
      (string-replace token "[REDACTED]" message)
    message))

(defun misskey-auth--storage-source ()
  "Return the first configured encrypted auth-source file."
  (or (cl-find-if
       (lambda (source)
         (and (stringp source)
              (equal (downcase (or (file-name-extension source) ""))
                     "gpg")))
       auth-sources)
      (user-error
       "Add an encrypted .gpg file to auth-sources before authorizing Misskey")))

(defun misskey-auth--store-token (account token)
  "Persist ACCOUNT's TOKEN through auth-source and return TOKEN."
  (let* ((storage-source (misskey-auth--storage-source))
         (auth-sources (list storage-source))
         (auth-source-ignore-non-existing-file nil)
         (spec (misskey--auth-source-spec account))
         (lookup-spec
          (append spec '(:require (:secret :port) :max 1)))
         (auth-source-creation-prompts
          '((secret . "%u Misskey API token: "))))
    (condition-case err
        (let* ((source
                (car
                 (apply #'auth-source-search
                        (append spec
                                (list :secret token
                                      :require '(:user :secret :port)
                                      :create t
                                      :max 1)))))
               (saved-token (and source (auth-info-password source)))
               (save-function (and source
                                   (plist-get source :save-function))))
          (unless (equal saved-token token)
            (error "Auth-source did not accept the new Misskey API token"))
          (unless (functionp save-function)
            (error "Auth-source did not provide persistent token storage"))
          (funcall save-function)
          (auth-source-forget lookup-spec)
          (unless (equal (misskey--stored-auth-token account) token)
            (error "Auth-source could not verify the saved Misskey API token"))
          token)
      (error
       (error "Could not save the Misskey API token: %s"
              (misskey-auth--redact-token
               (error-message-string err) token))))))

(defun misskey-auth--ensure-token (&optional account)
  "Return ACCOUNT's token, acquiring and storing one when absent."
  (let ((target (or account (misskey--current-account))))
    (or (misskey--stored-auth-token target)
        (let ((token (misskey-auth--request-token target)))
          (misskey-auth--store-token target token)
          (message "Authorized Misskey account %s on %s"
                   (misskey--account-auth-source-user target)
                   (misskey--account-origin target))
          token))))

(provide 'misskey-auth)

;;; misskey-auth.el ends here
