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

(defconst misskey-auth--permissions
  '("read:account"
    "read:notifications"
    "write:notes"
    "write:reactions"
    "write:favorites"
    "write:following"
    "write:notifications"
    "write:drive")
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

(defun misskey-auth--request-credential (account)
  "Authorize ACCOUNT and return its validated atomic credential."
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
           (token (and (consp payload) (alist-get 'token payload)))
           (user (and (consp payload) (alist-get 'user payload)))
           (user-id (and (consp user) (alist-get 'id user))))
      (unless (eq (alist-get 'ok payload) t)
        (user-error "The Misskey instance did not approve this authorization"))
      (unless (misskey--valid-token-p token)
        (user-error "Misskey returned an invalid API token"))
      (unless (misskey--valid-user-id-p user-id)
        (user-error "Misskey returned an invalid stable user ID"))
      (misskey--credential-create :token token :user-id user-id))))

(defun misskey-auth--redact-secrets (message &rest secrets)
  "Return MESSAGE with every nonempty string in SECRETS redacted."
  (dolist (secret secrets message)
    (when (and (stringp secret) (not (string-empty-p secret)))
      (setq message (string-replace secret "[REDACTED]" message)))))

(defun misskey-auth--source-file (source)
  "Return netrc/authinfo file named by auth SOURCE."
  (cond
   ((stringp source) source)
   ((and (listp source) (stringp (plist-get source :source)))
    (plist-get source :source))
   (t nil)))

(defun misskey-auth--storage-source ()
  "Return the first configured encrypted netrc/authinfo source."
  (or (cl-find-if
       (lambda (source)
         (when-let* ((file (misskey-auth--source-file source)))
           (let* ((plain (file-name-sans-extension file))
                  (kind
                   (downcase (or (file-name-extension plain) ""))))
             (and
              (string-equal
               (downcase (or (file-name-extension file) "")) "gpg")
              (not (member kind '("json" "plist")))))))
       auth-sources)
      (user-error
       "Add an encrypted netrc/authinfo .gpg file to auth-sources before authorizing Misskey")))

(defun misskey-auth--netrc-comment-position (line)
  "Return the first unquoted comment marker position in LINE."
  (let ((index 0)
        quoted
        escaped
        found)
    (while (and (< index (length line)) (not found))
      (let ((char (aref line index)))
        (cond
         (escaped (setq escaped nil))
         ((and quoted (eq char ?\\)) (setq escaped t))
         ((eq char ?\") (setq quoted (not quoted)))
         ((and (not quoted) (eq char ?#)) (setq found index))))
      (setq index (1+ index)))
    found))

(defun misskey-auth--netrc-block-matches-p (text spec)
  "Return non-nil when netrc block TEXT has locator SPEC."
  (let (data values)
    (dolist (line (split-string text "\n"))
      (unless (string-match-p "\\`[ \t]*#" line)
        (let ((comment (misskey-auth--netrc-comment-position line)))
          (push (if comment (substring line 0 comment) line) data))))
    (let ((tokens
           (split-string-and-unquote (string-join (nreverse data) " "))))
      (while (cdr tokens)
        (push (cons (pop tokens) (pop tokens)) values)))
    (and (equal (cdr (assoc "machine" values)) (plist-get spec :host))
         (equal (cdr (assoc "login" values)) (plist-get spec :user))
         (equal (cdr (assoc "port" values)) (plist-get spec :port)))))

(defun misskey-auth--netrc-block-operations (start end)
  "Return comment-preserving deletion operations from START through END."
  (let (operations)
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((line-start (line-beginning-position))
               (line-end (line-end-position))
               (next (min end
                          (if (< line-end (point-max))
                              (1+ line-end)
                            line-end)))
               (line (buffer-substring-no-properties line-start line-end))
               (comment (misskey-auth--netrc-comment-position line))
               (data (string-trim
                      (if comment (substring line 0 comment) line))))
          (unless (string-empty-p data)
            (push
             (list line-start next
                   (if comment
                       (concat (substring line comment)
                               (if (< line-end (point-max)) "\n" ""))
                     ""))
             operations))
          (goto-char next))))
    operations))

(defun misskey-auth--verify-netrc-candidate (file spec secret)
  "Verify candidate authinfo FILE contains exactly SPEC with SECRET."
  (auth-source-forget-all-cached)
  (let* ((auth-sources (list file))
         (auth-source-ignore-non-existing-file nil)
         (matches
          (apply #'auth-source-search
                 (append spec '(:require (:secret :port) :max 2)))))
    (unless (and (= (length matches) 1)
                 (equal (auth-info-password (car matches)) secret))
      (error "Candidate auth-source did not contain one replacement credential"))))

(defun misskey-auth--upsert-netrc-file (file spec secret)
  "Atomically replace SPEC with one SECRET entry in authinfo FILE."
  (let* ((expanded (expand-file-name file))
         (directory (file-name-directory expanded))
         (extension (or (file-name-extension expanded t) ""))
         (suffix (if (string-empty-p extension) ".authinfo" extension))
         (temporary
          (make-temp-file (expand-file-name ".misskey-auth-" directory)))
         (candidate (concat temporary suffix))
         operations)
    (unwind-protect
        (progn
          ;; Create securely without invoking the encryption handler, then give
          ;; the unique candidate its final extension before writing it.
          (rename-file temporary candidate)
          (setq temporary nil)
          (with-temp-buffer
            (when (file-exists-p expanded)
              (insert-file-contents expanded))
            (when auth-source-gpg-encrypt-to
              (make-local-variable 'epa-file-encrypt-to)
              (when (listp auth-source-gpg-encrypt-to)
                (setq epa-file-encrypt-to auth-source-gpg-encrypt-to)))
            (goto-char (point-min))
            (while (not (eobp))
              (cond
               ((looking-at "^[ \t]*\\(machine\\|default\\)[ \t]+")
                (let ((machinep (equal (match-string 1) "machine"))
                      (start (line-beginning-position))
                      end)
                  (forward-line 1)
                  (while (and
                          (not (eobp))
                          (not
                           (looking-at
                            "^[ \t]*\\(?:machine\\|default\\|macdef\\)[ \t]+")))
                    (forward-line 1))
                  (setq end (point))
                  (when (and machinep
                             (misskey-auth--netrc-block-matches-p
                              (buffer-substring-no-properties start end) spec))
                    (setq operations
                          (nconc (misskey-auth--netrc-block-operations start end)
                                 operations)))))
               ((looking-at "^[ \t]*macdef[ \t]+")
                (forward-line 1)
                (while (and (not (eobp))
                            (not (looking-at "^[ \t]*$")))
                  (forward-line 1))
                (unless (eobp)
                  (forward-line 1)))
               (t (forward-line 1))))
            (dolist (operation
                     (sort operations
                           (lambda (left right) (> (car left) (car right)))))
              (goto-char (nth 0 operation))
              (delete-region (nth 0 operation) (nth 1 operation))
              (insert (nth 2 operation)))
            (goto-char (point-min))
            (insert
             (format "machine %s login %s port %s password %s\n"
                     (plist-get spec :host)
                     (plist-get spec :user)
                     (plist-get spec :port)
                     secret))
            (let ((coding-system-for-write 'utf-8-unix)
                  (inhibit-message t))
              (write-region
               (point-min) (point-max) candidate nil 'silent)))
          (misskey-auth--verify-netrc-candidate candidate spec secret)
          (when (file-exists-p expanded)
            (set-file-modes candidate (file-modes expanded)))
          (rename-file candidate expanded t)
          (setq candidate nil))
      (when (and candidate (file-exists-p candidate))
        (delete-file candidate))
      (when (and temporary (file-exists-p temporary))
        (delete-file temporary)))))

(defun misskey-auth--store-credential (account credential)
  "Upsert ACCOUNT's validated CREDENTIAL and return it."
  (let* ((token (and (misskey--credential-p credential)
                     (misskey--credential-token credential)))
         (secret (misskey--credential-string credential))
         (storage-source (misskey-auth--storage-source))
         (storage-file (misskey-auth--source-file storage-source))
         (spec (misskey--auth-source-spec account)))
    (condition-case err
        (progn
          (misskey-auth--upsert-netrc-file storage-file spec secret)
          (auth-source-forget-all-cached)
          credential)
      (error
       (error "Could not save the Misskey credential: %s"
              (misskey-auth--redact-secrets
               (error-message-string err) token secret))))))

(defun misskey-auth--replace-session (account old-credential new-credential)
  "Replace ACCOUNT's live OLD-CREDENTIAL session with NEW-CREDENTIAL."
  (when old-credential
    (let* ((old-id (misskey--credential-user-id old-credential))
           (old-key (list (misskey--account-origin account) old-id))
           (old-app (gethash old-key misskey--apps))
           (livep (appkit-app-live-p old-app)))
      (when livep
        (appkit-stop-app old-app))
      (remhash old-key misskey--apps)
      (when livep
        (misskey-app
         (misskey--account-create
          :origin (misskey--account-origin account)
          :auth-source-user (misskey--account-auth-source-user account)
          :remote-user-id (misskey--credential-user-id new-credential)))))))

(defun misskey-auth-authorize (&optional account)
  "Acquire and atomically replace ACCOUNT's scoped credential."
  (let* ((target (or account (misskey--current-account-locator)))
         (old-credential
          (condition-case nil
              (misskey--stored-credential target)
            (user-error nil)))
         (credential (misskey-auth--request-credential target)))
    (misskey-auth--store-credential target credential)
    (misskey-auth--replace-session target old-credential credential)
    (message "Authorized Misskey user %s on %s"
             (misskey--credential-user-id credential)
             (misskey--account-origin target))
    (misskey--credential-token credential)))

(defun misskey-auth--ensure-token (&optional account)
  "Return ACCOUNT's token, authorizing only when no record exists."
  (let ((target (or account (misskey--current-account-locator))))
    (or (misskey--stored-auth-token target)
        (misskey-auth-authorize target))))

(provide 'misskey-auth)

;;; misskey-auth.el ends here
