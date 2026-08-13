;;; misskey-core.el --- Misskey session and configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own the configured Misskey instance, auth-source credential lookup, and
;; Appkit application session.

;;; Code:

(require 'cl-lib)
(require 'auth-source)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)

(defgroup misskey nil
  "Use Misskey-compatible servers from Emacs."
  :group 'applications)

(defconst misskey--auth-source-service "misskey"
  "Auth-source service label for default-port Misskey API tokens.")

(defcustom misskey-instance-url nil
  "HTTPS origin of the Misskey-compatible server.

Use only the origin, for example, `https://example.social'."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'misskey)

(defcustom misskey-auth-source-user "misskey.el"
  "Credential label used to find the API token in auth-source.

The matching auth-source host is the host from `misskey-instance-url'.  This
label need not equal the Misskey username."
  :type 'string
  :group 'misskey)

(cl-defstruct (misskey--account
               (:constructor misskey--account-create))
  "One configured Misskey account target."
  origin
  auth-source-user)

(cl-defstruct (misskey--session
               (:constructor misskey--session-create))
  "State owned by one Misskey application session."
  account
  timeline-states
  avatar-images
  media-images)

(defun misskey--make-session (account)
  "Return initialized application state for ACCOUNT."
  (misskey--session-create
   :account account
   :timeline-states (make-hash-table :test #'eq)
   :avatar-images (make-hash-table :test #'equal)
   :media-images (make-hash-table :test #'equal)))

(appkit-define-app-kind misskey)

(defvar misskey--apps (make-hash-table :test #'equal)
  "Live Appkit sessions keyed by Misskey account.")

(defun misskey--instance-origin ()
  "Return the validated configured Misskey HTTPS origin."
  (unless (and (stringp misskey-instance-url)
               (not (string-empty-p misskey-instance-url)))
    (user-error "Set misskey-instance-url to your server's HTTPS origin"))
  (let ((url (url-generic-parse-url misskey-instance-url)))
    (unless (and (equal (url-type url) "https")
                 (stringp (url-host url))
                 (not (string-empty-p (url-host url)))
                 (null (url-user url))
                 (null (url-password url))
                 (member (url-filename url) '("" "/"))
                 (null (url-target url)))
      (user-error
       "Misskey instance URL must be an HTTPS origin without credentials, path, query, or fragment"))
    (concat "https://" (url-host url)
            (if (= (url-port url) 443)
                ""
              (format ":%d" (url-port url))))))

(defun misskey--current-account ()
  "Return the validated account selected by current customization."
  (unless (and (stringp misskey-auth-source-user)
               (not (string-empty-p misskey-auth-source-user)))
    (user-error "Set misskey-auth-source-user to an auth-source login"))
  (misskey--account-create
   :origin (misskey--instance-origin)
   :auth-source-user misskey-auth-source-user))

(defun misskey--account-key (account)
  "Return the stable Appkit identity for ACCOUNT."
  (unless (misskey--account-p account)
    (error "Invalid Misskey account"))
  (list (misskey--account-origin account)
        (misskey--account-auth-source-user account)))

(defun misskey--auth-source-spec (&optional account)
  "Return auth-source identity arguments for ACCOUNT.

ACCOUNT defaults to the account selected by current customization."
  (let* ((target (or account (misskey--current-account)))
         (origin (misskey--account-origin target))
         (url (url-generic-parse-url origin))
         (port (url-port url)))
    (list :host (url-host url)
          :user (misskey--account-auth-source-user target)
          :port (if (= port 443)
                    misskey--auth-source-service
                  (format "%s-%d" misskey--auth-source-service port)))))

(defun misskey--stored-auth-token (&optional account)
  "Return ACCOUNT's stored Misskey API token, or nil when absent.

ACCOUNT defaults to the account selected by current customization."
  (let* ((spec (misskey--auth-source-spec account))
         (source
          (car (apply #'auth-source-search
                      (append spec '(:require (:secret :port) :max 1)))))
         (token (and source (auth-info-password source))))
    (and (stringp token) (not (string-empty-p token)) token)))

(defun misskey--auth-token (&optional account)
  "Return ACCOUNT's current Misskey API token from auth-source.

ACCOUNT defaults to the account selected by current customization."
  (let* ((spec (misskey--auth-source-spec account))
         (token (misskey--stored-auth-token account)))
    (unless token
      (user-error
       "No Misskey API token for host %s, user %s, and service %s in auth-source"
       (plist-get spec :host)
       (plist-get spec :user)
       (plist-get spec :port)))
    token))

(defun misskey-app (&optional account)
  "Return ACCOUNT's live Misskey Appkit session, creating it when needed.

ACCOUNT defaults to the account selected by current customization."
  (let* ((target (or account (misskey--current-account)))
         (key (misskey--account-key target))
         (app (gethash key misskey--apps)))
    (unless (appkit-app-live-p app)
      (setq app
            (appkit-start-app
             'misskey :id key :state (misskey--make-session target)))
      (puthash key app misskey--apps))
    app))

(defun misskey--session (app)
  "Return validated application state owned by APP."
  (let ((state (and (appkit-app-p app) (appkit-app-state app))))
    (unless (misskey--session-p state)
      (error "Invalid Misskey application session"))
    state))

(defun misskey-stop ()
  "Stop all Misskey sessions and cancel their owned asynchronous work."
  (interactive)
  (let (first-error)
    (maphash
     (lambda (_key app)
       (condition-case err
           (when (appkit-app-live-p app)
             (appkit-stop-app app))
         (error
          (unless first-error
            (setq first-error err)))))
     misskey--apps)
    (clrhash misskey--apps)
    (when first-error
      (signal (car first-error) (cdr first-error)))))

(provide 'misskey-core)

;;; misskey-core.el ends here
