;;; misskey-core.el --- Misskey session and configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own the configured Misskey instance, auth-source credential lookup, and
;; Appkit application session.

;;; Code:

(require 'auth-source)
(require 'subr-x)
(require 'url-parse)
(require 'appkit-core)

(defgroup misskey nil
  "Use Misskey-compatible servers from Emacs."
  :group 'applications)

(defcustom misskey-instance-url nil
  "HTTPS origin of the Misskey-compatible server.

Use only the origin, for example, `https://example.social'."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'misskey)

(defcustom misskey-auth-source-user "misskey.el"
  "User name used to find the API token in auth-source.

The matching auth-source host is the host from `misskey-instance-url'."
  :type 'string
  :group 'misskey)

(appkit-define-app-kind misskey)

(defvar misskey--app nil
  "Lazy Appkit application session owned by Misskey.")

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

(defun misskey--auth-token ()
  "Return the current Misskey API token from auth-source."
  (let* ((origin (misskey--instance-origin))
         (host (url-host (url-generic-parse-url origin)))
         (source (car (auth-source-search
                       :host host
                       :user misskey-auth-source-user
                       :require '(:secret)
                       :max 1)))
         (token (and source (auth-info-password source))))
    (unless (and (stringp token) (not (string-empty-p token)))
      (user-error
       "No Misskey API token for host %s and user %s in auth-source"
       host misskey-auth-source-user))
    token))

(defun misskey-app ()
  "Return the live Misskey Appkit session, creating it when needed."
  (let ((origin (misskey--instance-origin)))
    (when (and (appkit-app-live-p misskey--app)
               (not (equal origin (appkit-app-id misskey--app))))
      (appkit-stop-app misskey--app)
      (setq misskey--app nil))
    (unless (appkit-app-live-p misskey--app)
      (setq misskey--app (appkit-start-app 'misskey :id origin)))
    misskey--app))

(defun misskey-stop ()
  "Stop Misskey and cancel its owned asynchronous work."
  (interactive)
  (unwind-protect
      (when (appkit-app-live-p misskey--app)
        (appkit-stop-app misskey--app))
    (setq misskey--app nil)))

(provide 'misskey-core)

;;; misskey-core.el ends here
