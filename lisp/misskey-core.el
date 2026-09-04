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
(require 'json)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-invalidation)

(defgroup misskey nil
  "Use Misskey-compatible servers from Emacs."
  :group 'applications)

(defconst misskey--auth-source-service "misskey"
  "Auth-source service label for Misskey credential records.")

(defcustom misskey-instance-url nil
  "HTTPS origin of the Misskey-compatible server.

Use only the origin, for example, `https://example.social'."
  :type '(choice (const :tag "Not configured" nil) string)
  :group 'misskey)

(defcustom misskey-auth-source-user "misskey.el"
  "Credential label used to locate the account record in auth-source.

The matching auth-source host is the host from `misskey-instance-url'.  This
label is not account identity and need not equal the Misskey username."
  :type 'string
  :group 'misskey)

(cl-defstruct (misskey--account
               (:constructor misskey--account-create))
  "One configured Misskey account target."
  origin
  auth-source-user
  remote-user-id)

(cl-defstruct (misskey--credential
               (:constructor misskey--credential-create))
  "One validated atomic Misskey credential record."
  token
  user-id)

(cl-defstruct (misskey--session
               (:constructor misskey--session-create))
  "State owned by one Misskey application session."
  account
  timeline-states
  revision
  note-overrides
  note-revisions
  user-overrides
  user-revisions)

(defun misskey--make-session (account)
  "Return initialized application state for ACCOUNT."
  (misskey--session-create
   :account account
   :timeline-states (make-hash-table :test #'eq)
   :revision 0
   :note-overrides (make-hash-table :test #'equal)
   :note-revisions (make-hash-table :test #'equal)
   :user-overrides (make-hash-table :test #'equal)
   :user-revisions (make-hash-table :test #'equal)))

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
                 (not (string-match-p "[^A-Za-z0-9.:-]" (url-host url)))
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

(defun misskey--current-account-locator ()
  "Return the validated credential locator selected by customization."
  (unless (and (stringp misskey-auth-source-user)
               (string-match-p "\\`[A-Za-z0-9._@+-]+\\'"
                               misskey-auth-source-user))
    (user-error
     "Set misskey-auth-source-user to a safe nonempty auth-source login"))
  (misskey--account-create
   :origin (misskey--instance-origin)
   :auth-source-user misskey-auth-source-user))

(defun misskey--current-account ()
  "Return the selected account bound to its stored stable remote identity."
  (misskey--authenticated-account (misskey--current-account-locator)))

(defun misskey--account-key (account)
  "Return the stable Appkit identity for authenticated ACCOUNT."
  (unless (and (misskey--account-p account)
               (misskey--valid-user-id-p
                (misskey--account-remote-user-id account)))
    (error "Misskey account has no validated remote user identity"))
  (list (misskey--account-origin account)
        (misskey--account-remote-user-id account)))

(defun misskey--auth-source-spec (&optional account)
  "Return auth-source identity arguments for ACCOUNT.

ACCOUNT defaults to the account selected by current customization."
  (let* ((target (or account (misskey--current-account-locator)))
         (origin (misskey--account-origin target))
         (url (url-generic-parse-url origin))
         (port (url-port url)))
    (list :host (url-host url)
          :user (misskey--account-auth-source-user target)
          :port (if (= port 443)
                    misskey--auth-source-service
                  (format "%s-%d" misskey--auth-source-service port)))))

(defun misskey--valid-token-p (token)
  "Return non-nil when TOKEN is an unambiguous RFC 6750 bearer value."
  (and (stringp token)
       (string-match-p
        "\\`[A-Za-z0-9][A-Za-z0-9._~+/-]*\\(?:=+\\)?\\'" token)))

(defun misskey--valid-user-id-p (user-id)
  "Return non-nil when USER-ID is a safe stable Misskey object ID."
  (and (stringp user-id)
       (string-match-p "\\`[A-Za-z0-9_-]+\\'" user-id)))

(defun misskey--credential-string (credential)
  "Serialize validated CREDENTIAL as one auth-source-safe secret."
  (unless (and (misskey--credential-p credential)
               (misskey--valid-token-p
                (misskey--credential-token credential))
               (misskey--valid-user-id-p
                (misskey--credential-user-id credential)))
    (error "Invalid Misskey credential"))
  (concat
   "misskey-v1:"
   (base64-encode-string
    (json-serialize
     `((version . 1)
       (token . ,(misskey--credential-token credential))
       (userId . ,(misskey--credential-user-id credential)))
     :null-object :json-null :false-object :json-false)
    t)))

(defun misskey--parse-credential (secret)
  "Parse and validate atomic credential SECRET.

Signal a clear reauthorization error for legacy token-only or malformed
records."
  (let ((payload
         (and (stringp secret)
              (string-prefix-p "misskey-v1:" secret)
              (condition-case nil
                  (json-parse-string
                   (decode-coding-string
                    (base64-decode-string
                     (substring secret (length "misskey-v1:")))
                    'utf-8)
                   :object-type 'alist
                   :array-type 'array
                   :null-object :json-null
                   :false-object :json-false)
                (error nil)))))
    (unless (and (consp payload)
                 (equal (alist-get 'version payload) 1)
                 (misskey--valid-token-p (alist-get 'token payload))
                 (misskey--valid-user-id-p (alist-get 'userId payload)))
      (user-error
       "Stored Misskey credential is obsolete or invalid; reauthorize with M-x misskey-authorize"))
    (misskey--credential-create
     :token (alist-get 'token payload)
     :user-id (alist-get 'userId payload))))

(defun misskey--stored-credential (&optional account)
  "Return ACCOUNT's validated stored credential, or nil when absent."
  (let* ((spec (misskey--auth-source-spec account))
         (source
          (car (apply #'auth-source-search
                      (append spec '(:require (:secret :port) :max 1)))))
         (secret (and source (auth-info-password source))))
    (and secret (misskey--parse-credential secret))))

(defun misskey--authenticated-account (&optional account)
  "Return ACCOUNT bound to the stable identity in its credential."
  (let* ((target (or account (misskey--current-account-locator)))
         (credential (misskey--stored-credential target))
         (user-id (and credential (misskey--credential-user-id credential))))
    (unless credential
      (let ((spec (misskey--auth-source-spec target)))
        (user-error
         "No Misskey credential for host %s, user %s, and service %s in auth-source"
         (plist-get spec :host)
         (plist-get spec :user)
         (plist-get spec :port))))
    (when (and (misskey--account-remote-user-id target)
               (not (equal (misskey--account-remote-user-id target) user-id)))
      (user-error
       "This Misskey session belongs to a replaced credential; reopen it"))
    (misskey--account-create
     :origin (misskey--account-origin target)
     :auth-source-user (misskey--account-auth-source-user target)
     :remote-user-id user-id)))

(defun misskey--stored-auth-token (&optional account)
  "Return ACCOUNT's validated stored token, or nil when absent."
  (when-let* ((credential (misskey--stored-credential account)))
    (misskey--credential-token credential)))

(defun misskey--auth-token (&optional account)
  "Return ACCOUNT's validated token after checking its stable identity."
  (let* ((target (or account (misskey--current-account)))
         (authenticated (misskey--authenticated-account target))
         (credential (misskey--stored-credential authenticated)))
    (misskey--credential-token credential)))

(defun misskey-app (&optional account)
  "Return ACCOUNT's live identity-bound Misskey Appkit session."
  (let* ((target (misskey--authenticated-account account))
         (key (misskey--account-key target))
         (app (gethash key misskey--apps)))
    (unless (appkit-app-live-p app)
      (setq app
            (appkit-app-start
             'misskey :id key :state (misskey--make-session target)))
      (puthash key app misskey--apps))
    app))

(defun misskey--session (app)
  "Return validated application state owned by APP."
  (let ((state (and (appkit-app-p app) (appkit-app-state app))))
    (unless (misskey--session-p state)
      (error "Invalid Misskey application session"))
    state))

(defun misskey--override-value (table id property fallback)
  "Return TABLE's ID PROPERTY state, or FALLBACK when absent."
  (let ((state (and id (gethash id table))))
    (if (and state (plist-member state property))
        (plist-get state property)
      fallback)))

(defun misskey-note-state-value (app note-id property fallback)
  "Return APP's NOTE-ID PROPERTY state, or FALLBACK when unobserved."
  (misskey--override-value
   (misskey--session-note-overrides (misskey--session app))
   note-id property fallback))

(defun misskey-user-state-value (app user-id property fallback)
  "Return APP's USER-ID PROPERTY state, or FALLBACK when unobserved."
  (misskey--override-value
   (misskey--session-user-overrides (misskey--session app))
   user-id property fallback))

(defun misskey-state-observe (app)
  "Return a new authoritative observation revision for APP."
  (let ((session (misskey--session app)))
    (cl-incf (misskey--session-revision session))))

(defun misskey--validate-state-update (id properties)
  "Validate state ID and property-value PROPERTIES."
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "Misskey state identity must be a non-empty string"))
  (unless (zerop (% (length properties) 2))
    (error "Misskey state updates must be property-value pairs")))

(defun misskey-invalidate-resource (app resource)
  "Invalidate RESOURCE in every live view owned by APP."
  (unless (appkit-app-live-p app)
    (error "Cannot invalidate a dead Misskey application"))
  (maphash
   (lambda (_id view)
     (when (appkit-view-live-p view)
       (appkit-request-sync view :resource resource :position t)))
   (appkit-app-view-registry app)))

(defun misskey--record-state-values
    (app table revisions id resource properties)
  "Record ID PROPERTIES in TABLE and REVISIONS for APP, then invalidate RESOURCE."
  (misskey--validate-state-update id properties)
  (let* ((revision (misskey-state-observe app))
         (state (gethash id table))
         (property-revisions (gethash id revisions))
         (cursor properties))
    (while cursor
      (let ((property (pop cursor))
            (value (pop cursor)))
        (setq state (plist-put state property value)
              property-revisions
              (plist-put property-revisions property revision))))
    (puthash id state table)
    (puthash id property-revisions revisions)
    (misskey-invalidate-resource app resource)
    revision))

(defun misskey--fence-state (app revisions id properties)
  "Fence ID PROPERTIES in REVISIONS against reads older than this APP write."
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "Misskey state identity must be a non-empty string"))
  (let ((revision (misskey-state-observe app))
        (property-revisions (gethash id revisions)))
    (dolist (property properties)
      (unless (keywordp property)
        (error "Misskey state property must be a keyword"))
      (setq property-revisions
            (plist-put property-revisions property revision)))
    (puthash id property-revisions revisions)
    revision))

(defun misskey-fence-note-state (app note-id &rest properties)
  "Fence APP's NOTE-ID PROPERTIES against reads predating a write."
  (misskey--fence-state
   app
   (misskey--session-note-revisions (misskey--session app))
   note-id properties))

(defun misskey-fence-user-state (app user-id &rest properties)
  "Fence APP's USER-ID PROPERTIES against reads predating a write."
  (misskey--fence-state
   app
   (misskey--session-user-revisions (misskey--session app))
   user-id properties))

(defun misskey-set-note-state-values (app note-id &rest properties)
  "Set APP's NOTE-ID state PROPERTIES and invalidate its note resource."
  (misskey--record-state-values
   app
   (misskey--session-note-overrides (misskey--session app))
   (misskey--session-note-revisions (misskey--session app))
   note-id (list :note note-id) properties))

(defun misskey-set-user-state-values (app user-id &rest properties)
  "Set APP's USER-ID state PROPERTIES and invalidate its user resource."
  (misskey--record-state-values
   app
   (misskey--session-user-overrides (misskey--session app))
   (misskey--session-user-revisions (misskey--session app))
   user-id (list :user user-id) properties))

(defun misskey--wire-boolean (value)
  "Return non-nil only when wire VALUE is JSON true."
  (eq value t))

(defun misskey--merge-state
    (app table revisions id resource payload observation fields)
  "Merge PAYLOAD FIELDS for ID into APP's TABLE at OBSERVATION.

REVISIONS stores per-field fences.  RESOURCE identifies dependent views to
invalidate.  FIELDS contains (WIRE-KEY STATE-PROPERTY NORMALIZER) entries.
Missing wire keys do not alter state; an explicitly present nil value does."
  (unless (and (integerp observation) (> observation 0))
    (error "Misskey observation revision must be a positive integer"))
  (unless (and (stringp id) (not (string-empty-p id)))
    (error "Misskey authoritative payload has no stable identity"))
  (let ((state (gethash id table))
        (property-revisions (gethash id revisions))
        changed
        observed)
    (dolist (field fields)
      (let* ((wire-key (nth 0 field))
             (property (nth 1 field))
             (normalizer (nth 2 field))
             (wire-cell (assq wire-key payload))
             (current-revision
              (or (plist-get property-revisions property) 0)))
        (when (and wire-cell (>= observation current-revision))
          (let ((value (if normalizer
                           (funcall normalizer (cdr wire-cell))
                         (cdr wire-cell))))
            (unless (and (plist-member state property)
                         (equal (plist-get state property) value))
              (setq changed t))
            (setq state (plist-put state property value)
                  property-revisions
                  (plist-put property-revisions property observation)
                  observed t)))))
    (when observed
      (puthash id state table)
      (puthash id property-revisions revisions))
    (when changed
      (misskey-invalidate-resource app resource))
    observed))

(defun misskey-merge-note-state (app note observation)
  "Merge server-backed NOTE and nested note fields at APP OBSERVATION."
  (let ((id (and (consp note) (alist-get 'id note))))
    (misskey--merge-state
     app
     (misskey--session-note-overrides (misskey--session app))
     (misskey--session-note-revisions (misskey--session app))
     id (list :note id) note observation
     '((myReaction :my-reaction nil)
       (reactionCount :reaction-count nil)
       (renoteCount :renote-count nil))))
  (let ((user (alist-get 'user note)))
    (when (and (consp user)
               (or (assq 'isFollowing user)
                   (assq 'hasPendingFollowRequestFromYou user)))
      (misskey-merge-user-state app user observation)))
  (dolist (key '(renote reply))
    (when-let* ((nested (alist-get key note))
                ((consp nested)))
      (misskey-merge-note-state app nested observation))))

(defun misskey-merge-user-state (app user observation)
  "Merge server-backed USER fields observed at APP OBSERVATION."
  (let ((id (and (consp user) (alist-get 'id user))))
    (misskey--merge-state
     app
     (misskey--session-user-overrides (misskey--session app))
     (misskey--session-user-revisions (misskey--session app))
     id (list :user id) user observation
     '((isFollowing :following-p misskey--wire-boolean)
       (hasPendingFollowRequestFromYou
        :follow-pending-p misskey--wire-boolean)))))

(defun misskey-note-deleted-p (app note)
  "Return non-nil when APP locally records NOTE as deleted."
  (misskey-note-state-value
   app (and (consp note) (alist-get 'id note)) :deleted-p nil))

(defun misskey-stop ()
  "Stop all Misskey sessions and cancel their owned asynchronous work."
  (interactive)
  (let (first-error)
    (maphash
     (lambda (_key app)
       (condition-case err
           (when (appkit-app-live-p app)
             (appkit-app-close app))
         (error
          (unless first-error
            (setq first-error err)))))
     misskey--apps)
    (clrhash misskey--apps)
    (when first-error
      (signal (car first-error) (cdr first-error)))))

(provide 'misskey-core)

;;; misskey-core.el ends here
