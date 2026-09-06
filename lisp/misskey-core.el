;;; misskey-core.el --- Misskey session and configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own the configured Misskey instance, auth-source credential lookup, and
;; Appkit application session.

;;; Code:

(require 'appkit-effect)
(require 'appkit-command)
(require 'appkit-surface)
(require 'appkit-app)
(require 'cl-lib)
(require 'auth-source)
(require 'subr-x)
(require 'json)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-projection)

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

(cl-defstruct (misskey--session (:constructor misskey--session-create))
  "Account domain state owned by one Misskey App."
  account timeline revision note-overrides note-revisions
  user-overrides user-revisions requests resources address)

(defun misskey--make-session (account)
  "Return initialized application state for ACCOUNT."
  (misskey--session-create
   :account account :revision 0
   :note-overrides (make-hash-table :test #'equal)
   :note-revisions (make-hash-table :test #'equal)
   :user-overrides (make-hash-table :test #'equal)
   :user-revisions (make-hash-table :test #'equal)
   :requests (make-hash-table :test #'equal)
   :resources (make-hash-table :test #'equal)))

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
             misskey--app-type :identity key :input (misskey--make-session target)))
      (puthash key app misskey--apps))
    app))

(defun misskey--session (app)
  "Return validated application state owned by APP."
  (let ((state (and (appkit-app-p app) (appkit-app-model app))))
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

(defvar misskey--collect-invalidations nil
  "Whether account invalidations are deferred until this transition commits.")

(defvar misskey--invalidations nil
  "Account/resource sets accumulated by the current App transition.")

(defun misskey-invalidate-resource (app resource)
  "Request a redraw for APP's RESOURCE after the current commit."
  (misskey-invalidate-resources app (list resource)))

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

(defconst misskey--app-type
  (appkit-app-type-create :name 'misskey :init
                          (lambda (context input)
                            (when (misskey--session-p input)
                              (setf (misskey--session-address input)
                                    (appkit-transition-context-owner-address
                                     context)))
                            (appkit-next :model input :render
                                         appkit-render-none))
                          :update #'misskey-app-update))

(defun misskey-request-table (app)
  "Return APP's account-local mutation lanes."
  (misskey--session-requests (misskey--session app)))

(defun misskey-resource-store (app)
  "Return APP's account-local preview resources."
  (misskey--session-resources (misskey--session app)))

(cl-defstruct (misskey-read-token (:constructor misskey-read-token-create))
  surface key live-p)

(defvar-local misskey--reads nil
  "Active domain read transactions in this host.")

(defun misskey-read-begin (surface key)
  "Replace the domain read at KEY in SURFACE."
  (misskey-read-cancel surface key)
  (let ((token (misskey-read-token-create :surface surface :key key :live-p t)))
    (with-current-buffer (appkit-surface-buffer surface)
      (push token misskey--reads))
    token))

(defun misskey-read-current-p (token)
  "Whether TOKEN can still install its response."
  (and (misskey-read-token-live-p token)
       (appkit-surface-live-p (misskey-read-token-surface token))))

(defun misskey-read-finish (token)
  "Accept TOKEN's terminal response once."
  (when (misskey-read-current-p token)
    (setf (misskey-read-token-live-p token) nil)
    (with-current-buffer (appkit-surface-buffer (misskey-read-token-surface token))
      (setq misskey--reads (delq token misskey--reads)))
    t))

(defun misskey-read-cancel (surface key)
  "Cancel SURFACE's current read transaction at KEY."
  (when (appkit-surface-live-p surface)
    (with-current-buffer (appkit-surface-buffer surface)
      (when-let*
          ((token
            (cl-find key misskey--reads :key #'misskey-read-token-key
                     :test #'equal)))
        (misskey-read-finish token)
        (misskey-dispatch surface (list :cancel-read key)) t))))

(defun misskey-surface--update (_context model message)
  "Accept a host intent against committed MODEL."
  (pcase message
    (`(:render ,change) (appkit-next :model model :render change))
    (`(:replace-model ,replacement)
     (setq replacement
           (plist-put replacement :address (plist-get model :address)))
     (setq replacement (plist-put replacement :media-intent nil))
     (appkit-next :model replacement :render appkit-render-none
                  :commands
                  (list
                   (appkit-command-cancel-effect
                    'misskey-media-acquire)
                   (appkit-command-cancel-effect
                    'misskey-media-present))))
    (`(:cancel-read ,key)
     (appkit-next :model model :render appkit-render-none :commands
                  (list (appkit-command-cancel-effect key))))
    (`(:read-effect ,effect)
     (appkit-next :model model :render appkit-render-none :commands
                  (list (appkit-command-start-effect effect))))
    (`(:read-delivered ,token ,callback ,payload)
     (when (misskey-read-current-p token) (funcall callback payload))
     (appkit-next :model model :render appkit-render-none))
    (_ (misskey-media-update model message))))

(cl-defun misskey-open-surface
    (&key app identity mode buffer-name input setup select)
  "Open or focus one canonical Misskey host at IDENTITY."
  (or
   (when-let* ((existing (appkit-app-surface app identity)))
     (when select (pop-to-buffer (appkit-surface-buffer existing)))
     existing)
   (let
       ((surface
         (appkit-open-generated-surface
          (appkit-surface-type-create :name mode :mode mode :init
                                      (lambda (context state)
                                        (setq state
                                              (plist-put state
                                                         :address
                                                         (appkit-transition-context-owner-address
                                                          context)))
                                        (appkit-next :model state
                                                     :render
                                                     (appkit-projection-change-create
                                                      :full-p t
                                                      :frame-p t
                                                      :position 'first)))
                                      :update #'misskey-surface-update
                                      :renderer-factory
                                      #'misskey-renderer-create)
          :app app :identity identity :input input :buffer-name
          buffer-name :select select)))
     (when setup (funcall setup surface)) surface)))

(defun misskey-renderer-create (surface)
  "Create the Renderer selected by SURFACE's host type."
  (let* ((mode (appkit-surface-type-mode (appkit-surface-type surface)))
         (directory-p (eq mode 'misskey-directory-mode)))
    (if (memq mode '(misskey-directory-mode misskey-notifications-mode))
        (appkit-generated-renderer-create
         :mount (lambda (_host _app _model)
                  (appkit-directory-configure
                   (appkit-directory-surface)
                   :item-inserter (if directory-p #'misskey-directory--insert-user
                                    #'misskey-notifications--insert-item)
                   :activate-function (if directory-p #'misskey-directory--activate-user
                                        #'misskey-notifications--activate-item)))
         :merge #'appkit-projection-change-merge
         :render (lambda (host _app model _change)
                   (appkit-directory-reconcile
                    (appkit-directory-surface)
                    (if directory-p (misskey-directory--project host model)
                      (misskey-notifications--project model)))
                   nil)
         :unmount #'ignore)
      (appkit-projection-renderer-create
       :project-all (lambda (host _app model)
                      (if (eq (plist-get model :type) 'thread)
                          (misskey-thread--project model (appkit-surface-app host))
                        (misskey-render-project-notes
                         (plist-get model :items) (appkit-surface-app host))))
       :project-frame #'misskey-render-frame
       :printer (lambda (_host _app row) (misskey-render-insert-row row))
       :anchor-property appkit-discussion-key-property :no-separator-p t))))

(defun misskey-render-frame (_surface _app state)
  "Project STATE's host-specific frame and media failure."
  (let* ((frame
          (pcase (plist-get state :type)
            ('timeline
             (cons (misskey-timeline--frame state)
                   (concat "\ng refresh   TAB next timeline   n/p note   "
                           (if (plist-get state :older-exhausted-p) "older exhausted" "N older")
                           "   ? menu   RET link/CW   O open URL   B browser   w copy link   c compose\n")))
            ('thread
             (cons (misskey-thread--frame state)
                   (concat "\ng refresh   n/p note   ? menu   RET link/CW   O open URL   B browser   w copy link"
                           (if (plist-get state :replies-exhausted-p) "   replies exhausted\n" "   N more replies\n"))))
            (_ (cons (misskey-feed--generated-text state :header-function #'misskey-feed-default-header)
                     (misskey-feed--generated-text state :footer-function #'misskey-feed-default-footer)))))
         (failure (plist-get state :media-error)))
    (if failure (cons (concat (car frame) "\nMedia: " failure "\n") (cdr frame)) frame)))

(defun misskey-app--update (context model message)
  "Commit account-owned results and return exact replies."
  (or (and (fboundp 'misskey-media-app-update)
           (misskey-media-app-update context model message))
      (let ((reply
             (pcase message
               (`(:timeline-snapshot ,snapshot)
                (setf (misskey--session-timeline model) snapshot) nil)
               (`(:timeline-observe ,request-id)
                (list :timeline-observed request-id
                      (cl-incf (misskey--session-revision model))))
               (`(:timeline-merge ,request-id ,observation ,notes)
                (let ((app (gethash (misskey--account-key (misskey--session-account model))
                                    misskey--apps)))
                  (dolist (note notes) (misskey-merge-note-state app note observation)))
                (list :timeline-committed request-id notes)))))
        (appkit-next
         :model model :render appkit-render-none
         :commands (when reply
                     (list (appkit-command-post-message
                            :target (appkit-transition-context-reply-route context)
                            :message reply :delivery 'report)))))))

(defvar misskey--transition-context nil
  "Current Misskey transition's routing capabilities.")
(defvar misskey--transition-commands nil
  "Closed commands produced by nested Misskey domain handlers.")

(defun misskey-dispatch (owner message)
  "Deliver MESSAGE to OWNER, returning a closed post during a transition."
  (if misskey--transition-context
      (let ((address (if (appkit-surface-p owner)
                         (plist-get (appkit-surface-model owner) :address)
                       (misskey--session-address (appkit-app-model owner)))))
        (push (appkit-command-post-message :target address :message message :delivery 'report)
              misskey--transition-commands))
    (if (appkit-surface-p owner)
        (appkit-surface-send owner message)
      (appkit-app-send owner message))))

(defun misskey-surface-update (context model message)
  "Serialize domain handlers and their closed follow-on commands."
  (let* ((misskey--transition-context context)
         (misskey--transition-commands nil)
         (timeline-p (eq (plist-get model :type) 'timeline))
         (next (or (and timeline-p (misskey-timeline-update context model message))
                   (misskey-surface--update context model message))))
    (if (appkit-next-rejected-p next) next
      (appkit-next
       :model (appkit-next-model next) :render (appkit-next-render next)
       :commands
       (append (appkit-next-commands next)
               (when (and timeline-p
                          (memq (car-safe message) '(:timeline-committed :timeline-select :timeline-reveal)))
                 (list (appkit-command-post-message
                        :target (appkit-transition-context-parent-address context)
                        :message (list :timeline-snapshot (misskey-timeline--snapshot (appkit-next-model next)))
                        :delivery 'report)))
               (nreverse misskey--transition-commands))))))

(defun misskey-app-update (context model message)
  "Serialize account results and batch their dependent host updates."
  (let* ((misskey--transition-context context)
         (misskey--transition-commands nil)
         (misskey--collect-invalidations t)
         (misskey--invalidations nil)
         (next (misskey-app--update context model message)))
    (if (appkit-next-rejected-p next) next
      (let ((misskey--collect-invalidations nil))
        (dolist (entry misskey--invalidations)
          (misskey-invalidate-resources (car entry) (cdr entry))))
      (appkit-next :model (appkit-next-model next) :render (appkit-next-render next)
                   :commands (append (appkit-next-commands next)
                                     (nreverse misskey--transition-commands))))))

(defun misskey-invalidate-resources (app resources)
  "Request one redraw per live APP host for RESOURCES."
  (if misskey--collect-invalidations
      (setf (alist-get app misskey--invalidations nil nil #'eq)
            (cl-union resources (alist-get app misskey--invalidations nil nil #'eq)
                      :test #'equal))
    (maphash
     (lambda (_identity entry)
       (let ((surface (cdr entry)))
         (when (appkit-surface-live-p surface)
           (misskey-dispatch
            surface
            (list :render
                  (appkit-projection-change-create
                   :resources resources :position 'preserve :frame-p t
                   :full-p (cl-some (lambda (resource) (eq (car-safe resource) :note))
                                    resources)))))))
     (appkit-app-surfaces app))))

(provide 'misskey-core)

;;; misskey-core.el ends here
