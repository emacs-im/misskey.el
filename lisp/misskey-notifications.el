;;; misskey-notifications.el --- Browse Misskey notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Read account notifications without implicit acknowledgement.  Opening and
;; paging always send markAsRead=false; only the explicit mark command mutates
;; remote read state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-directory)
(require 'misskey-actions)
(require 'appkit-projection)
(require 'misskey-auth)
(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-note)
(require 'misskey-render)
(require 'misskey-profile)
(require 'misskey-thread)

(defcustom misskey-notifications-limit 30
  "Maximum number of notifications requested per page."
  :type 'integer
  :group 'misskey)

(defconst misskey-notifications--request-key 'notifications
  "Operation key for the active notification read.")

(defconst misskey-notifications--mark-key 'notifications-mark-read
  "Operation key for the explicit read mutation.")

(defconst misskey-notifications--type-labels
  '(("follow" . "Followed you")
    ("mention" . "Mentioned you")
    ("reply" . "Replied")
    ("renote" . "Renoted")
    ("quote" . "Quoted")
    ("reaction" . "Reacted")
    ("pollEnded" . "Poll ended")
    ("receiveFollowRequest" . "Requested to follow")
    ("followRequestAccepted" . "Accepted your follow request")
    ("roleAssigned" . "Role assigned")
    ("achievementEarned" . "Achievement earned")
    ("app" . "Application notification")
    ("note" . "New note"))
  "Readable labels for known Misskey notification types.")

(declare-function misskey-menu "misskey-menu" nil)

(defvar-keymap misskey-notifications-mode-map
  :doc "Keymap for `misskey-notifications-mode'." :parent
  appkit-directory-mode-map "g" #'misskey-notifications-refresh "N"
  #'misskey-notifications-load-more "M"
  #'misskey-notifications-mark-all-read "a" misskey-actions-map "?"
  #'misskey-menu)

(define-derived-mode misskey-notifications-mode appkit-directory-mode
  "Misskey-Notifications"
  "Major mode for account-directed Misskey notifications.")

(defun misskey-notifications--state (view)
  "Return VIEW's validated notification state."
  (let
      ((state
        (and (appkit-surface-p view) (appkit-surface-model view))))
    (unless
        (and (listp state) (eq (plist-get state :type) 'notifications)
             (misskey--account-p (plist-get state :account))
             (hash-table-p (plist-get state :acknowledged-ids)))
      (error "Invalid Misskey notification state"))
    state))

(defun misskey-notifications--current-view ()
  "Return the current live notification view, or nil."
  (when-let*
      ((view (appkit-current-surface)) ((appkit-surface-live-p view))
       (state (appkit-surface-model view))
       ((eq (plist-get state :type) 'notifications)))
    view))

(defun misskey-notifications--validate-list (payload)
  "Return validated notification PAYLOAD with unique stable IDs."
  (unless (listp payload)
    (error "Misskey notification response is not a list"))
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (notification payload)
      (let ((id (and (consp notification)
                     (alist-get 'id notification)))
            (type (and (consp notification)
                       (alist-get 'type notification))))
        (unless (and (stringp id) (not (string-empty-p id))
                     (stringp type) (not (string-empty-p type)))
          (error "Misskey returned a malformed notification"))
        (when (gethash id seen)
          (error "Misskey duplicated notification %s" id))
        (puthash id t seen))))
  payload)

(defun misskey-notifications--new-items (current candidates)
  "Return CANDIDATES whose IDs do not occur in CURRENT."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (notification current)
      (puthash (alist-get 'id notification) t seen))
    (dolist (notification candidates (nreverse result))
      (let ((id (alist-get 'id notification)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push notification result))))))

(defun misskey-notifications--type-label (notification)
  "Return a readable type label for NOTIFICATION."
  (let ((type (alist-get 'type notification)))
    (or (alist-get type misskey-notifications--type-labels nil nil #'equal)
        (capitalize
         (replace-regexp-in-string
          "\\([[:lower:]]\\)\\([[:upper:]]\\)" "\\1 \\2" type)))))

(defun misskey-notifications--one-line (text)
  "Return TEXT collapsed into one trimmed line."
  (and (stringp text)
       (string-trim
        (replace-regexp-in-string "[[:space:]\n]+" " " text))))

(defun misskey-notifications--insert-item (_surface entry)
  "Insert the notification carried by directory ENTRY."
  (let* ((notification (appkit-directory-entry-payload entry))
         (user (alist-get 'user notification))
         (note (alist-get 'note notification))
         (reaction (alist-get 'reaction notification))
         (text (and (consp note)
                    (not (misskey-note-content-warning-p note))
                    (misskey-notifications--one-line
                     (alist-get 'text (misskey-note-display-note note))))))
    (insert (propertize (misskey-notifications--type-label notification)
                        'face 'bold))
    (when (consp user)
      (insert "  " (misskey-user-label user)))
    (when (and (stringp reaction) (not (string-empty-p reaction)))
      (insert "  " reaction))
    (when (and text (not (string-empty-p text)))
      (insert " — " text))
    (when-let* ((created-at (misskey-render-time notification))
                ((not (string-empty-p created-at))))
      (insert "  " (propertize created-at 'face 'shadow)))
    (insert "\n")))

(defun misskey-notifications--activate-item (_surface entry)
  "Open the note or actor carried by notification ENTRY."
  (let* ((notification (appkit-directory-entry-payload entry))
         (note (alist-get 'note notification))
         (user (alist-get 'user notification))
         (view (misskey-notifications--current-view))
         (account (and view
                       (plist-get (misskey-notifications--state view)
                                  :account))))
    (unless account
      (user-error "Current buffer is not a Misskey notification view"))
    (cond
     ((consp note)
      (let* ((target (misskey-note-display-note note))
             (id (misskey-note-id target)))
        (unless (and (stringp id) (not (string-empty-p id)))
          (user-error "This Misskey notification has no valid note target"))
        (misskey-thread-open id account)))
     ((consp user) (misskey-profile-open user account))
     (t (user-error "This Misskey notification has no navigable target")))))

(defun misskey-notifications--entry-properties (notification)
  "Return domain text properties for NOTIFICATION."
  (let ((note (alist-get 'note notification))
        (user (alist-get 'user notification)))
    (append
     (when (consp user)
       (list misskey-user-property user
             misskey-user-id-property (misskey-user-id user)))
     (when (consp note)
       (list misskey-note-property note
             misskey-note-id-property (misskey-note-id note))))))

(defun misskey-notifications--project (state)
  "Project notification STATE into Appkit directory entries."
  (let* ((phase (plist-get state :phase))
         (message (plist-get state :message))
         (items (plist-get state :items))
         (acknowledged (plist-get state :acknowledged-ids))
         (all-read-p
          (and items
               (cl-every
                (lambda (notification)
                  (or (eq (alist-get 'isRead notification) t)
                      (gethash (alist-get 'id notification) acknowledged)))
                items)))
         (section-key '(notifications section))
         (entries
          (list
           (appkit-directory-entry-create
            :key section-key :role 'section
            :label (if all-read-p "Notifications · read"
                     "Notifications · not acknowledged")))))
    (when (memq phase '(initial refresh older error))
      (push
       (appkit-directory-entry-create
        :key '(notifications status) :role 'note
        :section-key section-key :indent 2
        :face (if (eq phase 'error) 'error 'shadow)
        :label
        (pcase phase
          ('initial "Loading notifications without marking read...")
          ('refresh "Refreshing without marking read...")
          ('older "Loading older notifications...")
          ('error (format "Unable to load notifications: %s" message))))
       entries))
    (dolist (notification items)
      (let* ((id (alist-get 'id notification))
             (item-read-p
              (or (eq (alist-get 'isRead notification) t)
                  (gethash id acknowledged))))
        (push
         (appkit-directory-entry-create
          :key id :role 'item :section-key section-key :item-p t
          :unread-p (not item-read-p)
          :payload notification :stamp (list notification item-read-p)
          :help-echo "RET: Open notification target"
          :properties (misskey-notifications--entry-properties notification))
         entries)))
    (when (plist-get state :older-exhausted-p)
      (push
       (appkit-directory-entry-create
        :key '(notifications exhausted) :role 'note
        :section-key section-key :indent 2 :face 'shadow
        :label "No older notifications.")
       entries))
    (when (and (null items) (eq phase 'ready))
      (push
       (appkit-directory-entry-create
        :key '(notifications empty) :role 'note
        :section-key section-key :indent 2 :face 'shadow
        :label "No notifications returned.")
       entries))
    (nreverse entries)))

(defun misskey-notifications--handle-read-error (view state failure)
  "Install notification read FAILURE in VIEW STATE."
  (setf (plist-get state :phase) 'error (plist-get state :message)
        failure)
  (misskey-dispatch view
                    (list :render
                          (appkit-projection-change-create :full-p t
                                                           :frame-p t
                                                           :position
                                                           'preserve)))
  (message "%s" failure))

(defun misskey-notifications--handle-read-success
    (view state observation phase payload)
  "Install notification PAYLOAD for PHASE in VIEW STATE.

OBSERVATION versions canonical entity merges."
  (condition-case err
      (let*
          ((notifications
            (misskey-notifications--validate-list payload))
           (current (plist-get state :items))
           (new
            (if (eq phase 'older)
                (misskey-notifications--new-items current
                                                  notifications)
              notifications))
           (installed
            (pcase phase
              ('initial notifications)
              ('refresh
               (append notifications
                       (misskey-notifications--new-items notifications
                                                         current)))
              ('older (append current new)))))
        (dolist (notification notifications)
          (let
              ((note (alist-get 'note notification))
               (user (alist-get 'user notification)))
            (when
                (and (consp note) (stringp (misskey-note-id note))
                     (not (string-empty-p (misskey-note-id note))))
              (misskey-merge-note-state (appkit-surface-app view) note
                                        observation))
            (when
                (and (consp user) (stringp (misskey-user-id user))
                     (not (string-empty-p (misskey-user-id user))))
              (misskey-merge-user-state (appkit-surface-app view) user
                                        observation))))
        (setf (plist-get state :items) installed
              (plist-get state :phase) 'ready
              (plist-get state :message) nil
              (plist-get state :loaded-p) t)
        (pcase phase
          ('initial
           (setf (plist-get state :older-exhausted-p)
                 (null notifications)))
          ('older
           (setf (plist-get state :older-exhausted-p) (null new))))
        (misskey-dispatch view
                          (list :render
                                (appkit-projection-change-create
                                 :full-p t :frame-p t :position
                                 'preserve)))
        (message "Loaded %d Misskey notifications" (length new)))
    (error
     (misskey-notifications--handle-read-error view state
                                               (error-message-string
                                                err)))))

(defun misskey-notifications--request (view phase)
  "Start notification VIEW read for PHASE without marking it read."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Misskey notification phase: %S" phase))
  (let ((state (misskey-notifications--state view)))
    (when (and (eq phase 'older) (plist-get state :older-exhausted-p))
      (user-error "No older Misskey notifications available"))
    (let
        ((parameters
          (list :limit misskey-notifications-limit :markAsRead
                :json-false)))
      (when (eq phase 'older)
        (let*
            ((oldest (car (last (plist-get state :items))))
             (cursor (and oldest (alist-get 'id oldest))))
          (unless oldest
            (user-error "The Misskey notification view has no items"))
          (unless (and (stringp cursor) (not (string-empty-p cursor)))
            (user-error
             "The oldest Misskey notification has no valid ID"))
          (setq parameters (plist-put parameters :untilId cursor))))
      (let*
          ((observation
            (misskey-state-observe (appkit-surface-app view)))
           (operation
            (misskey-read-begin view
                                misskey-notifications--request-key)))
        (setf (plist-get state :phase) phase
              (plist-get state :message) nil)
        (misskey-dispatch view
                          (list :render
                                (appkit-projection-change-create
                                 :full-p t :frame-p t :position
                                 'preserve)))
        (misskey-http-read "i/notifications" parameters
                           (lambda (payload)
                             (when (misskey-read-finish operation)
                               (misskey-notifications--handle-read-success
                                view state observation phase payload)))
                           :errback
                           (lambda (failure)
                             (when (misskey-read-finish operation)
                               (misskey-notifications--handle-read-error
                                view state failure)))
                           :account (plist-get state :account) :owner
                           operation)))))

(defun misskey-notifications--finish-mark (view state message &optional ids)
  "Retire VIEW STATE's read mutation and report MESSAGE.

Acknowledge only the snapshot IDS supplied by a successful mutation."
  (let ((acknowledged (plist-get state :acknowledged-ids)))
    (dolist (id ids)
      (puthash id t acknowledged)))
  (setf (plist-get state :marking-p) nil)
  (misskey-dispatch
   view
   (list :render
         (appkit-projection-change-create
          :full-p t :frame-p t :position 'preserve)))
  (message "%s" message))

(defun misskey-notifications-mark-all-read ()
  "Explicitly mark all account notifications read." (interactive)
  (if-let* ((view (misskey-notifications--current-view)))
      (let ((state (misskey-notifications--state view)))
        (when (plist-get state :marking-p)
          (user-error
           "Notification acknowledgement is already in flight"))
        (let*
            ((ids
              (mapcar
               (lambda (notification) (alist-get 'id notification))
               (plist-get state :items)))
             (operation
              (misskey-read-begin view misskey-notifications--mark-key)))
          (setf (plist-get state :marking-p) t)
          (misskey-dispatch view
                            (list :render
                                  (appkit-projection-change-create
                                   :full-p t :frame-p t :position
                                   'preserve)))
          (misskey-http-post "notifications/mark-all-as-read"
                             (make-hash-table)
                             (lambda (_payload)
                               (when (misskey-read-finish operation)
                                 (misskey-notifications--finish-mark
                                  view state "Marked all Misskey notifications read" ids)))
                             :errback
                             (lambda (failure)
                               (when (misskey-read-finish operation)
                                 (misskey-notifications--finish-mark
                                  view state failure)))
                             :account (plist-get state :account)
                             :owner operation)))
    (user-error "Current buffer is not a Misskey notification view")))

(defun misskey-notifications--setup-view (view)
  "Initialize notification VIEW."
  (misskey-notifications--request view 'initial))

(defun misskey-notifications-refresh ()
  "Refresh notifications without changing remote read state."
  (interactive)
  (if-let* ((view (misskey-notifications--current-view)))
      (misskey-notifications--request
       view (if (plist-get (misskey-notifications--state view) :loaded-p)
                'refresh
              'initial))
    (user-error "Current buffer is not a Misskey notification view")))

(defun misskey-notifications-load-more ()
  "Load one older notification page without marking it read."
  (interactive)
  (if-let* ((view (misskey-notifications--current-view)))
      (misskey-notifications--request view 'older)
    (user-error "Current buffer is not a Misskey notification view")))

;;;###autoload
(defun misskey-notifications (&optional account)
  "Open ACCOUNT's notification view without marking notifications read."
  (interactive)
  (let*
      ((target (or account (misskey--current-account)))
       (_token
        (and (called-interactively-p 'interactive)
             (misskey-auth--ensure-token target)))
       (app (misskey-app target)) (id '(notifications))
       (existing (appkit-app-surface app id))
       (state
        (or (and existing (appkit-surface-model existing))
            (list :type 'notifications :account target :items nil
                  :phase 'initial :message nil :marking-p nil
                  :loaded-p nil :older-exhausted-p nil
                  :acknowledged-ids (make-hash-table :test #'equal)))))
    (misskey-open-surface :app app :identity id :mode
                          #'misskey-notifications-mode :buffer-name
                          "*misskey notifications*" :input state
                          :setup #'misskey-notifications--setup-view
                          :select t)))

(provide 'misskey-notifications)

;;; misskey-notifications.el ends here
