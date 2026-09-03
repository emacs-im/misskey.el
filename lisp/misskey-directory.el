;;; misskey-directory.el --- Browse Misskey user relationships -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Present followers and following relationships as stable Appkit directory
;; entries keyed by Misskey user ID, while retaining relationship IDs solely as
;; pagination cursors.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-directory)
(require 'misskey-actions)
(require 'appkit-invalidation)
(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-note)
(require 'misskey-profile)

(defcustom misskey-directory-limit 30
  "Maximum number of relationships requested per directory page."
  :type 'integer
  :group 'misskey)

(defconst misskey-directory--kind-specs
  '((followers "Followers" "users/followers" follower)
    (following "Following" "users/following" followee))
  "Relationship kinds with labels, endpoints, and populated user fields.")

(defconst misskey-directory--request-key 'relationships
  "Operation key for the active relationship request.")

(defvar-keymap misskey-directory-mode-map
  :doc "Keymap for `misskey-directory-mode'."
  :parent appkit-directory-mode-map
  "g" #'misskey-directory-refresh
  "a" misskey-actions-map
  "N" #'misskey-directory-load-more)

(define-derived-mode misskey-directory-mode appkit-directory-mode
  "Misskey-Directory"
  "Major mode for a Misskey followers or following directory.")

(defun misskey-directory--kind-spec (kind)
  "Return the relationship specification for KIND."
  (or (assq kind misskey-directory--kind-specs)
      (error "Invalid Misskey relationship kind: %S" kind)))

(defun misskey-directory--kind-label (kind)
  "Return the display label for relationship KIND."
  (cadr (misskey-directory--kind-spec kind)))

(defun misskey-directory--endpoint (kind)
  "Return the API endpoint for relationship KIND."
  (nth 2 (misskey-directory--kind-spec kind)))

(defun misskey-directory--user-field (kind)
  "Return the populated user field for relationship KIND."
  (nth 3 (misskey-directory--kind-spec kind)))

(defun misskey-directory--state (view)
  "Return VIEW's validated relationship state."
  (let ((state (and (appkit-view-p view) (appkit-view-state view))))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'relationship-directory)
                 (assq (plist-get state :kind)
                       misskey-directory--kind-specs)
                 (misskey--account-p (plist-get state :account))
                 (consp (plist-get state :subject-user)))
      (error "Invalid Misskey relationship directory state"))
    state))

(defun misskey-directory--current-view ()
  "Return the current live relationship directory view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'relationship-directory)))
    view))

(defun misskey-directory--relationship-user (state relationship)
  "Return the populated user from RELATIONSHIP according to STATE."
  (alist-get (misskey-directory--user-field (plist-get state :kind))
             relationship))

(defun misskey-directory--validate-payload (state payload)
  "Return validated relationship PAYLOAD for STATE."
  (unless (listp payload)
    (error "Misskey relationship response is not a list"))
  (let ((relationship-ids (make-hash-table :test #'equal))
        (user-ids (make-hash-table :test #'equal)))
    (dolist (relationship payload)
      (let* ((relationship-id (and (consp relationship)
                                   (alist-get 'id relationship)))
             (user (and (consp relationship)
                        (misskey-directory--relationship-user
                         state relationship)))
             (user-id (misskey-user-id user)))
        (unless (and (stringp relationship-id)
                     (not (string-empty-p relationship-id))
                     (consp user)
                     (stringp user-id)
                     (not (string-empty-p user-id)))
          (error "Misskey returned a malformed relationship"))
        (when (gethash relationship-id relationship-ids)
          (error "Misskey duplicated relationship %s" relationship-id))
        (when (gethash user-id user-ids)
          (error "Misskey duplicated relationship user %s" user-id))
        (puthash relationship-id t relationship-ids)
        (puthash user-id t user-ids))))
  payload)

(defun misskey-directory--new-relationships (current candidates)
  "Return CANDIDATES whose user IDs do not occur in CURRENT."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (relationship current)
      (puthash
       (misskey-user-id
        (or (alist-get 'follower relationship)
            (alist-get 'followee relationship)))
       t seen))
    (dolist (relationship candidates (nreverse result))
      (let ((id
             (misskey-user-id
              (or (alist-get 'follower relationship)
                  (alist-get 'followee relationship)))))
        (unless (gethash id seen)
          (puthash id t seen)
          (push relationship result))))))

(defun misskey-directory--insert-user (_surface entry)
  "Insert the Misskey user carried by directory ENTRY."
  (let* ((user (appkit-directory-entry-payload entry))
         (view (misskey-directory--current-view))
         (app (and view (appkit-view-app view)))
         (following-p
          (and (appkit-app-live-p app)
               (misskey-user-state-value
                app (misskey-user-id user) :following-p
                (eq (alist-get 'isFollowing user) t))))
         (notes (alist-get 'notesCount user))
         (followers (alist-get 'followersCount user)))
    (insert (misskey-user-label user)
            "  "
            (propertize (misskey-user-handle user) 'face 'shadow))
    (when following-p
      (insert (propertize "  following" 'face 'success)))
    (when (and (integerp notes) (>= notes 0))
      (insert (propertize (format "  %d notes" notes) 'face 'shadow)))
    (when (and (integerp followers) (>= followers 0))
      (insert (propertize (format "  %d followers" followers)
                          'face 'shadow)))
    (insert "\n")))

(defun misskey-directory--activate-user (_surface entry)
  "Open the profile for the user carried by directory ENTRY."
  (let ((view (misskey-directory--current-view)))
    (unless view
      (error "No live Misskey relationship directory"))
    (misskey-profile-open
     (appkit-directory-entry-payload entry)
     (plist-get (misskey-directory--state view) :account))))

(defun misskey-directory--project (view state)
  "Project relationship STATE for VIEW into Appkit directory entries."
  (let* ((kind (plist-get state :kind))
         (subject (plist-get state :subject-user))
         (phase (plist-get state :phase))
         (message (plist-get state :message))
         (items (plist-get state :items))
         (section-key (list 'relationship kind
                            (misskey-user-id subject)))
         (entries
          (list
           (appkit-directory-entry-create
            :key section-key :role 'section
            :label (format "%s · %s"
                           (misskey-directory--kind-label kind)
                           (misskey-user-label subject))))))
    (when (memq phase '(initial refresh older error))
      (setq entries
            (nconc
             entries
             (list
              (appkit-directory-entry-create
               :key (list section-key 'status) :role 'note
               :section-key section-key :indent 2
               :face (if (eq phase 'error) 'error 'shadow)
               :label
               (pcase phase
                 ('initial "Loading users...")
                 ('refresh "Refreshing users...")
                 ('older "Loading more users...")
                 ('error (format "Unable to load users: %s" message))))))))
    (dolist (relationship items)
      (let* ((user (misskey-directory--relationship-user state relationship))
             (user-id (misskey-user-id user))
             (following-p
              (misskey-user-state-value
               (appkit-view-app view) user-id :following-p
               (eq (alist-get 'isFollowing user) t))))
        (setq entries
              (nconc
               entries
               (list
                (appkit-directory-entry-create
                 :key user-id :role 'item :section-key section-key
                 :item-p t :payload user :stamp (list user following-p)
                 :help-echo "RET: Open Misskey profile"
                 :properties
                 (list misskey-user-property user
                       misskey-user-id-property user-id)))))))
    (unless (or items (memq phase '(initial refresh)))
      (setq entries
            (nconc
             entries
             (list
              (appkit-directory-entry-create
               :key (list section-key 'empty) :role 'note
               :section-key section-key :indent 2 :face 'shadow
               :label "No users returned.")))))
    (when items
      (setq entries
            (nconc
             entries
             (list
              (appkit-directory-entry-create
               :key (list section-key 'footer) :role 'note
               :section-key section-key :indent 2 :face 'shadow
               :label
               (if (plist-get state :older-exhausted-p)
                   "No more users.  g refresh"
                 "N load more   g refresh"))))))
    entries))

(defun misskey-directory--sync (view _invalidations _events)
  "Synchronize relationship directory VIEW."
  (with-current-buffer (appkit-view-buffer view)
    (appkit-directory-reconcile
     (appkit-directory-surface)
     (misskey-directory--project
      view (misskey-directory--state view)))))

(defun misskey-directory--handle-error (view state failure)
  "Install relationship FAILURE in VIEW STATE."
  (setf (plist-get state :phase) 'error
        (plist-get state :message) failure)
  (appkit-request-sync view :structure t :part 'directory)
  (message "%s" failure))

(defun misskey-directory--handle-success
    (view state observation phase payload)
  "Install relationship PAYLOAD for PHASE in VIEW STATE.

OBSERVATION versions canonical entity merges."
  (condition-case err
      (let* ((relationships
              (misskey-directory--validate-payload state payload))
             (current (plist-get state :items))
             (new
              (if (eq phase 'older)
                  (misskey-directory--new-relationships
                   current relationships)
                relationships)))
        (dolist (relationship relationships)
          (misskey-merge-user-state
           (appkit-view-app view)
           (misskey-directory--relationship-user state relationship)
           observation))
        (setf (plist-get state :items)
              (if (eq phase 'older) (append current new) relationships)
              (plist-get state :phase) 'ready
              (plist-get state :message) nil
              (plist-get state :loaded-p) t)
        (setf (plist-get state :older-exhausted-p)
              (if (eq phase 'older)
                  (null new)
                (null relationships)))
        (appkit-request-sync view :structure t :part 'directory)
        (message (if (eq phase 'older)
                     "Loaded %d more Misskey users"
                   "Loaded %d Misskey users")
                 (length new)))
    (error
     (misskey-directory--handle-error
      view state (error-message-string err)))))

(defun misskey-directory--request (view phase)
  "Start relationship VIEW request for PHASE."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Misskey relationship phase: %S" phase))
  (let ((state (misskey-directory--state view)))
    (when (and (eq phase 'older)
               (plist-get state :older-exhausted-p))
      (user-error "No more Misskey users available"))
    (let ((parameters
           (list :userId (misskey-user-id (plist-get state :subject-user))
                 :limit misskey-directory-limit)))
      (when (eq phase 'older)
        (let* ((last-relationship
                (car (last (plist-get state :items))))
               (cursor (and last-relationship
                            (alist-get 'id last-relationship))))
          (unless last-relationship
            (user-error "The Misskey directory has no users"))
          (unless (and (stringp cursor) (not (string-empty-p cursor)))
            (user-error "The last Misskey relationship has no valid ID"))
          (setq parameters (plist-put parameters :untilId cursor))))
      (let* ((observation (misskey-state-observe (appkit-view-app view)))
             (operation
              (appkit-view-operation-begin
               view misskey-directory--request-key)))
        (setf (plist-get state :phase) phase
              (plist-get state :message) nil)
        (appkit-request-sync view :structure t :part 'directory)
        (misskey-http-read
         (misskey-directory--endpoint (plist-get state :kind))
         parameters
         (lambda (payload)
           (when (appkit-view-operation-finish operation)
             (misskey-directory--handle-success
              view state observation phase payload)))
         :errback
         (lambda (failure)
           (when (appkit-view-operation-finish operation)
             (misskey-directory--handle-error view state failure)))
         :account (plist-get state :account)
         :owner operation)))))

(defun misskey-directory--setup-view (view)
  "Initialize relationship directory VIEW."
  (with-current-buffer (appkit-view-buffer view)
    (appkit-directory-configure
     (appkit-directory-surface)
     :item-inserter #'misskey-directory--insert-user
     :activate-function #'misskey-directory--activate-user))
  (appkit-invalidate view :structure t :part 'directory :position t)
  (appkit-sync-invalidations view)
  (misskey-directory--request view 'initial))

(defun misskey-directory-refresh ()
  "Refresh the current Misskey relationship directory."
  (interactive)
  (if-let* ((view (misskey-directory--current-view)))
      (misskey-directory--request
       view (if (plist-get (misskey-directory--state view) :loaded-p)
                'refresh
              'initial))
    (user-error "Current buffer is not a Misskey user directory")))

(defun misskey-directory-load-more ()
  "Load another page in the current Misskey relationship directory."
  (interactive)
  (if-let* ((view (misskey-directory--current-view)))
      (misskey-directory--request view 'older)
    (user-error "Current buffer is not a Misskey user directory")))

(cl-defun misskey-directory-open (kind user &optional account)
  "Open relationship KIND for USER under ACCOUNT."
  (let* ((target (or account (misskey--current-account)))
         (user-id (misskey-user-id user)))
    (misskey-directory--kind-spec kind)
    (unless (and (stringp user-id) (not (string-empty-p user-id)))
      (user-error "Misskey relationship subject has no stable user ID"))
    (let* ((app (misskey-app target))
           (id (list 'relationship kind user-id))
           (existing (appkit-view-for-id app id))
           (state
            (or (and existing (appkit-view-state existing))
                (list :type 'relationship-directory
                      :account target :kind kind :subject-user user
                      :items nil :phase 'initial :message nil
                      :loaded-p nil :older-exhausted-p nil)))
           (view
            (appkit-open-view
             :app app :id id :mode #'misskey-directory-mode
             :buffer-name
             (format "*misskey %s %s*"
                     (downcase (misskey-directory--kind-label kind))
                     (misskey-user-handle user))
             :state state :sync-function #'misskey-directory--sync
             :parts '(directory) :position-policy 'semantic
             :setup #'misskey-directory--setup-view :select t)))
      view)))

(defun misskey-profile-open-followers (user &optional account)
  "Open USER's followers directory for ACCOUNT."
  (misskey-directory-open 'followers user account))

(defun misskey-profile-open-following (user &optional account)
  "Open USER's following directory for ACCOUNT."
  (misskey-directory-open 'following user account))

(provide 'misskey-directory)

;;; misskey-directory.el ends here
