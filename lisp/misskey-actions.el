;;; misskey-actions.el --- Misskey note and user actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Route note and user mutations through one authenticated POST path.  Remote
;; success installs account-scoped state overrides and invalidates matching
;; resources across every live Appkit view; failure never guesses the outcome
;; or retries a write.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-note)

(defvar-keymap misskey-actions-map
  :doc "Prefix map for Misskey note and user write actions."
  "r" #'misskey-react-at-point
  "R" #'misskey-unreact-at-point
  "f" #'misskey-favorite-at-point
  "F" #'misskey-unfavorite-at-point
  "n" #'misskey-renote-at-point
  "d" #'misskey-delete-note-at-point
  "+" #'misskey-follow-at-point
  "-" #'misskey-unfollow-at-point)

(defun misskey-actions--view ()
  "Return the current live Misskey Appkit view."
  (or (when-let* ((view (appkit-current-view))
                  ((appkit-view-live-p view))
                  (state (appkit-view-state view))
                  ((misskey--account-p (plist-get state :account))))
        view)
      (user-error "Current buffer has no live Misskey view")))

(defun misskey-actions--note-at-point (&optional display-note-p)
  "Return the Misskey note at point.

When DISPLAY-NOTE-P is non-nil, unwrap a pure renote."
  (let ((note (get-text-property (point) misskey-note-property)))
    (unless (consp note)
      (user-error "No Misskey note at point"))
    (if display-note-p (misskey-note-display-note note) note)))

(defun misskey-actions--user-at-point ()
  "Return the Misskey user at point."
  (or (get-text-property (point) misskey-user-property)
      (user-error "No Misskey user at point")))

(cl-defstruct (misskey-actions--lane
               (:constructor misskey-actions--lane-create))
  "One serialized per-target mutation lane."
  key
  app
  account
  requested-action
  value
  request
  token
  desired-target
  desired-action
  desired-value
  desired-callback)

(defun misskey-actions--user-action-p (action)
  "Return non-nil when ACTION targets a user."
  (memq action '(follow unfollow cancel-follow)))

(defun misskey-actions--target-id (action target)
  "Return ACTION TARGET's stable identity."
  (let ((id (if (misskey-actions--user-action-p action)
                (misskey-user-id target)
              (misskey-note-id target))))
    (unless (and (stringp id) (not (string-empty-p id)))
      (error "Misskey action target has no stable identity"))
    id))

(defun misskey-actions--validate-intent (action value)
  "Validate queued ACTION and VALUE without constructing a request."
  (unless (memq action
                '(react unreact favorite unfavorite renote delete-note
                  follow unfollow cancel-follow))
    (error "Unknown Misskey action: %S" action))
  (when (and (eq action 'react)
             (not (and (stringp value) (not (string-empty-p value)))))
    (user-error "Misskey reaction cannot be empty")))

(defun misskey-actions--spec (action target value)
  "Return endpoint and parameters for ACTION on TARGET with VALUE."
  (let ((id (misskey-actions--target-id action target)))
    (pcase action
      ('react
       (unless (and (stringp value) (not (string-empty-p value)))
         (user-error "Misskey reaction cannot be empty"))
       (list "notes/reactions/create" (list :noteId id :reaction value)))
      ('unreact (list "notes/reactions/delete" (list :noteId id)))
      ('favorite (list "notes/favorites/create" (list :noteId id)))
      ('unfavorite (list "notes/favorites/delete" (list :noteId id)))
      ('renote (list "notes/create" (list :renoteId id)))
      ('delete-note (list "notes/delete" (list :noteId id)))
      ('follow (list "following/create" (list :userId id)))
      ('unfollow (list "following/delete" (list :userId id)))
      ('cancel-follow
       (list "following/requests/cancel" (list :userId id)))
      (_ (error "Unknown Misskey action: %S" action)))))

(defun misskey-actions--integer (value)
  "Return nonnegative integer VALUE, or zero."
  (if (and (integerp value) (>= value 0)) value 0))

(defun misskey-actions--apply-note-success (app action note value)
  "Apply successful ACTION for NOTE with VALUE under APP."
  (let ((id (misskey-note-id note)))
    (pcase action
      ('react
       (let* ((old
               (misskey-note-state-value
                app id :my-reaction (alist-get 'myReaction note)))
              (count
               (misskey-actions--integer
                (misskey-note-state-value
                 app id :reaction-count (alist-get 'reactionCount note)))))
         (misskey-set-note-state-values
          app id :my-reaction value
          :reaction-count (if old count (1+ count)))))
      ('unreact
       (let* ((old
               (misskey-note-state-value
                app id :my-reaction (alist-get 'myReaction note)))
              (count
               (misskey-actions--integer
                (misskey-note-state-value
                 app id :reaction-count (alist-get 'reactionCount note)))))
         (misskey-set-note-state-values
          app id :my-reaction nil
          :reaction-count (if old (max 0 (1- count)) count))))
      ('favorite
       (misskey-set-note-state-values app id :favorited-p t))
      ('unfavorite
       (misskey-set-note-state-values app id :favorited-p nil))
      ('renote
       (let ((count
              (misskey-actions--integer
               (misskey-note-state-value
                app id :renote-count (alist-get 'renoteCount note)))))
         (misskey-set-note-state-values app id :renote-count (1+ count))))
      ('delete-note
       (misskey-set-note-state-values app id :deleted-p t)))))

(defun misskey-actions--response-user (target payload)
  "Return PAYLOAD with TARGET's identity when it carries relationship fields."
  (when (and (consp payload)
             (or (assq 'isFollowing payload)
                 (assq 'hasPendingFollowRequestFromYou payload)))
    (if (assq 'id payload)
        payload
      (cons (cons 'id (misskey-user-id target)) payload))))

(defun misskey-actions--apply-user-success
    (app action target payload)
  "Apply successful user ACTION for TARGET under APP using authoritative PAYLOAD."
  (let ((id (misskey-user-id target)))
    (pcase action
      ('follow
       (if (eq (alist-get 'isLocked target) t)
           (misskey-set-user-state-values
            app id :following-p nil :follow-pending-p t)
         (misskey-set-user-state-values
          app id :following-p t :follow-pending-p nil)))
      ((or 'unfollow 'cancel-follow)
       (misskey-set-user-state-values
        app id :following-p nil :follow-pending-p nil)))
    (when-let* ((user (misskey-actions--response-user target payload)))
      (misskey-merge-user-state app user (misskey-state-observe app)))))

(defun misskey-actions--apply-success
    (app action target value payload)
  "Apply successful ACTION for TARGET with VALUE and response PAYLOAD under APP."
  (if (misskey-actions--user-action-p action)
      (misskey-actions--apply-user-success app action target payload)
    (misskey-actions--apply-note-success app action target value)))

(defun misskey-actions--label (action)
  "Return a readable status label for ACTION."
  (alist-get
   action
   '((react . "Reaction added")
     (unreact . "Reaction removed")
     (favorite . "Note favorited")
     (unfavorite . "Favorite removed")
     (renote . "Note renoted")
     (delete-note . "Note deleted")
     (follow . "Follow request sent")
     (unfollow . "User unfollowed")
     (cancel-follow . "Follow request canceled"))))

(defun misskey-actions--lane-kind (action)
  "Return the mutation lane kind shared by ACTION and its inverse."
  (cond
   ((memq action '(react unreact)) 'reaction)
   ((memq action '(favorite unfavorite)) 'favorite)
   ((misskey-actions--user-action-p action) 'follow)
   (t action)))

(defun misskey-actions--effective-action (app action target)
  "Resolve requested ACTION for TARGET against APP's current relationship."
  (if (and (eq action 'unfollow)
           (misskey-user-state-value
            app (misskey-user-id target) :follow-pending-p
            (eq (alist-get 'hasPendingFollowRequestFromYou target) t)))
      'cancel-follow
    action))

(defun misskey-actions--fence (app action target)
  "Fence APP's server-backed state affected by pending ACTION on TARGET."
  (let ((id (misskey-actions--target-id action target)))
    (pcase action
      ((or 'react 'unreact)
       (misskey-fence-note-state
        app id :my-reaction :reaction-count))
      ('renote
       (misskey-fence-note-state app id :renote-count))
      ((or 'follow 'unfollow 'cancel-follow)
       (misskey-fence-user-state
        app id :following-p :follow-pending-p)))))

(defun misskey-actions--same-intent-p (lane)
  "Return non-nil when LANE's desired intent equals its dispatched intent."
  (and (eq (misskey-actions--lane-desired-action lane)
           (misskey-actions--lane-requested-action lane))
       (equal (misskey-actions--lane-desired-value lane)
              (misskey-actions--lane-value lane))))

(defun misskey-actions--invoke-callback (callback payload)
  "Invoke optional action CALLBACK with PAYLOAD without corrupting lane state."
  (when callback
    (condition-case err
        (funcall callback payload)
      (error
       (message "Misskey action callback failed: %s"
                (error-message-string err))))))


(defun misskey-actions--dispatch-lane (lane)
  "Dispatch LANE's latest desired mutation."
  (let* ((app (misskey-actions--lane-app lane))
         (table (appkit-app-request-table app))
         (key (misskey-actions--lane-key lane))
         (target (misskey-actions--lane-desired-target lane))
         (requested (misskey-actions--lane-desired-action lane))
         (action (misskey-actions--effective-action app requested target))
         (value (misskey-actions--lane-desired-value lane))
         (callback (misskey-actions--lane-desired-callback lane))
         (operation (cons action nil))
         (spec (misskey-actions--spec action target value))
         callback-ran-p
         request)
    (setf (misskey-actions--lane-requested-action lane) requested
          (misskey-actions--lane-value lane) value
          (misskey-actions--lane-token lane) operation
          (misskey-actions--lane-desired-target lane) nil
          (misskey-actions--lane-desired-action lane) nil
          (misskey-actions--lane-desired-value lane) nil
          (misskey-actions--lane-desired-callback lane) nil)
    (misskey-actions--fence app action target)
    (setq
     request
     (misskey-http-post
      (car spec) (cadr spec)
      (lambda (payload)
        (setq callback-ran-p t)
        (when (and (appkit-app-live-p app)
                   (eq lane (gethash key table))
                   (eq operation (misskey-actions--lane-token lane)))
          (let ((applied-p t)
                desired-callback)
            (condition-case err
                (misskey-actions--apply-success
                 app action target value payload)
              (error
               (setq applied-p nil)
               (message "Misskey action state update failed: %s"
                        (error-message-string err))))
            ;; Settle or advance the lane before arbitrary client code runs.
            (if (misskey-actions--lane-desired-action lane)
                (if (misskey-actions--same-intent-p lane)
                    (progn
                      (setq desired-callback
                            (misskey-actions--lane-desired-callback lane))
                      (remhash key table))
                  (condition-case err
                      (misskey-actions--dispatch-lane lane)
                    (error
                     (remhash key table)
                     (message "Misskey queued action failed: %s"
                              (error-message-string err)))))
              (remhash key table))
            (when applied-p
              (misskey-actions--invoke-callback callback payload)
              (misskey-actions--invoke-callback desired-callback payload)
              (message "%s" (misskey-actions--label action))))))
      :errback
      (lambda (failure)
        (setq callback-ran-p t)
        (when (and (appkit-app-live-p app)
                   (eq lane (gethash key table))
                   (eq operation (misskey-actions--lane-token lane)))
          (message "%s" failure)
          (if (and (misskey-actions--lane-desired-action lane)
                   (not (misskey-actions--same-intent-p lane)))
              (condition-case err
                  (misskey-actions--dispatch-lane lane)
                (error
                 (remhash key table)
                 (message "Misskey queued action failed: %s"
                          (error-message-string err))))
            (remhash key table))))
      :account (misskey-actions--lane-account lane)
      :owner app))
    (when (and request (not callback-ran-p)
               (eq lane (gethash key table))
               (eq operation (misskey-actions--lane-token lane)))
      (setf (misskey-actions--lane-request lane) request))
    (when (and (null request) (not callback-ran-p)
               (eq lane (gethash key table))
               (eq operation (misskey-actions--lane-token lane)))
      (remhash key table))
    request))

(cl-defun misskey-actions-perform
    (action target &key value account callback)
  "Serialize ACTION for TARGET with optional VALUE under ACCOUNT.

CALLBACK receives the successful response payload after the lane settles.
Inverse mutations share a per-target lane.  A later call replaces the queued
intent but never dispatches concurrently with the lane's active write."
  (misskey-actions--validate-intent action value)
  (let* ((target-account (or account (misskey--current-account)))
         (app (misskey-app target-account))
         (id (misskey-actions--target-id action target))
         (key (list 'misskey-action
                    (misskey-actions--lane-kind action) id))
         (table (appkit-app-request-table app))
         (lane (gethash key table)))
    (if lane
        (progn
          (setf (misskey-actions--lane-desired-target lane) target
                (misskey-actions--lane-desired-action lane) action
                (misskey-actions--lane-desired-value lane) value
                (misskey-actions--lane-desired-callback lane) callback)
          (misskey-actions--lane-request lane))
      (setq lane
            (misskey-actions--lane-create
             :key key :app app :account target-account
             :desired-target target :desired-action action
             :desired-value value :desired-callback callback))
      (puthash key lane table)
      (misskey-actions--dispatch-lane lane))))

(defun misskey-actions--perform-note (action &optional value raw-note-p)
  "Perform note ACTION with VALUE at point.

RAW-NOTE-P non-nil targets a pure-renote wrapper instead of its displayed note."
  (let* ((view (misskey-actions--view))
         (note (misskey-actions--note-at-point (not raw-note-p))))
    (misskey-actions-perform
     action note :value value
     :account (plist-get (appkit-view-state view) :account))))

(defun misskey-react-at-point (reaction)
  "Add REACTION to the displayed Misskey note at point."
  (interactive "sReaction: ")
  (misskey-actions--perform-note 'react reaction))

(defun misskey-unreact-at-point ()
  "Remove the current account's reaction from the note at point."
  (interactive)
  (misskey-actions--perform-note 'unreact))

(defun misskey-favorite-at-point ()
  "Add the displayed Misskey note at point to private favorites."
  (interactive)
  (misskey-actions--perform-note 'favorite))

(defun misskey-unfavorite-at-point ()
  "Remove the displayed Misskey note at point from private favorites."
  (interactive)
  (misskey-actions--perform-note 'unfavorite))

(defun misskey-renote-at-point ()
  "Create a pure renote of the displayed Misskey note at point."
  (interactive)
  (misskey-actions--perform-note 'renote))

(defun misskey-delete-note-at-point ()
  "Delete the exact Misskey note row at point."
  (interactive)
  (let ((note (misskey-actions--note-at-point)))
    (unless (yes-or-no-p (format "Delete Misskey note %s? "
                                 (misskey-note-id note)))
      (user-error "Delete canceled"))
    (misskey-actions--perform-note 'delete-note nil t)))

(defun misskey-actions--perform-user (action)
  "Perform user ACTION at point."
  (let ((view (misskey-actions--view)))
    (misskey-actions-perform
     action (misskey-actions--user-at-point)
     :account (plist-get (appkit-view-state view) :account))))

(defun misskey-follow-at-point ()
  "Follow the Misskey user at point."
  (interactive)
  (misskey-actions--perform-user 'follow))

(defun misskey-unfollow-at-point ()
  "Unfollow the Misskey user at point."
  (interactive)
  (misskey-actions--perform-user 'unfollow))

(provide 'misskey-actions)

;;; misskey-actions.el ends here
