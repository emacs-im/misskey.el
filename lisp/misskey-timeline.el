;;; misskey-timeline.el --- Browse Misskey timelines -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch and render authenticated Misskey timelines through Appkit's view
;; lifecycle, keyed projection, and protocol-neutral discussion rows.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-discussion)
(require 'appkit-projection)
(require 'appkit-view)
(require 'appkit-position)
(require 'appkit-ui)
(require 'misskey-actions)
(require 'misskey-compose)
(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-media)
(require 'misskey-note)
(require 'misskey-render)
(require 'misskey-thread)

(defcustom misskey-timeline-limit 20
  "Maximum number of notes requested for each timeline page."
  :type 'integer
  :group 'misskey)

(defconst misskey-timeline--kind-specs
  '((home "Home" "notes/timeline")
    (local "Local" "notes/local-timeline")
    (social "Social" "notes/hybrid-timeline")
    (global "Global" "notes/global-timeline"))
  "Basic timeline kinds with display labels and API endpoints.")

(defconst misskey-timeline--request-key 'timeline
  "View request-table key for the active timeline transport.")


(defvar misskey-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'misskey-timeline-next-kind)
    (define-key map (kbd "g") #'misskey-timeline-refresh)
    (define-key map (kbd "n") #'appkit-discussion-next-entry)
    (define-key map (kbd "p") #'appkit-discussion-previous-entry)
    (define-key map (kbd "RET") #'misskey-render-toggle-content-warning)
    (define-key map (kbd "N") #'misskey-timeline-load-more)
    (define-key map (kbd "c") #'misskey-timeline-compose)
    (define-key map (kbd "t") #'misskey-thread-at-point)
    (define-key map (kbd "r") #'misskey-compose-reply-at-point)
    (define-key map (kbd "q") #'misskey-compose-quote-at-point)
    (define-key map (kbd "a") misskey-actions-map)
    map)
  "Keymap for `misskey-timeline-mode'.")


(define-derived-mode misskey-timeline-mode special-mode "Misskey-Timeline"
  "Major mode for authenticated Misskey timelines."
  (setq-local header-line-format
              '(:eval (misskey-timeline--header-line)))
  (setq-local line-spacing 0))

(defun misskey-timeline--make-state (account kind)
  "Return fresh canonical state for ACCOUNT's timeline KIND."
  (list :type 'timeline
        :account account
        :kind kind
        :items nil
        :phase 'initial
        :message nil
        :request-token nil
        :loaded-p nil
        :position nil
        :older-exhausted-p nil
        :revealed-content (make-hash-table :test #'equal)))

(defun misskey-timeline--feed-state (app kind)
  "Return APP's canonical state for timeline KIND."
  (misskey-timeline--kind-spec kind)
  (let* ((session (misskey--session app))
         (states (misskey--session-timeline-states session)))
    (or (gethash kind states)
        (puthash
         kind
         (misskey-timeline--make-state
          (misskey--session-account session) kind)
         states))))

(defun misskey-timeline--view-state (view)
  "Return VIEW's validated active timeline state."
  (let* ((state (appkit-view-state view))
         (session (misskey--session (appkit-view-app view))))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'timeline)
                 (assq (plist-get state :kind)
                       misskey-timeline--kind-specs)
                 (equal (plist-get state :account)
                        (misskey--session-account session))
                 (hash-table-p (plist-get state :revealed-content)))
      (error "Invalid Misskey timeline view state"))
    state))

(defun misskey-timeline--current-view ()
  "Return the current live Misskey timeline view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'timeline)))
    view))

(defun misskey-timeline--kind-spec (kind)
  "Return the timeline specification for KIND."
  (or (assq kind misskey-timeline--kind-specs)
      (error "Invalid Misskey timeline kind: %S" kind)))

(defun misskey-timeline--kind-label (kind)
  "Return the display label for timeline KIND."
  (cadr (misskey-timeline--kind-spec kind)))

(defun misskey-timeline--endpoint (kind)
  "Return the Misskey API endpoint for timeline KIND."
  (caddr (misskey-timeline--kind-spec kind)))

(defun misskey-timeline--next-kind (kind)
  "Return the timeline kind following KIND."
  (let ((tail (memq (misskey-timeline--kind-spec kind)
                    misskey-timeline--kind-specs)))
    (car (or (cadr tail) (car misskey-timeline--kind-specs)))))

(defun misskey-timeline--header-command (kind)
  "Return an interactive command that switches to timeline KIND."
  (lambda ()
    (interactive)
    (if-let* ((view (misskey-timeline--current-view)))
        (misskey-timeline--switch-kind view kind)
      (user-error "Current buffer is not a Misskey timeline"))))

(defun misskey-timeline--header-keymap (kind)
  "Return a header-line keymap that switches to timeline KIND."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line down-mouse-1] #'ignore)
    (define-key map [header-line mouse-1]
                (misskey-timeline--header-command kind))
    (define-key map [follow-link] 'mouse-face)
    map))

(defvar misskey-timeline--header-keymaps
  (mapcar (lambda (spec)
            (cons (car spec)
                  (misskey-timeline--header-keymap (car spec))))
          misskey-timeline--kind-specs)
  "Mouse keymaps for timeline items in the header line.")

(defun misskey-timeline--header-item (view kind label)
  "Return VIEW's header-line LABEL for timeline KIND."
  (let ((active (eq kind
                    (plist-get
                     (misskey-timeline--view-state view) :kind))))
    (propertize label
                'face (if active 'mode-line-emphasis 'shadow)
                'keymap (alist-get kind misskey-timeline--header-keymaps)
                'mouse-face 'mode-line-highlight
                'help-echo (format "Mouse-1: Open %s timeline" label)
                'follow-link 'ignore)))

(defun misskey-timeline--header-line ()
  "Return the current timeline header line."
  (when-let* ((view (misskey-timeline--current-view)))
    (concat
     " "
     (string-join
      (mapcar
       (lambda (spec)
         (misskey-timeline--header-item view (car spec) (cadr spec)))
       misskey-timeline--kind-specs)
      "   "))))


(defun misskey-timeline--frame (state)
  "Return the generated frame for timeline STATE."
  (let ((origin
         (misskey--account-origin (plist-get state :account)))
        (kind (plist-get state :kind))
        (phase (plist-get state :phase))
        (message (plist-get state :message))
        (items (plist-get state :items)))
    (concat
     (propertize
      (format "%s · %s" (misskey-timeline--kind-label kind) origin)
      'face 'bold)
     "\n"
     (pcase phase
       ('initial "Loading notes...\n\n")
       ('refresh "Refreshing notes...\n\n")
       ('older "Loading older notes...\n\n")
       ('error (format "Unable to load notes.\n%s\n\n" message))
       (_ (if items "\n" "No notes returned.\n\n"))))))

(defun misskey-timeline--position-intent (events)
  "Return the effective semantic position intent from EVENTS."
  (or (cl-loop for event in events
               when (eq (plist-get event :position) 'first)
               return 'first)
      (cl-loop for event in (reverse events)
               for position = (plist-get event :position)
               when (and position (not (eq position 'preserve)))
               return position)
      (cl-loop for event in (reverse events)
               for position = (plist-get event :position)
               when position return position)
      'preserve))

(defun misskey-timeline--sync (view invalidations)
  "Synchronize VIEW from coalesced INVALIDATIONS."
  (let* ((state (misskey-timeline--view-state view))
         (events (appkit-view-pending-events-snapshot view))
         (event-count (length events))
         (position (misskey-timeline--position-intent events))
         (parts (appkit-invalidations-parts invalidations))
         (geometry-p (memq 'geometry parts))
         (resources (appkit-invalidations-resource-keys invalidations))
         (all-resources-p (memq 'all resources))
         (entry-keys (appkit-invalidations-entry-keys invalidations))
         (reconcile-p
          (or geometry-p
              (appkit-invalidations-structure-p invalidations)
              entry-keys
              resources))
         (rows
          (and reconcile-p
               (misskey-render-project-notes
                (plist-get state :items) (appkit-view-app view))))
         (force-keys
          (append
           entry-keys
           (and (or geometry-p all-resources-p)
                (mapcar #'appkit-projection-row-key rows)))))
    (appkit-projection-sync
     view rows
     :header (misskey-timeline--frame state)
     :footer
     (concat "\ng refresh   TAB next timeline   n/p note   "
             (if (plist-get state :older-exhausted-p)
                 "older exhausted"
               "N older")
             "   RET reveal CW   c compose\n")
     :force-keys force-keys
     :changed-dependencies (and (not all-resources-p) resources)
     :position position
     :reconcile-p reconcile-p)
    (appkit-view-acknowledge-events view event-count)))

(defun misskey-timeline--setup-view (view)
  "Initialize VIEW's keyed timeline projection."
  (appkit-projection-ensure
   view
   :printer #'misskey-render-insert-row
   :anchor-property appkit-discussion-key-property
   :no-separator-p t)
  (appkit-view-enable-responsive-geometry view)
  (appkit-view-enqueue-event view (list :position 'first))
  (appkit-invalidate view :structure t :part 'frame :position t)
  (appkit-sync-invalidations view))


(defun misskey-timeline--new-notes (current candidates)
  "Return CANDIDATES whose IDs do not occur in CURRENT."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (note current)
      (puthash (misskey-note-id note) t seen))
    (dolist (note candidates (nreverse result))
      (let ((id (misskey-note-id note)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push note result))))))

(defun misskey-timeline--request-current-p (view state token)
  "Return non-nil when TOKEN may still update STATE in VIEW."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq token (plist-get state :request-token))))

(defun misskey-timeline--retire-request (view state token)
  "Retire VIEW's transport when TOKEN still owns STATE."
  (when (misskey-timeline--request-current-p view state token)
    (remhash misskey-timeline--request-key
             (appkit-view-request-table view))))

(defun misskey-timeline--handle-error (view state token failure)
  "Show FAILURE when TOKEN still owns STATE in VIEW."
  (when (misskey-timeline--request-current-p view state token)
    (setf (plist-get state :phase) 'error
          (plist-get state :message) failure
          (plist-get state :request-token) nil)
    (appkit-request-sync view :part 'frame :position t)
    (message "%s" failure)))

(defun misskey-timeline--handle-success
    (view state token phase payload)
  "Install PAYLOAD when TOKEN still owns STATE in VIEW for request PHASE."
  (when (misskey-timeline--request-current-p view state token)
    (condition-case err
        (let* ((notes (misskey-note-validate-list payload))
               (current (plist-get state :items))
               (new-notes
                (if (eq phase 'older)
                    (misskey-timeline--new-notes current notes)
                  notes))
               (installed
                (pcase phase
                  ('initial notes)
                  ('refresh
                   (append notes
                           (misskey-timeline--new-notes notes current)))
                  ('older (append current new-notes))
                  (_
                   (error
                    "Invalid Misskey timeline request phase: %S" phase)))))
          (dolist (note notes)
            (misskey-merge-note-state
             (appkit-view-app view) note (nth 1 token)))
          (unless (eq phase 'older)
            (clrhash (plist-get state :revealed-content)))
          (setf (plist-get state :items) installed
                (plist-get state :phase) 'ready
                (plist-get state :message) nil
                (plist-get state :request-token) nil
                (plist-get state :loaded-p) t)
          (pcase phase
            ('initial
             (setf (plist-get state :older-exhausted-p) (null notes)))
            ('older
             (setf (plist-get state :older-exhausted-p)
                   (null new-notes))))
          (appkit-view-enqueue-event
           view (list :position (if (eq phase 'initial)
                                    'first
                                  'preserve)))
          (appkit-request-sync
           view :structure t :part 'frame :position t)
          (misskey-media-prefetch-notes view new-notes)
          (if (eq phase 'older)
              (if new-notes
                  (message "Loaded %d older Misskey notes"
                           (length new-notes))
                (message "No older Misskey notes"))
            (message "Loaded %d Misskey notes" (length notes))))
      (error
       (misskey-timeline--handle-error
        view state token (error-message-string err))))))

(defun misskey-timeline--interrupt-state-request (state)
  "Retire STATE's request token and restore its settled phase."
  (when (plist-get state :request-token)
    (setf (plist-get state :request-token) nil
          (plist-get state :phase)
          (if (plist-get state :loaded-p) 'ready 'initial)
          (plist-get state :message) nil)))

(defun misskey-timeline--cancel-request (view)
  "Cancel VIEW's active timeline transport, if any."
  (let* ((table (appkit-view-request-table view))
         (request (gethash misskey-timeline--request-key table)))
    (remhash misskey-timeline--request-key table)
    (when request
      (misskey-http-cancel request))))

(defun misskey-timeline--request (view phase)
  "Start one timeline PHASE request owned by VIEW."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Misskey timeline request phase: %S" phase))
  (let* ((state (misskey-timeline--view-state view))
         (items (plist-get state :items)))
    (unless (and (integerp misskey-timeline-limit)
                 (<= 1 misskey-timeline-limit 100))
      (user-error "Misskey timeline limit must be between 1 and 100"))
    (when (plist-get state :request-token)
      (user-error "The Misskey timeline is already loading"))
    (when (eq phase 'older)
      (unless items
        (user-error "The Misskey timeline has no notes"))
      (when (plist-get state :older-exhausted-p)
        (user-error "No older Misskey notes available")))
    (let* ((token
            (list phase
                  (misskey-state-observe (appkit-view-app view))))
           (account (plist-get state :account))
           (kind (plist-get state :kind))
           (until-id
            (and (eq phase 'older)
                 (misskey-note-id (car (last items)))))
           callback-ran-p
           request)
      (unless (or (not (eq phase 'older)) until-id)
        (error "Misskey timeline has no older-page cursor"))
      (setf (plist-get state :request-token) token
            (plist-get state :phase) phase
            (plist-get state :message) nil)
      (appkit-request-sync view :part 'frame :position t)
      (setq
       request
       (misskey-http-read
        (misskey-timeline--endpoint kind)
        (append
         (list :limit misskey-timeline-limit :allowPartial t)
         (and until-id (list :untilId until-id)))
        (lambda (payload)
          (setq callback-ran-p t)
          (misskey-timeline--retire-request view state token)
          (misskey-timeline--handle-success
           view state token phase payload))
        :errback
        (lambda (failure)
          (setq callback-ran-p t)
          (misskey-timeline--retire-request view state token)
          (misskey-timeline--handle-error view state token failure))
        :owner view
        :account account))
      (cond
       ((and (not callback-ran-p)
             request
             (misskey-timeline--request-current-p view state token))
        (puthash misskey-timeline--request-key request
                 (appkit-view-request-table view)))
       ((and (not callback-ran-p)
             (null request)
             (misskey-timeline--request-current-p view state token))
        (misskey-timeline--handle-error
         view state token "Misskey timeline request did not start")))
      request)))

(defun misskey-timeline--capture-position (view)
  "Return VIEW's semantic position snapshot."
  (with-current-buffer (appkit-view-buffer view)
    (appkit-position-capture
     :anchor-property appkit-discussion-key-property
     :preserve-window-start t)))

(defun misskey-timeline--switch-kind (view kind &optional refresh-p)
  "Switch timeline VIEW to KIND.

When REFRESH-P is non-nil, refresh KIND after switching."
  (let* ((state (misskey-timeline--view-state view))
         (current (plist-get state :kind))
         (target
          (misskey-timeline--feed-state
           (appkit-view-app view) kind)))
    (unless (eq current kind)
      (when (plist-get state :items)
        (setf (plist-get state :position)
              (misskey-timeline--capture-position view)))
      ;; Revoke the token before cancellation synchronously delivers its
      ;; errback at the transport boundary.
      (misskey-timeline--interrupt-state-request state)
      (misskey-timeline--cancel-request view)
      (misskey-timeline--interrupt-state-request target)
      (setf (appkit-view-state view) target
            (appkit-view-pending-events view) nil)
      (appkit-view-enqueue-event
       view (list :position (or (plist-get target :position) 'first)))
      (appkit-request-sync
       view :structure t :part 'frame :position t)
      (force-mode-line-update))
    (let ((active (misskey-timeline--view-state view)))
      (unless (plist-get active :request-token)
        (when (or refresh-p (not (plist-get active :loaded-p)))
          (misskey-timeline--request
           view
           (if (plist-get active :loaded-p) 'refresh 'initial)))))
    view))

(defun misskey-timeline--refresh-view (view)
  "Refresh live Misskey timeline VIEW."
  (let ((state (misskey-timeline--view-state view)))
    (misskey-timeline--request
     view (if (plist-get state :loaded-p) 'refresh 'initial))))

(defun misskey-timeline-refresh ()
  "Refresh the current Misskey timeline."
  (interactive)
  (if-let* ((view (misskey-timeline--current-view)))
      (misskey-timeline--refresh-view view)
    (user-error "Current buffer is not a Misskey timeline")))

(defun misskey-timeline-load-more ()
  "Load one older page in the current Misskey timeline."
  (interactive)
  (if-let* ((view (misskey-timeline--current-view)))
      (misskey-timeline--request view 'older)
    (user-error "Current buffer is not a Misskey timeline")))

(defun misskey-timeline-next-kind ()
  "Switch to the next basic Misskey timeline."
  (interactive)
  (if-let* ((view (misskey-timeline--current-view)))
      (misskey-timeline--switch-kind
       view
       (misskey-timeline--next-kind
        (plist-get (misskey-timeline--view-state view) :kind)))
    (user-error "Current buffer is not a Misskey timeline")))


(defun misskey-timeline-compose ()
  "Open a compose buffer for the current timeline account."
  (interactive)
  (if-let* ((view (misskey-timeline--current-view)))
      (misskey-compose-open
       (plist-get (misskey-timeline--view-state view) :account))
    (user-error "Current buffer is not a Misskey timeline")))

(defun misskey-timeline-open (&optional kind)
  "Open the selected account's timeline buffer at KIND.

KIND defaults to `home'.  Opening an existing buffer refreshes KIND."
  (let* ((kind (or kind 'home))
         (_spec (misskey-timeline--kind-spec kind))
         (account (misskey--current-account))
         (app (misskey-app account))
         (id 'timeline)
         (existing (appkit-view-for-id app id))
         (state
          (or (and existing (appkit-view-state existing))
              (misskey-timeline--feed-state app kind))))
    (unless existing
      (misskey-timeline--interrupt-state-request state))
    (let ((view
           (appkit-open-view
            :app app
            :id id
            :mode #'misskey-timeline-mode
            :buffer-name
            (format "*misskey: %s@%s*"
                    (misskey--account-auth-source-user account)
                    (string-remove-prefix
                     "https://" (misskey--account-origin account)))
            :state state
            :sync-function #'misskey-timeline--sync
            :parts '(frame entries)
            :position-policy appkit-discussion-key-property
            :setup #'misskey-timeline--setup-view
            :select t)))
      (misskey-timeline--switch-kind view kind t)
      view)))

(provide 'misskey-timeline)

;;; misskey-timeline.el ends here
