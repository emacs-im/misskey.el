;;; misskey-thread.el --- Browse Misskey note threads -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch a focused note, its visible ancestor chain, and direct replies into a
;; dedicated Appkit projection using the shared Misskey note renderer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-discussion)
(require 'appkit-invalidation)
(require 'appkit-projection)
(require 'misskey-actions)
(require 'misskey-compose)
(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-media)
(require 'misskey-note)
(require 'misskey-render)

(defcustom misskey-thread-reply-limit 20
  "Maximum number of direct replies requested per thread page."
  :type 'integer
  :group 'misskey)

(defconst misskey-thread--request-key 'thread
  "View request-table key for the active thread request.")

(defvar misskey-thread-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'misskey-thread-refresh)
    (define-key map (kbd "N") #'misskey-thread-load-more)
    (define-key map (kbd "n") #'appkit-discussion-next-entry)
    (define-key map (kbd "p") #'appkit-discussion-previous-entry)
    (define-key map (kbd "RET") #'misskey-render-toggle-content-warning)
    (define-key map (kbd "r") #'misskey-compose-reply-at-point)
    (define-key map (kbd "q") #'misskey-compose-quote-at-point)
    (define-key map (kbd "a") misskey-actions-map)
    map)
  "Keymap for `misskey-thread-mode'.")

(define-derived-mode misskey-thread-mode special-mode "Misskey-Thread"
  "Major mode for one Misskey note thread."
  (setq-local header-line-format nil)
  (setq-local line-spacing 0))

(defun misskey-thread--state (view)
  "Return VIEW's validated thread state."
  (let ((state (and (appkit-view-live-p view) (appkit-view-state view))))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'thread)
                 (let ((focus-id (plist-get state :focus-id)))
                   (and (stringp focus-id)
                        (not (string-empty-p focus-id))))
                 (hash-table-p (plist-get state :revealed-content)))
      (error "Invalid Misskey thread view state"))
    state))

(defun misskey-thread--items (state)
  "Return ordered notes displayed by thread STATE."
  (append (plist-get state :ancestors)
          (and (plist-get state :focus)
               (list (plist-get state :focus)))
          (plist-get state :replies)))

(defun misskey-thread--frame (state)
  "Return generated frame text for thread STATE."
  (concat
   (propertize "Misskey thread" 'face 'bold)
   "\n"
   (pcase (plist-get state :phase)
     ('loading "Loading thread...\n\n")
     ('older "Loading more replies...\n\n")
     ('error (format "Unable to load thread.\n%s\n\n"
                     (plist-get state :message)))
     (_ "\n"))))

(defun misskey-thread--contexts (state)
  "Return presentation contexts indexed by note ID for STATE."
  (let* ((ancestors (plist-get state :ancestors))
         (focus (plist-get state :focus))
         (replies (plist-get state :replies))
         (focus-depth (length ancestors))
         (table (make-hash-table :test #'equal)))
    (cl-loop for note in ancestors
             for depth from 0
             for key = (misskey-note-id note)
             for parent = (and (> depth 0) (alist-get 'replyId note))
             do (puthash key
                         (list :parent-key parent
                               :depth depth
                               :connector 'continue)
                         table))
    (when focus
      (puthash (misskey-note-id focus)
               (list :parent-key
                     (and (> focus-depth 0)
                          (misskey-note-id (car (last ancestors))))
                     :depth focus-depth
                     :connector 'end)
               table))
    (dolist (reply replies)
      (puthash (misskey-note-id reply)
               (list :parent-key (and focus (misskey-note-id focus))
                     :depth (1+ focus-depth))
               table))
    table))

(defun misskey-thread--project (state app)
  "Project thread STATE under APP into dependency-indexed rows."
  (let ((contexts (misskey-thread--contexts state)))
    (appkit-projection-project
     (cl-remove-if
      (lambda (note) (misskey-note-deleted-p app note))
      (misskey-thread--items state))
     #'misskey-note-id
     :context-function
     (lambda (_previous note)
       (gethash (misskey-note-id note) contexts))
     :dependencies-function #'misskey-note-presentation-dependencies)))

(defun misskey-thread--position-intent (events)
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

(defun misskey-thread--sync (view invalidations)
  "Synchronize thread VIEW from coalesced INVALIDATIONS."
  (let* ((state (misskey-thread--state view))
         (events (appkit-view-pending-events-snapshot view))
         (event-count (length events))
         (position (misskey-thread--position-intent events))
         (resources (appkit-invalidations-resource-keys invalidations))
         (all-resources-p (memq 'all resources))
         (entry-keys (appkit-invalidations-entry-keys invalidations))
         (reconcile-p
          (or (appkit-invalidations-structure-p invalidations)
              entry-keys resources))
         (rows (and reconcile-p
                    (misskey-thread--project
                     state (appkit-view-app view)))))
    (appkit-projection-sync
     view rows
     :header (misskey-thread--frame state)
     :footer
     (concat "\ng refresh   n/p note   RET reveal CW"
             (if (plist-get state :replies-exhausted-p)
                 "   replies exhausted\n"
               "   N more replies\n"))
     :force-keys
     (append entry-keys
             (and all-resources-p
                  (mapcar #'appkit-projection-row-key rows)))
     :changed-dependencies (and (not all-resources-p) resources)
     :position position
     :reconcile-p reconcile-p)
    (appkit-view-acknowledge-events view event-count)))

(defun misskey-thread--setup-view (view)
  "Initialize VIEW's keyed thread projection."
  (appkit-projection-ensure
   view
   :printer #'misskey-render-insert-row
   :anchor-property appkit-discussion-key-property
   :no-separator-p t)
  (appkit-invalidate view :structure t :part 'frame :position t)
  (appkit-sync-invalidations view))

(defun misskey-thread--current-p (view state token)
  "Return non-nil when TOKEN still owns STATE in VIEW."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (eq token (plist-get state :request-token))))

(defun misskey-thread--cancel (view)
  "Cancel VIEW's active thread transport."
  (let* ((state (misskey-thread--state view))
         (table (appkit-view-request-table view))
         (request (gethash misskey-thread--request-key table)))
    (setf (plist-get state :stage-token) nil)
    (remhash misskey-thread--request-key table)
    (when request (misskey-http-cancel request))))

(defun misskey-thread--fail (view state token message)
  "Settle VIEW STATE TOKEN with failure MESSAGE."
  (when (misskey-thread--current-p view state token)
    (setf (plist-get state :request-token) nil
          (plist-get state :stage-token) nil
          (plist-get state :phase) 'error
          (plist-get state :message) message)
    (appkit-request-sync view :part 'frame :position t)
    (message "%s" message)))

(defun misskey-thread--read (view state token endpoint parameters success)
  "Read ENDPOINT with PARAMETERS for VIEW STATE TOKEN, then call SUCCESS."
  (let ((stage (cons endpoint nil))
        (observation (misskey-state-observe (appkit-view-app view)))
        callback-ran-p request)
    (setf (plist-get state :stage-token) stage)
    (condition-case err
        (setq request
              (misskey-http-read
               endpoint parameters
               (lambda (payload)
                 (setq callback-ran-p t)
                 (when (and (misskey-thread--current-p view state token)
                            (eq stage (plist-get state :stage-token)))
                   (remhash misskey-thread--request-key
                            (appkit-view-request-table view))
                   (setf (plist-get state :stage-token) nil)
                   (condition-case handler-error
                       (funcall success payload observation)
                     (error
                      (misskey-thread--fail
                       view state token
                       (error-message-string handler-error))))))
               :errback
               (lambda (failure)
                 (setq callback-ran-p t)
                 (when (and (misskey-thread--current-p view state token)
                            (eq stage (plist-get state :stage-token)))
                   (remhash misskey-thread--request-key
                            (appkit-view-request-table view))
                   (setf (plist-get state :stage-token) nil)
                   (misskey-thread--fail view state token failure)))
               :owner view
               :account (plist-get state :account)))
      (error
       (setq callback-ran-p t)
       (misskey-thread--fail view state token (error-message-string err))))
    (cond
     ((and (not callback-ran-p) request
           (misskey-thread--current-p view state token)
           (eq stage (plist-get state :stage-token)))
      (puthash misskey-thread--request-key request
               (appkit-view-request-table view)))
     ((and (not callback-ran-p) (null request))
      (misskey-thread--fail
       view state token "Misskey thread request did not start")))
    request))

(defun misskey-thread--ancestor-chain (focus candidates)
  "Return FOCUS's oldest-first ancestor chain from CANDIDATES."
  (let ((by-id (make-hash-table :test #'equal))
        (seen (make-hash-table :test #'equal))
        chain
        (parent (alist-get 'replyId focus)))
    (dolist (note candidates)
      (let ((reply-id (alist-get 'replyId note)))
        (unless (or (null reply-id)
                    (and (stringp reply-id)
                         (not (string-empty-p reply-id))))
          (error "Misskey returned a malformed ancestor reference")))
      (puthash (misskey-note-id note) note by-id))
    (while parent
      (unless (and (stringp parent) (not (string-empty-p parent)))
        (error "Misskey returned a malformed ancestor reference"))
      (when (gethash parent seen)
        (error "Misskey returned a cyclic thread ancestry"))
      (puthash parent t seen)
      (let ((note (gethash parent by-id)))
        (unless note (setq parent nil))
        (when note
          (push note chain)
          (setq parent (alist-get 'replyId note)))))
    chain))

(defun misskey-thread--finish
    (view state token phase replies &optional focus ancestors)
  "Atomically install REPLIES for VIEW STATE TOKEN and PHASE.

FOCUS and ANCESTORS hold the staged initial thread context."
  (when (misskey-thread--current-p view state token)
    (let* ((loaded-p (plist-get state :loaded-p))
           (new
            (if (eq phase 'older)
                (let ((seen (make-hash-table :test #'equal)) result)
                  (dolist (note (plist-get state :replies))
                    (puthash (misskey-note-id note) t seen))
                  (dolist (note replies (nreverse result))
                    (unless (gethash (misskey-note-id note) seen)
                      (puthash (misskey-note-id note) t seen)
                      (push note result))))
              replies)))
      (if (eq phase 'initial)
          (setf (plist-get state :focus) focus
                (plist-get state :ancestors) ancestors
                (plist-get state :replies) new)
        (setf (plist-get state :replies)
              (append (plist-get state :replies) new)))
      (setf (plist-get state :replies-exhausted-p) (null new)
            (plist-get state :request-token) nil
            (plist-get state :stage-token) nil
            (plist-get state :phase) 'ready
            (plist-get state :message) nil
            (plist-get state :loaded-p) t)
      (appkit-view-enqueue-event
       view (list :position
                  (if (and (eq phase 'initial) (not loaded-p))
                      (plist-get state :focus-id)
                    'preserve)))
      (appkit-request-sync view :structure t :part 'frame :position t)
      (misskey-media-prefetch-notes view (misskey-thread--items state)))))

(defun misskey-thread--load-replies
    (view state token phase &optional focus ancestors until-id)
  "Load reply PHASE for VIEW STATE TOKEN, staging FOCUS and ANCESTORS.

UNTIL-ID is the required cursor for an older page."
  (when (and (eq phase 'older)
             (not (and (stringp until-id) (not (string-empty-p until-id)))))
    (error "The Misskey thread has no valid reply cursor"))
  (misskey-thread--read
   view state token "notes/replies"
   (append (list :noteId (plist-get state :focus-id)
                 :limit misskey-thread-reply-limit)
           (and until-id (list :untilId until-id)))
   (lambda (payload observation)
     (let ((replies (misskey-note-validate-list payload)))
       (dolist (note replies)
         (misskey-merge-note-state
          (appkit-view-app view) note observation))
       (misskey-thread--finish
        view state token phase replies focus ancestors)))))

(defun misskey-thread--load-initial (view state token)
  "Load the initial focused thread for VIEW STATE TOKEN."
  (misskey-thread--read
   view state token "notes/show"
   (list :noteId (plist-get state :focus-id))
   (lambda (payload observation)
     (let ((focus (misskey-note-validate payload)))
       (unless (equal (misskey-note-id focus) (plist-get state :focus-id))
         (error "Misskey returned the wrong focused note"))
       (misskey-merge-note-state
        (appkit-view-app view) focus observation)
       (misskey-thread--read
        view state token "notes/conversation"
        (list :noteId (plist-get state :focus-id) :limit 100)
        (lambda (conversation conversation-observation)
          (let* ((candidates (misskey-note-validate-list conversation))
                 (ancestors
                  (misskey-thread--ancestor-chain focus candidates)))
            (dolist (note candidates)
              (misskey-merge-note-state
               (appkit-view-app view) note conversation-observation))
            (misskey-thread--load-replies
             view state token 'initial focus ancestors))))))))

(defun misskey-thread--request (view phase)
  "Start VIEW thread request PHASE."
  (unless (memq phase '(initial older))
    (error "Invalid Misskey thread request phase: %S" phase))
  (unless (and (integerp misskey-thread-reply-limit)
               (<= 1 misskey-thread-reply-limit 100))
    (user-error "Misskey thread reply limit must be between 1 and 100"))
  (let ((state (misskey-thread--state view)))
    (when (plist-get state :request-token)
      (user-error "The Misskey thread is already loading"))
    (when (and (eq phase 'older)
               (plist-get state :replies-exhausted-p))
      (user-error "No more direct replies"))
    (let* ((replies (and (eq phase 'older) (plist-get state :replies)))
           (cursor (and replies
                        (misskey-note-id (car (last replies))))))
      (when (eq phase 'older)
        (unless replies
          (user-error "The Misskey thread has no replies to page"))
        (unless (and (stringp cursor) (not (string-empty-p cursor)))
          (user-error "The Misskey thread has no valid reply cursor")))
      (let ((token (cons phase nil)))
        (setf (plist-get state :request-token) token
              (plist-get state :phase) (if (eq phase 'older) 'older 'loading)
              (plist-get state :message) nil)
        (appkit-request-sync view :part 'frame :position t)
        (if (eq phase 'initial)
            (misskey-thread--load-initial view state token)
          (misskey-thread--load-replies
           view state token phase nil nil cursor))))))

(defun misskey-thread-refresh ()
  "Refresh the current Misskey thread."
  (interactive)
  (if-let* ((view (appkit-current-view))
            ((eq (plist-get (appkit-view-state view) :type) 'thread)))
      (progn
        (misskey-thread--cancel view)
        (setf (plist-get (appkit-view-state view) :request-token) nil)
        (misskey-thread--request view 'initial))
    (user-error "Current buffer is not a Misskey thread")))

(defun misskey-thread-load-more ()
  "Load another page of direct replies in the current thread."
  (interactive)
  (if-let* ((view (appkit-current-view))
            ((eq (plist-get (appkit-view-state view) :type) 'thread)))
      (misskey-thread--request view 'older)
    (user-error "Current buffer is not a Misskey thread")))

(defun misskey-thread-open (note-id &optional account)
  "Open NOTE-ID's thread for ACCOUNT.

ACCOUNT defaults to the account selected by current customization."
  (unless (and (stringp note-id) (not (string-empty-p note-id)))
    (user-error "Misskey note ID is required"))
  (let* ((account (or account (misskey--current-account)))
         (app (misskey-app account))
         (state (list :type 'thread :account account :focus-id note-id
                      :ancestors nil :focus nil :replies nil
                      :phase 'loading :message nil :request-token nil
                      :stage-token nil :loaded-p nil
                      :replies-exhausted-p nil
                      :revealed-content (make-hash-table :test #'equal)))
         (view
          (appkit-open-view
           :app app :id (list 'thread note-id)
           :mode #'misskey-thread-mode
           :buffer-name (format "*misskey thread: %s*" note-id)
           :state state :sync-function #'misskey-thread--sync
           :parts '(frame entries)
           :position-policy appkit-discussion-key-property
           :setup #'misskey-thread--setup-view :select t)))
    (unless (plist-get (misskey-thread--state view) :request-token)
      (misskey-thread--request view 'initial))
    view))

(defun misskey-thread-at-point ()
  "Open the thread for the Misskey note at point."
  (interactive)
  (if-let* ((note (misskey-render-note-at-point))
            (id (misskey-note-id note))
            (view (appkit-current-view)))
      (misskey-thread-open
       id (plist-get (appkit-view-state view) :account))
    (user-error "No Misskey note at point")))

(provide 'misskey-thread)

;;; misskey-thread.el ends here
