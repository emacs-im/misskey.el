;;; misskey-feed.el --- Shared paged Misskey note feeds -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own the common request, merge, projection, and position-preservation
;; workflow for query-backed lists of Misskey notes.  Consumers own query
;; semantics, titles, mode switching, and public commands.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-discussion)
(require 'appkit-invalidation)
(require 'appkit-projection)
(require 'appkit-presentation)
(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-media)
(require 'misskey-note)
(require 'misskey-render)

(defconst misskey-feed--request-key 'notes
  "Operation key for a paged note feed request.")

(cl-defun misskey-feed-make-state
    (&key type account title endpoint parameters (limit 20)
          header-function footer-function empty-message)
  "Return fresh note feed state.

TYPE identifies the consumer.  ACCOUNT authorizes ENDPOINT with PARAMETERS.
TITLE labels the feed, and LIMIT bounds each page.  HEADER-FUNCTION and
FOOTER-FUNCTION receive the state and return generated text.  EMPTY-MESSAGE
replaces the default empty result text."
  (unless (and (symbolp type) (misskey--account-p account)
               (stringp title) (integerp limit) (> limit 0))
    (error "Invalid Misskey feed configuration"))
  (list :type type
        :feed-p t
        :account account
        :title title
        :endpoint endpoint
        :parameters (copy-sequence parameters)
        :limit limit
        :header-function header-function
        :footer-function footer-function
        :empty-message empty-message
        :items nil
        :phase 'initial
        :message nil
        :loaded-p nil
        :older-exhausted-p nil
        :revealed-content (make-hash-table :test #'equal)))

(defun misskey-feed-state-p (state &optional type)
  "Return non-nil when STATE is a valid feed, optionally of TYPE."
  (and (listp state)
       (plist-get state :feed-p)
       (or (null type) (eq (plist-get state :type) type))
       (misskey--account-p (plist-get state :account))
       (stringp (plist-get state :title))
       (integerp (plist-get state :limit))
       (> (plist-get state :limit) 0)
       (hash-table-p (plist-get state :revealed-content))))

(defun misskey-feed-view-state (view &optional type)
  "Return VIEW's validated feed state, optionally requiring TYPE."
  (let ((state (and (appkit-view-p view) (appkit-view-state view))))
    (unless (misskey-feed-state-p state type)
      (error "Invalid Misskey %s feed state" (or type "note")))
    state))

(defun misskey-feed-current-view (&optional type)
  "Return the current live feed view, optionally requiring TYPE."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((misskey-feed-state-p state type)))
    view))

(defun misskey-feed--position-intent (events)
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

(defun misskey-feed-default-header (state)
  "Return the default generated header for feed STATE."
  (let ((phase (plist-get state :phase))
        (message (plist-get state :message))
        (items (plist-get state :items)))
    (concat
     (propertize
      (format "%s · %s"
              (plist-get state :title)
              (misskey--account-origin (plist-get state :account)))
      'face 'bold)
     "\n"
     (pcase phase
       ('initial "Loading notes...\n\n")
       ('refresh "Refreshing notes...\n\n")
       ('older "Loading older notes...\n\n")
       ('error (format "Unable to load notes.\n%s\n\n" message))
       (_ (if items
              "\n"
            (format "%s\n\n"
                    (or (plist-get state :empty-message)
                        "No notes returned."))))))))

(defun misskey-feed-default-footer (state)
  "Return the default generated footer for feed STATE."
  (concat
   "\ng refresh   n/p note   "
   (if (plist-get state :older-exhausted-p)
       "older exhausted"
     "N older")
   "   RET reveal CW   t thread   r reply   q quote\n"))

(defun misskey-feed--generated-text (state key fallback)
  "Return STATE's generated text at KEY, or call FALLBACK."
  (let ((function (plist-get state key)))
    (if function
        (funcall function state)
      (funcall fallback state))))

(defun misskey-feed-sync (view invalidations events)
  "Synchronize note feed VIEW from coalesced INVALIDATIONS and EVENTS."
  (let ((state (misskey-feed-view-state view)))
    (appkit-projection-sync-invalidations
        view invalidations
        (misskey-render-project-notes
         (plist-get state :items) (appkit-view-app view))
      :reconcile-parts '(entries)
      :header
      (misskey-feed--generated-text
       state :header-function #'misskey-feed-default-header)
      :footer
      (misskey-feed--generated-text
       state :footer-function #'misskey-feed-default-footer)
      :position (misskey-feed--position-intent events))))

(defun misskey-feed-setup-view (view)
  "Initialize VIEW's stable note projection."
  (misskey-feed-view-state view)
  (appkit-projection-ensure
   view
   :printer #'misskey-render-insert-row
   :anchor-property appkit-discussion-key-property
   :no-separator-p t)
  (appkit-view-enable-responsive-geometry view)
  (appkit-view-enqueue-event view (list :position 'first))
  (appkit-invalidate view :structure t :part 'frame :position t)
  (appkit-sync-invalidations view))

(defun misskey-feed--new-notes (current candidates)
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

(defun misskey-feed--handle-error (view state failure)
  "Install feed FAILURE in VIEW STATE."
  (setf (plist-get state :phase) 'error
        (plist-get state :message) failure)
  (appkit-request-sync view :part 'frame :position t)
  (message "%s" failure))

(defun misskey-feed--handle-success
    (view state observation phase payload)
  "Install PAYLOAD in VIEW STATE for PHASE.

OBSERVATION versions canonical note merges."
  (condition-case err
      (let* ((notes (misskey-note-validate-list payload))
             (current (plist-get state :items))
             (new-notes
              (if (eq phase 'older)
                  (misskey-feed--new-notes current notes)
                notes))
             (installed
              (pcase phase
                ('initial notes)
                ('refresh
                 (append notes (misskey-feed--new-notes notes current)))
                ('older (append current new-notes))
                (_ (error "Invalid Misskey feed phase: %S" phase)))))
        (dolist (note notes)
          (misskey-merge-note-state
           (appkit-view-app view) note observation))
        (unless (eq phase 'older)
          (clrhash (plist-get state :revealed-content)))
        (setf (plist-get state :items) installed
              (plist-get state :phase) 'ready
              (plist-get state :message) nil
              (plist-get state :loaded-p) t)
        (pcase phase
          ('initial
           (setf (plist-get state :older-exhausted-p) (null notes)))
          ('older
           (setf (plist-get state :older-exhausted-p)
                 (null new-notes))))
        (appkit-view-enqueue-event
         view (list :position (if (eq phase 'initial) 'first 'preserve)))
        (appkit-request-sync view :structure t :part 'frame :position t)
        (misskey-media-prefetch-notes view new-notes)
        (cond
         ((and (eq phase 'older) new-notes)
          (message "Loaded %d older Misskey notes" (length new-notes)))
         ((eq phase 'older)
          (message "No older Misskey notes"))
         (t
          (message "Loaded %d Misskey notes" (length notes)))))
    (error
     (misskey-feed--handle-error
      view state (error-message-string err)))))

(defun misskey-feed-cancel-request (view)
  "Cancel VIEW's active feed transport, if any."
  (when (appkit-view-operation-cancel view misskey-feed--request-key)
    (let ((state (misskey-feed-view-state view)))
      (setf (plist-get state :phase)
            (if (plist-get state :loaded-p) 'ready 'initial)))))

(defun misskey-feed--request-parameters (state phase)
  "Return API parameters for STATE request PHASE."
  (let ((parameters (copy-sequence (plist-get state :parameters))))
    (setq parameters
          (plist-put parameters :limit (plist-get state :limit)))
    (when (eq phase 'older)
      (let* ((oldest (car (last (plist-get state :items))))
             (cursor (and oldest (misskey-note-id oldest))))
        (unless oldest
          (user-error "The Misskey feed has no notes"))
        (unless (and (stringp cursor) (not (string-empty-p cursor)))
          (user-error "The oldest Misskey note has no valid ID"))
        (setq parameters (plist-put parameters :untilId cursor))))
    parameters))

(defun misskey-feed-request (view phase)
  "Start VIEW's note request for PHASE.

PHASE is `initial', `refresh', or `older'."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Misskey feed request phase: %S" phase))
  (let* ((state (misskey-feed-view-state view))
         (endpoint (plist-get state :endpoint)))
    (unless (and (stringp endpoint) (not (string-empty-p endpoint)))
      (error "Misskey feed has no endpoint"))
    (when (and (eq phase 'older)
               (plist-get state :older-exhausted-p))
      (user-error "No older Misskey notes available"))
    (let* ((parameters (misskey-feed--request-parameters state phase))
           (observation (misskey-state-observe (appkit-view-app view)))
           (operation
            (appkit-view-operation-begin view misskey-feed--request-key)))
      (setf (plist-get state :phase) phase
            (plist-get state :message) nil)
      (appkit-request-sync view :part 'frame :position t)
      (misskey-http-read
       endpoint parameters
       (lambda (payload)
         (when (appkit-view-operation-finish operation)
           (misskey-feed--handle-success
            view state observation phase payload)))
       :errback
       (lambda (failure)
         (when (appkit-view-operation-finish operation)
           (misskey-feed--handle-error view state failure)))
       :account (plist-get state :account)
       :owner operation))))

(defun misskey-feed-reset-query (view endpoint parameters &optional title)
  "Reset VIEW to ENDPOINT and PARAMETERS, optionally replacing TITLE."
  (let ((state (misskey-feed-view-state view)))
    (misskey-feed-cancel-request view)
    (setf (plist-get state :endpoint) endpoint
          (plist-get state :parameters) (copy-sequence parameters)
          (plist-get state :items) nil
          (plist-get state :phase) 'initial
          (plist-get state :message) nil
          (plist-get state :loaded-p) nil
          (plist-get state :older-exhausted-p) nil)
    (when title
      (setf (plist-get state :title) title))
    (clrhash (plist-get state :revealed-content))
    (appkit-view-enqueue-event view (list :position 'first))
    (appkit-request-sync view :structure t :part 'frame :position t)
    (misskey-feed-request view 'initial)))

(defun misskey-feed-refresh (view)
  "Refresh live feed VIEW."
  (misskey-feed-request view
                        (if (plist-get (misskey-feed-view-state view) :loaded-p)
                            'refresh
                          'initial)))

(defun misskey-feed-load-more (view)
  "Load one older page for live feed VIEW."
  (misskey-feed-request view 'older))

(provide 'misskey-feed)

;;; misskey-feed.el ends here
