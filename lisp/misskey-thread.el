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
(require 'appkit-projection)
(require 'appkit-presentation)
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
  "Operation key for the active thread request.")

(declare-function misskey-menu "misskey-menu" nil)

(defvar-keymap misskey-thread-mode-map
  :doc "Keymap for `misskey-thread-mode'." :parent special-mode-map
  "g" #'misskey-thread-refresh "N" #'misskey-thread-load-more "n"
  #'appkit-discussion-next-entry "p"
  #'appkit-discussion-previous-entry "RET"
  #'misskey-navigation-activate "<mouse-2>"
  #'misskey-navigation-mouse-activate "O"
  #'misskey-navigation-open-note-url "B" #'misskey-navigation-browse
  "w" #'misskey-navigation-copy-link "r"
  #'misskey-compose-reply-at-point "q"
  #'misskey-compose-quote-at-point "a" misskey-actions-map "?"
  #'misskey-menu)

(define-derived-mode misskey-thread-mode appkit-discussion-mode "Misskey-Thread"
  "Major mode for one Misskey note thread."
  (setq-local header-line-format nil)
  (setq-local line-spacing 0))

(defun misskey-thread--state (view)
  "Return VIEW's validated thread state."
  (let
      ((state
        (and (appkit-surface-live-p view) (appkit-surface-model view))))
    (unless
        (and (listp state) (eq (plist-get state :type) 'thread)
             (let ((focus-id (plist-get state :focus-id)))
               (and (stringp focus-id) (not (string-empty-p focus-id))))
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

(defun misskey-thread--setup-view (view)
  "Initialize VIEW's keyed thread projection."
  (appkit-surface-enable-responsive-geometry view
                                             (lambda (surface _width)
                                               (misskey-dispatch
                                                surface
                                                (list :render
                                                      (appkit-projection-change-create
                                                       :geometry-p t
                                                       :frame-p t
                                                       :position
                                                       'preserve))))))

(defun misskey-thread--cancel (view)
  "Cancel VIEW's active thread operation."
  (let ((state (misskey-thread--state view)))
    (setf (plist-get state :loading-p) nil)
    (misskey-read-cancel view misskey-thread--request-key)))

(defun misskey-thread--fail (view state message)
  "Install thread failure MESSAGE in VIEW STATE."
  (setf (plist-get state :loading-p) nil (plist-get state :phase)
        'error (plist-get state :message) message)
  (misskey-dispatch view
                    (list :render
                          (appkit-projection-change-create :frame-p t
                                                           :position
                                                           'preserve)))
  (message "%s" message))

(defun misskey-thread--read
    (view state operation endpoint parameters success)
  "Read ENDPOINT with PARAMETERS for VIEW STATE OPERATION, then call SUCCESS."
  (let
      ((observation (misskey-state-observe (appkit-surface-app view))))
    (condition-case err
        (misskey-http-read endpoint parameters
                           (lambda (payload)
                             (when (misskey-read-current-p operation)
                               (condition-case handler-error
                                   (funcall success payload
                                            observation)
                                 (error
                                  (when
                                      (misskey-read-finish operation)
                                    (misskey-thread--fail view state
                                                          (error-message-string
                                                           handler-error)))))))
                           :errback
                           (lambda (failure)
                             (when (misskey-read-finish operation)
                               (misskey-thread--fail view state
                                                     failure)))
                           :owner operation :account
                           (plist-get state :account))
      (error
       (when (misskey-read-finish operation)
         (misskey-thread--fail view state (error-message-string err)))))))

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
    (view state operation phase replies &optional focus ancestors)
  "Install REPLIES for VIEW STATE OPERATION and PHASE.

FOCUS and ANCESTORS hold the staged initial thread context."
  (when (misskey-read-finish operation)
    (let* ((loaded-p (plist-get state :loaded-p))
           (new (if (eq phase 'older)
                    (misskey-note-new-notes (plist-get state :replies) replies)
                  replies)))
      (if (eq phase 'initial)
          (setf (plist-get state :focus) focus
                (plist-get state :ancestors) ancestors
                (plist-get state :replies) new)
        (setf (plist-get state :replies)
              (append (plist-get state :replies) new)))
      (setf (plist-get state :replies-exhausted-p) (null new)
            (plist-get state :loading-p) nil
            (plist-get state :phase) 'ready
            (plist-get state :message) nil
            (plist-get state :loaded-p) t)
      (misskey-dispatch
       view
       (list :render
             (appkit-projection-change-create
              :full-p t :frame-p t
              :position (if (and (eq phase 'initial) (not loaded-p))
                            (plist-get state :focus-id)
                          'preserve))))
      (misskey-media-prefetch-notes view (misskey-thread--items state)))))

(defun misskey-thread--load-replies
    (view state operation phase &optional focus ancestors until-id)
  "Load reply PHASE for VIEW STATE OPERATION, staging FOCUS and ANCESTORS.

UNTIL-ID is the required cursor for an older page."
  (when
      (and (eq phase 'older)
           (not
            (and (stringp until-id) (not (string-empty-p until-id)))))
    (error "The Misskey thread has no valid reply cursor"))
  (misskey-thread--read view state operation "notes/replies"
                        (append
                         (list :noteId (plist-get state :focus-id)
                               :limit misskey-thread-reply-limit)
                         (and until-id (list :untilId until-id)))
                        (lambda (payload observation)
                          (let
                              ((replies
                                (misskey-note-validate-list payload)))
                            (dolist (note replies)
                              (misskey-merge-note-state
                               (appkit-surface-app view) note
                               observation))
                            (misskey-thread--finish view state
                                                    operation phase
                                                    replies focus
                                                    ancestors)))))

(defun misskey-thread--load-initial (view state operation)
  "Load the initial focused thread for VIEW STATE OPERATION."
  (misskey-thread--read view state operation "notes/show"
                        (list :noteId (plist-get state :focus-id))
                        (lambda (payload observation)
                          (let
                              ((focus (misskey-note-validate payload)))
                            (unless
                                (equal (misskey-note-id focus)
                                       (plist-get state :focus-id))
                              (error
                               "Misskey returned the wrong focused note"))
                            (misskey-merge-note-state
                             (appkit-surface-app view) focus
                             observation)
                            (misskey-thread--read view state operation
                                                  "notes/conversation"
                                                  (list :noteId
                                                        (plist-get
                                                         state
                                                         :focus-id)
                                                        :limit 100)
                                                  (lambda
                                                    (conversation
                                                     conversation-observation)
                                                    (let*
                                                        ((candidates
                                                          (misskey-note-validate-list
                                                           conversation))
                                                         (ancestors
                                                          (misskey-thread--ancestor-chain
                                                           focus
                                                           candidates)))
                                                      (dolist
                                                          (note
                                                           candidates)
                                                        (misskey-merge-note-state
                                                         (appkit-surface-app
                                                          view)
                                                         note
                                                         conversation-observation))
                                                      (misskey-thread--load-replies
                                                       view state
                                                       operation
                                                       'initial focus
                                                       ancestors))))))))

(defun misskey-thread--request (view phase)
  "Start VIEW thread request PHASE."
  (unless (memq phase '(initial older))
    (error "Invalid Misskey thread request phase: %S" phase))
  (unless
      (and (integerp misskey-thread-reply-limit)
           (<= 1 misskey-thread-reply-limit 100))
    (user-error "Misskey thread reply limit must be between 1 and 100"))
  (let ((state (misskey-thread--state view)))
    (when (plist-get state :loading-p)
      (user-error "The Misskey thread is already loading"))
    (when
        (and (eq phase 'older) (plist-get state :replies-exhausted-p))
      (user-error "No more direct replies"))
    (let*
        ((replies (and (eq phase 'older) (plist-get state :replies)))
         (cursor (and replies (misskey-note-id (car (last replies))))))
      (when (eq phase 'older)
        (unless replies
          (user-error "The Misskey thread has no replies to page"))
        (unless (and (stringp cursor) (not (string-empty-p cursor)))
          (user-error "The Misskey thread has no valid reply cursor")))
      (let
          ((operation
            (misskey-read-begin view misskey-thread--request-key)))
        (setf (plist-get state :loading-p) t (plist-get state :phase)
              (if (eq phase 'older) 'older 'loading)
              (plist-get state :message) nil)
        (misskey-dispatch view
                          (list :render
                                (appkit-projection-change-create
                                 :frame-p t :position 'preserve)))
        (if (eq phase 'initial)
            (misskey-thread--load-initial view state operation)
          (misskey-thread--load-replies view state operation phase nil
                                        nil cursor))))))

(defun misskey-thread-refresh ()
  "Refresh the current Misskey thread." (interactive)
  (if-let*
      ((view (appkit-current-surface))
       ((eq (plist-get (appkit-surface-model view) :type) 'thread)))
      (progn
        (misskey-thread--cancel view)
        (misskey-thread--request view 'initial))
    (user-error "Current buffer is not a Misskey thread")))

(defun misskey-thread-load-more ()
  "Load another page of direct replies in the current thread."
  (interactive)
  (if-let*
      ((view (appkit-current-surface))
       ((eq (plist-get (appkit-surface-model view) :type) 'thread)))
      (misskey-thread--request view 'older)
    (user-error "Current buffer is not a Misskey thread")))

(defun misskey-thread-open (note-id &optional account)
  "Open NOTE-ID's thread for ACCOUNT.

ACCOUNT defaults to the account selected by current customization."
  (unless (and (stringp note-id) (not (string-empty-p note-id)))
    (user-error "Misskey note ID is required"))
  (let*
      ((account (or account (misskey--current-account)))
       (app (misskey-app account))
       (state
        (list :type 'thread :account account :focus-id note-id
              :ancestors nil :focus nil :replies nil :phase 'loading
              :message nil :loading-p nil :loaded-p nil
              :replies-exhausted-p nil :revealed-content
              (make-hash-table :test #'equal)))
       (view
        (misskey-open-surface :app app :identity
                              (list 'thread note-id) :mode
                              #'misskey-thread-mode :buffer-name
                              (format "*misskey thread: %s*" note-id)
                              :input state :setup
                              #'misskey-thread--setup-view :select t)))
    (unless (plist-get (misskey-thread--state view) :loading-p)
      (misskey-thread--request view 'initial))
    view))

(defun misskey-thread-at-point ()
  "Open the thread for the Misskey note at point." (interactive)
  (if-let*
      ((note (misskey-render-note-at-point))
       (id (misskey-note-id note)) (view (appkit-current-surface)))
      (misskey-thread-open id
                           (plist-get (appkit-surface-model view)
                                      :account))
    (user-error "No Misskey note at point")))

(provide 'misskey-thread)

;;; misskey-thread.el ends here
