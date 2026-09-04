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
(require 'appkit-projection)
(require 'appkit-discussion)
(require 'appkit-projection)
(require 'appkit-presentation)
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
  "Operation key for the active timeline transport.")

(defvar-keymap misskey-timeline-mode-map
  :doc "Keymap for `misskey-timeline-mode'."
  :parent special-mode-map
  "TAB" #'misskey-timeline-next-kind
  "g" #'misskey-timeline-refresh
  "n" #'appkit-discussion-next-entry
  "p" #'appkit-discussion-previous-entry
  "RET" #'misskey-render-toggle-content-warning
  "N" #'misskey-timeline-load-more
  "c" #'misskey-timeline-compose
  "t" #'misskey-thread-at-point
  "r" #'misskey-compose-reply-at-point
  "q" #'misskey-compose-quote-at-point
  "a" misskey-actions-map)

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
        :loading-p nil
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
  (let*
      ((state (appkit-surface-model view))
       (session (misskey--session (appkit-surface-app view))))
    (unless
        (and (listp state) (eq (plist-get state :type) 'timeline)
             (assq (plist-get state :kind)
                   misskey-timeline--kind-specs)
             (equal (plist-get state :account)
                    (misskey--session-account session))
             (hash-table-p (plist-get state :revealed-content)))
      (error "Invalid Misskey timeline view state"))
    state))

(defun misskey-timeline--current-view ()
  "Return the current live Misskey timeline view, or nil."
  (when-let*
      ((view (appkit-current-surface)) ((appkit-surface-live-p view))
       (state (appkit-surface-model view))
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

(defun misskey-timeline--setup-view (view)
  "Initialize VIEW's keyed timeline projection."
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

(defun misskey-timeline--handle-error (view state failure)
  "Install timeline FAILURE in VIEW STATE."
  (setf (plist-get state :phase) 'error (plist-get state :message)
        failure (plist-get state :loading-p) nil)
  (misskey-dispatch view
                    (list :render
                          (appkit-projection-change-create :frame-p t
                                                           :position
                                                           'preserve)))
  (message "%s" failure))

(defun misskey-timeline--handle-success
    (view state observation phase payload)
  "Install PAYLOAD in VIEW STATE for request PHASE.\n\nOBSERVATION versions canonical note merges."
  (condition-case err
      (let*
          ((notes (misskey-note-validate-list payload))
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
               (error "Invalid Misskey timeline request phase: %S"
                      phase)))))
        (dolist (note notes)
          (misskey-merge-note-state (appkit-surface-app view) note
                                    observation))
        (unless (eq phase 'older)
          (clrhash (plist-get state :revealed-content)))
        (setf (plist-get state :items) installed
              (plist-get state :phase) 'ready
              (plist-get state :message) nil
              (plist-get state :loading-p) nil
              (plist-get state :loaded-p) t)
        (pcase phase
          ('initial
           (setf (plist-get state :older-exhausted-p) (null notes)))
          ('older
           (setf (plist-get state :older-exhausted-p) (null new-notes))))
        (misskey-dispatch view
                          (list :render
                                (appkit-projection-change-create
                                 :full-p t :frame-p t :position
                                 (if (eq phase 'initial) 'first
                                   'preserve))))
        (misskey-media-prefetch-notes view new-notes)
        (if (eq phase 'older)
            (if new-notes
                (message "Loaded %d older Misskey notes"
                         (length new-notes))
              (message "No older Misskey notes"))
          (message "Loaded %d Misskey notes" (length notes))))
    (error
     (misskey-timeline--handle-error view state
                                     (error-message-string err)))))

(defun misskey-timeline--interrupt-state-request (state)
  "Retire STATE's loading marker and restore its settled phase."
  (when (plist-get state :loading-p)
    (setf (plist-get state :loading-p) nil
          (plist-get state :phase)
          (if (plist-get state :loaded-p) 'ready 'initial)
          (plist-get state :message) nil)))

(defun misskey-timeline--request (view phase)
  "Start one timeline PHASE request owned by VIEW."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Misskey timeline request phase: %S" phase))
  (let*
      ((state (misskey-timeline--view-state view))
       (items (plist-get state :items)))
    (unless
        (and (integerp misskey-timeline-limit)
             (<= 1 misskey-timeline-limit 100))
      (user-error "Misskey timeline limit must be between 1 and 100"))
    (when (plist-get state :loading-p)
      (user-error "The Misskey timeline is already loading"))
    (when (eq phase 'older)
      (unless items (user-error "The Misskey timeline has no notes"))
      (when (plist-get state :older-exhausted-p)
        (user-error "No older Misskey notes available")))
    (let*
        ((observation
          (misskey-state-observe (appkit-surface-app view)))
         (account (plist-get state :account))
         (kind (plist-get state :kind))
         (until-id
          (and (eq phase 'older) (misskey-note-id (car (last items))))))
      (unless (or (not (eq phase 'older)) until-id)
        (error "Misskey timeline has no older-page cursor"))
      (let
          ((operation
            (misskey-read-begin view misskey-timeline--request-key)))
        (setf (plist-get state :loading-p) t (plist-get state :phase)
              phase (plist-get state :message) nil)
        (misskey-dispatch view
                          (list :render
                                (appkit-projection-change-create
                                 :frame-p t :position 'preserve)))
        (misskey-http-read (misskey-timeline--endpoint kind)
                           (append
                            (list :limit misskey-timeline-limit
                                  :allowPartial t)
                            (and until-id (list :untilId until-id)))
                           (lambda (payload)
                             (when (misskey-read-finish operation)
                               (misskey-timeline--handle-success view
                                                                 state
                                                                 observation
                                                                 phase
                                                                 payload)))
                           :errback
                           (lambda (failure)
                             (when (misskey-read-finish operation)
                               (misskey-timeline--handle-error view
                                                               state
                                                               failure)))
                           :owner operation :account account)))))

(defun misskey-timeline--capture-position (view)
  "Return VIEW's semantic position snapshot."
  (with-current-buffer (appkit-surface-buffer view)
    (appkit-position-capture :anchor-property
                             appkit-discussion-key-property
                             :preserve-window-start t)))

(defun misskey-timeline--switch-kind (view kind &optional refresh-p)
  "Switch timeline VIEW to KIND.\n\nWhen REFRESH-P is non-nil, refresh KIND after switching."
  (let*
      ((state (misskey-timeline--view-state view))
       (current (plist-get state :kind))
       (target
        (misskey-timeline--feed-state (appkit-surface-app view) kind)))
    (unless (eq current kind)
      (when (plist-get state :items)
        (setf (plist-get state :position)
              (misskey-timeline--capture-position view)))
      (misskey-timeline--interrupt-state-request state)
      (misskey-read-cancel view misskey-timeline--request-key)
      (misskey-timeline--interrupt-state-request target)
      (misskey-dispatch view (list :replace-model target))
      (misskey-dispatch view
                        (list :render
                              (appkit-projection-change-create :full-p
                                                               t
                                                               :frame-p
                                                               t
                                                               :position
                                                               (or
                                                                (plist-get
                                                                 target
                                                                 :position)
                                                                'first))))
      (force-mode-line-update))
    (let ((active (misskey-timeline--view-state view)))
      (unless (plist-get active :loading-p)
        (when (or refresh-p (not (plist-get active :loaded-p)))
          (misskey-timeline--request view
                                     (if (plist-get active :loaded-p)
                                         'refresh
                                       'initial)))))
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
  "Open the selected account's timeline buffer at KIND.\n\nKIND defaults to `home'.  Opening an existing buffer refreshes KIND."
  (let*
      ((kind (or kind 'home))
       (_spec (misskey-timeline--kind-spec kind))
       (account (misskey--current-account))
       (app (misskey-app account)) (id 'timeline)
       (existing (appkit-app-surface app id))
       (state
        (or (and existing (appkit-surface-model existing))
            (misskey-timeline--feed-state app kind))))
    (unless existing
      (misskey-timeline--interrupt-state-request state))
    (let
        ((view
          (misskey-open-surface :app app :identity id :mode
                                #'misskey-timeline-mode :buffer-name
                                (format "*misskey: %s@%s*"
                                        (misskey--account-auth-source-user
                                         account)
                                        (string-remove-prefix
                                         "https://"
                                         (misskey--account-origin
                                          account)))
                                :input state :setup
                                #'misskey-timeline--setup-view :select
                                t)))
      (misskey-timeline--switch-kind view kind t) view)))

(provide 'misskey-timeline)

;;; misskey-timeline.el ends here
