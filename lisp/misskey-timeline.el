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
  "Effect key for the active timeline page.")

(declare-function misskey-menu "misskey-menu" nil)

(defvar-keymap misskey-timeline-mode-map
  :doc "Keymap for `misskey-timeline-mode'."
  :parent special-mode-map
  "TAB" #'misskey-timeline-next-kind
  "g" #'misskey-timeline-refresh
  "n" #'appkit-discussion-next-entry
  "p" #'appkit-discussion-previous-entry
  "RET" #'misskey-navigation-activate
  "<mouse-2>" #'misskey-navigation-mouse-activate
  "O" #'misskey-navigation-open-note-url
  "B" #'misskey-navigation-browse
  "w" #'misskey-navigation-copy-link
  "N" #'misskey-timeline-load-more
  "c" #'misskey-timeline-compose
  "t" #'misskey-thread-at-point
  "r" #'misskey-compose-reply-at-point
  "q" #'misskey-compose-quote-at-point
  "a" misskey-actions-map
  "?" #'misskey-menu)

(define-derived-mode misskey-timeline-mode appkit-discussion-mode "Misskey-Timeline"
  "Major mode for authenticated Misskey timelines."
  (setq-local header-line-format
              '(:eval (misskey-timeline--header-line)))
  (setq-local line-spacing 0))

(defun misskey-timeline--make-state (account kind)
  "Return fresh Surface-owned state for ACCOUNT's timeline KIND."
  (list :type 'timeline :account account :kind kind :states nil :serial 0
        :request-id nil :request-parameters nil :observation nil
        :items nil :phase 'initial :message nil :loading-p nil :loaded-p nil
        :position nil :older-exhausted-p nil
        :revealed-content (make-hash-table :test #'equal)))

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

(defun misskey-timeline--request (view phase)
  "Send one timeline PHASE intent to VIEW."
  (misskey-timeline--validate-request
   (misskey-timeline--view-state view) phase misskey-timeline-limit)
  (misskey-dispatch view (list :timeline-request phase misskey-timeline-limit)))

(defun misskey-timeline--capture-position (view)
  "Return VIEW's semantic position snapshot."
  (with-current-buffer (appkit-surface-buffer view)
    (appkit-position-capture :anchor-property
                             appkit-discussion-key-property
                             :preserve-window-start t)))

(defun misskey-timeline--switch-kind (view kind &optional refresh-p)
  "Select KIND in VIEW, optionally requesting a fresh page with REFRESH-P."
  (misskey-timeline--kind-spec kind)
  (unless (and (integerp misskey-timeline-limit) (<= 1 misskey-timeline-limit 100))
    (user-error "Misskey timeline limit must be between 1 and 100"))
  (let* ((state (misskey-timeline--view-state view))
         (position (and (not (eq kind (plist-get state :kind)))
                        (plist-get state :items)
                        (misskey-timeline--capture-position view))))
    (misskey-dispatch view (list :timeline-select kind refresh-p position
                                 misskey-timeline-limit)))
  view)

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
  "Open the selected account's timeline at KIND, defaulting to Home."
  (let* ((kind (or kind 'home))
         (_spec (misskey-timeline--kind-spec kind))
         (account (misskey--current-account))
         (app (misskey-app account))
         (view (misskey-open-surface
                :app app :identity 'timeline :mode #'misskey-timeline-mode
                :buffer-name (format "*misskey: %s@%s*"
                                     (misskey--account-auth-source-user account)
                                     (string-remove-prefix "https://" (misskey--account-origin account)))
                :input (unless (appkit-app-surface app 'timeline)
                         (or (copy-sequence (misskey--session-timeline (misskey--session app)))
                             (misskey-timeline--make-state account kind)))
                :setup #'misskey-timeline--setup-view :select t)))
    (misskey-timeline--switch-kind view kind t)))

(defun misskey-timeline--validate-request (model phase limit)
  "Validate a page intent against MODEL, PHASE and LIMIT without mutation."
  (unless (memq phase '(initial refresh older))
    (error "Invalid Misskey timeline request phase: %S" phase))
  (unless (and (integerp limit) (<= 1 limit 100))
    (user-error "Misskey timeline limit must be between 1 and 100"))
  (when (plist-get model :loading-p)
    (user-error "The Misskey timeline is already loading"))
  (when (eq phase 'older)
    (unless (plist-get model :items)
      (user-error "The Misskey timeline has no notes"))
    (when (plist-get model :older-exhausted-p)
      (user-error "No older Misskey notes available"))
    (unless (misskey-note-id (car (last (plist-get model :items))))
      (user-error "Misskey timeline has no older-page cursor"))))

(defun misskey-timeline--begin (context model phase limit)
  "Commit a page intent before asking the App for its observation revision."
  (misskey-timeline--validate-request model phase limit)
  (let* ((next (copy-sequence model))
         (serial (1+ (plist-get model :serial)))
         (cursor (and (eq phase 'older) (misskey-note-id (car (last (plist-get model :items)))))))
    (setf (plist-get next :serial) serial
          (plist-get next :request-id) serial
          (plist-get next :request-parameters) (append (list :limit limit :allowPartial t)
                                                       (and cursor (list :untilId cursor)))
          (plist-get next :observation) nil
          (plist-get next :phase) phase
          (plist-get next :loading-p) t
          (plist-get next :message) nil)
    (appkit-next
     :model next :render (appkit-projection-change-create :frame-p t :position 'preserve)
     :commands (list (appkit-command-post-message
                      :target (appkit-transition-context-parent-address context)
                      :message (list :timeline-observe serial)
                      :reply-correlation serial :delivery 'report)))))

(defun misskey-timeline--effect-start (_context input _observe resolve reject)
  "Start the finite page INPUT and deliver only through Effect gates."
  (let ((request (misskey-http-read
                  (plist-get input :endpoint) (plist-get input :parameters) resolve
                  :errback reject :owner (plist-get input :owner)
                  :account (plist-get input :account))))
    (when request
      (appkit-cancellation-create
       :kind 'transport :cancel (lambda () (misskey-http-cancel request))))))

(defun misskey-timeline--fail (model failure)
  "Return settled timeline state for a failed page without discarding notes."
  (let ((next (misskey-timeline--retire-request model)))
    (setf (plist-get next :phase) 'error (plist-get next :message) failure)
    (appkit-next :model next
                 :render (appkit-projection-change-create :frame-p t :position 'preserve))))

(defun misskey-timeline--install (context model notes)
  "Install NOTES after their account state has committed."
  (let* ((phase (plist-get model :phase))
         (current (plist-get model :items))
         (new-notes (if (eq phase 'older) (misskey-note-new-notes current notes) notes))
         (next (misskey-timeline--retire-request model)))
    (setf (plist-get next :items)
          (pcase phase
            ('initial notes)
            ('refresh (append notes (misskey-note-new-notes notes current)))
            ('older (append current new-notes)))
          (plist-get next :phase) 'ready
          (plist-get next :message) nil
          (plist-get next :loaded-p) t)
    (unless (eq phase 'older)
      (setf (plist-get next :revealed-content) (make-hash-table :test #'equal)))
    (pcase phase
      ('initial (setf (plist-get next :older-exhausted-p) (null notes)))
      ('older (setf (plist-get next :older-exhausted-p) (null new-notes))))
    (appkit-next
     :model next :render (appkit-projection-change-create
                          :full-p t :frame-p t :position (if (eq phase 'initial) 'first 'preserve))
     :commands (and new-notes (delq nil (list (misskey-media-prefetch-command context next new-notes)))))))

(defun misskey-timeline--select (context model kind refresh-p position limit)
  "Commit KIND selection while retaining inactive pages within this Surface."
  (misskey-timeline--kind-spec kind)
  (let ((next model) (changed (not (eq kind (plist-get model :kind)))) commands)
    (when changed
      (let* ((saved (misskey-timeline--snapshot model))
             (states (assq-delete-all kind (copy-sequence (plist-get model :states))))
             (target (alist-get kind (plist-get model :states))))
        (setf (plist-get saved :states) nil (plist-get saved :position) position)
        (setq next (if target (copy-sequence target)
                     (misskey-timeline--make-state (plist-get model :account) kind)))
        (setf (plist-get next :states) (cons (cons (plist-get model :kind) saved) states)
              (plist-get next :address) (plist-get model :address)
              (plist-get next :serial) (plist-get model :serial)
              (plist-get next :media-intent) nil)
        (setq commands (list (appkit-command-cancel-effect misskey-timeline--request-key)
                             (appkit-command-cancel-effect 'misskey-media-acquire)
                             (appkit-command-cancel-effect 'misskey-media-present)))))
    (let ((result (if (and (not (plist-get next :loading-p))
                           (or refresh-p (not (plist-get next :loaded-p))))
                      (misskey-timeline--begin context next
                                               (if (plist-get next :loaded-p) 'refresh 'initial) limit)
                    (appkit-next :model next :render appkit-render-none))))
      (appkit-next
       :model (appkit-next-model result)
       :render (if changed
                   (appkit-projection-change-create :full-p t :frame-p t
                                                    :position (or (plist-get next :position) 'first))
                 (appkit-next-render result))
       :commands (append commands (appkit-next-commands result))))))

(defun misskey-timeline-update (context model message)
  "Reduce timeline MESSAGE, fencing all page replies before dispatch."
  (if (and (memq (car-safe message) '(:timeline-observed :timeline-received
                                      :timeline-committed :timeline-failed))
           (not (equal (cadr message) (plist-get model :request-id))))
      (appkit-next-reject 'superseded-timeline-request)
    (pcase message
      (`(:timeline-request ,phase ,limit)
       (condition-case err (misskey-timeline--begin context model phase limit)
         (user-error (appkit-next-reject (error-message-string err)))))
      (`(:timeline-select ,kind ,refresh-p ,position ,limit)
       (misskey-timeline--select context model kind refresh-p position limit))
      (`(:timeline-observed ,request-id ,observation)
       (let ((next (copy-sequence model)))
         (setf (plist-get next :observation) observation)
         (appkit-next
          :model next :render appkit-render-none
          :commands
          (list (appkit-command-start-effect
                 (appkit-effect-create
                  :key misskey-timeline--request-key
                  :input (list :request-id request-id :account (plist-get model :account)
                               :owner (appkit-current-surface)
                               :endpoint (misskey-timeline--endpoint (plist-get model :kind))
                               :parameters (plist-get model :request-parameters))
                  :start #'misskey-timeline--effect-start
                  :success (lambda (input payload) (list :timeline-received (plist-get input :request-id) payload))
                  :failure (lambda (input failure) (list :timeline-failed (plist-get input :request-id) failure))))))))
      (`(:timeline-received ,request-id ,payload)
       (condition-case err
           (let ((notes (misskey-note-validate-list payload)))
             (appkit-next
              :model model :render appkit-render-none
              :commands (list (appkit-command-post-message
                               :target (appkit-transition-context-parent-address context)
                               :message (list :timeline-merge request-id (plist-get model :observation) notes)
                               :reply-correlation request-id :delivery 'report))))
         (error (misskey-timeline--fail model (error-message-string err)))))
      (`(:timeline-committed ,_request-id ,notes) (misskey-timeline--install context model notes))
      (`(:timeline-failed ,_request-id ,failure) (misskey-timeline--fail model failure))
      (`(:timeline-reveal ,key)
       (let* ((next (copy-sequence model))
              (table (copy-hash-table (plist-get model :revealed-content))))
         (if (gethash key table) (remhash key table) (puthash key t table))
         (setf (plist-get next :revealed-content) table)
         (appkit-next
          :model next :render (appkit-projection-change-create :keys (list key) :position key)
          :commands (when (gethash key table)
                      (delq nil (list (misskey-media-prefetch-command
                                       context next
                                       (cl-remove-if-not (lambda (note) (equal key (misskey-note-id note)))
                                                         (plist-get model :items))))))))))))

(defun misskey-timeline--snapshot (model)
  "Return a settled page snapshot, never a second mutable timeline owner."
  (let ((snapshot (misskey-timeline--retire-request model)))
    (setf (plist-get snapshot :address) nil (plist-get snapshot :media-intent) nil)
    (when (plist-get model :loading-p)
      (setf (plist-get snapshot :phase) (if (plist-get model :loaded-p) 'ready 'initial)))
    snapshot))

(defun misskey-timeline--retire-request (model)
  "Copy MODEL without any authority or inputs from its finished request."
  (let ((next (copy-sequence model)))
    (setf (plist-get next :request-id) nil
          (plist-get next :request-parameters) nil
          (plist-get next :observation) nil
          (plist-get next :loading-p) nil)
    next))

(provide 'misskey-timeline)

;;; misskey-timeline.el ends here
