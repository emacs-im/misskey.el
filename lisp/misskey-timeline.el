;;; misskey-timeline.el --- Browse Misskey timelines -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch and render authenticated Misskey timelines through Appkit's view
;; lifecycle, keyed projection, and protocol-neutral discussion rows.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'appkit-core)
(require 'appkit-invalidation)
(require 'appkit-discussion)
(require 'appkit-chat-avatar)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-task-queue)
(require 'appkit-projection)
(require 'appkit-position)
(require 'appkit-ui)
(require 'misskey-compose)
(require 'misskey-core)
(require 'misskey-http)

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

(defcustom misskey-timeline-show-avatars t
  "When non-nil, fetch and display timeline author avatars.

Avatar requests run only when Emacs can display images."
  :type 'boolean
  :group 'misskey)

(defcustom misskey-timeline-show-media t
  "When non-nil, fetch and display timeline media previews.

Preview requests run only when Emacs can display images.  Sensitive files stay
hidden until their note's content warning is revealed."
  :type 'boolean
  :group 'misskey)

(defcustom misskey-timeline-media-preview-width 480
  "Maximum width in pixels for an inline timeline media preview."
  :type 'integer
  :group 'misskey)

(defcustom misskey-timeline-media-preview-height 320
  "Maximum height in pixels for an inline timeline media preview."
  :type 'integer
  :group 'misskey)

(defvar misskey-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "TAB") #'misskey-timeline-next-kind)
    (define-key map (kbd "g") #'misskey-timeline-refresh)
    (define-key map (kbd "n") #'appkit-discussion-next-entry)
    (define-key map (kbd "p") #'appkit-discussion-previous-entry)
    (define-key map (kbd "RET") #'misskey-timeline-toggle-content-warning)
    (define-key map (kbd "N") #'misskey-timeline-load-more)
    (define-key map (kbd "c") #'misskey-timeline-compose)
    map)
  "Keymap for `misskey-timeline-mode'.")

(defvar-local misskey-timeline--avatar-queue nil
  "Avatar transfer queue owned by the current timeline view.")

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

(defun misskey-timeline--note-id (note)
  "Return NOTE's stable identifier."
  (alist-get 'id note))

(defun misskey-timeline--pure-renote-p (note)
  "Return non-nil when NOTE is a pure renote wrapper."
  (and (null (alist-get 'text note))
       (consp (alist-get 'renote note))))

(defun misskey-timeline--display-note (note)
  "Return the note whose content NOTE primarily displays."
  (if (misskey-timeline--pure-renote-p note)
      (alist-get 'renote note)
    note))

(defun misskey-timeline--avatars-enabled-p ()
  "Return non-nil when timeline avatars can be displayed."
  (and misskey-timeline-show-avatars
       (display-images-p)))

(defun misskey-timeline--avatar-url (note)
  "Return the HTTPS avatar URL for NOTE's displayed author, or nil."
  (let* ((display-note (misskey-timeline--display-note note))
         (user (alist-get 'user display-note))
         (url (and (consp user) (alist-get 'avatarUrl user))))
    (and (stringp url)
         (string-match-p "\\`https://" url)
         url)))

(defun misskey-timeline--avatar-cache-base (url)
  "Return the extensionless cache path for avatar URL."
  (expand-file-name
   (secure-hash 'sha256 url)
   (locate-user-emacs-file "misskey/avatars/")))

(defun misskey-timeline--avatar-images (view)
  "Return the application avatar image cache used by VIEW."
  (misskey--session-avatar-images
   (misskey--session (appkit-view-app view))))

(defun misskey-timeline--avatar-image-from-file (file)
  "Return a two-line avatar image for cached FILE, or nil."
  (let ((pixel-size (appkit-chat-avatar-two-line-pixel-size)))
    (or (appkit-media-circular-image-from-file file pixel-size)
        (appkit-media-preview-image-from-file
         file pixel-size pixel-size))))

(defun misskey-timeline--avatar-image (view url)
  "Return VIEW's cached avatar image for URL, or nil."
  (when (and (misskey-timeline--avatars-enabled-p) url)
    (let ((images (misskey-timeline--avatar-images view)))
      (or (gethash url images)
          (when-let* ((file
                       (appkit-media-image-cache-existing-file
                        (misskey-timeline--avatar-cache-base url)))
                      (image
                       (misskey-timeline--avatar-image-from-file file)))
            (puthash url image images)
            image)))))

(defun misskey-timeline--avatar-queue (view)
  "Return VIEW's live avatar transfer queue."
  (with-current-buffer (appkit-view-buffer view)
    (if (appkit-task-queue-live-p misskey-timeline--avatar-queue)
        misskey-timeline--avatar-queue
      (setq-local
       misskey-timeline--avatar-queue
       (appkit-task-queue-create view appkit-media-transfer-concurrency)))))

(defun misskey-timeline--start-avatar-fetch (url complete)
  "Fetch avatar URL and call COMPLETE with its cached file or nil."
  (let ((transfer
         (appkit-media-cache-image-resource-async
          (appkit-media-resource-create :url url)
          (misskey-timeline--avatar-cache-base url)
          (lambda (file)
            (funcall complete file))
          (lambda (_failure)
            (funcall complete nil)))))
    (when (appkit-media-transfer-p transfer)
      (lambda ()
        (appkit-media-cancel-transfer transfer)))))

(defun misskey-timeline--avatar-note-keys (state url)
  "Return keys of notes in STATE whose displayed avatar uses URL."
  (cl-loop for note in (plist-get state :items)
           when (equal (misskey-timeline--avatar-url note) url)
           collect (misskey-timeline--note-id note)))

(defun misskey-timeline--finish-avatar-fetch (view url file)
  "Invalidate VIEW rows using URL after FILE has been cached."
  (when (and file (appkit-view-live-p view))
    (let* ((state (misskey-timeline--view-state view))
           (images (misskey-timeline--avatar-images view))
           (keys (misskey-timeline--avatar-note-keys state url)))
      (remhash url images)
      (when keys
        (appkit-request-sync view :entries keys :position t)))))

(defun misskey-timeline--prefetch-avatar (view url)
  "Schedule a missing avatar URL for VIEW."
  (unless (misskey-timeline--avatar-image view url)
    (appkit-task-queue-submit
     (misskey-timeline--avatar-queue view)
     url
     (lambda (complete)
       (misskey-timeline--start-avatar-fetch url complete))
     :finish
     (lambda (file)
       (misskey-timeline--finish-avatar-fetch view url file)))))

(defun misskey-timeline--prefetch-avatars (view notes)
  "Schedule missing avatars used by NOTES in live timeline VIEW."
  (when (and (misskey-timeline--avatars-enabled-p)
             (integerp appkit-media-transfer-concurrency)
             (> appkit-media-transfer-concurrency 0))
    (dolist (url
             (delete-dups
              (delq nil
                    (mapcar #'misskey-timeline--avatar-url notes))))
      (misskey-timeline--prefetch-avatar view url))))

(defun misskey-timeline--media-enabled-p ()
  "Return non-nil when timeline media previews can be displayed."
  (and misskey-timeline-show-media
       (display-images-p)))

(defun misskey-timeline--media-files (note)
  "Return image and video files displayed by NOTE."
  (cl-remove-if-not
   (lambda (file)
     (and (consp file)
          (stringp (alist-get 'id file))
          (stringp (alist-get 'type file))
          (string-match-p "\\`\\(?:image\\|video\\)/"
                          (alist-get 'type file))))
   (alist-get 'files (misskey-timeline--display-note note))))

(defun misskey-timeline--media-url (file)
  "Return FILE's HTTPS preview URL, or nil."
  (let ((url (or (alist-get 'thumbnailUrl file)
                 (and (string-prefix-p "image/" (alist-get 'type file))
                      (alist-get 'url file)))))
    (and (stringp url)
         (string-match-p "\\`https://" url)
         url)))

(defun misskey-timeline--media-cache-base (file)
  "Return the extensionless cache path for FILE's preview."
  (when-let* ((url (misskey-timeline--media-url file)))
    (expand-file-name
     (secure-hash 'sha256 url)
     (locate-user-emacs-file "misskey/media/"))))

(defun misskey-timeline--media-images (view)
  "Return the application media preview image cache used by VIEW."
  (misskey--session-media-images
   (misskey--session (appkit-view-app view))))

(defun misskey-timeline--media-image (view file)
  "Return VIEW's cached preview image for FILE, or nil."
  (when (misskey-timeline--media-enabled-p)
    (let* ((images (misskey-timeline--media-images view))
           (file-id (alist-get 'id file)))
      (or (gethash file-id images)
          (when-let* ((cache-base
                       (misskey-timeline--media-cache-base file))
                      (cached
                       (appkit-media-image-cache-existing-file cache-base))
                      (image
                       (appkit-media-preview-image-from-file
                        cached
                        misskey-timeline-media-preview-width
                        misskey-timeline-media-preview-height)))
            (puthash file-id image images)
            image)))))

(defun misskey-timeline--open-media (view file)
  "Open FILE from timeline VIEW through Appkit."
  (let* ((kind (if (string-prefix-p "video/" (alist-get 'type file))
                   'video
                 'image))
         (url (alist-get 'url file))
         (cache-base (misskey-timeline--media-cache-base file))
         (cached (and cache-base
                      (appkit-media-image-cache-existing-file cache-base))))
    (appkit-media-open-resource
     (appkit-media-resource-create
      :file (and (eq kind 'image) cached)
      :url url
      :name (alist-get 'name file)
      :mime-type (alist-get 'type file))
     :kind kind
     :cache-key (alist-get 'id file)
     :cache-directory (locate-user-emacs-file "misskey/media/")
     :client-label "Misskey media"
     :owner view)))

(defun misskey-timeline--media-alt-text (file)
  "Return accessible fallback text for FILE."
  (or (and (stringp (alist-get 'comment file))
           (not (string-empty-p (alist-get 'comment file)))
           (alist-get 'comment file))
      (and (stringp (alist-get 'name file))
           (not (string-empty-p (alist-get 'name file)))
           (format "[%s]" (alist-get 'name file)))
      "[media]"))

(defun misskey-timeline--insert-media (view file prefix properties hidden-p)
  "Insert FILE for VIEW with PREFIX and PROPERTIES.

When HIDDEN-P is non-nil, reserve only a sensitive-media placeholder."
  (let* ((start (point))
         (image (and (not hidden-p)
                     (misskey-timeline--media-image view file)))
         (alt (misskey-timeline--media-alt-text file)))
    (cond
     (hidden-p
      (insert "[sensitive media]"))
     (image
      (appkit-media-insert-image-slices
       image
       (lambda ()
         (misskey-timeline--open-media view file))
       nil alt "Open Misskey media"))
     (t
      (insert (if (misskey-timeline--media-url file)
                  (format "%s loading preview…" alt)
                alt))))
    (insert "\n")
    (appkit-ui-apply-line-prefix start (point) prefix)
    (add-text-properties start (point) properties)))

(defun misskey-timeline--insert-media-files
    (view note revealed prefix properties)
  "Insert NOTE's media into VIEW using REVEALED, PREFIX, and PROPERTIES."
  (dolist (file (misskey-timeline--media-files note))
    (misskey-timeline--insert-media
     view file prefix properties
     (and (eq (alist-get 'isSensitive file) t)
          (not revealed)))))

(defun misskey-timeline--media-note-keys (state file-id)
  "Return keys of notes in STATE containing FILE-ID."
  (cl-loop for note in (plist-get state :items)
           when (cl-find file-id (misskey-timeline--media-files note)
                         :key (lambda (file) (alist-get 'id file))
                         :test #'equal)
           collect (misskey-timeline--note-id note)))

(defun misskey-timeline--finish-media-fetch (view file-id file)
  "Invalidate VIEW rows using FILE-ID after FILE has been cached."
  (when (and file (appkit-view-live-p view))
    (let* ((state (misskey-timeline--view-state view))
           (images (misskey-timeline--media-images view))
           (keys (misskey-timeline--media-note-keys state file-id)))
      (remhash file-id images)
      (when keys
        (appkit-request-sync view :entries keys :position t)))))

(defun misskey-timeline--prefetch-media (view file)
  "Schedule FILE's missing preview for VIEW."
  (when-let* ((url (misskey-timeline--media-url file)))
    (unless (misskey-timeline--media-image view file)
      (appkit-task-queue-submit
       (misskey-timeline--avatar-queue view)
       (list 'media (alist-get 'id file))
       (lambda (complete)
         (let ((transfer
                (appkit-media-cache-image-resource-async
                 (appkit-media-resource-create
                  :url url
                  :name (alist-get 'name file)
                  :mime-type (alist-get 'type file))
                 (misskey-timeline--media-cache-base file)
                 (lambda (cached)
                   (funcall complete cached))
                 (lambda (_failure)
                   (funcall complete nil)))))
           (when (appkit-media-transfer-p transfer)
             (lambda ()
               (appkit-media-cancel-transfer transfer)))))
       :finish
       (lambda (cached)
         (misskey-timeline--finish-media-fetch
          view (alist-get 'id file) cached))))))

(defun misskey-timeline--prefetch-media-files (view notes)
  "Schedule missing media previews used by NOTES in live timeline VIEW."
  (when (and (misskey-timeline--media-enabled-p)
             (integerp appkit-media-transfer-concurrency)
             (> appkit-media-transfer-concurrency 0))
    (dolist (file
             (delete-dups
              (apply #'append
                     (mapcar #'misskey-timeline--media-files notes))))
      (misskey-timeline--prefetch-media view file))))

(defun misskey-timeline--user-label (note)
  "Return NOTE's readable author label."
  (let* ((user (alist-get 'user note))
         (name (and (consp user) (alist-get 'name user)))
         (username (and (consp user) (alist-get 'username user)))
         (host (and (consp user) (alist-get 'host user)))
         (handle (cond
                  ((and (stringp username) (stringp host))
                   (format "@%s@%s" username host))
                  ((stringp username) (format "@%s" username))
                  (t "@unknown"))))
    (if (and (stringp name) (not (string-empty-p name)))
        (format "%s %s" name handle)
      handle)))

(defun misskey-timeline--heading (note)
  "Return the discussion heading for NOTE."
  (if (misskey-timeline--pure-renote-p note)
      (format "%s renoted %s"
              (misskey-timeline--user-label note)
              (misskey-timeline--user-label (alist-get 'renote note)))
    (misskey-timeline--user-label note)))

(defun misskey-timeline--time (note)
  "Return NOTE's compact creation time."
  (let ((created-at (alist-get 'createdAt note)))
    (if (not (stringp created-at))
        ""
      (condition-case nil
          (format-time-string "%Y-%m-%d %H:%M" (date-to-time created-at))
        (error created-at)))))

(defun misskey-timeline--positive-count (key note label)
  "Return NOTE's positive count at KEY formatted with LABEL."
  (let ((count (alist-get key note)))
    (when (and (integerp count) (> count 0))
      (format "%d %s" count label))))

(defun misskey-timeline--footer (note)
  "Return a compact metadata footer for NOTE."
  (let* ((display-note (misskey-timeline--display-note note))
         (visibility (alist-get 'visibility display-note))
         (files (alist-get 'files display-note)))
    (string-join
     (delq nil
           (list
            (and (stringp visibility) (capitalize visibility))
            (and (eq (alist-get 'localOnly display-note) t)
                 "Local only")
            (misskey-timeline--positive-count
             'repliesCount display-note "replies")
            (misskey-timeline--positive-count
             'renoteCount display-note "renotes")
            (misskey-timeline--positive-count
             'reactionCount display-note "reactions")
            (and (listp files)
                 (> (length files) 0)
                 (format "%d attachment%s"
                         (length files)
                         (if (= (length files) 1) "" "s")))))
     " · ")))

(defun misskey-timeline--revealed-p (state key)
  "Return non-nil when STATE reveals content warnings for KEY."
  (gethash key (plist-get state :revealed-content)))

(defun misskey-timeline--insert-content (note revealed prefix properties)
  "Insert NOTE content using REVEALED, PREFIX, and PROPERTIES."
  (let ((warning (alist-get 'cw note))
        (text (alist-get 'text note)))
    (when (and (stringp warning) (not (string-empty-p warning)))
      (appkit-ui-insert-prefixed-lines
       prefix (format "CW: %s" warning)
       :face 'warning :properties properties))
    (if (and (stringp warning)
             (not (string-empty-p warning))
             (not revealed))
        (appkit-ui-insert-prefixed-lines
         prefix "[RET to reveal]" :face 'shadow :properties properties)
      (appkit-ui-insert-prefixed-lines
       prefix
       (if (and (stringp text) (not (string-empty-p text)))
           text
         "(no text)")
       :properties properties))))

(defun misskey-timeline--insert-body (note key state prefix properties)
  "Insert NOTE body for KEY and STATE using PREFIX and PROPERTIES."
  (let* ((pure-renote-p (misskey-timeline--pure-renote-p note))
         (primary (misskey-timeline--display-note note))
         (quoted (and (not pure-renote-p) (alist-get 'renote note)))
         (revealed (misskey-timeline--revealed-p state key))
         (view (appkit-current-view)))
    (misskey-timeline--insert-content primary revealed prefix properties)
    (misskey-timeline--insert-media-files
     view primary revealed prefix properties)
    (when (consp quoted)
      (appkit-ui-insert-prefixed-lines
       prefix (format "Quoting %s" (misskey-timeline--user-label quoted))
       :face 'shadow :properties properties)
      (misskey-timeline--insert-content quoted revealed prefix properties)
      (misskey-timeline--insert-media-files
       view quoted revealed prefix properties))))

(defun misskey-timeline--render-width ()
  "Return the current timeline render width in columns."
  (if-let* ((window (get-buffer-window (current-buffer) t)))
      (max 40 (window-body-width window))
    80))

(defun misskey-timeline--print-row (row)
  "Insert one projected timeline ROW at point."
  (let* ((note (appkit-projection-row-payload row))
         (key (appkit-projection-row-key row))
         (view (appkit-current-view))
         (state (misskey-timeline--view-state view))
         (avatar-p (misskey-timeline--avatars-enabled-p))
         (avatar-url (and avatar-p
                          (misskey-timeline--avatar-url note))))
    (appkit-discussion-insert-entry
     (appkit-discussion-entry-create
      :key key
      :depth 0
      :avatar (and avatar-url
                   (misskey-timeline--avatar-image view avatar-url))
      :avatar-fallback "@"
      :heading (misskey-timeline--heading note)
      :heading-face 'bold
      :time (misskey-timeline--time note)
      :body-inserter
      (lambda (prefix properties)
        (misskey-timeline--insert-body note key state prefix properties))
      :footer (misskey-timeline--footer note)
      :properties (list 'misskey-note note 'misskey-note-id key))
     :width (misskey-timeline--render-width)
     :avatar-p avatar-p)))

(defun misskey-timeline--project (notes)
  "Project NOTES into stable Appkit rows."
  (appkit-projection-project notes #'misskey-timeline--note-id))

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
         (force-keys (appkit-invalidations-entry-keys invalidations))
         (reconcile-p
          (or (appkit-invalidations-structure-p invalidations)
              force-keys))
         (rows
          (and reconcile-p
               (misskey-timeline--project (plist-get state :items)))))
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
     :position position
     :reconcile-p reconcile-p)
    (appkit-view-acknowledge-events view event-count)))

(defun misskey-timeline--setup-view (view)
  "Initialize VIEW's keyed timeline projection."
  (appkit-projection-ensure
   view
   :printer #'misskey-timeline--print-row
   :anchor-property appkit-discussion-key-property
   :no-separator-p t)
  (appkit-view-enqueue-event view (list :position 'first))
  (appkit-invalidate view :structure t :part 'frame :position t)
  (appkit-sync-invalidations view))

(defun misskey-timeline--validate-notes (payload)
  "Return validated timeline PAYLOAD."
  (unless (listp payload)
    (error "Misskey timeline response is not a list"))
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (note payload)
      (unless (and (consp note)
                   (stringp (misskey-timeline--note-id note))
                   (consp (alist-get 'user note)))
        (error "Misskey timeline contains a malformed note"))
      (let ((id (misskey-timeline--note-id note)))
        (when (gethash id seen)
          (error "Misskey timeline duplicates note %s" id))
        (puthash id t seen))))
  payload)

(defun misskey-timeline--new-notes (current candidates)
  "Return CANDIDATES whose IDs do not occur in CURRENT."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (note current)
      (puthash (misskey-timeline--note-id note) t seen))
    (dolist (note candidates (nreverse result))
      (let ((id (misskey-timeline--note-id note)))
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
        (let* ((notes (misskey-timeline--validate-notes payload))
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
          (misskey-timeline--prefetch-avatars view new-notes)
          (misskey-timeline--prefetch-media-files view new-notes)
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
    (let* ((token (cons phase nil))
           (account (plist-get state :account))
           (kind (plist-get state :kind))
           (until-id
            (and (eq phase 'older)
                 (misskey-timeline--note-id (car (last items)))))
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

(defun misskey-timeline--row-at-point (view)
  "Return VIEW's projected row at point, or nil."
  (when-let* ((key (appkit-discussion-key-at-point)))
    (appkit-projection-row view key)))

(defun misskey-timeline--content-warning-p (note)
  "Return non-nil when NOTE contains hidden warning-guarded content."
  (let ((primary (misskey-timeline--display-note note))
        (quoted (and (not (misskey-timeline--pure-renote-p note))
                     (alist-get 'renote note))))
    (or (and (stringp (alist-get 'cw primary))
             (not (string-empty-p (alist-get 'cw primary))))
        (and (consp quoted)
             (stringp (alist-get 'cw quoted))
             (not (string-empty-p (alist-get 'cw quoted)))))))

(defun misskey-timeline-toggle-content-warning ()
  "Toggle content hidden by a warning on the note at point."
  (interactive)
  (if-let* ((view (misskey-timeline--current-view))
            (row (misskey-timeline--row-at-point view))
            (note (appkit-projection-row-payload row))
            ((misskey-timeline--content-warning-p note)))
      (let* ((state (misskey-timeline--view-state view))
             (key (appkit-projection-row-key row))
             (revealed (plist-get state :revealed-content)))
        (if (gethash key revealed)
            (remhash key revealed)
          (puthash key t revealed))
        (appkit-view-enqueue-event view (list :position key))
        (appkit-request-sync view :entry key :position t))
    (user-error "Current note has no content warning")))

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
