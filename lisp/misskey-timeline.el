;;; misskey-timeline.el --- Browse Misskey home timelines -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Fetch and render the authenticated Misskey home timeline through Appkit's
;; view lifecycle, keyed projection, and protocol-neutral discussion rows.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'appkit-core)
(require 'appkit-discussion)
(require 'appkit-chat-avatar)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-task-queue)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'misskey-compose)
(require 'misskey-core)
(require 'misskey-http)

(defcustom misskey-timeline-limit 20
  "Maximum number of notes requested for the home timeline."
  :type 'integer
  :group 'misskey)

(defcustom misskey-timeline-show-avatars t
  "When non-nil, fetch and display home-timeline author avatars.

Avatar requests run only when Emacs can display images."
  :type 'boolean
  :group 'misskey)

(defvar misskey-timeline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'misskey-timeline-refresh)
    (define-key map (kbd "n") #'appkit-discussion-next-entry)
    (define-key map (kbd "p") #'appkit-discussion-previous-entry)
    (define-key map (kbd "RET") #'misskey-timeline-toggle-content-warning)
    (define-key map (kbd "c") #'misskey-timeline-compose)
    map)
  "Keymap for `misskey-timeline-mode'.")

(define-derived-mode misskey-timeline-mode special-mode "Misskey-Home"
  "Major mode for an authenticated Misskey home timeline."
  (setq-local header-line-format nil)
  (setq-local line-spacing 0))

(defun misskey-timeline--state (view)
  "Return VIEW's validated home timeline state."
  (let ((state (appkit-view-state view)))
    (unless (and (listp state)
                 (eq (plist-get state :type) 'home)
                 (misskey--account-p (plist-get state :account)))
      (error "Invalid Misskey home timeline state"))
    state))

(defun misskey-timeline--current-view ()
  "Return the current live Misskey home timeline view, or nil."
  (when-let* ((view (appkit-current-view))
              ((appkit-view-live-p view))
              (state (appkit-view-state view))
              ((eq (plist-get state :type) 'home)))
    view))

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

(defun misskey-timeline--avatar-images (state)
  "Return STATE's avatar image cache."
  (let ((images (plist-get state :avatar-images)))
    (unless (hash-table-p images)
      (error "Misskey timeline state has no avatar image cache"))
    images))

(defun misskey-timeline--avatar-image-from-file (file)
  "Return a two-line avatar image for cached FILE, or nil."
  (let ((pixel-size (appkit-chat-avatar-two-line-pixel-size)))
    (or (appkit-media-circular-image-from-file file pixel-size)
        (appkit-media-preview-image-from-file
         file pixel-size pixel-size))))

(defun misskey-timeline--avatar-image (view url)
  "Return VIEW's cached avatar image for URL, or nil."
  (when (and (misskey-timeline--avatars-enabled-p) url)
    (let* ((state (misskey-timeline--state view))
           (images (misskey-timeline--avatar-images state)))
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
  (let* ((state (misskey-timeline--state view))
         (queue (plist-get state :avatar-queue)))
    (if (appkit-task-queue-live-p queue)
        queue
      (setq queue
            (appkit-task-queue-create
             view appkit-media-transfer-concurrency))
      (setf (plist-get state :avatar-queue) queue)
      queue)))

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
  "Refresh VIEW rows using URL after FILE has been cached."
  (when (and file (appkit-view-live-p view))
    (let* ((state (misskey-timeline--state view))
           (images (misskey-timeline--avatar-images state))
           (keys (misskey-timeline--avatar-note-keys state url)))
      (remhash url images)
      (when keys
        (misskey-timeline--sync view keys 'preserve)))))

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

(defun misskey-timeline--prefetch-avatars (view)
  "Schedule missing avatars used by live timeline VIEW."
  (when (and (misskey-timeline--avatars-enabled-p)
             (integerp appkit-media-transfer-concurrency)
             (> appkit-media-transfer-concurrency 0))
    (let ((state (misskey-timeline--state view)))
      (dolist (url
               (delete-dups
                (delq nil
                      (mapcar #'misskey-timeline--avatar-url
                              (plist-get state :items)))))
        (misskey-timeline--prefetch-avatar view url)))))

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
         (revealed (misskey-timeline--revealed-p state key)))
    (misskey-timeline--insert-content primary revealed prefix properties)
    (when (consp quoted)
      (appkit-ui-insert-prefixed-lines
       prefix (format "Quoting %s" (misskey-timeline--user-label quoted))
       :face 'shadow :properties properties)
      (misskey-timeline--insert-content quoted revealed prefix properties))))

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
         (state (misskey-timeline--state view))
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
  (let* ((account (plist-get state :account))
         (origin (misskey--account-origin account))
         (phase (plist-get state :phase))
         (message (plist-get state :message))
         (items (plist-get state :items)))
    (concat
     (propertize (format "Home · %s" origin) 'face 'bold)
     "\n"
     (pcase phase
       ('initial "Loading notes...\n\n")
       ('refresh "Refreshing notes...\n\n")
       ('error (format "Unable to load notes.\n%s\n\n" message))
       (_ (if items "\n" "No notes returned.\n\n"))))))

(defun misskey-timeline--sync (view &optional force-keys position)
  "Synchronize VIEW, redrawing FORCE-KEYS and restoring POSITION."
  (let* ((state (misskey-timeline--state view))
         (rows (misskey-timeline--project (plist-get state :items))))
    (appkit-projection-sync
     view rows
     :header (misskey-timeline--frame state)
     :footer "\ng refresh   n/p note   RET reveal CW   c compose\n"
     :force-keys force-keys
     :position (or position 'preserve))))

(defun misskey-timeline--setup-view (view)
  "Initialize VIEW's keyed home timeline projection."
  (appkit-projection-ensure
   view
   :printer #'misskey-timeline--print-row
   :anchor-property appkit-discussion-key-property
   :no-separator-p t)
  (misskey-timeline--sync view nil 'first))

(defun misskey-timeline--validate-notes (payload)
  "Return validated timeline PAYLOAD."
  (unless (listp payload)
    (error "Misskey timeline response is not a list"))
  (dolist (note payload)
    (unless (and (consp note)
                 (stringp (misskey-timeline--note-id note))
                 (consp (alist-get 'user note)))
      (error "Misskey timeline contains a malformed note")))
  payload)

(defun misskey-timeline--generation-current-p (view state generation)
  "Return non-nil when GENERATION may still update STATE in VIEW."
  (and (appkit-view-live-p view)
       (eq state (appkit-view-state view))
       (= generation (plist-get state :generation))))

(defun misskey-timeline--handle-error (view state generation failure)
  "Show FAILURE for current GENERATION of STATE in VIEW."
  (when (misskey-timeline--generation-current-p view state generation)
    (setf (plist-get state :phase) 'error
          (plist-get state :message) failure)
    (misskey-timeline--sync view)
    (message "%s" failure)))

(defun misskey-timeline--handle-success (view state generation payload)
  "Install timeline PAYLOAD for current GENERATION of STATE in VIEW."
  (when (misskey-timeline--generation-current-p view state generation)
    (condition-case err
        (let ((notes (misskey-timeline--validate-notes payload))
              (initialp (eq (plist-get state :phase) 'initial)))
          (clrhash (plist-get state :revealed-content))
          (clrhash (misskey-timeline--avatar-images state))
          (setf (plist-get state :items) notes
                (plist-get state :phase) 'ready
                (plist-get state :message) nil)
          (misskey-timeline--sync view nil (if initialp 'first 'preserve))
          (misskey-timeline--prefetch-avatars view)
          (message "Loaded %d Misskey notes" (length notes)))
      (error
       (misskey-timeline--handle-error
        view state generation (error-message-string err))))))

(defun misskey-timeline--refresh-view (view)
  "Refresh live Misskey home timeline VIEW."
  (let* ((state (misskey-timeline--state view))
         (phase (plist-get state :phase)))
    (unless (and (integerp misskey-timeline-limit)
                 (<= 1 misskey-timeline-limit 100))
      (user-error "Misskey timeline limit must be between 1 and 100"))
    (when (memq phase '(initial refresh))
      (user-error "The Misskey home timeline is already loading"))
    (let ((generation (1+ (plist-get state :generation)))
          (account (plist-get state :account)))
      (setf (plist-get state :generation) generation
            (plist-get state :phase)
            (if (plist-get state :items) 'refresh 'initial)
            (plist-get state :message) nil)
      (misskey-timeline--sync view)
      (misskey-http-read
       "notes/timeline"
       (list :limit misskey-timeline-limit :allowPartial t)
       (lambda (payload)
         (misskey-timeline--handle-success
          view state generation payload))
       :errback
       (lambda (failure)
         (misskey-timeline--handle-error
          view state generation failure))
       :owner view
       :account account))))

(defun misskey-timeline-refresh ()
  "Refresh the current Misskey home timeline."
  (interactive)
  (if-let* ((view (misskey-timeline--current-view)))
      (misskey-timeline--refresh-view view)
    (user-error "Current buffer is not a Misskey home timeline")))

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
      (let* ((state (misskey-timeline--state view))
             (key (appkit-projection-row-key row))
             (revealed (plist-get state :revealed-content)))
        (if (gethash key revealed)
            (remhash key revealed)
          (puthash key t revealed))
        (misskey-timeline--sync view (list key) key))
    (user-error "Current note has no content warning")))

(defun misskey-timeline-compose ()
  "Open a compose buffer for the current timeline account."
  (interactive)
  (if-let* ((view (misskey-timeline--current-view)))
      (misskey-compose-open
       (plist-get (misskey-timeline--state view) :account))
    (user-error "Current buffer is not a Misskey home timeline")))

(defun misskey-timeline-open ()
  "Open and refresh the selected account's home timeline."
  (let* ((account (misskey--current-account))
         (app (misskey-app account))
         (id 'home)
         (existing (appkit-view-for-id app id))
         (state (or (and existing (appkit-view-state existing))
                    (list :type 'home
                          :account account
                          :items nil
                          :phase 'idle
                          :message nil
                          :generation 0
                          :revealed-content
                          (make-hash-table :test #'equal)
                          :avatar-images
                          (make-hash-table :test #'equal)
                          :avatar-queue nil)))
         (view
          (appkit-open-view
           :app app
           :id id
           :mode #'misskey-timeline-mode
           :buffer-name
           (format "*misskey home: %s@%s*"
                   (misskey--account-auth-source-user account)
                   (string-remove-prefix
                    "https://" (misskey--account-origin account)))
           :state state
           :setup #'misskey-timeline--setup-view
           :select t)))
    (misskey-timeline--refresh-view view)
    view))

(provide 'misskey-timeline)

;;; misskey-timeline.el ends here
