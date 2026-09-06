;;; misskey-media.el --- Shared Misskey media resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Adapt Misskey avatars and note files into Appkit media resources.  The
;; application resource store owns presentation state; Appkit owns transfer
;; deduplication, bounded scheduling, atomic cache writes, and cancellation.

;;; Code:
(require 'appkit-media-effect)
(require 'appkit-task-queue)

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-chat-avatar)
(require 'appkit-core)
(require 'appkit-projection)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-ui)
(require 'misskey-core)
(require 'misskey-note)

(defcustom misskey-timeline-show-avatars t
  "When non-nil, fetch and display Misskey author avatars.

Avatar requests run only when Emacs can display inline images."
  :type 'boolean
  :group 'misskey)

(defcustom misskey-timeline-show-media t
  "When non-nil, fetch and display Misskey media previews.

Preview requests run only when Emacs can display inline images.  Sensitive
files stay hidden until their note is explicitly revealed."
  :type 'boolean
  :group 'misskey)

(defcustom misskey-timeline-media-preview-width 480
  "Maximum width in pixels for an inline Misskey media preview."
  :type 'integer
  :group 'misskey)

(defcustom misskey-timeline-media-preview-height 320
  "Maximum height in pixels for an inline Misskey media preview."
  :type 'integer
  :group 'misskey)

(defun misskey-media-avatars-enabled-p ()
  "Return non-nil when Misskey avatars can be displayed."
  (and misskey-timeline-show-avatars
       (appkit-media-inline-image-rendering-available-p)))

(defun misskey-media-previews-enabled-p ()
  "Return non-nil when Misskey media previews can be displayed."
  (and misskey-timeline-show-media
       (appkit-media-inline-image-rendering-available-p)))

(defun misskey-media--cache-base (source kind account)
  "Return SOURCE's account-isolated extensionless cache path for KIND."
  (expand-file-name (secure-hash 'sha256 source)
                    (misskey-media--cache-directory account kind)))

(defun misskey-media--entry (view resource-key)
  "Return VIEW's Appkit resource entry for RESOURCE-KEY, or nil."
  (and (appkit-surface-live-p view)
       (gethash resource-key
                (misskey-resource-store (appkit-surface-app view)))))

(cl-defun misskey-media-request-resource
    (surface resource-key source kind &key name mime-type)
  "Post an account-scoped preview demand for SURFACE."
  (when (and (appkit-surface-live-p surface) resource-key
             (misskey-note--https-url-p source))
    (misskey-dispatch
     (appkit-surface-app surface)
     (list :preview-requested
           (list (list resource-key source kind
                       (misskey-media--cache-base
                        source kind (plist-get (appkit-surface-model surface) :account))
                       name mime-type))))
    (misskey-media--entry surface resource-key)))

(defun misskey-media-prefetch-notes (view notes)
  "Post one demand batch for unguarded previews used by NOTES in VIEW."
  (when (appkit-surface-live-p view)
    (when-let* ((demands (misskey-media--preview-demands
                          (appkit-surface-model view) notes)))
      (misskey-dispatch (appkit-surface-app view)
                        (list :preview-requested demands)))))

(defun misskey-media-avatar-image (view note)
  "Return VIEW's cached avatar image for NOTE, or nil."
  (when-let* ((url (and (misskey-media-avatars-enabled-p)
                        (misskey-note-avatar-url note)))
              (entry (misskey-media--entry view (list :avatar url)))
              ((eq (plist-get entry :status) 'ready))
              (file (plist-get entry :file)))
    (or (plist-get entry :image)
        (let ((pixel-size (appkit-chat-avatar-two-line-pixel-size)))
          (setf (plist-get entry :image)
                (or (appkit-media-circular-image-from-file file pixel-size)
                    (appkit-media-preview-image-from-file
                     file pixel-size pixel-size)))))))

(defun misskey-media-preview-image (view file)
  "Return VIEW's cached preview image for FILE, or nil."
  (when-let* (((misskey-media-previews-enabled-p))
              (entry (misskey-media--entry view (list :media (alist-get 'id file))))
              ((eq (plist-get entry :status) 'ready))
              (cached (plist-get entry :file)))
    (or (plist-get entry :image)
        (setf (plist-get entry :image)
              (appkit-media-preview-image-from-file
               cached misskey-timeline-media-preview-width
               misskey-timeline-media-preview-height)))))

(defun misskey-media-open-file (surface file)
  "Send FILE's open intent to its exact initiating SURFACE."
  (unless (appkit-surface-live-p surface)
    (user-error "Misskey media host is closed"))
  (let ((url (misskey-file-original-url file)))
    (unless url (user-error "Misskey media URL must use HTTPS"))
    (let* ((mime (or (alist-get 'type file) ""))
           (kind (cond ((string-prefix-p "video/" mime) 'video)
                       ((string-prefix-p "image/" mime) 'image)
                       (t 'file)))
           (entry (misskey-media--entry surface (list :media (alist-get 'id file))))
           (cached (and (eq (plist-get entry :status) 'ready)
                        (equal (plist-get entry :source) url)
                        (plist-get entry :file))))
      (misskey-dispatch
       surface
       (list :media-open
             (list :kind kind
                   :directory (misskey-media--cache-directory
                               (plist-get (appkit-surface-model surface) :account)
                               "media")
                   :key (secure-hash 'sha256 url)
                   :resource (appkit-media-resource-create
                              :file cached :url url :name (alist-get 'name file)
                              :mime-type mime)
                   :token (make-symbol "misskey-media-")))))))

(defun misskey-media-alt-text (file)
  "Return accessible fallback text for FILE."
  (or (and (stringp (alist-get 'comment file))
           (not (string-empty-p (alist-get 'comment file)))
           (alist-get 'comment file))
      (and (stringp (alist-get 'name file))
           (not (string-empty-p (alist-get 'name file)))
           (format "[%s]" (alist-get 'name file)))
      "[media]"))

(defun misskey-media-insert-file
    (view file prefix properties hidden-p)
  "Insert FILE for VIEW with PREFIX and PROPERTIES.

When HIDDEN-P is non-nil, reserve only a sensitive-media placeholder."
  (let*
      ((start (point)) (preview-url (misskey-file-preview-url file))
       (entry
        (misskey-media--entry view (list :media (alist-get 'id file))))
       (image
        (and (not hidden-p) (misskey-media-preview-image view file)))
       (alt (misskey-media-alt-text file)))
    (cond (hidden-p (insert "[sensitive media]"))
          (image
           (appkit-media-insert-image-slices image
                                             (lambda ()
                                               (misskey-media-open-file
                                                view file))
                                             nil alt
                                             "Open Misskey media"))
          (t
           (insert
            (cond
             ((not misskey-timeline-show-media)
              (format "%s [preview disabled]" alt))
             ((string-prefix-p "audio/" (alist-get 'type file))
              (format "%s [audio]" alt))
             ((eq (plist-get entry :status) 'failed)
              (format "%s [preview failed; refresh to retry]" alt))
             ((and preview-url (misskey-media-previews-enabled-p))
              (format "%s loading preview…" alt))
             (preview-url (format "%s [preview unavailable]" alt))
             (t alt)))
           (appkit-ui-add-action start (point)
                                 (lambda ()
                                   (misskey-media-open-file view file))
                                 :help-echo "Open Misskey media" :face
                                 'link)))
    (insert "\n") (appkit-ui-apply-line-prefix start (point) prefix)
    (add-text-properties start (point) properties)))

(defun misskey-media-insert-note-files
    (view note revealed prefix properties)
  "Insert NOTE media for VIEW using REVEALED, PREFIX, and PROPERTIES."
  (dolist (file (misskey-note-media-files note))
    (misskey-media-insert-file
     view file prefix properties
     (and (eq (alist-get 'isSensitive file) t)
          (not revealed)))))

(defun misskey-media-update (model message)
  "Commit media acquisition and schedule presentation for MODEL MESSAGE."
  (let ((render appkit-render-none) commands)
    (pcase message
      (`(:media-open ,intent)
       (let* ((kind (plist-get intent :kind))
              (token (plist-get intent :token))
              (base (expand-file-name (plist-get intent :key)
                                      (plist-get intent :directory)))
              (resource (plist-get intent :resource))
              (effect
               (appkit-effect-create
                :key 'misskey-media-acquire
                :input (pcase kind
                         ('video resource)
                         ('image (appkit-media-image-acquisition-create resource base))
                         (_ (let ((target (concat base "-"
                                                  (appkit-media-sanitize-filename
                                                   (or (appkit-media-resource-name resource)
                                                       "media.bin")))))
                              (appkit-media-acquisition-create
                               (if (appkit-media-file-present-p target)
                                   (appkit-media-resource-create :file target)
                                 resource)
                               target))))
                :start (pcase kind
                         ('video (lambda (_context input _observe resolve _reject)
                                   (funcall resolve input)
                                   nil))
                         ('image #'appkit-media-image-acquisition-start)
                         (_ #'appkit-media-acquisition-start))
                :success (lambda (_input file) (list :media-acquired intent file))
                :failure (lambda (_input failure) (list :media-failed token failure)))))
         (setf (plist-get model :media-error) nil
               (plist-get model :media-intent) token
               (plist-get model :media-status) 'acquiring)
         (setq render (appkit-projection-change-create :frame-p t)
               commands (list (appkit-command-cancel-effect 'misskey-media-present)
                              (appkit-command-start-effect effect)))))
      (`(:media-acquired ,intent ,file)
       (let ((token (plist-get intent :token)))
         (when (eq (plist-get model :media-intent) token)
           (let* ((video-p (eq (plist-get intent :kind) 'video))
                  (input (if video-p
                             (appkit-media-video-presentation-create
                              file :label "Misskey media"
                              :cache-key (plist-get intent :key)
                              :cache-directory (plist-get intent :directory))
                           file)))
             (setf (plist-get model :media-status) 'presenting
                   (plist-get model :media-file) file)
             (setq commands
                   (list
                    (appkit-command-start-effect
                     (appkit-effect-create
                      :key 'misskey-media-present :input input
                      :start (if video-p
                                 #'appkit-media-video-presentation-start
                               #'appkit-media-file-presentation-start)
                      :success (lambda (_input _result) (list :media-presented token))
                      :failure (lambda (_input failure) (list :media-failed token failure))))))))))
      (`(:media-failed ,token ,failure)
       (when (eq token (plist-get model :media-intent))
         (setf (plist-get model :media-status) 'failed
               (plist-get model :media-error) (format "%s" failure))
         (setq render (appkit-projection-change-create :frame-p t))))
      (`(:media-presented ,token)
       (when (eq token (plist-get model :media-intent))
         (setf (plist-get model :media-status) 'ready))))
    (appkit-next :model model :render render :commands commands)))

(defconst misskey-media--preview-concurrency 6
  "Preview Effect slots reserved per App, independent of transport queuing.")

(defvar misskey-media--preview-commands nil
  "Effect commands accumulated while advancing the preview task queue.")

(defun misskey-media--preview-demands (model notes)
  "Return data-only preview demands for NOTES under Surface MODEL."
  (let ((revealed-content (plist-get model :revealed-content))
        (account (plist-get model :account))
        (avatars (misskey-media-avatars-enabled-p))
        (media (misskey-media-previews-enabled-p))
        (seen (make-hash-table :test #'equal)) demands)
    (unless (hash-table-p revealed-content)
      (error "Misskey note view has no content-warning state"))
    (cl-labels
        ((demand (key source kind &optional name mime)
           (when (and source (misskey-note--https-url-p source)
                      (not (gethash key seen)))
             (puthash key t seen)
             (push (list key source kind
                         (misskey-media--cache-base source kind account)
                         name mime)
                   demands))))
      (dolist (note notes)
        (let ((quoted (misskey-note-quoted-note note))
              (revealed (gethash (misskey-note-id note) revealed-content)))
          (dolist (item (delq nil (list note quoted)))
            (when avatars
              (when-let* ((url (misskey-note-avatar-url item)))
                (demand (list :avatar url) url "avatars")))
            (when media
              (dolist (file (misskey-note-media-files item))
                (unless (and (eq (alist-get 'isSensitive file) t)
                             (not revealed))
                  (demand (list :media (alist-get 'id file))
                          (misskey-file-preview-url file) "media"
                          (alist-get 'name file) (alist-get 'type file)))))))))
    (nreverse demands)))

(defun misskey-media-prefetch-command (context model &optional notes)
  "Return one App demand command for Surface MODEL and optional NOTES."
  (when-let* ((demands (misskey-media--preview-demands
                        model (or notes (plist-get model :items)))))
    (appkit-command-post-message
     :target (appkit-transition-context-parent-address context)
     :message (list :preview-requested demands) :delivery 'report)))

(defun misskey-media--queue-preview (queue key entry input)
  "Reserve a logical preview slot in QUEUE for KEY, ENTRY and INPUT.
Only the App update submits or completes tasks.  Queue starters emit commands;
they never start transport.  The App Effect runtime owns physical cancellation."
  (appkit-task-queue-submit
   queue key
   (lambda (complete)
     (setf (plist-get entry :complete) complete)
     (let ((token (plist-get entry :token)))
       (push
        (appkit-command-start-effect
         (appkit-effect-create
          :key (list 'preview key) :input input
          :start #'appkit-media-image-acquisition-start
          :success (lambda (_input file)
                     (list :preview-settled key token file))
          :failure (lambda (_input _failure)
                     (list :preview-settled key token nil))))
        misskey-media--preview-commands))
     nil)))

(defun misskey-media-app-update (_context model message)
  "Commit preview domain MESSAGE against account MODEL, or return nil.
The existing Appkit task queue bounds reserved Effects, including transports
waiting in the shared download scheduler.  Completion callbacks only enter
the Effect gate; queue advancement and resource state belong to this update."
  (when (memq (car-safe message)
              '(:preview-requested :preview-settled :preview-cancel))
    (let* ((store (misskey--session-resources model))
           (app (gethash (misskey--account-key (misskey--session-account model))
                         misskey--apps))
           (queue (gethash 'misskey-media--preview-queue store))
           (misskey-media--preview-commands nil)
           changed)
      (pcase message
        (`(:preview-requested ,demands)
         (let (requests cancellations)
           ;; Revoke an entire replacement batch before the queue pumps;
           ;; otherwise cancelling one key could start another superseded key.
           (dolist (demand demands)
             (pcase-let* ((`(,key ,source ,_kind ,base ,name ,mime) demand)
                          (current (gethash key store)))
               (unless (and (equal source (plist-get current :source))
                            (or (eq (plist-get current :status) 'pending)
                                (and (eq (plist-get current :status) 'ready)
                                     (appkit-media-file-present-p
                                      (plist-get current :file)))))
                 (when (and queue (appkit-task-queue-pending-p queue key))
                   (push key cancellations)
                   (when (plist-get current :complete)
                     (push (appkit-command-cancel-effect (list 'preview key))
                           misskey-media--preview-commands)))
                 (let* ((cached (appkit-media-image-cache-existing-file base))
                        (entry (list :source source :status (if cached 'ready 'pending)
                                     :file cached :token (make-symbol "preview")
                                     :complete nil :image nil)))
                   (puthash key entry store)
                   (push key changed)
                   (unless cached
                     (push (list key entry
                                 (appkit-media-image-acquisition-create
                                  (appkit-media-resource-create
                                   :url source :name name :mime-type mime)
                                  base))
                           requests))))))
           (when cancellations
             (appkit-task-queue-cancel-keys queue cancellations))
           (when requests
             (unless queue
               (setq queue (appkit-task-queue-create
                            app misskey-media--preview-concurrency))
               (puthash 'misskey-media--preview-queue queue store))
             (dolist (request (nreverse requests))
               (apply #'misskey-media--queue-preview queue request)))))
        (`(:preview-settled ,key ,token ,file)
         (let ((entry (gethash key store)))
           (when (and (eq token (plist-get entry :token))
                      (eq (plist-get entry :status) 'pending))
             (unless (appkit-media-file-present-p file) (setq file nil))
             (setf (plist-get entry :status) (if file 'ready 'failed)
                   (plist-get entry :file) file)
             (when-let* ((complete (plist-get entry :complete)))
               (setf (plist-get entry :complete) nil)
               (funcall complete))
             (push key changed))))
        (`(:preview-cancel ,key)
         (let ((entry (gethash key store)))
           (when (eq (plist-get entry :status) 'pending)
             (setf (plist-get entry :status) 'failed
                   (plist-get entry :complete) nil
                   (plist-get entry :token) nil)
             (push (appkit-command-cancel-effect (list 'preview key))
                   misskey-media--preview-commands)
             (when queue (appkit-task-queue-cancel-key queue key))
             (push key changed)))))
      (misskey-invalidate-resources app (nreverse changed))
      (appkit-next :model model :render appkit-render-none
                   :commands (nreverse misskey-media--preview-commands)))))

(defun misskey-media--cache-directory (account kind)
  "Return the account-isolated KIND cache directory for ACCOUNT."
  (expand-file-name
   (format "%s/%s/"
           (secure-hash 'sha256 (prin1-to-string (misskey--account-key account)))
           kind)
   (locate-user-emacs-file "misskey/")))

(provide 'misskey-media)

;;; misskey-media.el ends here
