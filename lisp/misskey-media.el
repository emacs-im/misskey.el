;;; misskey-media.el --- Shared Misskey media resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Adapt Misskey avatars and note files into Appkit media resources.  The
;; application resource store owns presentation state; Appkit owns transfer
;; deduplication, bounded scheduling, atomic cache writes, and cancellation.

;;; Code:
(require 'appkit-media-effect)

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

(defun misskey-media--cache-base (source kind surface)
  "Return SOURCE's account-isolated extensionless cache path for KIND."
  (expand-file-name (secure-hash 'sha256 source)
                    (misskey-media--account-directory surface kind)))

(defun misskey-media--entry (view resource-key)
  "Return VIEW's Appkit resource entry for RESOURCE-KEY, or nil."
  (and (appkit-surface-live-p view)
       (gethash resource-key
                (misskey-resource-store (appkit-surface-app view)))))

(defun misskey-media--valid-cache-file-p (file)
  "Return non-nil when FILE is a usable Appkit image cache entry."
  (appkit-media-file-present-p file))

(defun misskey-media--finish-resource (app resource-key entry file)
  "Finish APP RESOURCE-KEY ENTRY with cached FILE or failure."
  (when (appkit-app-live-p app)
    (when
        (eq entry (gethash resource-key (misskey-resource-store app)))
      (unless (misskey-media--valid-cache-file-p file)
        (setq file nil))
      (setf (plist-get entry :status) (if file 'ready 'failed)
            (plist-get entry :file) file
            (plist-get entry :avatar-image) nil
            (plist-get entry :preview-image) nil)
      (misskey-invalidate-resource app resource-key))))

(cl-defun misskey-media-request-resource
    (surface resource-key source kind &key name mime-type)
  "Acquire SOURCE as an account-scoped preview Effect for SURFACE."
  (when
      (and (appkit-surface-live-p surface) resource-key
           (misskey-note--https-url-p source))
    (let*
        ((app (appkit-surface-app surface))
         (store (misskey-resource-store app))
         (current (gethash resource-key store)))
      (if
          (and (equal (plist-get current :source) source)
               (or (eq (plist-get current :status) 'pending)
                   (and (eq (plist-get current :status) 'ready)
                        (misskey-media--valid-cache-file-p
                         (plist-get current :file)))))
          current
        (let*
            ((base (misskey-media--cache-base source kind surface))
             (cached (appkit-media-image-cache-existing-file base))
             (entry
              (list :source source :status (if cached 'ready 'pending)
                    :file cached))
             (effect
              (appkit-effect-create :key (list 'preview resource-key)
                                    :input
                                    (appkit-media-image-acquisition-create
                                     (appkit-media-resource-create
                                      :url source :name name
                                      :mime-type mime-type)
                                     base)
                                    :start
                                    #'appkit-media-image-acquisition-start
                                    :success
                                    (lambda (_input file)
                                      (list :preview-settled app
                                            resource-key entry file))
                                    :failure
                                    (lambda (_input _failure)
                                      (list :preview-settled app
                                            resource-key entry nil)))))
          (puthash resource-key entry store)
          (misskey-invalidate-resource app resource-key)
          (unless cached
            (misskey-dispatch app (list :preview-effect effect)))
          entry)))))

(defun misskey-media-request-avatar (view note)
  "Request NOTE's displayed author avatar for VIEW."
  (when (misskey-media-avatars-enabled-p)
    (when-let* ((url (misskey-note-avatar-url note)))
      (misskey-media-request-resource
       view (list :avatar url) url "avatars"))))

(defun misskey-media-request-file (view file)
  "Request FILE's preview for VIEW."
  (when (misskey-media-previews-enabled-p)
    (when-let* ((url (misskey-file-preview-url file)))
      (misskey-media-request-resource
       view (list :media (alist-get 'id file)) url "media"
       :name (alist-get 'name file)
       :mime-type (alist-get 'type file)))))

(defun misskey-media-prefetch-notes (view notes)
  "Request unguarded avatars and media used by NOTES for VIEW."
  (when (appkit-surface-live-p view)
    (let
        ((revealed-content
          (plist-get (appkit-surface-model view) :revealed-content)))
      (unless (hash-table-p revealed-content)
        (error "Misskey note view has no content-warning state"))
      (dolist (note notes)
        (let*
            ((quoted (misskey-note-quoted-note note))
             (revealed
              (gethash (misskey-note-id note) revealed-content)))
          (misskey-media-request-avatar view note)
          (when quoted (misskey-media-request-avatar view quoted))
          (dolist
              (file
               (append (misskey-note-media-files note)
                       (and quoted (misskey-note-media-files quoted))))
            (unless
                (and (eq (alist-get 'isSensitive file) t)
                     (not revealed))
              (misskey-media-request-file view file))))))))

(defun misskey-media-avatar-image (view note)
  "Return VIEW's cached avatar image for NOTE, or nil."
  (when-let* ((url (and (misskey-media-avatars-enabled-p)
                        (misskey-note-avatar-url note)))
              (entry (misskey-media--entry view (list :avatar url)))
              ((eq (plist-get entry :status) 'ready))
              (file (plist-get entry :file)))
    (or (plist-get entry :avatar-image)
        (let* ((pixel-size (appkit-chat-avatar-two-line-pixel-size))
               (image
                (or (appkit-media-circular-image-from-file file pixel-size)
                    (appkit-media-preview-image-from-file
                     file pixel-size pixel-size))))
          (setf (plist-get entry :avatar-image) image)
          image))))

(defun misskey-media-preview-image (view file)
  "Return VIEW's cached preview image for FILE, or nil."
  (when-let* (((misskey-media-previews-enabled-p))
              (entry
               (misskey-media--entry
                view (list :media (alist-get 'id file))))
              ((eq (plist-get entry :status) 'ready))
              (cached (plist-get entry :file)))
    (or (plist-get entry :preview-image)
        (let ((image
               (appkit-media-preview-image-from-file
                cached
                misskey-timeline-media-preview-width
                misskey-timeline-media-preview-height)))
          (setf (plist-get entry :preview-image) image)
          image))))

(defun misskey-media-open-file (surface file)
  "Send FILE's open intent to its exact initiating SURFACE."
  (unless (appkit-surface-live-p surface)
    (user-error "Misskey media host is closed"))
  (let*
      ((url (misskey-file-original-url file))
       (_validated
        (unless url (user-error "Misskey media URL must use HTTPS")))
       (mime (or (alist-get 'type file) ""))
       (kind
        (cond ((string-prefix-p "video/" mime) 'video)
              ((string-prefix-p "image/" mime) 'image) (t 'file)))
       (entry
        (misskey-media--entry surface
                              (list :media (alist-get 'id file))))
       (cached
        (and (eq (plist-get entry :status) 'ready)
             (equal (plist-get entry :source) url)
             (plist-get entry :file))))
    (unless url (user-error "Misskey media URL must use HTTPS"))
    (misskey-dispatch surface
                      (list :media-open
                            (list :kind kind :directory
                                  (misskey-media--account-directory
                                   surface "media")
                                  :key (secure-hash 'sha256 url)
                                  :resource
                                  (appkit-media-resource-create :file
                                                                cached
                                                                :url
                                                                url
                                                                :name
                                                                (alist-get
                                                                 'name
                                                                 file)
                                                                :mime-type
                                                                mime)
                                  :token
                                  (make-symbol "misskey-media-"))))))

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
  "Insert FILE for VIEW with PREFIX and PROPERTIES.\n\nWhen HIDDEN-P is non-nil, reserve only a sensitive-media placeholder."
  (let*
      ((start (point)) (preview-url (misskey-file-preview-url file))
       (entry
        (and (appkit-surface-live-p view)
             (misskey-media--entry view
                                   (list :media (alist-get 'id file)))))
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

(defun misskey-media--account-directory (surface kind)
  "Return the account-isolated KIND cache for SURFACE."
  (expand-file-name
   (format "%s/%s/" (secure-hash 'sha256 (prin1-to-string
                                          (misskey--account-key (plist-get (appkit-surface-model surface) :account)))) kind)
   (locate-user-emacs-file "misskey/")))

(defun misskey-media-update (model message)
  "Commit media acquisition and schedule presentation for MODEL MESSAGE."
  (pcase message
    (`(:media-open ,intent)
     (let*
         ((next model) (kind (plist-get intent :kind))
          (base
           (expand-file-name (plist-get intent :key)
                             (plist-get intent :directory)))
          (resource (plist-get intent :resource))
          (effect
           (appkit-effect-create :key 'misskey-media-acquire :input
                                 (pcase kind
                                   ('video resource)
                                   ('image
                                    (appkit-media-image-acquisition-create
                                     resource base))
                                   (_
                                    (let
                                        ((target
                                          (expand-file-name
                                           (concat
                                            (plist-get intent :key)
                                            "-"
                                            (appkit-media-sanitize-filename
                                             (or
                                              (appkit-media-resource-name
                                               resource)
                                              "media.bin")))
                                           (plist-get intent
                                                      :directory))))
                                      (appkit-media-acquisition-create
                                       (if
                                           (appkit-media-file-present-p
                                            target)
                                           (appkit-media-resource-create
                                            :file target)
                                         resource)
                                       target))))
                                 :start
                                 (pcase kind
                                   ('video
                                    (lambda
                                      (_context input _observe resolve
                                                _reject)
                                      (funcall resolve input)
                                      nil))
                                   ('image
                                    #'appkit-media-image-acquisition-start)
                                   (_ #'appkit-media-acquisition-start))
                                 :success
                                 (lambda (_input file)
                                   (list :media-acquired intent file))
                                 :failure
                                 (lambda (_input failure)
                                   (list :media-failed
                                         (plist-get intent :token)
                                         failure)))))
       (setq next (plist-put next :media-error nil) next
             (plist-put next :media-intent (plist-get intent :token)))
       (setq next (plist-put next :media-status 'acquiring))
       (appkit-next :model next :render
                    (appkit-projection-change-create :frame-p t)
                    :commands
                    (list
                     (appkit-command-cancel-effect
                      'misskey-media-present)
                     (appkit-command-start-effect effect)))))
    (`(:media-acquired ,intent ,file)
     (if
         (eq (plist-get model :media-intent) (plist-get intent :token))
         (let*
             ((next model)
              (video-p (eq (plist-get intent :kind) 'video))
              (input
               (if video-p
                   (appkit-media-video-presentation-create file :label
                                                           "Misskey media"
                                                           :cache-key
                                                           (plist-get
                                                            intent
                                                            :key)
                                                           :cache-directory
                                                           (plist-get
                                                            intent
                                                            :directory))
                 file)))
           (setq next (plist-put next :media-status 'presenting))
           (setq next (plist-put next :media-file file))
           (appkit-next :model next :render appkit-render-none
                        :commands
                        (list
                         (appkit-command-start-effect
                          (appkit-effect-create :key
                                                'misskey-media-present
                                                :input input :start
                                                (if video-p
                                                    #'appkit-media-video-presentation-start
                                                  #'appkit-media-file-presentation-start)
                                                :success
                                                (lambda (_input _result)
                                                  (list
                                                   :media-presented
                                                   (plist-get intent
                                                              :token)))
                                                :failure
                                                (lambda (_input failure)
                                                  (list :media-failed
                                                        (plist-get
                                                         intent :token)
                                                        failure)))))))
       (appkit-next :model model :render appkit-render-none)))
    (`(:media-failed ,token ,failure)
     (if (eq token (plist-get model :media-intent))
         (let ((next model))
           (setq next (plist-put next :media-status 'failed))
           (setq next
                 (plist-put next :media-error (format "%s" failure)))
           (appkit-next :model next :render
                        (appkit-projection-change-create :frame-p t)))
       (appkit-next :model model :render appkit-render-none)))
    (`(:media-presented ,token)
     (appkit-next :model
                  (if (eq token (plist-get model :media-intent))
                      (plist-put model :media-status 'ready)
                    model)
                  :render appkit-render-none))
    (_ (appkit-next :model model :render appkit-render-none))))

(provide 'misskey-media)

;;; misskey-media.el ends here
