;;; misskey-media.el --- Shared Misskey media resources -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Adapt Misskey avatars and note files into Appkit media resources.  The
;; application resource store owns presentation state; Appkit owns transfer
;; deduplication, bounded scheduling, atomic cache writes, and cancellation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-chat-avatar)
(require 'appkit-core)
(require 'appkit-invalidation)
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

(defun misskey-media--cache-base (source kind)
  "Return the extensionless cache path for SOURCE of KIND."
  (expand-file-name
   (secure-hash 'sha256 source)
   (locate-user-emacs-file (format "misskey/%s/" kind))))

(defun misskey-media--entry (view resource-key)
  "Return VIEW's Appkit resource entry for RESOURCE-KEY, or nil."
  (and (appkit-view-live-p view)
       (gethash resource-key
                (appkit-app-resource-store (appkit-view-app view)))))

(defun misskey-media--valid-cache-file-p (file)
  "Return non-nil when FILE is a usable Appkit image cache entry."
  (appkit-media-file-present-p file))


(defun misskey-media--finish-resource (app resource-key entry file)
  "Finish APP RESOURCE-KEY ENTRY with cached FILE or failure."
  (when (appkit-app-live-p app)
    (when-let* ((handle (plist-get entry :handle)))
      (appkit-retire-handle handle)
      (setf (plist-get entry :handle) nil))
    (when (eq entry (gethash resource-key
                             (appkit-app-resource-store app)))
      (unless (misskey-media--valid-cache-file-p file)
        (setq file nil))
      (setf (plist-get entry :status) (if file 'ready 'failed)
            (plist-get entry :file) file
            (plist-get entry :avatar-image) nil
            (plist-get entry :preview-image) nil)
      (misskey-invalidate-resource app resource-key))))

(cl-defun misskey-media-request-resource
    (view resource-key source kind &key name mime-type)
  "Acquire SOURCE of KIND as RESOURCE-KEY on behalf of VIEW.

The view's application shares resource state.  Appkit shares and bounds the
atomic transfer.  NAME and MIME-TYPE provide optional media hints.  Return the
canonical application resource entry."
  (when (and (appkit-view-live-p view)
             resource-key
             (misskey-note--https-url-p source))
    (let* ((app (appkit-view-app view))
           (store (appkit-app-resource-store app))
           (current (gethash resource-key store)))
      (cond
       ((and (eq (plist-get current :status) 'ready)
             (equal (plist-get current :source) source)
             (misskey-media--valid-cache-file-p
              (plist-get current :file)))
        current)
       ((and (eq (plist-get current :status) 'pending)
             (equal (plist-get current :source) source))
        current)
       (t
        (when (eq (plist-get current :status) 'pending)
          (when-let* ((handle (plist-get current :handle)))
            (setf (plist-get current :handle) nil)
            (appkit-cancel-handle handle)))
        (let* ((cache-base (misskey-media--cache-base source kind))
               (cached (appkit-media-image-cache-existing-file cache-base))
               (entry (list :source source
                            :status (if cached 'ready 'pending)
                            :file cached
                            :handle nil))
               transfer)
          (puthash resource-key entry store)
          (misskey-invalidate-resource app resource-key)
          (unless cached
            (setq transfer
                  (appkit-media-cache-image-resource-async
                   (appkit-media-resource-create
                    :url source :name name :mime-type mime-type)
                   cache-base
                   (lambda (file)
                     (misskey-media--finish-resource
                      app resource-key entry file))
                   (lambda (_message)
                     (misskey-media--finish-resource
                      app resource-key entry nil))))
            (when (and (eq (plist-get entry :status) 'pending)
                       (appkit-media-transfer-p transfer))
              (setf (plist-get entry :handle)
                    (appkit-register-handle
                     app 'function transfer
                     #'appkit-media-cancel-transfer))))
          entry))))))

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
  (when (appkit-view-live-p view)
    (let ((revealed-content
           (plist-get (appkit-view-state view) :revealed-content)))
      (unless (hash-table-p revealed-content)
        (error "Misskey note view has no content-warning state"))
      (dolist (note notes)
        (let* ((quoted (misskey-note-quoted-note note))
               (revealed
                (gethash (misskey-note-id note) revealed-content)))
          (misskey-media-request-avatar view note)
          (when quoted
            (misskey-media-request-avatar view quoted))
          (dolist (file
                   (append
                    (misskey-note-media-files note)
                    (and quoted (misskey-note-media-files quoted))))
            (unless (and (eq (alist-get 'isSensitive file) t)
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

(defun misskey-media-open-file (view file)
  "Open Misskey FILE's original Drive resource from VIEW through Appkit."
  (let* ((type (alist-get 'type file))
         (url (misskey-file-original-url file))
         (_ (unless url
              (user-error "Misskey media URL must use HTTPS")))
         (kind
          (cond
           ((string-prefix-p "video/" type) 'video)
           ((string-prefix-p "image/" type) 'image)
           (t 'file)))
         (entry
          (misskey-media--entry view (list :media (alist-get 'id file))))
         (cached (and (eq (plist-get entry :status) 'ready)
                      (equal (plist-get entry :source) url)
                      (plist-get entry :file))))
    (appkit-media-open-resource
     (appkit-media-resource-create
      :file cached
      :url url
      :name (alist-get 'name file)
      :mime-type type)
     :kind kind
     :cache-key (alist-get 'id file)
     :cache-directory (locate-user-emacs-file "misskey/media/")
     :client-label "Misskey media"
     :owner view)))

(defun misskey-media-alt-text (file)
  "Return accessible fallback text for FILE."
  (or (and (stringp (alist-get 'comment file))
           (not (string-empty-p (alist-get 'comment file)))
           (alist-get 'comment file))
      (and (stringp (alist-get 'name file))
           (not (string-empty-p (alist-get 'name file)))
           (format "[%s]" (alist-get 'name file)))
      "[media]"))

(defun misskey-media-insert-file (view file prefix properties hidden-p)
  "Insert FILE for VIEW with PREFIX and PROPERTIES.

When HIDDEN-P is non-nil, reserve only a sensitive-media placeholder."
  (let* ((start (point))
         (preview-url (misskey-file-preview-url file))
         (entry
          (and (appkit-view-live-p view)
               (misskey-media--entry
                view (list :media (alist-get 'id file)))))
         (image (and (not hidden-p)
                     (misskey-media-preview-image view file)))
         (alt (misskey-media-alt-text file)))
    (cond
     (hidden-p (insert "[sensitive media]"))
     (image
      (appkit-media-insert-image-slices
       image
       (lambda () (misskey-media-open-file view file))
       nil alt "Open Misskey media"))
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
        (preview-url
         (format "%s [preview unavailable]" alt))
        (t alt)))
      (appkit-ui-add-action
       start (point)
       (lambda () (misskey-media-open-file view file))
       :help-echo "Open Misskey media"
       :face 'link)))
    (insert "\n")
    (appkit-ui-apply-line-prefix start (point) prefix)
    (add-text-properties start (point) properties)))

(defun misskey-media-insert-note-files
    (view note revealed prefix properties)
  "Insert NOTE media for VIEW using REVEALED, PREFIX, and PROPERTIES."
  (dolist (file (misskey-note-media-files note))
    (misskey-media-insert-file
     view file prefix properties
     (and (eq (alist-get 'isSensitive file) t)
          (not revealed)))))

(provide 'misskey-media)

;;; misskey-media.el ends here
