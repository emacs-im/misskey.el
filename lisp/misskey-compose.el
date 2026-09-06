;;; misskey-compose.el --- Compose Misskey notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provide the standalone Appkit-backed editor for plain-text Misskey notes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-compose)
(require 'appkit-chat-compose)
(require 'appkit-core)
(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-note)

(defvar-local misskey-compose--account nil
  "Account captured when the current draft was opened.")

(defvar-local misskey-compose-visibility 'public
  "Visibility selected for the current draft.")

(defvar-local misskey-compose-reply-id nil
  "Initial reply target ID for the current draft.")

(defvar-local misskey-compose-renote-id nil
  "Initial quote target ID for the current draft.")

(defvar-local misskey-compose-target-label nil
  "Readable target label shown in the current draft.")

(defvar-local misskey-compose--request nil
  "Opaque request currently owned by this draft, or nil.")

(defvar-local misskey-compose--submission-token nil
  "Identity of the active publish chain, or nil.")

(defconst misskey-compose--visibility-choices
  '((public . "Public")
    (home . "Home")
    (followers . "Followers"))
  "Supported Misskey visibility values and labels.")

(defvar misskey-compose--serial 0
  "Serial used to name fresh compose buffers.")

(defconst misskey-compose--max-attachments 16
  "Maximum number of Drive files attached to one Misskey note.")

(defun misskey-compose--set-body-read-only (read-only)
  "Set the current compose body READ-ONLY state."
  (setq-local buffer-read-only (and read-only t)))

(defvar-keymap misskey-compose-mode-map
  :doc "Keymap for `misskey-compose-mode'."
  "C-c C-c" #'misskey-compose-send
  "C-c C-k" #'misskey-compose-cancel
  "C-c C-n" #'misskey-compose-add-note
  "C-c C-p" #'misskey-compose-remove-note
  "C-c C-v" #'misskey-compose-set-visibility
  "C-c C-a" #'misskey-compose-attach-file
  "C-c C-d" #'misskey-compose-remove-attachment)

(define-derived-mode misskey-compose-mode appkit-chat-compose-mode "Misskey-Compose"
  "Major mode for composing a standalone Misskey note."
  (setq-local misskey-compose--request nil
              misskey-compose--submission-token nil)
  (add-hook 'kill-buffer-hook #'misskey-compose--kill-buffer-cleanup -90 t)
  (add-hook 'change-major-mode-hook
            #'misskey-compose--kill-buffer-cleanup -90 t))

(defun misskey-compose--account ()
  "Return the account bound to the current draft."
  (or misskey-compose--account
      (misskey--current-account)))

(defun misskey-compose--context ()
  "Return the generated context shown above the note body."
  (let ((origin
         (misskey--account-origin (misskey-compose--account))))
    (cond
     (misskey-compose-reply-id
      (format "Replying to %s\non %s\n"
              (or misskey-compose-target-label
                  misskey-compose-reply-id)
              origin))
     (misskey-compose-renote-id
      (format "Quoting %s\non %s\n"
              (or misskey-compose-target-label
                  misskey-compose-renote-id)
              origin))
     (t
      (format "New note on %s\n" origin)))))

(defun misskey-compose--status-fields ()
  "Return generated status fields for the current note."
  (let* ((items (appkit-chat-compose-items))
         (file-count
          (cl-loop for item in items
                   sum (length (plist-get item :attachments))))
         (fields
          (list
           (list :label "Visibility"
                 :value
                 (or (alist-get misskey-compose-visibility
                                misskey-compose--visibility-choices)
                     (format "%s" misskey-compose-visibility)))
           (list :label "State"
                 :value (or (appkit-compose-status-text) "Draft")))))
    (when (> (length items) 1)
      (push (list :label "Notes" :value (format "%d" (length items)))
            fields))
    (when (> file-count 0)
      (push (list :label "Files" :value (format "%d" file-count)) fields))
    fields))

(defun misskey-compose--attachment-section (item)
  "Return Appkit's attachment section for compose ITEM."
  (when-let* ((attachments (plist-get item :attachments)))
    (list
     :title "Attachments"
     :items
     (mapcar
      (lambda (attachment)
        (let ((path (plist-get attachment :path)))
          (list
           :label (file-name-nondirectory path)
           :state (if (plist-get attachment :drive-id)
                      "uploaded"
                    "pending upload"))))
      attachments))))

(defun misskey-compose--parts ()
  "Return Appkit compose parts for the current draft."
  (let* ((items (appkit-chat-compose-items))
         (total (length items))
         (index 0))
    (mapcar
     (lambda (item)
       (setq index (1+ index))
       (list :title (and (> total 1)
                         (format "Note %d/%d" index total))
             :attachments (misskey-compose--attachment-section item)))
     items)))

(defun misskey-compose--footer ()
  "Return the generated compose command footer."
  (propertize
   (if (appkit-compose-operation-active-p)
       "Publishing; wait for the server response"
     (concat "C-c C-c publish   C-c C-v visibility   "
             "C-c C-a attach   C-c C-d detach\n"
             "C-c C-n add note   C-c C-p drop note   C-c C-k cancel"))
   'face 'shadow))

(defun misskey-compose--refresh ()
  "Refresh generated compose presentation for the current buffer."
  (appkit-chat-compose-refresh))

(cl-defun misskey-compose-open
    (&optional account &key reply-id renote-id target-label
               (visibility 'public))
  "Create, display, and return a fresh compose buffer for ACCOUNT.

REPLY-ID or RENOTE-ID sets the first note's target; they are mutually
exclusive.  TARGET-LABEL describes that target.  VISIBILITY is `public',
`home', or `followers'.  ACCOUNT defaults to current customization."
  (when (and reply-id renote-id)
    (error "A Misskey draft cannot reply and quote simultaneously"))
  (unless (assq visibility misskey-compose--visibility-choices)
    (error "Unsupported Misskey visibility: %S" visibility))
  (let* ((target (or account (misskey--current-account)))
         (buffer (generate-new-buffer
                  (format "*misskey compose %d*"
                          (cl-incf misskey-compose--serial)))))
    (pop-to-buffer buffer)
    (misskey-compose-mode)
    (setq-local misskey-compose--account target)
    (setq-local misskey-compose-visibility visibility)
    (setq-local misskey-compose-reply-id reply-id)
    (setq-local misskey-compose-renote-id renote-id)
    (setq-local misskey-compose-target-label target-label)
    (appkit-chat-compose-setup
     :app (misskey-app target)
     :context-function #'misskey-compose--context
     :status-fields-function #'misskey-compose--status-fields
     :parts-function #'misskey-compose--parts
     :footer-function #'misskey-compose--footer)
    buffer))

(defun misskey-compose--copy-item (item)
  "Return a draft copy of compose ITEM with independent attachments."
  (let ((copy (copy-sequence item)))
    (plist-put
     copy :attachments
     (mapcar #'copy-sequence (plist-get item :attachments)))))

(defun misskey-compose--snapshot-items ()
  "Return validated independent copies of every compose item."
  (mapcar
   (lambda (item)
     (let* ((copy (misskey-compose--copy-item item))
            (text (or (plist-get copy :text) ""))
            (attachments (plist-get copy :attachments)))
       (when (> (length attachments) misskey-compose--max-attachments)
         (user-error "A Misskey note accepts at most %d attachments"
                     misskey-compose--max-attachments))
       (dolist (attachment attachments)
         (let ((drive-id (plist-get attachment :drive-id))
               (path (plist-get attachment :path)))
           (when (and (stringp path) (file-remote-p path))
             (user-error "Remote attachment paths are unsupported: %s" path))
           (unless (or (and (stringp drive-id)
                            (not (string-empty-p drive-id)))
                       (and (stringp path)
                            (file-regular-p path)
                            (file-readable-p path)))
             (user-error "Attachment is no longer readable: %s" path))))
       (unless (or attachments (string-match-p "[^[:space:]]" text))
         (user-error "Each note needs text or an attachment"))
       copy))
   (appkit-chat-compose-items)))

(defun misskey-compose-attach-file (file)
  "Attach readable FILE to the current Misskey compose part."
  (interactive "fAttach file: ")
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (let* ((path (expand-file-name file))
         (item (appkit-chat-compose-current-item))
         (attachments (copy-sequence (plist-get item :attachments))))
    (when (file-remote-p path)
      (user-error "Remote attachment paths are unsupported: %s" path))
    (unless (and (file-regular-p path) (file-readable-p path))
      (user-error "Attachment is not a readable regular file: %s" path))
    (when (>= (length attachments) misskey-compose--max-attachments)
      (user-error "A Misskey note accepts at most %d attachments"
                  misskey-compose--max-attachments))
    (when (cl-find path attachments
                   :key (lambda (attachment)
                          (plist-get attachment :path))
                   :test #'equal)
      (user-error "File is already attached to this note: %s" path))
    (setq item
          (plist-put
           item :attachments
           (append attachments (list (list :path path)))))
    (appkit-chat-compose-update-current-item item)
    (set-buffer-modified-p t)
    (message "Attached %s" (file-name-nondirectory path))))

(defun misskey-compose-remove-attachment (&optional file)
  "Remove FILE from the current Misskey compose part.

Interactively, select one of the current part's attachments."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (let* ((item (appkit-chat-compose-current-item))
         (attachments (plist-get item :attachments))
         (paths (mapcar (lambda (attachment)
                          (plist-get attachment :path))
                        attachments))
         (target
          (or file
              (and paths
                   (completing-read
                    "Remove attachment: " paths nil t)))))
    (unless (member target paths)
      (user-error "No such attachment in the current note"))
    (setq item
          (plist-put
           item :attachments
           (cl-remove target attachments
                      :key (lambda (attachment)
                             (plist-get attachment :path))
                      :test #'equal)))
    (appkit-chat-compose-update-current-item item)
    (set-buffer-modified-p t)
    (message "Detached %s" (file-name-nondirectory target))))

(defun misskey-compose--drive-file-id (payload)
  "Return the uploaded Drive file identity from PAYLOAD, or nil."
  (when-let* ((id (and (listp payload) (alist-get 'id payload)))
              ((stringp id))
              ((not (string-empty-p id))))
    id))

(defun misskey-compose--created-note-id (payload)
  "Return the created note identifier from successful PAYLOAD."
  (when-let* ((created-note (and (listp payload)
                                 (alist-get 'createdNote payload)))
              ((listp created-note))
              (id (alist-get 'id created-note))
              ((stringp id))
              ((not (string-empty-p id))))
    id))

(defun misskey-compose--submission-current-p (buffer token)
  "Return non-nil when BUFFER's live compose view still owns TOKEN."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq misskey-compose--submission-token token)
              (appkit-surface-live-p (car token))
              (eq (appkit-current-surface) (car token))))))

(defun misskey-compose--accept-callback (buffer token)
  "Accept a callback for BUFFER and TOKEN, clearing its completed request."
  (when (misskey-compose--submission-current-p buffer token)
    (with-current-buffer buffer
      (setq-local misskey-compose--request nil))
    t))

(defun misskey-compose--handle-success (buffer token note-id)
  "Finish BUFFER's TOKEN after creating the final note NOTE-ID."
  (when (misskey-compose--submission-current-p buffer token)
    (with-current-buffer buffer
      (setq-local misskey-compose--submission-token nil
                  misskey-compose--request nil))
    (kill-buffer buffer)
    (message "Published Misskey note %s" note-id)))

(defun misskey-compose--handle-error (buffer token failure)
  "Restore compose BUFFER after TOKEN fails with FAILURE."
  (when (misskey-compose--submission-current-p buffer token)
    (with-current-buffer buffer
      (setq-local misskey-compose--submission-token nil
                  misskey-compose--request nil)
      (appkit-compose-operation-finish (appkit-compose-operation-owner))
      (misskey-compose--unlock-bodies)
      (misskey-compose--refresh))
    (message "%s" failure))
  nil)

(defun misskey-compose--remember-request
    (buffer token request callback-ran-p)
  "Remember REQUEST for BUFFER's TOKEN unless its callback already ran."
  (when (and request
             (not callback-ran-p)
             (misskey-compose--submission-current-p buffer token))
    (with-current-buffer buffer
      (setq-local misskey-compose--request request)))
  request)

(defun misskey-compose--abort-submission (buffer token)
  "Cancel BUFFER's request and reclaim local state owned by TOKEN.

Unlike result callbacks, teardown remains valid after the Surface is revoked."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and token (eq misskey-compose--submission-token token))
        (let ((request misskey-compose--request))
          (setq-local misskey-compose--submission-token nil
                      misskey-compose--request nil)
          (unwind-protect
              (progn
                (misskey-compose--unlock-bodies)
                (appkit-compose-operation-finish
                 (appkit-compose-operation-owner))
                (when (and (appkit-surface-live-p (car token))
                           (eq (appkit-current-surface) (car token)))
                  (misskey-compose--refresh)))
            (when request
              (misskey-http-cancel request))))))))

(defun misskey-compose--kill-buffer-cleanup ()
  "Invalidate and cancel this draft's active publish chain."
  (when misskey-compose--submission-token
    (let ((request misskey-compose--request))
      (setq-local misskey-compose--submission-token nil
                  misskey-compose--request nil)
      (when request
        (misskey-http-cancel request)))))

(defun misskey-compose--lock-bodies ()
  "Mark the compose surface read-only while a publish request is in flight."
  (misskey-compose--set-body-read-only t))

(defun misskey-compose--unlock-bodies ()
  "Restore the compose surface to an editable state."
  (misskey-compose--set-body-read-only nil))

(cl-defun misskey-compose--update-submit
    (buffer &key (label nil label-p) (progress nil progress-p))
  "Update BUFFER's compose submit from LABEL and PROGRESS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (appkit-compose-operation-active-p)
        (apply #'appkit-compose-operation-update (appkit-compose-operation-owner)
               (append (and label-p (list :label label))
                       (and progress-p (list :progress progress))))
        (misskey-compose--refresh)))))

(defun misskey-compose--upload-progress (buffer name index count event)
  "Update BUFFER from upload EVENT for NAME at INDEX of COUNT."
  (let ((progress (plist-get event :progress)))
    (misskey-compose--update-submit
     buffer
     :label
     (cond
      ((and (numberp progress) (>= progress 1.0))
       (format "Processing %s..." name))
      ((> count 1)
       (format "Uploading %s %d/%d" name index count))
      (t (format "Uploading %s" name)))
     :progress progress)))

(defun misskey-compose--persist-items (buffer token items)
  "Persist submitted ITEMS back into BUFFER while TOKEN is current."
  (when (misskey-compose--submission-current-p buffer token)
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (appkit-chat-compose-set-items items)
        (set-buffer-modified-p t)))))

(defun misskey-compose--persist-confirmed
    (buffer token remaining-items note-id)
  "Persist REMAINING-ITEMS after BUFFER's TOKEN confirmed NOTE-ID."
  (when (misskey-compose--submission-current-p buffer token)
    (with-current-buffer buffer
      (setq-local misskey-compose-reply-id note-id
                  misskey-compose-renote-id nil
                  misskey-compose-target-label nil))
    (misskey-compose--persist-items buffer token remaining-items)))

(defun misskey-compose--next-upload-index (item)
  "Return ITEM's first attachment lacking a Drive identity, or nil."
  (cl-position-if
   (lambda (attachment)
     (let ((id (plist-get attachment :drive-id)))
       (not (and (stringp id) (not (string-empty-p id))))))
   (plist-get item :attachments)))

(defun misskey-compose--install-drive-id
    (submission note-index attachment-index drive-id)
  "Install DRIVE-ID in SUBMISSION at NOTE-INDEX and ATTACHMENT-INDEX."
  (let* ((items (plist-get submission :items))
         (item (misskey-compose--copy-item (nth note-index items)))
         (attachments (plist-get item :attachments))
         (attachment (copy-sequence (nth attachment-index attachments))))
    (setf (plist-get attachment :drive-id) drive-id
          (nth attachment-index attachments) attachment
          (plist-get item :attachments) attachments
          (nth note-index items) item)
    items))

(defun misskey-compose--file-ids (item)
  "Return ITEM's validated uploaded Drive file identities as a vector."
  (vconcat
   (mapcar
    (lambda (attachment)
      (or (let ((id (plist-get attachment :drive-id)))
            (and (stringp id) (not (string-empty-p id)) id))
          (error "Misskey attachment has not been uploaded")))
    (plist-get item :attachments))))

(defun misskey-compose--note-parameters (submission item)
  "Return `notes/create' parameters for SUBMISSION ITEM."
  (let* ((index (plist-get submission :index))
         (previous-id (plist-get submission :previous-id))
         (text (or (plist-get item :text) ""))
         (file-ids (misskey-compose--file-ids item))
         (parameters
          (list :visibility
                (symbol-name (plist-get submission :visibility)))))
    (when (string-match-p "[^[:space:]]" text)
      (setq parameters (append (list :text text) parameters)))
    (when (> (length file-ids) 0)
      (setq parameters (append parameters (list :fileIds file-ids))))
    (cond
     (previous-id
      (setq parameters (append parameters (list :replyId previous-id))))
     ((and (= index 0) (plist-get submission :reply-id))
      (setq parameters
            (append parameters
                    (list :replyId (plist-get submission :reply-id)))))
     ((and (= index 0) (plist-get submission :renote-id))
      (setq parameters
            (append parameters
                    (list :renoteId (plist-get submission :renote-id))))))
    parameters))

(defun misskey-compose--publish-item (buffer account submission item)
  "Publish ITEM for ACCOUNT in BUFFER under SUBMISSION."
  (let* ((index (plist-get submission :index))
         (total (length (plist-get submission :items)))
         (token (plist-get submission :token))
         callback-ran-p
         request)
    (misskey-compose--update-submit
     buffer
     :label (format "Publishing note %d/%d..." (1+ index) total)
     :progress nil)
    (setq
     request
     (misskey-http-post
      "notes/create"
      (misskey-compose--note-parameters submission item)
      (lambda (payload)
        (setq callback-ran-p t)
        (when (misskey-compose--accept-callback buffer token)
          (if-let* ((note-id (misskey-compose--created-note-id payload)))
              (let* ((next-index (1+ index))
                     (remaining
                      (cl-subseq (plist-get submission :items) next-index)))
                (if remaining
                    (let ((next (copy-sequence submission)))
                      (misskey-compose--persist-confirmed
                       buffer token remaining note-id)
                      (setf (plist-get next :index) next-index
                            (plist-get next :previous-id) note-id)
                      (misskey-compose--send-next buffer account next))
                  (misskey-compose--handle-success buffer token note-id)))
            (misskey-compose--handle-error
             buffer token
             (concat
              "Misskey returned success without a nonempty created note ID; "
              "the remote outcome is unknown")))))
      :errback
      (lambda (error-message)
        (setq callback-ran-p t)
        (when (misskey-compose--accept-callback buffer token)
          (misskey-compose--handle-error buffer token error-message)))
      :account account
      :owner (plist-get submission :owner)))
    (misskey-compose--remember-request
     buffer token request callback-ran-p)))

(defun misskey-compose--upload-attachment
    (buffer account submission note-index attachment-index)
  "Upload ACCOUNT's SUBMISSION attachment at ATTACHMENT-INDEX.
BUFFER's NOTE-INDEX selects the draft entry."
  (let* ((item (nth note-index (plist-get submission :items)))
         (attachment (nth attachment-index
                          (plist-get item :attachments)))
         (path (plist-get attachment :path))
         (name (file-name-nondirectory path))
         (count (length (plist-get item :attachments)))
         (index (1+ attachment-index))
         (token (plist-get submission :token))
         callback-ran-p
         request)
    (when (file-remote-p path)
      (user-error "Remote attachment paths are unsupported: %s" path))
    (misskey-compose--update-submit
     buffer
     :label (if (> count 1)
                (format "Uploading %s %d/%d" name index count)
              (format "Uploading %s..." name))
     :progress nil)
    (setq
     request
     (misskey-http-upload-file
      path
      (lambda (payload)
        (setq callback-ran-p t)
        (when (misskey-compose--accept-callback buffer token)
          (if-let* ((drive-id (misskey-compose--drive-file-id payload)))
              (let ((items
                     (misskey-compose--install-drive-id
                      submission note-index attachment-index drive-id)))
                (misskey-compose--persist-items
                 buffer token (cl-subseq items note-index))
                (misskey-compose--send-next buffer account submission))
            (misskey-compose--handle-error
             buffer token
             (concat
              "Misskey returned success without a nonempty uploaded file ID; "
              "the remote outcome is unknown")))))
      :errback
      (lambda (error-message)
        (setq callback-ran-p t)
        (when (misskey-compose--accept-callback buffer token)
          (misskey-compose--handle-error buffer token error-message)))
      :account account
      :owner (plist-get submission :owner)
      :progress
      (lambda (event)
        (misskey-compose--upload-progress
         buffer name index count event))))
    (misskey-compose--remember-request
     buffer token request callback-ran-p)))

(defun misskey-compose--send-next (buffer account submission)
  "Continue publishing SUBMISSION for ACCOUNT in BUFFER."
  (when (misskey-compose--submission-current-p
         buffer (plist-get submission :token))
    (let* ((items (plist-get submission :items))
           (index (plist-get submission :index)))
      (if (>= index (length items))
          (misskey-compose--handle-success
           buffer
           (plist-get submission :token)
           (plist-get submission :previous-id))
        (let* ((item (nth index items))
               (attachment-index
                (misskey-compose--next-upload-index item)))
          (if attachment-index
              (misskey-compose--upload-attachment
               buffer account submission index attachment-index)
            (misskey-compose--publish-item
             buffer account submission item)))))))

(defun misskey-compose-set-visibility (&optional visibility)
  "Set the current draft VISIBILITY.

Interactively, choose `public', `home', or `followers'."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (let* ((choices misskey-compose--visibility-choices)
         (label
          (and (null visibility)
               (completing-read
                "Visibility: " (mapcar #'cdr choices) nil t nil nil
                (alist-get misskey-compose-visibility choices))))
         (choice (or visibility (car (rassoc label choices)))))
    (unless (assq choice choices)
      (user-error "Unsupported Misskey visibility: %S" choice))
    (unless (eq misskey-compose-visibility choice)
      (setq-local misskey-compose-visibility choice)
      (appkit-compose-touch)
      (misskey-compose--refresh))
    choice))

(defun misskey-compose-add-note ()
  "Insert an empty note after the current draft item."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (appkit-chat-compose-add-item)
  (set-buffer-modified-p t))

(defun misskey-compose-remove-note ()
  "Remove the current extra note from the draft."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (unless (> (length (appkit-chat-compose-items)) 1)
    (user-error "The draft already has only one note"))
  (let ((index (or (appkit-chat-compose-current-part-index) 0)))
    (appkit-chat-compose-drop-item index)
    (set-buffer-modified-p t)
    (message "Removed note %d." (1+ index))))

(defun misskey-compose-send ()
  "Publish the current Misskey draft once.

Local attachments upload to Drive before their note is created.  Successful
uploads and confirmed note prefixes are persisted immediately, so retries
reuse Drive files and never recreate confirmed notes."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "This note is already being published"))
  (let*
      ((items (misskey-compose--snapshot-items))
       (buffer (current-buffer)) (account (misskey-compose--account))
       (owner (appkit-current-surface)) (token (cons owner buffer))
       (label
        (if (> (length items) 1) "Publishing Misskey notes..."
          "Publishing Misskey note...")))
    (unless (appkit-surface-live-p owner)
      (error "Misskey compose has no live lifecycle owner"))
    (setq-local misskey-compose--submission-token token
                misskey-compose--request nil)
    (appkit-compose-operation-begin 'publish :label label :cancel-function
                                    (lambda ()
                                      (misskey-compose--abort-submission
                                       buffer token)))
    (misskey-compose--refresh) (misskey-compose--lock-bodies)
    (message "%s" label)
    (condition-case err
        (misskey-compose--send-next buffer account
                                    (list :items items :index 0
                                          :previous-id nil :visibility
                                          misskey-compose-visibility
                                          :reply-id
                                          misskey-compose-reply-id
                                          :renote-id
                                          misskey-compose-renote-id
                                          :owner owner :token token))
      ((error quit)
       (when (misskey-compose--submission-current-p buffer token)
         (setq-local misskey-compose--submission-token nil
                     misskey-compose--request nil)
         (appkit-compose-operation-finish (appkit-compose-operation-owner))
         (misskey-compose--unlock-bodies) (misskey-compose--refresh))
       (signal (car err) (cdr err))))))

(defun misskey-compose--note-at-point ()
  "Return the Misskey note at point, or signal a user error."
  (or (get-text-property (point) misskey-note-property)
      (user-error "No Misskey note at point")))

(defun misskey-compose-reply-at-point ()
  "Open a reply draft for the displayed Misskey note at point."
  (interactive)
  (let*
      ((raw-note (misskey-compose--note-at-point))
       (note (misskey-note-display-note raw-note))
       (view (appkit-current-surface))
       (account
        (and (appkit-surface-live-p view)
             (plist-get (appkit-surface-model view) :account))))
    (unless note
      (user-error "The displayed Misskey note was deleted"))
    (misskey-compose-open account :reply-id (misskey-note-id note)
                          :target-label
                          (misskey-user-label (misskey-note-user note)))))

(defun misskey-compose-quote-at-point ()
  "Open a quote draft for the displayed Misskey note at point."
  (interactive)
  (let*
      ((raw-note (misskey-compose--note-at-point))
       (note (misskey-note-display-note raw-note))
       (view (appkit-current-surface))
       (account
        (and (appkit-surface-live-p view)
             (plist-get (appkit-surface-model view) :account))))
    (unless note
      (user-error "The displayed Misskey note was deleted"))
    (misskey-compose-open account :renote-id (misskey-note-id note)
                          :target-label
                          (misskey-user-label (misskey-note-user note)))))

(defun misskey-compose-cancel ()
  "Cancel the active request, if any, and kill the current draft."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (appkit-compose-cancel-operation))
  (kill-buffer (current-buffer)))

(provide 'misskey-compose)

;;; misskey-compose.el ends here
