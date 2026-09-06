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

(defvar-local misskey-compose-cw nil
  "Content warning shared by all parts, or nil.")

(defvar-local misskey-compose-local-only nil
  "Non-nil when all parts must stay on this instance.")

(defvar-local misskey-compose-recipients nil
  "Specified recipients as (stable ID . readable label) pairs.")

(defvar-local misskey-compose--target-visibility nil
  "Audience ceiling imposed by the original reply or quote.")

(defvar-local misskey-compose--target-local-only nil
  "Whether the target forces local-only publication.")

(defvar-local misskey-compose--max-note-text-length nil
  "Instance-provided Unicode code-point limit, or nil when unknown.")

(defvar-local misskey-compose--meta-status "Not loaded; C-c C-m refresh"
  "Visible state of the instance metadata read.")

(defvar-local misskey-compose--recipient-status nil
  "Visible state of a recipient lookup, or nil.")

(defvar-local misskey-compose--reads nil
  "Owned read slots, each (KIND REQUEST).")

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
    (followers . "Followers")
    (specified . "Specified"))
  "Supported Misskey visibility values and labels.")

(defvar misskey-compose--serial 0
  "Serial used to name fresh compose buffers.")

(defconst misskey-compose--max-attachments 16
  "Maximum number of Drive files attached to one Misskey note.")

(declare-function misskey-compose-menu "misskey-menu" nil)

(defvar-keymap misskey-compose-mode-map
  :doc "Keymap for `misskey-compose-mode'."
  "C-c C-c" #'misskey-compose-send
  "C-c C-k" #'misskey-compose-cancel
  "C-c C-n" #'misskey-compose-add-note
  "C-c C-p" #'misskey-compose-remove-note
  "C-c C-v" #'misskey-compose-set-visibility
  "C-c C-a" #'misskey-compose-attach-file
  "C-c C-d" #'misskey-compose-remove-attachment
  "C-c C-w" #'misskey-compose-set-cw
  "C-c C-l" #'misskey-compose-toggle-local-only
  "C-c C-r" #'misskey-compose-add-recipient
  "C-c C-x" #'misskey-compose-remove-recipient
  "C-c C-m" #'misskey-compose-refresh-metadata
  "C-c C-o" #'misskey-compose-menu)

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
           (list :label "Characters"
                 :value (mapconcat
                         (lambda (item)
                           (format "%d/%s%s" (length (or (plist-get item :text) ""))
                                   (or misskey-compose--max-note-text-length "?")
                                   (if (and misskey-compose--max-note-text-length
                                            (> (length (or (plist-get item :text) ""))
                                               misskey-compose--max-note-text-length))
                                       " OVER" "")))
                         items " | "))
           (list :label "Limit" :value misskey-compose--meta-status)
           (list :label "CW" :value (or misskey-compose-cw "None"))
           (list :label "Federation"
                 :value (if misskey-compose-local-only "Local only" "Enabled"))
           (list :label "State"
                 :value (or (appkit-compose-status-text) "Draft")))))
    (when (eq misskey-compose-visibility 'specified)
      (push (list :label "Recipients"
                  :value (if misskey-compose-recipients
                             (mapconcat (lambda (entry)
                                          (format "%s [%s]" (cdr entry) (car entry)))
                                        misskey-compose-recipients ", ")
                           "None; C-c C-r add"))
            fields))
    (when misskey-compose--recipient-status
      (push (list :label "Recipient lookup" :value misskey-compose--recipient-status) fields))
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
     (concat "C-c C-o options   C-c C-c publish   C-c C-v visibility   "
             "C-c C-a attach   C-c C-d detach\n"
             "C-c C-w CW/clear   C-c C-l local only   C-c C-m refresh limit\n"
             "C-c C-r add recipient   C-c C-x remove recipient\n"
             "C-c C-n add note   C-c C-p drop note   C-c C-k cancel"))
   'face 'shadow))

(defun misskey-compose--refresh ()
  "Refresh generated compose presentation for the current buffer."
  (appkit-chat-compose-refresh))

(cl-defun misskey-compose-open
    (&optional account &key reply-id renote-id target-label target-note
               (visibility 'public) (cw nil cw-p) local-only recipients)
  "Create, display, and return a fresh compose buffer for ACCOUNT.
REPLY-ID or RENOTE-ID sets the first target; they are mutually exclusive.
TARGET-NOTE is required for replies/quotes and supplies the original payload
for privacy-safe audience, CW,
local-only and specified-recipient defaults.  TARGET-LABEL describes it.
VISIBILITY is public, home, followers or specified.  CW, LOCAL-ONLY and
RECIPIENTS (ID . label pairs) apply to every part.  Instance limits are
loaded asynchronously; publishing is blocked until a valid limit arrives."
  (when (and reply-id renote-id)
    (error "A Misskey draft cannot reply and quote simultaneously"))
  (when (and (or reply-id renote-id) (null target-note))
    (user-error "Original note metadata is required to compose a safe reply or quote"))
  (unless (assq visibility misskey-compose--visibility-choices)
    (error "Unsupported Misskey visibility: %S" visibility))
  (let* ((app (misskey-app account))
         (target (misskey--session-account (misskey--session app)))
         (self (misskey--account-remote-user-id target))
         (author (and target-note (misskey-user-id (misskey-note-user target-note))))
         (audience (and target-note
                        (let ((value (alist-get 'visibility target-note)))
                          (and (stringp value) (intern value)))))
         (target-local (and target-note (eq (alist-get 'localOnly target-note) t))))
    (when target-note
      (unless (and (equal (misskey-note-id target-note) (or reply-id renote-id))
                   (assq audience misskey-compose--visibility-choices))
        (user-error "Invalid compose target metadata"))
      (when (and renote-id
                 (or (eq audience 'specified)
                     (and (eq audience 'followers) (not (equal author self)))
                     (alist-get 'channelId target-note)))
        (user-error "Cannot quote specified, another user's followers-only, or channel notes"))
      (let ((order '(public home followers specified)))
        (when (> (length (memq visibility order)) (length (memq audience order)))
          (setq visibility audience)))
      (setq local-only (or local-only target-local))
      (unless cw-p
        (setq cw (let ((warning (alist-get 'cw target-note)))
                   (and (stringp warning) (not (string-empty-p warning)) warning))))
      (when (and reply-id (eq audience 'specified))
        (let ((ids (alist-get 'visibleUserIds target-note)))
          (unless (and (assq 'visibleUserIds target-note)
                       (or (listp ids) (vectorp ids))
                       (misskey--valid-user-id-p self)
                       (misskey--valid-user-id-p author)
                       (cl-every #'misskey--valid-user-id-p ids))
            (user-error "Reply audience is incomplete; reload the original note"))
          (setq recipients
                (mapcar (lambda (id)
                          (cons id (if (equal id author)
                                       (misskey-user-label (misskey-note-user target-note))
                                     id)))
                        (delete-dups
                         (cl-remove self (append ids (list author)) :test #'equal)))))))
    (let ((buffer (generate-new-buffer
                   (format "*misskey compose %d*" (cl-incf misskey-compose--serial)))))
      (pop-to-buffer buffer)
      (misskey-compose-mode)
      (setq-local misskey-compose--account target
                  misskey-compose-visibility visibility
                  misskey-compose-cw (and cw (copy-sequence cw))
                  misskey-compose-local-only local-only
                  misskey-compose-recipients (copy-tree recipients)
                  misskey-compose--target-visibility audience
                  misskey-compose--target-local-only target-local
                  misskey-compose-reply-id reply-id
                  misskey-compose-renote-id renote-id
                  misskey-compose-target-label target-label)
      (appkit-chat-compose-setup
       :app app
       :context-function #'misskey-compose--context
       :status-fields-function #'misskey-compose--status-fields
       :parts-function #'misskey-compose--parts
       :footer-function #'misskey-compose--footer)
      (misskey-compose-refresh-metadata)
      buffer)))

(defun misskey-compose--copy-item (item)
  "Return a draft copy of compose ITEM with independent attachments."
  (let ((copy (copy-sequence item)))
    (plist-put
     copy :attachments
     (mapcar #'copy-sequence (plist-get item :attachments)))))

(defun misskey-compose--snapshot-items ()
  "Return validated independent copies of every compose item."
  (misskey-compose--validate-metadata)
  (mapcar
   (lambda (item)
     (let* ((copy (misskey-compose--copy-item item))
            (text (or (plist-get copy :text) ""))
            (attachments (plist-get copy :attachments)))
       ;; JSON Schema maxLength counts Unicode code points, not UTF-16 units.
       (when (> (length text) misskey-compose--max-note-text-length)
         (user-error "Note exceeds instance limit: %d/%d Unicode characters"
                     (length text) misskey-compose--max-note-text-length))
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

(defun misskey-compose--start-request (buffer token start callback)
  "Call START with guarded result handlers for BUFFER's TOKEN.
CALLBACK receives successful payloads; failures restore the draft.  A
synchronous result must not overwrite the next request's cancellation handle."
  (let (callback-ran-p request)
    (cl-flet ((accept ()
                (setq callback-ran-p t)
                (when (misskey-compose--submission-current-p buffer token)
                  (with-current-buffer buffer
                    (setq-local misskey-compose--request nil))
                  t)))
      (setq request
            (funcall start
                     (lambda (payload)
                       (when (accept)
                         (funcall callback payload)))
                     (lambda (failure)
                       (when (accept)
                         (misskey-compose--handle-error buffer token failure))))))
    (when (and request (not callback-ran-p)
               (misskey-compose--submission-current-p buffer token))
      (with-current-buffer buffer
        (setq-local misskey-compose--request request)))
    request))

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
      (setq-local buffer-read-only nil)
      (misskey-compose--refresh))
    (message "%s" failure))
  nil)

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
                (setq-local buffer-read-only nil)
                (appkit-compose-operation-finish
                 (appkit-compose-operation-owner))
                (when (and (appkit-surface-live-p (car token))
                           (eq (appkit-current-surface) (car token)))
                  (misskey-compose--refresh)))
            (when request
              (misskey-http-cancel request))))))))

(defun misskey-compose--kill-buffer-cleanup ()
  "Invalidate and cancel this draft's active publish chain."
  (dolist (kind (mapcar #'car misskey-compose--reads))
    (misskey-compose--cancel-read kind))
  (when misskey-compose--submission-token
    (let ((request misskey-compose--request))
      (setq-local misskey-compose--submission-token nil
                  misskey-compose--request nil)
      (when request
        (misskey-http-cancel request)))))

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
    (when (plist-get submission :cw)
      (setq parameters (append parameters (list :cw (plist-get submission :cw)))))
    (when (plist-get submission :local-only)
      (setq parameters (append parameters (list :localOnly t))))
    (when (eq (plist-get submission :visibility) 'specified)
      (setq parameters
            (append parameters
                    (list :visibleUserIds (plist-get submission :visible-user-ids)))))
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
         (token (plist-get submission :token)))
    (misskey-compose--update-submit
     buffer
     :label (format "Publishing note %d/%d..." (1+ index) total)
     :progress nil)
    (misskey-compose--start-request
     buffer token
     (lambda (callback errback)
       (misskey-http-post
        "notes/create" (misskey-compose--note-parameters submission item)
        callback :errback errback :account account
        :owner (plist-get submission :owner)))
     (lambda (payload)
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
           "the remote outcome is unknown")))))))

(defun misskey-compose--upload-attachment
    (buffer account submission note-index attachment-index)
  "Upload ACCOUNT's SUBMISSION attachment at ATTACHMENT-INDEX.
BUFFER's NOTE-INDEX selects the draft entry."
  (let* ((item (nth note-index (plist-get submission :items)))
         (attachment (nth attachment-index (plist-get item :attachments)))
         (path (plist-get attachment :path))
         (name (file-name-nondirectory path))
         (count (length (plist-get item :attachments)))
         (index (1+ attachment-index))
         (token (plist-get submission :token)))
    (when (file-remote-p path)
      (user-error "Remote attachment paths are unsupported: %s" path))
    (misskey-compose--update-submit
     buffer
     :label (if (> count 1)
                (format "Uploading %s %d/%d" name index count)
              (format "Uploading %s..." name))
     :progress nil)
    (misskey-compose--start-request
     buffer token
     (lambda (callback errback)
       (misskey-http-upload-file
        path callback :errback errback :account account
        :owner (plist-get submission :owner)
        :progress (lambda (event)
                    (misskey-compose--upload-progress
                     buffer name index count event))))
     (lambda (payload)
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
           "the remote outcome is unknown")))))))

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

Interactively, choose Public, Home, Followers, or Specified within the
target audience."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (let* ((choices (cl-remove-if-not
                   (lambda (entry) (misskey-compose--audience-allowed-p (car entry)))
                   misskey-compose--visibility-choices))
         (label
          (and (null visibility)
               (completing-read
                "Visibility: " (mapcar #'cdr choices) nil t nil nil
                (alist-get misskey-compose-visibility choices))))
         (choice (or visibility (car (rassoc label choices)))))
    (unless (assq choice choices)
      (user-error "Unsupported Misskey visibility: %S" choice))
    (unless (eq misskey-compose-visibility choice)
      (misskey-compose--cancel-read 'recipient)
      (setq-local misskey-compose--recipient-status nil
                  misskey-compose-visibility choice)
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
    (misskey-compose--refresh) (setq-local buffer-read-only t)
    (message "%s" label)
    (condition-case err
        (misskey-compose--send-next buffer account
                                    (list :items items :index 0
                                          :previous-id nil :visibility
                                          misskey-compose-visibility
                                          :cw (and misskey-compose-cw
                                                   (copy-sequence misskey-compose-cw))
                                          :local-only misskey-compose-local-only
                                          :visible-user-ids
                                          (vconcat (mapcar (lambda (entry)
                                                             (copy-sequence (car entry)))
                                                           misskey-compose-recipients))
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
         (setq-local buffer-read-only nil) (misskey-compose--refresh))
       (signal (car err) (cdr err))))))

(defun misskey-compose--note-at-point ()
  "Return the Misskey note at point, or signal a user error."
  (or (get-text-property (point) misskey-note-property)
      (user-error "No Misskey note at point")))

(defun misskey-compose--open-at-point (target-key)
  "Open a draft targeting the displayed note at point with TARGET-KEY."
  (let* ((note (misskey-note-display-note (misskey-compose--note-at-point)))
         (view (appkit-current-surface))
         (account (and (appkit-surface-live-p view)
                       (plist-get (appkit-surface-model view) :account))))
    (unless note
      (user-error "The displayed Misskey note was deleted"))
    (misskey-compose-open account target-key (misskey-note-id note)
                          :target-note note
                          :target-label
                          (misskey-user-label (misskey-note-user note)))))

(defun misskey-compose-reply-at-point ()
  "Open a reply draft for the displayed Misskey note at point."
  (interactive)
  (misskey-compose--open-at-point :reply-id))

(defun misskey-compose-quote-at-point ()
  "Open a quote draft for the displayed Misskey note at point."
  (interactive)
  (misskey-compose--open-at-point :renote-id))

(defun misskey-compose-cancel ()
  "Cancel the active request, if any, and kill the current draft."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (appkit-compose-cancel-operation))
  (kill-buffer (current-buffer)))

(defun misskey-compose--cancel-read (kind)
  "Invalidate and cancel the owned read of KIND."
  (when-let* ((slot (assq kind misskey-compose--reads)))
    (setq misskey-compose--reads (delq slot misskey-compose--reads))
    (when (cadr slot) (misskey-http-cancel (cadr slot)))))

(defun misskey-compose--read (kind endpoint parameters callback errback)
  "Start a replaceable KIND read of ENDPOINT with PARAMETERS.
Deliver CALLBACK or ERRBACK only to this exact live draft and account."
  (misskey-compose--cancel-read kind)
  (let* ((buffer (current-buffer))
         (owner (appkit-current-surface))
         (account (misskey-compose--account))
         (slot (list kind nil))
         (completed nil))
    (unless (appkit-surface-live-p owner)
      (user-error "Misskey compose has no live lifecycle owner"))
    (push slot misskey-compose--reads)
    (cl-flet
        ((deliver (function value)
           (setq completed t)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (when (and (eq slot (assq kind misskey-compose--reads))
                          (eq account misskey-compose--account)
                          (appkit-surface-live-p owner)
                          (eq owner (appkit-current-surface)))
                 (setq misskey-compose--reads
                       (delq slot misskey-compose--reads))
                 (funcall function value))))))
      (condition-case err
          (let ((request
                  (misskey-http-read
                   endpoint parameters
                   (lambda (payload) (deliver callback payload))
                   :errback (lambda (failure) (deliver errback failure))
                   :owner owner :account account)))
            (unless completed (setf (cadr slot) request)))
        (error (deliver errback (error-message-string err)))))))

(defun misskey-compose-refresh-metadata ()
  "Reload this draft's instance text limit; never guess a fallback limit."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (setq misskey-compose--max-note-text-length nil
        misskey-compose--meta-status "Loading")
  (misskey-compose--refresh)
  (misskey-compose--read
   'meta "meta" '(:detail :json-false)
   (lambda (payload)
     (let ((limit (and (listp payload) (alist-get 'maxNoteTextLength payload))))
       (if (and (integerp limit) (> limit 0))
           (setq misskey-compose--max-note-text-length limit
                 misskey-compose--meta-status "Ready")
         (setq misskey-compose--meta-status
               "Invalid instance limit; C-c C-m retry")))
     (misskey-compose--refresh))
   (lambda (failure)
     (setq misskey-compose--meta-status (format "%s; C-c C-m retry" failure))
     (misskey-compose--refresh))))

(defun misskey-compose--audience-allowed-p (visibility)
  "Whether VISIBILITY respects the target's audience ceiling."
  (let ((order '(public home followers specified)))
    (and (memq visibility order)
         (or (null misskey-compose--target-visibility)
             (<= (length (memq visibility order))
                 (length (memq misskey-compose--target-visibility order)))))))

(defun misskey-compose-set-cw (warning)
  "Set the draft's content WARNING; an empty string clears it."
  (interactive (list (read-string "Content warning (empty clears): "
                                  misskey-compose-cw)))
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (unless (and (stringp warning) (<= (length warning) 100))
    (user-error "A content warning accepts at most 100 Unicode characters"))
  (let ((value (unless (string-empty-p warning) warning)))
    (unless (equal value misskey-compose-cw)
      (setq misskey-compose-cw value)
      (appkit-compose-touch)
      (misskey-compose--refresh))))

(defun misskey-compose-toggle-local-only ()
  "Toggle instance-only publication, unless the target requires it."
  (interactive)
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (when misskey-compose--target-local-only
    (user-error "This target requires local-only publication"))
  (setq misskey-compose-local-only (not misskey-compose-local-only))
  (appkit-compose-touch)
  (misskey-compose--refresh))

(defun misskey-compose-add-recipient (handle)
  "Resolve and add HANDLE, an @user[@host] or stable local user ID.
A later recipient or audience edit supersedes an unfinished lookup."
  (interactive "sRecipient (@user[@host] or user ID): ")
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (unless (eq misskey-compose-visibility 'specified)
    (user-error "Choose Specified visibility before adding recipients"))
  (let ((parameters
         (cond
          ((string-match "\\`@\\([^@[:space:]]+\\)\\(?:@\\([^@[:space:]]+\\)\\)?\\'" handle)
           (append (list :username (match-string 1 handle))
                   (when (match-string 2 handle)
                     (list :host (match-string 2 handle)))))
          ((misskey--valid-user-id-p handle) (list :userId handle))
          (t (user-error "Use @user, @user@host, or a stable user ID")))))
    (setq misskey-compose--recipient-status (format "Resolving %s" handle))
    (misskey-compose--refresh)
    (misskey-compose--read
     'recipient "users/show" parameters
     (lambda (user)
       (let ((id (and (listp user) (misskey-user-id user))))
         (cond
          ((not (misskey--valid-user-id-p id))
           (setq misskey-compose--recipient-status "Invalid user response; C-c C-r retry"))
          ((equal id (misskey--account-remote-user-id (misskey-compose--account)))
           (setq misskey-compose--recipient-status nil)
           (message "You are already included as sender"))
          (t
           (setq misskey-compose--recipient-status nil)
           (unless (assoc id misskey-compose-recipients)
             (setq misskey-compose-recipients
                   (append misskey-compose-recipients
                           (list (cons id (misskey-user-label user)))))
             (appkit-compose-touch)))))
       (misskey-compose--refresh))
     (lambda (failure)
       (setq misskey-compose--recipient-status (format "%s; C-c C-r retry" failure))
       (misskey-compose--refresh)))))

(defun misskey-compose-remove-recipient (id)
  "Remove specified recipient ID, choosing a labelled recipient interactively."
  (interactive
   (list (let* ((choices (mapcar (lambda (entry)
                                   (cons (format "%s [%s]" (cdr entry) (car entry))
                                         (car entry)))
                                 misskey-compose-recipients))
                (label (completing-read "Remove recipient: " choices nil t)))
           (cdr (assoc label choices)))))
  (when (appkit-compose-operation-active-p)
    (user-error "Wait for the current publish request to finish"))
  (unless (assoc id misskey-compose-recipients)
    (user-error "No such recipient"))
  (misskey-compose--cancel-read 'recipient)
  (setq misskey-compose--recipient-status nil
        misskey-compose-recipients (assoc-delete-all id misskey-compose-recipients))
  (appkit-compose-touch)
  (misskey-compose--refresh))

(defun misskey-compose--validate-metadata ()
  "Reject incomplete or invalid publication metadata before any write."
  (unless (and (integerp misskey-compose--max-note-text-length)
               (> misskey-compose--max-note-text-length 0))
    (user-error "Instance text limit unavailable: %s" misskey-compose--meta-status))
  (unless (misskey-compose--audience-allowed-p misskey-compose-visibility)
    (user-error "Visibility would widen the target audience"))
  (when (and misskey-compose--target-local-only (not misskey-compose-local-only))
    (user-error "This target requires local-only publication"))
  (unless (or (null misskey-compose-cw)
              (and (stringp misskey-compose-cw)
                   (<= 1 (length misskey-compose-cw) 100)))
    (user-error "Content warning must contain 1 to 100 Unicode characters"))
  (when (eq misskey-compose-visibility 'specified)
    (when misskey-compose--recipient-status
      (user-error "Resolve recipient state first: %s" misskey-compose--recipient-status))
    (unless (and (consp misskey-compose-recipients)
                 (cl-every (lambda (entry)
                             (and (consp entry)
                                  (misskey--valid-user-id-p (car entry))))
                           misskey-compose-recipients)
                 (= (length misskey-compose-recipients)
                    (length (delete-dups (mapcar #'car misskey-compose-recipients)))))
      (user-error "Specified visibility requires valid, unique recipient IDs"))))

(provide 'misskey-compose)

;;; misskey-compose.el ends here
