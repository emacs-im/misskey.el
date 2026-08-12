;;; misskey-compose.el --- Compose Misskey notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provide the standalone Appkit-backed editor for plain-text Misskey notes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-compose)
(require 'misskey-core)
(require 'misskey-http)

(defvar-local misskey-compose--account nil
  "Account captured when the current draft was opened.")

(defvar-local misskey-compose-items nil
  "Ordered compose items for the current draft.

Each item is a placeholder plist.  Editable text lives in the Appkit
compose parts until send snapshots the bodies.")

(defvar misskey-compose--serial 0
  "Serial used to name fresh compose buffers.")

(defun misskey-compose--set-body-read-only (read-only)
  "Set the current compose body READ-ONLY state."
  (setq-local buffer-read-only (and read-only t)))

(defvar misskey-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'misskey-compose-send)
    (define-key map (kbd "C-c C-k") #'misskey-compose-cancel)
    (define-key map (kbd "C-c C-n") #'misskey-compose-add-note)
    (define-key map (kbd "C-c C-p") #'misskey-compose-remove-note)
    map)
  "Keymap for `misskey-compose-mode'.")

(define-derived-mode misskey-compose-mode appkit-compose-mode "Misskey-Compose"
  "Major mode for composing a standalone Misskey note.")

(defun misskey-compose--account ()
  "Return the account bound to the current draft."
  (or misskey-compose--account
      (misskey--current-account)))

(defun misskey-compose--context ()
  "Return the generated context shown above the note body."
  (format "New note on %s\n"
          (misskey--account-origin (misskey-compose--account))))

(defun misskey-compose--status-fields ()
  "Return generated status fields for the current note."
  (let ((fields
         (list (list :label "Visibility" :value "Public")
               (list :label "State"
                     :value (or (appkit-compose-progress-text) "Draft")))))
    (when (> (length (appkit-compose-items)) 1)
      (push (list :label "Notes"
                  :value (format "%d" (length (appkit-compose-items))))
            fields))
    fields))

(defun misskey-compose--parts ()
  "Return Appkit compose parts for the current draft."
  (let* ((items (or (and (fboundp 'appkit-compose-items)
                         (appkit-compose-items))
                    misskey-compose-items
                    (list nil)))
         (total (length items))
         (index 0))
    (mapcar (lambda (_item)
              (setq index (1+ index))
              (list :title (and (> total 1)
                                (format "Note %d/%d" index total))))
            items)))

(defun misskey-compose--footer ()
  "Return the generated compose command footer."
  (propertize
   (if (appkit-compose-submitting-p)
       "Publishing; wait for the server response"
     "C-c C-c publish   C-c C-n add note   C-c C-p drop note   C-c C-k cancel")
   'face 'shadow))

(defun misskey-compose--refresh ()
  "Refresh generated compose presentation for the current buffer."
  (appkit-compose-refresh))

(defun misskey-compose-open (&optional account)
  "Create, display, and return a fresh compose buffer for ACCOUNT.

ACCOUNT defaults to the account selected by current customization."
  (let* ((target (or account (misskey--current-account)))
         (buffer (generate-new-buffer
                  (format "*misskey compose %d*"
                          (cl-incf misskey-compose--serial)))))
    (pop-to-buffer buffer)
    (misskey-compose-mode)
    (setq-local misskey-compose--account target)
    (setq-local misskey-compose-items (list nil))
    (appkit-compose-setup
     :app (ignore-errors (misskey-app target))
     :context-function #'misskey-compose--context
     :status-fields-function #'misskey-compose--status-fields
     :parts-function #'misskey-compose--parts
     :footer-function #'misskey-compose--footer)
    buffer))

(defun misskey-compose--body-text ()
  "Return the validated note body from the current compose part."
  (let ((text (appkit-compose-body)))
    (unless (string-match-p "[^[:space:]]" text)
      (user-error "Note text cannot be empty"))
    text))

(defun misskey-compose--snapshot-bodies ()
  "Return every compose body after requiring each one to contain text."
  (let ((bodies (appkit-compose-bodies)))
    (dolist (text bodies)
      (unless (string-match-p "[^[:space:]]" text)
        (user-error "Note text cannot be empty")))
    bodies))

(defun misskey-compose--created-note-id (payload)
  "Return the created note identifier from successful PAYLOAD."
  (when-let* ((created-note (and (listp payload)
                                 (alist-get 'createdNote payload)))
              ((listp created-note))
              (id (alist-get 'id created-note))
              ((stringp id))
              ((not (string-empty-p id))))
    id))

(defun misskey-compose--handle-success (buffer payload)
  "Handle successful note PAYLOAD for compose BUFFER."
  (let ((note-id (misskey-compose--created-note-id payload)))
    (if (not note-id)
        (misskey-compose--handle-error
         buffer "Misskey returned success without a created note ID")
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (message "Published Misskey note %s" note-id))))

(defun misskey-compose--handle-error (buffer failure)
  "Restore compose BUFFER after FAILURE and show it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (appkit-compose-finish-submit)
      (misskey-compose--refresh)
      (misskey-compose--unlock-bodies)))
  (message "%s" failure)
  nil)

(defun misskey-compose--lock-bodies ()
  "Mark the compose surface read-only while a publish request is in flight."
  (misskey-compose--set-body-read-only t))

(defun misskey-compose--unlock-bodies ()
  "Restore the compose surface to an editable state."
  (misskey-compose--set-body-read-only nil))

(defun misskey-compose--send-next (buffer account bodies index previous-id)
  "Publish BODIES from INDEX for ACCOUNT in BUFFER.

PREVIOUS-ID is the last created note, used as `replyId' for later notes."
  (if (>= index (length bodies))
      (misskey-compose--handle-success
       buffer `((createdNote (id . ,previous-id))))
    (let ((parameters (list :text (nth index bodies)
                            :visibility "public")))
      (when previous-id
        (setq parameters
              (append parameters (list :replyId previous-id))))
      (misskey-http-post
       "notes/create"
       parameters
       (lambda (payload)
         (if (not (misskey-compose--created-note-id payload))
             (misskey-compose--handle-error
              buffer "Misskey returned success without a created note ID")
           (misskey-compose--send-next
            buffer account bodies (1+ index)
            (misskey-compose--created-note-id payload))))
       :errback (lambda (error-message)
                  (misskey-compose--handle-error buffer error-message))
       :account account
       :owner (misskey-app account)))))

(defun misskey-compose-add-note ()
  "Insert an empty note after the current draft item."
  (interactive)
  (when (appkit-compose-submitting-p)
    (user-error "Wait for the current publish request to finish"))
  (appkit-compose-add-item)
  (setq-local misskey-compose-items (appkit-compose-items))
  (set-buffer-modified-p t))

(defun misskey-compose-remove-note ()
  "Remove the current extra note from the draft."
  (interactive)
  (when (appkit-compose-submitting-p)
    (user-error "Wait for the current publish request to finish"))
  (unless (> (length (appkit-compose-items)) 1)
    (user-error "The draft already has only one note"))
  (let ((index (or (appkit-compose-current-part-index) 0)))
    (appkit-compose-drop-item index)
    (setq-local misskey-compose-items (appkit-compose-items))
    (set-buffer-modified-p t)
    (message "Removed note %d." (1+ index))))

(defun misskey-compose-send ()
  "Publish the current Misskey draft once.

A multi-note draft creates the first note, then each later note as a reply."
  (interactive)
  (when (appkit-compose-submitting-p)
    (user-error "This note is already being published"))
  (let* ((bodies (misskey-compose--snapshot-bodies))
         (buffer (current-buffer))
         (account (misskey-compose--account))
         (label (if (> (length bodies) 1)
                    "Publishing Misskey notes..."
                  "Publishing Misskey note...")))
    (appkit-compose-begin-submit :label label)
    (misskey-compose--refresh)
    (misskey-compose--lock-bodies)
    (message "%s" label)
    (condition-case err
        (misskey-compose--send-next buffer account bodies 0 nil)
      ((error quit)
       (appkit-compose-finish-submit)
       (misskey-compose--refresh)
       (misskey-compose--unlock-bodies)
       (signal (car err) (cdr err))))))

(defun misskey-compose-cancel ()
  "Cancel and kill the current Misskey draft."
  (interactive)
  (unless (appkit-compose-cancel-submit)
    (kill-buffer (current-buffer))))

(provide 'misskey-compose)

;;; misskey-compose.el ends here
