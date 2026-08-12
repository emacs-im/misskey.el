;;; misskey-compose.el --- Compose Misskey notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Provide the standalone Appkit-backed editor for plain-text Misskey notes.

;;; Code:

(require 'subr-x)
(require 'appkit-compose)
(require 'misskey-core)
(require 'misskey-http)

(defvar-local misskey-compose--sending nil
  "Non-nil while the current compose buffer is sending a note.")

(defvar-local misskey-compose--account nil
  "Account captured when the current draft was opened.")

(defvar misskey-compose--serial 0
  "Serial used to name fresh compose buffers.")

(defun misskey-compose--set-body-read-only (read-only)
  "Set the current compose body READ-ONLY state."
  (when-let* ((bounds (appkit-compose-body-region-bounds)))
    (let ((inhibit-read-only t))
      (if read-only
          (add-text-properties
           (car bounds) (cdr bounds)
           '(read-only t rear-nonsticky (read-only)))
        (remove-text-properties
         (car bounds) (cdr bounds)
         '(read-only nil rear-nonsticky nil))))))

(defvar misskey-compose-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'misskey-compose-send)
    (define-key map (kbd "C-c C-k") #'misskey-compose-cancel)
    map)
  "Keymap for `misskey-compose-mode'.")

(define-derived-mode misskey-compose-mode appkit-compose-mode "Misskey-Compose"
  "Major mode for composing a standalone Misskey note."
  (setq-local misskey-compose--sending nil))

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
  (list (list :label "Visibility" :value "Public")
        (list :label "State"
              :value (if misskey-compose--sending "Publishing" "Draft"))))

(defun misskey-compose--footer ()
  "Return the generated compose command footer."
  (propertize
   (if misskey-compose--sending
       "Publishing; wait for the server response"
     "C-c C-c publish   C-c C-k cancel")
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
    (appkit-compose-setup
     :context-function #'misskey-compose--context
     :status-fields-function #'misskey-compose--status-fields
     :footer-function #'misskey-compose--footer)
    buffer))

(defun misskey-compose--body-text ()
  "Return the validated note body from the current compose buffer."
  (let ((text (appkit-compose-body)))
    (unless (string-match-p "[^[:space:]]" text)
      (user-error "Note text cannot be empty"))
    text))

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
      (setq-local misskey-compose--sending nil)
      (misskey-compose--refresh)
      (misskey-compose--set-body-read-only nil)))
  (message "%s" failure)
  nil)

(defun misskey-compose-send ()
  "Publish the current Misskey note once."
  (interactive)
  (when misskey-compose--sending
    (user-error "This note is already being published"))
  (let ((text (misskey-compose--body-text))
        (buffer (current-buffer))
        (account (misskey-compose--account)))
    (setq-local misskey-compose--sending t)
    (misskey-compose--refresh)
    (misskey-compose--set-body-read-only t)
    (message "Publishing Misskey note...")
    (condition-case err
        (misskey-http-post
         "notes/create"
         (list :text text :visibility "public")
         (lambda (payload)
           (misskey-compose--handle-success buffer payload))
         :errback (lambda (error-message)
                    (misskey-compose--handle-error buffer error-message))
         :account account
         :owner (misskey-app account))
      ((error quit)
       (setq-local misskey-compose--sending nil)
       (misskey-compose--refresh)
       (misskey-compose--set-body-read-only nil)
       (signal (car err) (cdr err))))))

(defun misskey-compose-cancel ()
  "Cancel and kill the current Misskey draft."
  (interactive)
  (when misskey-compose--sending
    (user-error "Wait for the current publish request to finish"))
  (kill-buffer (current-buffer)))

(provide 'misskey-compose)

;;; misskey-compose.el ends here
