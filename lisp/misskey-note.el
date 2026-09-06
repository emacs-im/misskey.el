;;; misskey-note.el --- Shared Misskey note semantics -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Validate and interpret Misskey notes without owning transport, view state,
;; rendering, or buffer mutation.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)

(defconst misskey-note-property 'misskey-note
  "Text property carrying a Misskey note payload.")

(defconst misskey-note-id-property 'misskey-note-id
  "Text property carrying a stable Misskey note ID.")

(defconst misskey-user-property 'misskey-user
  "Text property carrying a Misskey user payload.")

(defconst misskey-user-id-property 'misskey-user-id
  "Text property carrying a stable Misskey user ID.")

(defun misskey-note-id (note)
  "Return NOTE's stable identifier, or nil."
  (and (consp note) (alist-get 'id note)))

(defun misskey-note-user (note)
  "Return NOTE's author object, or nil."
  (and (consp note) (alist-get 'user note)))

(defun misskey-user-id (user)
  "Return USER's stable identifier, or nil."
  (and (consp user) (alist-get 'id user)))

(defun misskey-user-handle (user)
  "Return USER's canonical readable handle."
  (let ((username (and (consp user) (alist-get 'username user)))
        (host (and (consp user) (alist-get 'host user))))
    (cond
     ((and (stringp username) (stringp host)
           (not (string-empty-p host)))
      (format "@%s@%s" username host))
     ((stringp username) (format "@%s" username))
     (t "@unknown"))))

(defun misskey-user-label (user)
  "Return USER's display name followed by its canonical handle."
  (let ((name (and (consp user) (alist-get 'name user)))
        (handle (misskey-user-handle user)))
    (if (and (stringp name) (not (string-empty-p name)))
        (format "%s %s" name handle)
      handle)))

(defun misskey-note--empty-array-p (value)
  "Return non-nil when wire array VALUE is absent or empty."
  (or (null value)
      (and (vectorp value) (= (length value) 0))))

(defun misskey-note-pure-renote-p (note)
  "Return non-nil when NOTE contains no authored content beyond a renote."
  (and (consp note)
       (or (consp (alist-get 'renote note))
           (let ((renote-id (alist-get 'renoteId note)))
             (and (stringp renote-id)
                  (not (string-empty-p renote-id)))))
       (null (alist-get 'text note))
       (null (alist-get 'cw note))
       (misskey-note--empty-array-p (alist-get 'files note))
       (misskey-note--empty-array-p (alist-get 'fileIds note))
       (null (alist-get 'poll note))
       (null (alist-get 'reply note))
       (null (alist-get 'replyId note))))

(defun misskey-note-display-note (note)
  "Return the note whose content NOTE primarily displays.

Return nil for a pure wrapper whose deleted target was omitted by the server."
  (if (misskey-note-pure-renote-p note)
      (and (consp (alist-get 'renote note))
           (alist-get 'renote note))
    note))

(defun misskey-note-quoted-note (note)
  "Return NOTE's quoted note, or nil.

A pure renote is a wrapper rather than a quote."
  (and (not (misskey-note-pure-renote-p note))
       (consp (alist-get 'renote note))
       (alist-get 'renote note)))

(defun misskey-note--https-url-p (url)
  "Return non-nil when URL is a safe credential-free HTTPS URL."
  (and (stringp url)
       (not (string-empty-p url))
       (not (string-match-p "[[:space:]\"\\\\]" url))
       (condition-case nil
           (let ((parsed (url-generic-parse-url url)))
             (and (string-equal (url-type parsed) "https")
                  (stringp (url-host parsed))
                  (not (string-empty-p (url-host parsed)))
                  (null (url-user parsed))
                  (null (url-password parsed))))
         (error nil))))

(defun misskey-note-avatar-url (note)
  "Return the HTTPS avatar URL for NOTE's displayed author, or nil."
  (let* ((display-note (misskey-note-display-note note))
         (user (misskey-note-user display-note))
         (url (and (consp user) (alist-get 'avatarUrl user))))
    (and (misskey-note--https-url-p url) url)))

(defun misskey-note-media-files (note)
  "Return every displayable DriveFile attached to NOTE."
  (cl-remove-if-not
   (lambda (file)
     (let ((id (and (consp file) (alist-get 'id file)))
           (type (and (consp file) (alist-get 'type file))))
       (and (stringp id)
            (not (string-empty-p id))
            (stringp type)
            (not (string-empty-p type)))))
   (alist-get 'files (misskey-note-display-note note))))

(defun misskey-file-original-url (file)
  "Return FILE's original HTTPS Drive URL, or nil."
  (let ((url (and (consp file) (alist-get 'url file))))
    (and (misskey-note--https-url-p url) url)))

(defun misskey-file-preview-url (file)
  "Return FILE's HTTPS image or video preview URL, or nil."
  (let* ((type (and (consp file) (alist-get 'type file)))
         (previewable-p
          (and (stringp type)
               (or (string-prefix-p "image/" type)
                   (string-prefix-p "video/" type))))
         (url
          (and previewable-p
               (or (alist-get 'thumbnailUrl file)
                   (and (string-prefix-p "image/" type)
                        (alist-get 'url file))))))
    (and (misskey-note--https-url-p url) url)))

(defun misskey-note-sensitive-media-p (note)
  "Return non-nil when NOTE displays a sensitive attachment."
  (let ((quoted (misskey-note-quoted-note note)))
    (cl-some
     (lambda (file) (eq (alist-get 'isSensitive file) t))
     (append
      (misskey-note-media-files note)
      (and quoted (misskey-note-media-files quoted))))))

(defun misskey-note-content-warning-p (note)
  "Return non-nil when any content displayed by NOTE has a warning."
  (let* ((display-note (misskey-note-display-note note))
         (quoted (misskey-note-quoted-note note))
         (warnings (list (alist-get 'cw display-note)
                         (and quoted (alist-get 'cw quoted)))))
    (cl-some
     (lambda (warning)
       (and (stringp warning) (not (string-empty-p warning))))
     warnings)))

(defun misskey-note-validate (note)
  "Return NOTE after validating stable note and author identities."
  (let* ((id (misskey-note-id note))
         (user (misskey-note-user note))
         (user-id (misskey-user-id user)))
    (unless (and (consp note)
                 (stringp id) (not (string-empty-p id))
                 (consp user)
                 (stringp user-id) (not (string-empty-p user-id)))
      (error "Misskey response contains a malformed note")))
  (dolist (key '(renote reply))
    (when-let* ((nested (alist-get key note))
                ((consp nested)))
      (misskey-note-validate nested)))
  note)

(defun misskey-note-new-notes (current candidates)
  "Return CANDIDATES whose IDs do not occur in CURRENT."
  (let ((seen (make-hash-table :test #'equal))
        result)
    (dolist (note current)
      (puthash (misskey-note-id note) t seen))
    (dolist (note candidates (nreverse result))
      (let ((id (misskey-note-id note)))
        (unless (gethash id seen)
          (puthash id t seen)
          (push note result))))))

(defun misskey-note-validate-list (payload)
  "Return validated note PAYLOAD with unique stable IDs."
  (unless (listp payload)
    (error "Misskey note response is not a list"))
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (note payload)
      (misskey-note-validate note)
      (let ((id (misskey-note-id note)))
        (when (gethash id seen)
          (error "Misskey response duplicates note %s" id))
        (puthash id t seen))))
  payload)

(defun misskey-note-presentation-dependencies (note)
  "Return shared presentation dependency keys for NOTE."
  (let* ((quoted (misskey-note-quoted-note note))
         (notes (list note (misskey-note-display-note note) quoted))
         (avatar (misskey-note-avatar-url note))
         (files (append (misskey-note-media-files note)
                        (and quoted (misskey-note-media-files quoted)))))
    (delete-dups
     (delq nil
           (append
            (mapcar (lambda (dependency-note)
                      (when-let* ((id (misskey-note-id dependency-note)))
                        (list :note id)))
                    notes)
            (list (and avatar (list :avatar avatar)))
            (mapcar (lambda (dependency-note)
                      (when-let* ((id (misskey-user-id
                                       (misskey-note-user dependency-note))))
                        (list :user id)))
                    notes)
            (mapcar (lambda (file)
                      (list :media (alist-get 'id file)))
                    files))))))

(provide 'misskey-note)

;;; misskey-note.el ends here
