;;; misskey-notifications-test.el --- Tests for Misskey notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-notifications)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(defun misskey-notifications-test--payload ()
  "Return one valid test notification payload."
  '(((id . "notification-1")
     (type . "mention")
     (createdAt . "2026-08-13T00:00:00.000Z")
     (user . ((id . "u1") (username . "alice") (name . "Alice")))
     (note . ((id . "note-1") (text . "hello")
              (user . ((id . "u1") (username . "alice"))))))))

(ert-deftest misskey-notifications-read-never-implicitly-acknowledges ()
  (let* ((misskey--apps (make-hash-table :test #'equal))
         (account (misskey--account-create :origin "https://example.social" :auth-source-user "TOKEN" :remote-user-id "self"))
         read-parameters writes view)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-read)
                   (lambda (endpoint parameters callback &rest _options)
                     (should (equal endpoint "i/notifications"))
                     (setq read-parameters parameters)
                     (funcall callback (misskey-notifications-test--payload))
                     'read-request))
                  ((symbol-function 'misskey-http-post)
                   (lambda (endpoint parameters callback &rest _options)
                     (push (cons endpoint parameters) writes)
                     (funcall callback nil)
                     'write-request)))
          (setq view (misskey-notifications account))
          (appkit-sync-invalidations view)
          (should (eq (plist-get read-parameters :markAsRead) :json-false))
          (should-not writes)
          (with-current-buffer (appkit-view-buffer view)
            (let ((entry
                   (appkit-directory-entry-for-key
                    (appkit-directory-surface) "notification-1")))
              (should entry)
              (should (appkit-directory-entry-unread-p entry)))
            (misskey-notifications-mark-all-read))
          (appkit-sync-invalidations view)
          (should (= (length writes) 1))
          (should (equal (caar writes)
                         "notifications/mark-all-as-read"))
          (should (hash-table-p (cdar writes)))
          (should (= (hash-table-count (cdar writes)) 0))
          (with-current-buffer (appkit-view-buffer view)
            (should-not
             (appkit-directory-entry-unread-p
              (appkit-directory-entry-for-key
               (appkit-directory-surface) "notification-1")))))
      (when (appkit-view-p view)
        (appkit-kill-view view t))
      (misskey-stop))))

(ert-deftest misskey-notifications-rejects-duplicate-identities ()
  (let ((payload (append (misskey-notifications-test--payload)
                         (misskey-notifications-test--payload))))
    (should-error (misskey-notifications--validate-list payload))))

(ert-deftest misskey-notifications-activation-uses-note-id-and-source-account ()
  (let* ((misskey--apps (make-hash-table :test #'equal))
         (account (misskey--account-create :origin "https://example.social" :auth-source-user "TOKEN" :remote-user-id "self"))
         thread-args profile-args view)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-read)
                   (lambda (_endpoint _parameters callback &rest _options)
                     (funcall callback (misskey-notifications-test--payload))
                     nil))
                  ((symbol-function 'misskey-thread-open)
                   (lambda (&rest args) (setq thread-args args)))
                  ((symbol-function 'misskey-profile-open)
                   (lambda (&rest args) (setq profile-args args))))
          (setq view (misskey-notifications account))
          (appkit-sync-invalidations view)
          (with-current-buffer (appkit-view-buffer view)
            (let ((surface (appkit-directory-surface)))
              (appkit-directory-activate-entry
               surface
               (appkit-directory-entry-for-key surface "notification-1"))
              (should (equal thread-args (list "note-1" account)))
              (misskey-notifications--activate-item
               surface
               (appkit-directory-entry-create
                :key "renote" :role 'item :item-p t
                :payload
                '((id . "renote")
                  (type . "renote")
                  (note
                   (id . "wrapper")
                   (renoteId . "display")
                   (renote
                    (id . "display")
                    (text . "shown")
                    (user (id . "u1") (username . "alice")))))))
              (should (equal thread-args (list "display" account)))
              (misskey-notifications--activate-item
               surface
               (appkit-directory-entry-create
                :key "actor" :role 'item :item-p t
                :payload
                '((id . "actor") (type . "follow")
                  (user . ((id . "u2") (username . "bob"))))))
              (should (equal (cadr profile-args) account)))))
      (when (appkit-view-p view)
        (appkit-kill-view view t))
      (misskey-stop))))

(ert-deftest misskey-notifications-summary-redacts-content-warning-text ()
  (with-temp-buffer
    (misskey-notifications--insert-item
     nil
     (appkit-directory-entry-create
      :key "cw" :role 'item :item-p t
      :payload
      '((id . "cw") (type . "mention")
        (note . ((id . "note-cw") (cw . "Spoiler") (text . "secret")
                 (user . ((id . "u1") (username . "alice"))))))))
    (should-not (string-match-p "secret" (buffer-string)))))

(ert-deftest misskey-notifications-mark-snapshot-leaves-later-arrival-unread ()
  (let* ((misskey--apps (make-hash-table :test #'equal))
         (account (misskey--account-create :origin "https://example.social" :auth-source-user "TOKEN" :remote-user-id "self"))
         (old (car (misskey-notifications-test--payload)))
         (new '((id . "notification-2") (type . "follow")
                (createdAt . "2026-08-13T01:00:00.000Z")
                (user . ((id . "u2") (username . "bob")))))
         (reads 0)
         mark-callback
         view)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-read)
                   (lambda (_endpoint _parameters callback &rest _options)
                     (cl-incf reads)
                     (funcall callback
                              (if (= reads 1) (list old) (list new old)))
                     nil))
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint _parameters callback &rest _options)
                     (setq mark-callback callback)
                     'mark-request)))
          (setq view (misskey-notifications account))
          (appkit-sync-invalidations view)
          (with-current-buffer (appkit-view-buffer view)
            (misskey-notifications-mark-all-read)
            (misskey-notifications-refresh))
          (funcall mark-callback nil)
          (appkit-sync-invalidations view)
          (with-current-buffer (appkit-view-buffer view)
            (let ((surface (appkit-directory-surface)))
              (should-not
               (appkit-directory-entry-unread-p
                (appkit-directory-entry-for-key surface "notification-1")))
              (should
               (appkit-directory-entry-unread-p
                (appkit-directory-entry-for-key surface "notification-2"))))))
      (when (appkit-view-p view)
        (appkit-kill-view view t))
      (misskey-stop))))

(ert-deftest misskey-notifications-empty-load-more-preserves-initial-request ()
  (let* ((misskey--apps (make-hash-table :test #'equal))
         (account (misskey--account-create :origin "https://example.social" :auth-source-user "TOKEN" :remote-user-id "self"))
         (request (list 'initial-request))
         cancelled
         operation
         view)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-read)
                   (lambda (_endpoint _parameters _callback &rest options)
                     (setq operation (plist-get options :owner))
                     (appkit-register-handle
                      operation 'function request #'misskey-http-cancel)
                     request))
                  ((symbol-function 'misskey-http-cancel)
                   (lambda (active) (setq cancelled active))))
          (setq view (misskey-notifications account))
          (with-current-buffer (appkit-view-buffer view)
            (should-error (misskey-notifications-load-more)
                          :type 'user-error))
          (should (appkit-view-operation-current-p operation))
          (should-not cancelled)
          (appkit-view-operation-cancel
           view misskey-notifications--request-key)
          (should (eq cancelled request)))
      (when (appkit-view-p view)
        (appkit-kill-view view t))
      (misskey-stop))))

(provide 'misskey-notifications-test)

;;; misskey-notifications-test.el ends here
