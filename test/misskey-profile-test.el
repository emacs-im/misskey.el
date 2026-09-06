;;; misskey-profile-test.el --- Tests for Misskey profiles -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-profile)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(defun misskey-profile-test--note (id text)
  "Return one valid test note with ID and TEXT."
  `((id . ,id)
    (text . ,text)
    (createdAt . "2026-08-13T00:00:00.000Z")
    (visibility . "public")
    (user . ((id . "u1") (username . "alice") (name . "Alice")))))

(ert-deftest misskey-profile-resolves-user-and-switches-note-mode ()
  (misskey-test-with-session
    (let*
        ((misskey--apps (make-hash-table :test #'equal))
         (account
          (misskey--account-create :origin "https://example.social"
                                   :auth-source-user "TOKEN"
                                   :remote-user-id "self"))
         requests view)
      (unwind-protect
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey-http-read)
                (lambda (endpoint parameters callback &rest _options)
                  (push (cons endpoint parameters) requests)
                  (pcase endpoint
                    ("users/show"
                     (funcall callback
                              '((id . "u1") (username . "alice")
                                (name . "Alice") (description . "Hello")
                                (notesCount . 4) (followersCount . 2)
                                (followingCount . 3))))
                    ("users/notes"
                     (funcall callback
                              (list
                               (misskey-profile-test--note
                                (if (plist-get parameters :withFiles)
                                    "media-1"
                                  "note-1")
                                "hello"))))
                    (_ (error "Unexpected endpoint: %s" endpoint)))
                  'request)))
            (setq view (misskey-profile-open "u1" account)) nil
            (should
             (equal (misskey-test-visible-note-keys view) '("note-1")))
            (with-current-buffer (appkit-surface-buffer view)
              (should (string-match-p "Alice" (buffer-string)))
              (should (string-match-p "Hello" (buffer-string)))
              (should (string-match-p "2 followers" (buffer-string)))
              (goto-char (point-min))
              (should (get-text-property (point) misskey-user-property))
              (let (opened)
                (cl-letf
                    (((symbol-function 'misskey-profile-open)
                      (lambda (&rest args) (setq opened args))))
                  (misskey-profile-open-at-point))
                (should (equal (cadr opened) account)))
              (misskey-profile-switch-mode 'media))
            (misskey-test-drain view)
            (should
             (equal (misskey-test-visible-note-keys view) '("media-1")))
            (should
             (eq (plist-get (appkit-surface-model view) :profile-mode)
                 'media))
            (let
                ((media-request
                  (cl-find-if
                   (lambda (request)
                     (and (equal (car request) "users/notes")
                          (plist-get (cdr request) :withFiles)))
                   requests)))
              (should media-request)
              (should
               (equal (plist-get (cdr media-request) :userId) "u1"))
              (should
               (= (plist-get (cdr media-request) :limit)
                  misskey-profile-note-limit))))
        (when (appkit-surface-p view)
          (kill-buffer (appkit-surface-buffer view)))
        (misskey-stop)))))

(ert-deftest misskey-profile-reference-parses-local-and-remote-handles ()
  (misskey-test-with-session
    (should (equal (misskey-profile--reference "@alice")
                   '(:username "alice")))
    (should (equal (misskey-profile--reference "@alice@example.net")
                   '(:username "alice" :host "example.net")))
    (should-error (misskey-profile--reference "@alice@example.net@extra")
                  :type 'user-error)))

(ert-deftest
    misskey-profile-defers-mode-and-load-more-during-user-lookup ()
  (misskey-test-with-session
    (let*
        ((misskey--apps (make-hash-table :test #'equal))
         (account
          (misskey--account-create :origin "https://example.social"
                                   :auth-source-user "TOKEN"
                                   :remote-user-id "self"))
         user-callback notes-parameters cancelled view)
      (unwind-protect
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey-http-read)
                (lambda (endpoint parameters callback &rest _options)
                  (pcase endpoint
                    ("users/show" (setq user-callback callback)
                     'user-request)
                    ("users/notes" (setq notes-parameters parameters)
                     (funcall callback nil) nil)
                    (_ (error "Unexpected endpoint: %s" endpoint)))))
               ((symbol-function 'misskey-http-cancel)
                (lambda (request) (setq cancelled request))))
            (setq view (misskey-profile-open "u1" account))
            (let ((state (appkit-surface-model view)))
              (with-current-buffer (appkit-surface-buffer view)
                (misskey-profile-switch-mode 'media)
                (should-error (misskey-profile-load-more) :type
                              'user-error))
              (should (plist-get state :profile-loading-p))
              (should (eq 'media (plist-get state :profile-mode)))
              (should-not notes-parameters) (should-not cancelled)
              (funcall user-callback
                       '((id . "u1") (username . "alice")
                         (name . "Alice")))
              (should-not (plist-get state :profile-loading-p))
              (should (equal (plist-get notes-parameters :userId) "u1"))
              (should (plist-get notes-parameters :withFiles))
              (with-current-buffer (appkit-surface-buffer view)
                (should-error (misskey-profile-load-more) :type
                              'user-error))))
        (when (appkit-surface-p view)
          (kill-buffer (appkit-surface-buffer view)))
        (misskey-stop)))))

(ert-deftest misskey-feed-empty-load-more-preserves-initial-request
    ()
  (misskey-test-with-session
    (let*
        ((misskey--apps (make-hash-table :test #'equal))
         (account
          (misskey--account-create :origin "https://example.social"
                                   :auth-source-user "TOKEN"
                                   :remote-user-id "self"))
         (request (misskey-http--request-create :callback #'ignore :errback #'ignore)) cancelled operation view)
      (unwind-protect
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey-http--start-request)
                (lambda (endpoint _parameters callback &rest options)
                  (if (equal endpoint "users/show")
                      (progn
                        (funcall callback
                                 '((id . "u1") (username . "alice")
                                   (name . "Alice")))
                        nil)
                    (setq operation (plist-get options :owner))
                    (appkit-register-handle operation 'function request
                                            #'misskey-http-cancel)
                    request)))
               ((symbol-function 'misskey-http-cancel)
                (lambda (active) (setq cancelled active))))
            (setq view (misskey-profile-open "u1" account))
            (misskey-test-drain view)
            (should-error (misskey-feed-load-more view) :type
                          'user-error)
            nil (should-not cancelled)
            (misskey-read-cancel view misskey-feed--request-key)
            (should (eq cancelled request)))
        (when (appkit-surface-p view)
          (kill-buffer (appkit-surface-buffer view)))
        (misskey-stop)))))

(provide 'misskey-profile-test)

;;; misskey-profile-test.el ends here
