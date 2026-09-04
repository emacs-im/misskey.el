;;; misskey-directory-test.el --- Tests for Misskey directories -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-directory)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(defun misskey-directory-test--user (id username)
  "Return a valid test user with ID and USERNAME."
  `((id . ,id) (username . ,username) (name . ,(capitalize username))
    (notesCount . 4) (followersCount . 2)))

(ert-deftest misskey-directory-pages-by-relationship-and-keys-by-user
    ()
  (misskey-test-with-session
    (let*
        ((misskey--apps (make-hash-table :test #'equal))
         (account
          (misskey--account-create :origin "https://example.social"
                                   :auth-source-user "TOKEN"
                                   :remote-user-id "self"))
         (subject (misskey-directory-test--user "owner" "owner"))
         requests view)
      (unwind-protect
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey-http-read)
                (lambda (endpoint parameters callback &rest _options)
                  (push (cons endpoint parameters) requests)
                  (funcall callback
                           (if (plist-get parameters :untilId)
                               `
                               (((id . "rel-2")
                                 (follower . ,(misskey-directory-test--user
                                               "u2" "bob"))))
                             `
                             (((id . "rel-1")
                               (follower . ,(misskey-directory-test--user
                                             "u1" "alice"))))))
                  'request)))
            (setq view
                  (misskey-directory-open 'followers subject account))
            (misskey-test-drain view)
            (with-current-buffer (appkit-surface-buffer view)
              (let ((surface (appkit-directory-surface)))
                (should (appkit-directory-entry-for-key surface "u1"))
                (should-not
                 (appkit-directory-entry-for-key surface "rel-1")))
              (should
               (text-property-search-forward misskey-user-id-property
                                             "u1" #'equal))
              (misskey-directory-load-more))
            (misskey-test-drain view)
            (with-current-buffer (appkit-surface-buffer view)
              (should
               (appkit-directory-entry-for-key
                (appkit-directory-surface) "u2")))
            (let
                ((older
                  (cl-find-if
                   (lambda (request)
                     (equal (plist-get (cdr request) :untilId) "rel-1"))
                   requests)))
              (should older)
              (should (equal (car older) "users/followers"))))
        (when (appkit-surface-p view)
          (kill-buffer (appkit-surface-buffer view)))
        (misskey-stop)))))

(ert-deftest misskey-directory-rejects-malformed-relationship-user ()
  (misskey-test-with-session
    (let ((state '(:kind followers)))
      (should-error
       (misskey-directory--validate-payload
        state '(((id . "rel-1") (follower . ((username . "missing-id"))))))))))

(ert-deftest misskey-directory-refresh-recomputes-exhaustion ()
  (misskey-test-with-session
    (let*
        ((misskey--apps (make-hash-table :test #'equal))
         (account
          (misskey--account-create :origin "https://example.social"
                                   :auth-source-user "TOKEN"
                                   :remote-user-id "self"))
         (subject (misskey-directory-test--user "owner" "owner"))
         (requests 0) view)
      (unwind-protect
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey-http-read)
                (lambda (_endpoint _parameters callback &rest _options)
                  (cl-incf requests)
                  (funcall callback
                           (pcase requests
                             (1
                              `
                              (((id . "rel-1")
                                (follower . ,(misskey-directory-test--user
                                              "u1" "alice")))))
                             (2 nil)
                             (3
                              `
                              (((id . "rel-2")
                                (follower . ,(misskey-directory-test--user
                                              "u2" "bob")))))))
                  nil)))
            (setq view
                  (misskey-directory-open 'followers subject account))
            (misskey-test-drain view)
            (with-current-buffer (appkit-surface-buffer view)
              (misskey-directory-load-more))
            (misskey-test-drain view)
            (should
             (plist-get (appkit-surface-model view) :older-exhausted-p))
            (with-current-buffer (appkit-surface-buffer view)
              (misskey-directory-refresh))
            (misskey-test-drain view)
            (should-not
             (plist-get (appkit-surface-model view) :older-exhausted-p))
            (should
             (equal
              (alist-get 'id
                         (car
                          (plist-get (appkit-surface-model view) :items)))
              "rel-2")))
        (when (appkit-surface-p view)
          (kill-buffer (appkit-surface-buffer view)))
        (misskey-stop)))))

(ert-deftest
    misskey-directory-empty-load-more-preserves-initial-request ()
  (misskey-test-with-session
    (let*
        ((misskey--apps (make-hash-table :test #'equal))
         (account
          (misskey--account-create :origin "https://example.social"
                                   :auth-source-user "TOKEN"
                                   :remote-user-id "self"))
         (subject (misskey-directory-test--user "owner" "owner"))
         (request (list 'directory-request)) cancelled operation view)
      (unwind-protect
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey-http--start-request)
                (lambda (_endpoint _parameters _callback &rest options)
                  (setq operation (plist-get options :owner))
                  (appkit-register-handle operation 'function request
                                          #'misskey-http-cancel)
                  request))
               ((symbol-function 'misskey-http-cancel)
                (lambda (active) (setq cancelled active))))
            (setq view
                  (misskey-directory-open 'followers subject account))
            (with-current-buffer (appkit-surface-buffer view)
              (should-error (misskey-directory-load-more) :type
                            'user-error))
            nil (should-not cancelled)
            (misskey-read-cancel view misskey-directory--request-key)
            (should (eq cancelled request)))
        (when (appkit-surface-p view)
          (kill-buffer (appkit-surface-buffer view)))
        (misskey-stop)))))

(provide 'misskey-directory-test)

;;; misskey-directory-test.el ends here
