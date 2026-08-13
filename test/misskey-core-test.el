;;; misskey-core-test.el --- Tests for Misskey configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-core)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(ert-deftest misskey-instance-origin-normalizes-default-port ()
  (let ((misskey-instance-url "https://example.social/"))
    (should (equal (misskey--instance-origin)
                   "https://example.social"))))

(ert-deftest misskey-instance-origin-preserves-nondefault-port ()
  (let ((misskey-instance-url "https://example.social:8443"))
    (should (equal (misskey--instance-origin)
                   "https://example.social:8443"))))

(ert-deftest misskey-instance-origin-rejects-untrusted-shapes ()
  (dolist (instance '(nil
                      ""
                      "http://example.social"
                      "https://user@example.social"
                      "https://bad%0ahost"
                      "https://bad\\host"
                      "https://example.social/base"
                      "https://example.social?query=yes"
                      "https://example.social/#fragment"))
    (let ((misskey-instance-url instance))
      (should-error (misskey--instance-origin) :type 'user-error))))

(ert-deftest misskey-current-account-loads-atomic-credential-identity ()
  (let* ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "credential-label")
         (credential
          (misskey--credential-create
           :token "TOKEN-1" :user-id "stable-user-id"))
         (secret (misskey--credential-string credential))
         captured)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (setq captured args)
                 (list (list :secret (lambda () secret))))))
      (let ((account (misskey--current-account)))
        (should (equal (misskey--account-auth-source-user account)
                       "credential-label"))
        (should (equal (misskey--account-remote-user-id account)
                       "stable-user-id"))
        (should (equal (misskey--account-key account)
                       '("https://example.social" "stable-user-id")))
        (should (equal (misskey--auth-token account) "TOKEN-1")))
      (should (equal (plist-get captured :host) "example.social"))
      (should (equal (plist-get captured :user) "credential-label"))
      (should (equal (plist-get captured :port) "misskey")))))

(ert-deftest misskey-token-only-credential-requires-reauthorization ()
  (let ((misskey-instance-url "https://example.social")
        failure)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _)
                 (list (list :secret (lambda () "LEGACY-TOKEN"))))))
      (condition-case err
          (misskey--current-account)
        (user-error (setq failure (error-message-string err))))
      (should (string-match-p "reauthorize" failure))
      (should-not (misskey--valid-token-p "x\nheader = evil")))))

(ert-deftest misskey-auth-source-spec-distinguishes-nondefault-port ()
  (let ((misskey-instance-url "https://example.social:8443")
        (misskey-auth-source-user "alice"))
    (should (equal (misskey--auth-source-spec)
                   '(:host "example.social"
                     :user "alice"
                     :port "misskey-8443")))))

(ert-deftest misskey-account-key-rejects-provisional-locator ()
  (let ((misskey-instance-url "https://example.social"))
    (should-error
     (misskey--account-key (misskey--current-account-locator)))))

(defun misskey-core-test--app ()
  "Return an isolated live Misskey app for state merge tests."
  (let ((account
         (misskey--account-create
          :origin "https://example.social"
          :auth-source-user "TOKEN"
          :remote-user-id "self")))
    (appkit-start-app
     'misskey :id (list 'core-test (make-symbol "app"))
     :state (misskey--make-session account))))

(ert-deftest misskey-state-stale-read-cannot-roll-back-write-fences ()
  (let ((app (misskey-core-test--app)))
    (unwind-protect
        (let ((stale (misskey-state-observe app)))
          (misskey-fence-note-state
           app "n1" :my-reaction :reaction-count)
          (misskey-set-note-state-values
           app "n1" :my-reaction ":wave:" :reaction-count 4
           :favorited-p t)
          (misskey-merge-note-state
           app '((id . "n1") (myReaction) (reactionCount . 3)) stale)
          (should
           (equal (misskey-note-state-value
                   app "n1" :my-reaction nil)
                  ":wave:"))
          (should
           (= (misskey-note-state-value
               app "n1" :reaction-count 0)
              4))
          (let ((fresh (misskey-state-observe app)))
            (misskey-merge-note-state
             app '((id . "n1") (myReaction) (reactionCount . 7)) fresh))
          (should-not
           (misskey-note-state-value
            app "n1" :my-reaction "unexpected"))
          (should
           (= (misskey-note-state-value
               app "n1" :reaction-count 0)
              7))
          ;; Private favorites have no authoritative note field to merge.
          (should
           (misskey-note-state-value app "n1" :favorited-p nil)))
      (appkit-stop-app app))))

(ert-deftest misskey-state-authoritative-merge-respects-field-presence ()
  (let ((app (misskey-core-test--app)))
    (unwind-protect
        (progn
          (misskey-merge-note-state
           app
           '((id . "n1") (myReaction . ":heart:") (reactionCount . 2))
           (misskey-state-observe app))
          (misskey-merge-note-state
           app '((id . "n1") (reactionCount . 3))
           (misskey-state-observe app))
          (should
           (equal (misskey-note-state-value
                   app "n1" :my-reaction nil)
                  ":heart:"))
          (should
           (= (misskey-note-state-value
               app "n1" :reaction-count 0)
              3))
          (misskey-merge-note-state
           app '((id . "n1") (myReaction))
           (misskey-state-observe app))
          (should-not
           (misskey-note-state-value
            app "n1" :my-reaction "unexpected")))
      (appkit-stop-app app))))

(ert-deftest misskey-state-newer-read-wins-over-later-stale-callback ()
  (let ((app (misskey-core-test--app)))
    (unwind-protect
        (let ((older (misskey-state-observe app))
              (newer (misskey-state-observe app)))
          (misskey-merge-user-state
           app
           '((id . "u1")
             (isFollowing . t)
             (hasPendingFollowRequestFromYou))
           newer)
          (misskey-merge-user-state
           app
           '((id . "u1")
             (isFollowing)
             (hasPendingFollowRequestFromYou . t))
           older)
          (should
           (misskey-user-state-value app "u1" :following-p nil))
          (should-not
           (misskey-user-state-value
            app "u1" :follow-pending-p t)))
      (appkit-stop-app app))))

(provide 'misskey-core-test)

;;; misskey-core-test.el ends here
