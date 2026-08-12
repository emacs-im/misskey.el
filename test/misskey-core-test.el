;;; misskey-core-test.el --- Tests for Misskey configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-core)

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
                      "https://example.social/base"
                      "https://example.social?query=yes"
                      "https://example.social/#fragment"))
    (let ((misskey-instance-url instance))
      (should-error (misskey--instance-origin) :type 'user-error))))

(ert-deftest misskey-auth-token-uses-configured-host-and-user ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "misskey.el")
        captured)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (setq captured args)
                 (list (list :user "misskey.el"
                             :secret (lambda () "TOKEN"))))))
      (should (equal (misskey--auth-token) "TOKEN"))
      (should (equal (plist-get captured :host) "example.social"))
      (should (equal (plist-get captured :user) "misskey.el"))
      (should (equal (plist-get captured :port) "misskey"))
      (should (equal (plist-get captured :require) '(:secret :port)))
      (should (equal (plist-get captured :max) 1))
      (should (equal (misskey--auth-source-spec)
                     '(:host "example.social"
                       :user "misskey.el"
                       :port "misskey"))))))

(ert-deftest misskey-auth-source-spec-distinguishes-nondefault-port ()
  (let ((misskey-instance-url "https://example.social:8443")
        (misskey-auth-source-user "alice"))
    (should (equal (misskey--auth-source-spec)
                   '(:host "example.social"
                     :user "alice"
                     :port "misskey-8443")))))

(ert-deftest misskey-auth-token-rejects-missing-secret ()
  (let ((misskey-instance-url "https://example.social"))
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (should-error (misskey--auth-token) :type 'user-error))))

(provide 'misskey-core-test)

;;; misskey-core-test.el ends here
