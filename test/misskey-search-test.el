;;; misskey-search-test.el --- Tests for Misskey search -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-search)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(defun misskey-search-test--note (id text)
  "Return one valid test note with ID and TEXT."
  `((id . ,id) (text . ,text) (visibility . "public")
    (user . ((id . "u1") (username . "alice")))))

(ert-deftest misskey-search-keeps-query-pagination-independent ()
  (misskey-test-with-session
    (let*
        ((misskey--apps (make-hash-table :test #'equal))
         (misskey-search--serial 0)
         (account
          (misskey--account-create :origin "https://example.social"
                                   :auth-source-user "TOKEN"
                                   :remote-user-id "self"))
         requests foo bar)
      (unwind-protect
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey-http-read)
                (lambda (endpoint parameters callback &rest _options)
                  (push (cons endpoint parameters) requests)
                  (let*
                      ((query (plist-get parameters :query))
                       (until-id (plist-get parameters :untilId))
                       (id
                        (if until-id (concat query "-2")
                          (concat query "-1"))))
                    (funcall callback
                             (list (misskey-search-test--note id query))))
                  'request)))
            (setq foo (misskey-search "foo" account) bar
                  (misskey-search "bar" account))
            (misskey-test-drain foo)
            (should
             (equal (misskey-test-visible-note-keys foo) '("foo-1")))
            (should
             (equal (misskey-test-visible-note-keys bar) '("bar-1")))
            (with-current-buffer (appkit-surface-buffer foo)
              (misskey-search-load-more))
            (misskey-test-drain foo)
            (should
             (equal (misskey-test-visible-note-keys foo)
                    '("foo-1" "foo-2")))
            (should
             (equal (misskey-test-visible-note-keys bar) '("bar-1")))
            (let
                ((older
                  (cl-find-if
                   (lambda (request)
                     (equal (plist-get (cdr request) :untilId) "foo-1"))
                   requests)))
              (should older) (should (equal (car older) "notes/search"))
              (should (equal (plist-get (cdr older) :query) "foo"))))
        (when (appkit-surface-p foo)
          (kill-buffer (appkit-surface-buffer foo)))
        (when (appkit-surface-p bar)
          (kill-buffer (appkit-surface-buffer bar)))
        (misskey-stop)))))

(ert-deftest misskey-search-rejects-empty-query ()
  (misskey-test-with-session
    (should-error (misskey-search "  ") :type 'user-error)))

(provide 'misskey-search-test)

;;; misskey-search-test.el ends here
