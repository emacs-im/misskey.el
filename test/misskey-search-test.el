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

(ert-deftest misskey-search-hashtag-endpoint-and-independent-pagination ()
  (misskey-test-with-session
    (let ((account (misskey--current-account)) requests tag text)
      (unwind-protect
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey-http-read)
                     (lambda (endpoint parameters callback &rest _options)
                       (push (cons endpoint parameters) requests)
                       (let ((id (if (equal endpoint "notes/search-by-tag")
                                     (if (plist-get parameters :untilId) "tag-older" "tag-first")
                                   "text-first")))
                         (funcall callback (list (misskey-search-test--note id "#猫")))))))
            (setq tag (misskey-search-tag "猫" account)
                  text (misskey-search "猫" account))
            (misskey-test-drain tag)
            (with-current-buffer (appkit-surface-buffer tag) (misskey-search-load-more))
            (misskey-test-drain tag)
            (should (equal (misskey-test-visible-note-keys tag) '("tag-first" "tag-older")))
            (should (equal (misskey-test-visible-note-keys text) '("text-first")))
            (let ((older (cl-find-if (lambda (request) (plist-get (cdr request) :untilId)) requests)))
              (should (equal (car older) "notes/search-by-tag"))
              (should (equal (plist-get (cdr older) :tag) "猫"))
              (should (equal (plist-get (cdr older) :untilId) "tag-first"))
              (should-not (plist-member (cdr older) :query))))
        (dolist (view (list tag text))
          (when (appkit-surface-p view) (kill-buffer (appkit-surface-buffer view))))))))

(provide 'misskey-search-test)

;;; misskey-search-test.el ends here
