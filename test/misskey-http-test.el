;;; misskey-http-test.el --- Tests for Misskey transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-http)

(defmacro misskey-http-test--response (status body &rest forms)
  "Evaluate FORMS in a fake HTTP response with STATUS and BODY."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (set-buffer-multibyte nil)
     (insert "HTTP/1.1 " (number-to-string ,status) " Test\r\n\r\n" ,body)
     (setq-local url-http-response-status ,status)
     (setq-local url-http-end-of-headers
                 (copy-marker
                  (progn
                    (goto-char (point-min))
                    (search-forward "\r\n\r\n"))))
     ,@forms))

(ert-deftest misskey-http-endpoint-url-keeps-api-under-origin ()
  (let ((misskey-instance-url "https://example.social"))
    (should (equal (misskey-http--endpoint-url "notes/create")
                   "https://example.social/api/notes/create"))
    (dolist (endpoint '("/notes/create" "../notes/create" "notes//create"
                        "notes/create?elsewhere=yes"))
      (should-error (misskey-http--endpoint-url endpoint)))))

(ert-deftest misskey-http-decodes-successful-created-note ()
  (misskey-http-test--response
      200 "{\"createdNote\":{\"id\":\"note-1\"}}"
    (should
     (equal (misskey-http--decode-response nil t)
            '(success (createdNote (id . "note-1")))))))

(ert-deftest misskey-http-decodes-api-error-with-code ()
  (misskey-http-test--response
      400 "{\"error\":{\"message\":\"Too long\",\"code\":\"MAX_LENGTH\"}}"
    (let ((message (cdr (misskey-http--decode-response nil t))))
      (should (string-match-p "unknown" message))
      (should (string-match-p "HTTP 400" message))
      (should (string-match-p "Too long (MAX_LENGTH)" message)))))

(ert-deftest misskey-http-decodes-empty-read-array ()
  (misskey-http-test--response 200 "[]"
    (should (equal (misskey-http--decode-response nil nil)
                   '(success)))))

(ert-deftest misskey-http-rejects-oversized-response-before-parsing ()
  (let ((misskey-http--response-limit 4))
    (misskey-http-test--response 200 "12345"
      (should-error (misskey-http--response-body)))))

(ert-deftest misskey-http-post-uses-bearer-json-without-token-body ()
  (let ((misskey-instance-url "https://example.social")
        captured callback-p)
    (cl-letf (((symbol-function 'misskey--auth-token)
               (lambda (&optional _account) "SECRET"))
              ((symbol-function 'misskey-app)
               (lambda (&optional _account) 'owner))
              ((symbol-function 'misskey-http--post-once)
               (lambda (url callback callback-args)
                 (setq captured
                       (list :url url
                             :method url-request-method
                             :headers url-request-extra-headers
                             :data url-request-data
                             :callback callback
                             :callback-args callback-args))
                 nil)))
      (misskey-http-post
       "notes/create" '(:text "hello" :visibility "public")
       (lambda (_payload) (setq callback-p t))
       :errback #'ignore)
      (should (equal (plist-get captured :url)
                     "https://example.social/api/notes/create"))
      (should (equal (plist-get captured :method) "POST"))
      (should (equal (cdr (assoc "Authorization"
                                 (plist-get captured :headers)))
                     "Bearer SECRET"))
      (should (equal (cdr (assoc "Content-Type"
                                 (plist-get captured :headers)))
                     "application/json"))
      (should (equal
               (json-parse-string
                (decode-coding-string (plist-get captured :data) 'utf-8)
                :object-type 'alist)
               '((text . "hello") (visibility . "public"))))
      (should-not (string-match-p "SECRET" (plist-get captured :data)))
      (should-not callback-p))))

(ert-deftest misskey-http-redacts-token-from-post-dispatch-errors ()
  (let ((misskey-instance-url "https://example.social")
        failure)
    (cl-letf (((symbol-function 'misskey--auth-token)
               (lambda (&optional _account) "SECRET"))
              ((symbol-function 'misskey-app)
               (lambda (&optional _account) 'owner))
              ((symbol-function 'url-do-setup) #'ignore)
              ((symbol-function 'url-find-proxy-for-url) (lambda (&rest _) nil))
              ((symbol-function 'url-http)
               (lambda (&rest _)
                 (error "transport exposed SECRET"))))
      (misskey-http-post
       "notes/create" '(:text "hello") #'ignore
       :errback (lambda (message) (setq failure message)))
      (should (string-match-p "unknown" failure))
      (should (string-match-p "\\[REDACTED\\]" failure))
      (should-not (string-match-p "SECRET" failure)))))

(ert-deftest misskey-http-cancel-reports-unknown-outcome-once ()
  (let ((buffer (generate-new-buffer " *misskey-http-test*"))
        (calls 0)
        message)
    (unwind-protect
        (let* ((owner (progn
                        (unless (appkit-app-kind-registered-p 'misskey-test)
                          (appkit-register-app-kind 'misskey-test nil))
                        (appkit-start-app 'misskey-test)))
               (handle
                (appkit-register-handle
                 owner 'function
                 (list :buffer buffer
                       :errback (lambda (failure)
                                  (setq calls (1+ calls)
                                        message failure))
                       :writep t)
                 #'misskey-http--cancel-request)))
          (with-current-buffer buffer
            (setq-local misskey-http--request-handle handle))
          (should (misskey-http-cancel-request buffer))
          (should (= calls 1))
          (should (string-match-p "unknown" message))
          (should-not (buffer-live-p buffer))
          (should-not (appkit-cancel-handle handle))
          (appkit-stop-app owner))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'misskey-http-test)

;;; misskey-http-test.el ends here
