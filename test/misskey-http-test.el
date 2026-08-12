;;; misskey-http-test.el --- Tests for Misskey transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-http)

(ert-deftest misskey-http-endpoint-url-keeps-api-under-origin ()
  (let ((misskey-instance-url "https://example.social"))
    (should (equal (misskey-http--endpoint-url "notes/create")
                   "https://example.social/api/notes/create"))
    (dolist (endpoint '("/notes/create" "../notes/create" "notes//create"
                        "notes/create?elsewhere=yes"))
      (should-error (misskey-http--endpoint-url endpoint)))))

(ert-deftest misskey-http-decodes-successful-created-note ()
  (should
   (equal
    (misskey-http--decode-response
     (make-plz-response
      :status 200 :body "{\"createdNote\":{\"id\":\"note-1\"}}")
     t)
    '(success (createdNote (id . "note-1"))))))

(ert-deftest misskey-http-decodes-api-error-with-code ()
  (let ((message
         (cdr
          (misskey-http--decode-response
           (make-plz-response
            :status 400
            :body "{\"error\":{\"message\":\"Too long\",\"code\":\"MAX_LENGTH\"}}")
           t))))
    (should (string-match-p "unknown" message))
    (should (string-match-p "HTTP 400" message))
    (should (string-match-p "Too long (MAX_LENGTH)" message))))

(ert-deftest misskey-http-decodes-empty-read-array ()
  (should (equal
           (misskey-http--decode-response
            (make-plz-response :status 200 :body "[]") nil)
           '(success))))

(ert-deftest misskey-http-rejects-oversized-response-before-parsing ()
  (let ((misskey-http--response-limit 4))
    (should-error
     (misskey-http--decode-response
      (make-plz-response :status 200 :body "12345") nil))))

(ert-deftest misskey-http-post-uses-safe-plz-profile-and-bearer-json ()
  (let* ((misskey-instance-url "https://example.social")
         (owner (appkit-start-app 'misskey :id (make-symbol "http-test")))
         (process (make-pipe-process
                   :name "misskey-http-test" :noquery t))
         captured callback-value request)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/curl"))
                  ((symbol-function 'misskey--auth-token)
                   (lambda (&optional _account) "SECRET"))
                  ((symbol-function 'plz)
                   (lambda (method url &rest arguments)
                     (setq captured
                           (list :method method
                                 :url url
                                 :arguments arguments
                                 :curl-args
                                 (copy-sequence plz-curl-default-args)))
                     process)))
          (setq request
                (misskey-http-post
                 "notes/create" '(:text "hello" :visibility "public")
                 (lambda (payload) (setq callback-value payload))
                 :errback #'ert-fail :owner owner))
          (let* ((arguments (plist-get captured :arguments))
                 (headers (plist-get arguments :headers))
                 (data (plist-get arguments :body)))
            (should (misskey-http--request-p request))
            (should (eq (plist-get captured :method) 'post))
            (should (equal (plist-get captured :url)
                           "https://example.social/api/notes/create"))
            (should (equal (plist-get captured :curl-args)
                           misskey-http--curl-args))
            (should (equal (cdr (assoc "Authorization" headers))
                           "Bearer SECRET"))
            (should (equal (cdr (assoc "Content-Type" headers))
                           "application/json"))
            (should (eq (plist-get arguments :body-type) 'binary))
            (should (eq (plist-get arguments :as) 'response))
            (should (plist-get arguments :decode))
            (should (plist-get arguments :noquery))
            (should (equal
                     (json-parse-string
                      (decode-coding-string data 'utf-8)
                      :object-type 'alist)
                     '((text . "hello") (visibility . "public"))))
            (should-not (string-match-p "SECRET" data))
            (should (= (length (appkit-app-handles owner)) 1))
            (funcall
             (plist-get arguments :then)
             (make-plz-response
              :status 200
              :body "{\"createdNote\":{\"id\":\"note-1\"}}"))
            (should (equal callback-value
                           '((createdNote (id . "note-1")))))
            (should-not (appkit-app-handles owner))
            (should (misskey-http--request-settled-p request))))
      (when (process-live-p process)
        (delete-process process))
      (when (appkit-app-live-p owner)
        (appkit-stop-app owner)))))

(ert-deftest misskey-http-read-decodes-plz-http-error-without-uncertainty ()
  (let* ((misskey-instance-url "https://example.social")
         (owner (appkit-start-app 'misskey :id (make-symbol "http-read")))
         (process (make-pipe-process
                   :name "misskey-http-read-test" :noquery t))
         arguments failure)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/curl"))
                  ((symbol-function 'misskey--auth-token)
                   (lambda (&optional _account) "SECRET"))
                  ((symbol-function 'plz)
                   (lambda (_method _url &rest options)
                     (setq arguments options)
                     process)))
          (misskey-http-read
           "notes/timeline" '(:limit 20) #'ignore
           :errback (lambda (message) (setq failure message))
           :owner owner)
          (funcall
           (plist-get arguments :else)
           (make-plz-error
            :response
            (make-plz-response
             :status 400
             :body "{\"error\":{\"message\":\"Denied\",\"code\":\"NO_PERMISSION\"}}")))
          (should (string-match-p "HTTP 400" failure))
          (should (string-match-p "Denied (NO_PERMISSION)" failure))
          (should-not (string-match-p "unknown" failure))
          (should-not (appkit-app-handles owner)))
      (when (process-live-p process)
        (delete-process process))
      (when (appkit-app-live-p owner)
        (appkit-stop-app owner)))))

(ert-deftest misskey-http-missing-curl-is-certain-setup-failure ()
  (let* ((misskey-instance-url "https://example.social")
         (owner (appkit-start-app 'misskey :id (make-symbol "http-setup")))
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) nil))
                  ((symbol-function 'misskey--auth-token)
                   (lambda (&optional _account) "SECRET"))
                  ((symbol-function 'plz)
                   (lambda (&rest _)
                     (ert-fail "Plz must not start without curl"))))
          (misskey-http-post
           "notes/create" '(:text "hello") #'ignore
           :errback (lambda (message) (setq failure message))
           :owner owner)
          (should (string-match-p "curl executable is unavailable" failure))
          (should-not (string-match-p "unknown" failure))
          (should-not (appkit-app-handles owner)))
      (when (appkit-app-live-p owner)
        (appkit-stop-app owner)))))

(ert-deftest misskey-http-redacts-token-from-post-dispatch-errors ()
  (let* ((misskey-instance-url "https://example.social")
         (owner (appkit-start-app 'misskey :id (make-symbol "http-error")))
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/curl"))
                  ((symbol-function 'misskey--auth-token)
                   (lambda (&optional _account) "SECRET"))
                  ((symbol-function 'plz)
                   (lambda (&rest _)
                     (error "Transport exposed SECRET"))))
          (misskey-http-post
           "notes/create" '(:text "hello") #'ignore
           :errback (lambda (message) (setq failure message))
           :owner owner)
          (should (string-match-p "unknown" failure))
          (should (string-match-p "\\[REDACTED\\]" failure))
          (should-not (string-match-p "SECRET" failure))
          (should-not (appkit-app-handles owner)))
      (when (appkit-app-live-p owner)
        (appkit-stop-app owner)))))

(ert-deftest misskey-http-app-stop-reports-unknown-outcome-once ()
  (let* ((misskey-instance-url "https://example.social")
         (owner (appkit-start-app 'misskey :id (make-symbol "http-cancel")))
         (process (make-pipe-process
                   :name "misskey-http-cancel-test" :noquery t))
         arguments request message
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/curl"))
                  ((symbol-function 'misskey--auth-token)
                   (lambda (&optional _account) "SECRET"))
                  ((symbol-function 'plz)
                   (lambda (_method _url &rest options)
                     (setq arguments options)
                     process)))
          (setq request
                (misskey-http-post
                 "notes/create" '(:text "hello") #'ignore
                 :errback (lambda (failure)
                            (setq calls (1+ calls)
                                  message failure))
                 :owner owner))
          (appkit-stop-app owner)
          (should (= calls 1))
          (should (string-match-p "unknown" message))
          (should-not (process-live-p process))
          (should-not (appkit-app-handles owner))
          (should (misskey-http--request-settled-p request))
          (funcall (plist-get arguments :else)
                   (make-plz-error :message "curl process killed"))
          (should (= calls 1)))
      (when (process-live-p process)
        (delete-process process))
      (when (appkit-app-live-p owner)
        (appkit-stop-app owner)))))

(provide 'misskey-http-test)

;;; misskey-http-test.el ends here
