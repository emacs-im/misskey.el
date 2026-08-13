;;; misskey-http-test.el --- Tests for Misskey transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-http)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

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

(ert-deftest misskey-http-serializes-explicit-json-sentinels-and-vectors ()
  (should
   (equal
    (misskey-http--json-data
     '(:false :json-false :null :json-null :ids ["a" "b"]))
    "{\"false\":false,\"null\":null,\"ids\":[\"a\",\"b\"]}")))

(ert-deftest misskey-http-rejects-hostile-token-before-curl-config ()
  (let* ((misskey-instance-url "https://example.social")
         (owner (appkit-start-app 'misskey :id (make-symbol "hostile-token")))
         (started-p nil)
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/curl"))
                  ((symbol-function 'misskey--auth-token)
                   (lambda (&optional _account)
                     "safe\"\nheader = \"X-Evil: yes"))
                  ((symbol-function 'make-process)
                   (lambda (&rest _)
                     (setq started-p t)
                     (ert-fail "Hostile token reached curl"))))
          (misskey-http-post
           "notes/create" '(:text "hello") #'ignore
           :errback (lambda (message) (setq failure message))
           :owner owner)
          (should-not started-p)
          (should (string-match-p "invalid" failure))
          (should-not (string-match-p "X-Evil" failure)))
      (when (appkit-app-live-p owner)
        (appkit-stop-app owner)))))

(ert-deftest misskey-http-redacts-secret-from-delivered-errors ()
  (let (failure)
    (misskey-http--deliver
     (misskey-http--request-create
      :callback #'ert-fail
      :errback (lambda (message) (setq failure message))
      :writep t
      :token "SECRET")
     '(error . "Server reflected SECRET"))
    (should (string-match-p "\\[REDACTED\\]" failure))
    (should-not (string-match-p "SECRET" failure))))

(ert-deftest misskey-http-post-dispatch-errors-are-unknown-and-redacted ()
  (let* ((misskey-instance-url "https://example.social")
         (owner (appkit-start-app 'misskey :id (make-symbol "post-error")))
         failure)
    (unwind-protect
        (cl-letf (((symbol-function 'executable-find)
                   (lambda (_program) "/usr/bin/curl"))
                  ((symbol-function 'misskey--auth-token)
                   (lambda (&optional _account) "SECRET"))
                  ((symbol-function 'misskey-http--start-curl)
                   (lambda (request &rest _)
                     (setf (misskey-http--request-dispatched-p request) t)
                     (error "send failed with SECRET"))))
          (misskey-http-post
           "notes/create" '(:text "hello") #'ignore
           :errback (lambda (message) (setq failure message))
           :owner owner)
          (should (string-match-p "unknown" failure))
          (should (string-match-p "\\[REDACTED\\]" failure))
          (should-not (string-match-p "SECRET" failure)))
      (when (appkit-app-live-p owner)
        (appkit-stop-app owner)))))

(ert-deftest misskey-http-auth-config-keeps-token-off-command-line ()
  (let ((file (make-temp-file "misskey-http-command-")))
    (unwind-protect
        (let ((command
               (misskey-http--upload-command
                "https://example.social/api/drive/files/create" file)))
          (should (member "--progress-meter" command))
          (should-not (member "--silent" command))
          (should-not (string-match-p "SECRET" (prin1-to-string command)))
          (should (equal (misskey-http--curl-authorization-config "SECRET")
                         "header = \"Authorization: Bearer SECRET\"\n"))
          (dolist (hostile
                   '("" "x\nheader=x" "x\"y" "x\\y" "x y" "--config"))
            (should-error
             (misskey-http--curl-authorization-config hostile))))
      (delete-file file))))

(ert-deftest misskey-http-parses-curl-upload-meter-and-keeps-errors ()
  (should (eql (misskey-http--curl-upload-ratio
                "  12  4096    0     0   12   512      0   512")
               0.12))
  (should-not (misskey-http--curl-upload-ratio
               "  % Total    % Received % Xferd"))
  (let ((parsed
         (misskey-http--split-curl-stderr
          nil
          (concat
           "  % Total    % Received % Xferd  Average Speed  Time    Time    Time   Current\n"
           "                                 Dload  Upload  Total   Spent   Left   Speed\n"
           "\r  0      0   0      0   0      0      0      0                              0"
           "\r100  2.00M   0      0 100  2.00M      0  1.10M   00:01   00:01          1.92M"
           "\r 58  4.00M  16 331.9k 100  2.00M 118.4k 730.4k   00:17   00:02   00:15  1.13M"
           "\r100  4.00"))))
    (should (eql (plist-get parsed :progress) 1.0))
    (should (equal (plist-get parsed :pending) "100  4.00"))
    (should-not (plist-get parsed :diagnostics)))
  (let ((parsed
         (misskey-http--split-curl-stderr
          nil "curl: (56) Recv failure\n")))
    (should-not (plist-get parsed :progress))
    (should (equal (plist-get parsed :diagnostics)
                   '("curl: (56) Recv failure")))))

(ert-deftest misskey-http-stderr-progress-does-not-fill-diagnostics ()
  (let* ((buffer (generate-new-buffer " *misskey-progress-stderr*"))
         events
         (request
          (misskey-http--request-create
           :callback #'ignore :errback #'ignore :writep t
           :progress (lambda (event) (push (plist-get event :progress)
                                           events))))
         (process
          (make-pipe-process :name "misskey-progress-stderr"
                             :buffer buffer :noquery t)))
    (unwind-protect
        (progn
          (process-put process 'misskey-http-request request)
          (misskey-http--stderr-filter
           process
           (concat
            "  % Total    % Received % Xferd\n"
            "\r 25  1024    0     0   25   256\n"
            "curl: (52) Empty reply from server\n"))
          (should (equal (nreverse events) '(0.25)))
          (should (string-match-p "Empty reply"
                                  (misskey-http--buffer-contents buffer)))
          (should-not (string-match-p "Xferd"
                                      (misskey-http--buffer-contents buffer))))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest misskey-http-upload-rejects-noncallable-progress ()
  (let ((file (make-temp-file "misskey-http-progress-")))
    (unwind-protect
        (should-error
         (misskey-http-upload-file file #'ignore :progress 'nope))
      (delete-file file))))

(ert-deftest misskey-http-stream-filter-caps-body-at-next-byte ()
  (let* ((misskey-http--response-limit 8)
         (buffer (generate-new-buffer " *misskey-cap-test*"))
         failure
         (request
          (misskey-http--request-create
           :callback #'ert-fail
           :errback (lambda (message) (setq failure message))
           :writep t
           :buffers (list buffer)))
         (process
          (make-process :name "misskey-cap-test" :command '("cat")
                        :buffer buffer :noquery t)))
    (setf (misskey-http--request-process request) process)
    (misskey-http--response-filter
     request process "HTTP/1.1 200 OK\r\nX: y\r\n\r\n12345678")
    (should (= (buffer-size buffer) 8))
    (misskey-http--response-filter request process "9")
    (should (misskey-http--request-settled-p request))
    (should-not (process-live-p process))
    (should-not (buffer-live-p buffer))
    (should (string-match-p "unknown" failure))))

(ert-deftest misskey-http-stream-filter-bounds-hostile-single-chunks ()
  (dolist (output
           (list
            (make-string 100000 ?x)
            (concat "HTTP/1.1 200 OK\r\n\r\n" (make-string 100000 ?x))))
    (let* ((misskey-http--header-limit 32)
           (misskey-http--response-limit 8)
           (buffer (generate-new-buffer " *misskey-hostile-chunk*"))
           failure
           (request
            (misskey-http--request-create
             :callback #'ert-fail
             :errback (lambda (message) (setq failure message))
             :writep nil
             :buffers (list buffer)))
           (process
            (make-process :name "misskey-hostile-chunk" :command '("cat")
                          :buffer buffer :coding 'binary :noquery t)))
      (with-current-buffer buffer
        (set-buffer-multibyte nil))
      (setf (misskey-http--request-process request) process)
      (misskey-http--response-filter request process output)
      (should failure)
      (should-not (process-live-p process))
      (should-not (buffer-live-p buffer)))))

(ert-deftest misskey-http-stream-filter-does-not-count-body-as-headers ()
  (let* ((misskey-http--header-limit 32)
         (misskey-http--response-limit 100)
         (buffer (generate-new-buffer " *misskey-header-test*"))
         (request
          (misskey-http--request-create
           :callback #'ignore :errback #'ert-fail :writep nil))
         (process
          (make-process :name "misskey-header-test" :command '("cat")
                        :buffer buffer :noquery t)))
    (unwind-protect
        (progn
          (misskey-http--response-filter
           request process
           (concat "HTTP/1.1 200 OK\r\n\r\n" (make-string 64 ?x)))
          (should (= (buffer-size buffer) 64))
          (should (= (process-get process 'misskey-http-status) 200)))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest misskey-http-stderr-filter-is-independently-bounded ()
  (let* ((misskey-http--stderr-limit 4)
         (buffer (generate-new-buffer " *misskey-stderr-test*"))
         (process
          (make-pipe-process :name "misskey-stderr-test"
                             :buffer buffer :noquery t)))
    (unwind-protect
        (progn
          (misskey-http--stderr-filter process "123456")
          (should (= (buffer-size buffer) 4))
          (should (process-get process 'misskey-http-truncated)))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest misskey-http-partial-start-cleans-stderr-and-buffers ()
  (let ((request
         (misskey-http--request-create
          :callback #'ignore :errback #'ignore :writep nil))
        (real-make-process (symbol-function 'make-process))
        created-stderr)
    (cl-letf (((symbol-function 'make-pipe-process)
               (lambda (&rest arguments)
                 (setq created-stderr
                       (apply real-make-process
                              :command '("cat") arguments))))
              ((symbol-function 'make-process)
               (lambda (&rest _)
                 (error "main process failed"))))
      (should-error
       (misskey-http--start-curl
        request "curl" nil "" "" " *response*" " *stderr*"))
      (should-not (process-live-p created-stderr))
      (should-not (misskey-http--request-buffers request))
      (should-not (misskey-http--request-stderr-process request)))))

(provide 'misskey-http-test)

;;; misskey-http-test.el ends here
