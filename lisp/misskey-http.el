;;; misskey-http.el --- Misskey JSON and file transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own authenticated Misskey API JSON and Drive upload requests, bounded
;; response decoding, and Appkit lifecycle cancellation.  A dispatched write
;; is never retried or redirected, and every later failure has an unknown
;; remote outcome.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'plz)
(require 'appkit-core)
(require 'misskey-core)

(defconst misskey-http--response-limit (* 1024 1024)
  "Maximum number of bytes accepted in one Misskey API response body.")

(defconst misskey-http--header-limit (* 64 1024)
  "Maximum bytes accepted before curl completes response headers.")

(defconst misskey-http--stderr-limit (* 64 1024)
  "Maximum curl diagnostic bytes retained for one request.")

(defconst misskey-http--curl-safety-args
  '("--disable" "--max-redirs" "0" "--retry" "0")
  "Fixed curl arguments that prohibit redirects and retries.")

(defconst misskey-http--curl-args
  (append misskey-http--curl-safety-args '("--silent"))
  "Silent curl arguments that prohibit redirects and retries.")

(cl-defstruct (misskey-http--request
               (:constructor misskey-http--request-create))
  "One Appkit-owned Misskey request lifecycle."
  callback
  errback
  writep
  token
  process
  stderr-process
  dispatched-p
  handle
  buffers
  settled-p
  progress
  notified-progress)

(defun misskey-http--unknown-write-outcome (message)
  "Mark write failure MESSAGE as having an unknown remote outcome."
  (concat
   "Misskey write outcome is unknown; the request may have succeeded. "
   "Check the server before trying again. " message))

(defun misskey-http--endpoint-url (endpoint &optional account)
  "Return ACCOUNT's trusted API URL for ENDPOINT."
  (unless (and (stringp endpoint)
               (string-match-p "\\`[[:alnum:]][[:alnum:]/_-]*\\'" endpoint)
               (not (string-match-p "//" endpoint)))
    (error "Misskey API endpoint is invalid: %S" endpoint))
  (concat (if account
              (misskey--account-origin account)
            (misskey--instance-origin))
          "/api/" endpoint))

(defun misskey-http--json-data (parameters)
  "Return PARAMETERS encoded as explicit UTF-8 JSON."
  (unless (or (listp parameters) (hash-table-p parameters))
    (error "Misskey request parameters must be a plist or hash table"))
  (encode-coding-string
   (json-serialize parameters
                   :null-object :json-null
                   :false-object :json-false)
   'utf-8))

(defun misskey-http--bounded-body (body)
  "Return response BODY after enforcing the byte limit."
  (unless (stringp body)
    (error "Misskey response body is missing"))
  (when (> (string-bytes body) misskey-http--response-limit)
    (error "Misskey response body exceeds %d bytes"
           misskey-http--response-limit))
  body)

(defun misskey-http--parse-body (body)
  "Parse non-empty JSON BODY.

Return a cons whose car is t and whose cdr is the decoded value.  This keeps a
valid empty array distinct from malformed JSON, which returns nil."
  (unless (string-empty-p (string-trim body))
    (condition-case nil
        (cons t
              (json-parse-string body
                                 :object-type 'alist
                                 :array-type 'list
                                 :null-object nil
                                 :false-object nil))
      (error nil))))

(defun misskey-http--api-error-message (payload)
  "Return the readable Misskey API error from PAYLOAD, or nil."
  (when-let* ((remote-error (and (consp payload)
                                 (alist-get 'error payload)))
              (message (and (consp remote-error)
                            (alist-get 'message remote-error))))
    (let ((code (alist-get 'code remote-error)))
      (if (and code
               (not (string-match-p
                     (format "(%s)\\'"
                             (regexp-quote (format "%s" code)))
                     message)))
          (format "%s (%s)" message code)
        message))))

(defun misskey-http--outcome-message (message writep)
  "Return remote failure MESSAGE with WRITEP outcome semantics."
  (if writep
      (misskey-http--unknown-write-outcome message)
    message))

(defun misskey-http--decode-status-body (status body writep)
  "Decode HTTP STATUS and JSON BODY using WRITEP outcome semantics."
  (let* ((bounded (misskey-http--bounded-body body))
         (parsed (misskey-http--parse-body bounded))
         (payload (and parsed (cdr parsed)))
         (api-error (misskey-http--api-error-message payload)))
    (cond
     ((not (and (integerp status) (<= 200 status 299)))
      (cons
       'error
       (misskey-http--outcome-message
        (format "Misskey request failed%s%s"
                (if (integerp status) (format " (HTTP %d)" status) "")
                (if api-error (format ": %s" api-error) ""))
        writep)))
     ((not parsed)
      (cons 'error
            (misskey-http--outcome-message
             "Misskey returned invalid JSON" writep)))
     (api-error
      (cons 'error
            (misskey-http--outcome-message
             (format "Misskey request failed: %s" api-error) writep)))
     (t (cons 'success payload)))))

(defun misskey-http--decode-response (response writep)
  "Decode Plz RESPONSE using WRITEP outcome semantics.

Return a cons whose car is `success' or `error' and whose cdr is the decoded
payload or readable failure message."
  (unless (plz-response-p response)
    (error "Misskey transport returned an invalid response"))
  (misskey-http--decode-status-body
   (plz-response-status response) (plz-response-body response) writep))

(defun misskey-http--plz-error-result (failure writep)
  "Return decoded result for Plz FAILURE using WRITEP semantics."
  (unless (plz-error-p failure)
    (error "Misskey transport returned an invalid failure"))
  (if-let* ((response (plz-error-response failure)))
      (misskey-http--decode-response response writep)
    (let* ((curl-error (plz-error-curl-error failure))
           (message
            (or (plz-error-message failure)
                (and curl-error
                     (format "curl exited with code %s%s"
                             (car curl-error)
                             (if (cdr curl-error)
                                 (format ": %s" (cdr curl-error))
                               "")))
                "the transport failed without a response")))
      (cons 'error
            (misskey-http--outcome-message
             (format "Misskey request failed: %s" message) writep)))))

(defun misskey-http--cleanup-transport (request)
  "Terminate processes and kill buffers owned by REQUEST."
  (dolist (process
           (list (misskey-http--request-process request)
                 (misskey-http--request-stderr-process request)))
    (when (and (processp process) (process-live-p process))
      (delete-process process)))
  (setf (misskey-http--request-process request) nil
        (misskey-http--request-stderr-process request) nil)
  (dolist (buffer (misskey-http--request-buffers request))
    (when (buffer-live-p buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer))))
  (setf (misskey-http--request-buffers request) nil))

(defun misskey-http--deliver (request result)
  "Settle REQUEST by delivering decoded RESULT exactly once."
  (unless (misskey-http--request-settled-p request)
    (setf (misskey-http--request-settled-p request) t)
    (when-let* ((handle (misskey-http--request-handle request)))
      (appkit-retire-handle handle))
    (misskey-http--cleanup-transport request)
    (setf (misskey-http--request-handle request) nil)
    (pcase (car result)
      ('success
       (setf (misskey-http--request-token request) nil)
       (funcall (misskey-http--request-callback request) (cdr result)))
      ('error
       (let ((message (cdr result))
             (token (misskey-http--request-token request)))
         (setf (misskey-http--request-token request) nil)
         (when (and (stringp token) (not (string-empty-p token)))
           (setq message (string-replace token "[REDACTED]" message)))
         (funcall (misskey-http--request-errback request) message)))
      (_ (error "Misskey response decoder returned an invalid result")))
    t))

(defun misskey-http--finish (request decoder value)
  "Decode VALUE with DECODER and settle REQUEST."
  (unless (misskey-http--request-settled-p request)
    (let ((result
           (condition-case err
               (funcall decoder value
                        (misskey-http--request-writep request))
             (error
              (cons
               'error
               (misskey-http--outcome-message
                (format "Misskey response processing failed: %s"
                        (error-message-string err))
                (misskey-http--request-writep request)))))))
      (misskey-http--deliver request result))))

(defun misskey-http--cancel-request (request)
  "Cancel active Misskey REQUEST and report its outcome exactly once."
  (unless (misskey-http--request-settled-p request)
    (let ((process (misskey-http--request-process request)))
      (when (and (processp process) (process-live-p process))
        (delete-process process)))
    (misskey-http--deliver
     request
     (cons 'error
           (misskey-http--outcome-message
            "The request was canceled before a response was received"
            (misskey-http--request-writep request))))))

(defun misskey-http--safe-error-message (error-data token)
  "Return ERROR-DATA's message with TOKEN redacted."
  (let ((message (error-message-string error-data)))
    (if (and (stringp token) (not (string-empty-p token)))
        (string-replace token "[REDACTED]" message)
      message)))

(defun misskey-http--fail-transport-limit (request process kind limit)
  "Cancel PROCESS and settle REQUEST because KIND exceeded LIMIT."
  (when (process-live-p process)
    (delete-process process))
  (misskey-http--deliver
   request
   (cons 'error
         (misskey-http--outcome-message
          (format "Misskey %s exceeds %d bytes" kind limit)
          (misskey-http--request-writep request)))))

(defun misskey-http--response-filter (request process output)
  "Insert curl OUTPUT from PROCESS while enforcing REQUEST's raw limits."
  (unless (misskey-http--request-settled-p request)
    (let ((buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (if (process-get process 'misskey-http-headers-complete)
            (let* ((used (or (process-get process 'misskey-http-body-bytes) 0))
                   (total (+ used (string-bytes output))))
              (if (> total misskey-http--response-limit)
                  (misskey-http--fail-transport-limit
                   request process "response body" misskey-http--response-limit)
                (with-current-buffer buffer
                  (goto-char (point-max))
                  (insert output))
                (process-put process 'misskey-http-body-bytes total)))
          (let* ((headers
                  (or (process-get process 'misskey-http-headers) ""))
                 (header-bytes (string-bytes headers))
                 (remaining (- misskey-http--header-limit header-bytes))
                 (output-bytes (string-bytes output))
                 (probe-bytes (min output-bytes (+ remaining 4)))
                 (probe
                  (if (= probe-bytes output-bytes)
                      output
                    (substring output 0 probe-bytes)))
                 (combined (concat headers probe)))
            (if (string-match "\r?\n\r?\n" combined)
                (let* ((header-end (match-end 0))
                       (header (substring combined 0 header-end))
                       (header-size (string-bytes header))
                       (status
                        (and (<= header-size misskey-http--header-limit)
                             (string-match
                              "\\`HTTP/[0-9.]+ \\([0-9][0-9][0-9]\\)"
                              header)
                             (string-to-number (match-string 1 header))))
                       (body-prefix (substring combined header-end)))
                  (if (not status)
                      (misskey-http--fail-transport-limit
                       request process "response headers"
                       misskey-http--header-limit)
                    (process-put process 'misskey-http-status status)
                    (process-put process 'misskey-http-headers-complete t)
                    (process-put process 'misskey-http-headers nil)
                    (misskey-http--response-filter
                     request process body-prefix)
                    (when (and (< probe-bytes output-bytes)
                               (not (misskey-http--request-settled-p request)))
                      (misskey-http--response-filter
                       request process
                       (substring output probe-bytes)))))
              (if (> output-bytes remaining)
                  (misskey-http--fail-transport-limit
                   request process "response headers"
                   misskey-http--header-limit)
                (process-put process 'misskey-http-headers combined)))))))))

(defun misskey-http--curl-progress-header-p (line)
  "Return non-nil when LINE is a curl progress-meter header."
  (or (string-match-p "\\`[ \t]*%[ \t]+Total\\>" line)
      (string-match-p "\\`[ \t]*Dload[ \t]+Upload\\>" line)))

(defun misskey-http--curl-upload-ratio (line)
  "Return LINE's curl % Xferd as a 0-1 float, or nil."
  (when (string-match
         (concat "\\`[ \t]*[0-9]+[ \t]+[^ \t]+[ \t]+[0-9]+[ \t]+"
                 "[^ \t]+[ \t]+\\([0-9]+\\)\\>")
         line)
    (/ (float (string-to-number (match-string 1 line))) 100.0)))

(defun misskey-http--split-curl-stderr (pending output)
  "Split PENDING plus OUTPUT into progress, diagnostics, and a remainder.

Return a plist with `:progress' as the latest 0-1 upload ratio or nil,
`:diagnostics' as complete non-progress lines, and `:pending' as the
unterminated suffix."
  (let* ((text (concat (or pending "") output))
         (terminated (string-match-p "[\r\n]\\'" text))
         (parts (split-string text "[\r\n]" t))
         (pending (if (or terminated (null parts))
                      ""
                    (car (last parts))))
         (lines (if (or terminated (null parts))
                    parts
                  (butlast parts)))
         diagnostics
         progress)
    (dolist (line lines)
      (if-let* ((ratio (misskey-http--curl-upload-ratio line)))
          (setq progress ratio)
        (unless (misskey-http--curl-progress-header-p line)
          (push line diagnostics))))
    (list :progress progress
          :diagnostics (nreverse diagnostics)
          :pending pending)))

(defun misskey-http--notify-upload-progress (request progress)
  "Deliver PROGRESS as a 0-1 upload event for REQUEST when it changed."
  (when (and (functionp (misskey-http--request-progress request))
             (not (misskey-http--request-settled-p request))
             (numberp progress)
             (<= 0 progress)
             (<= progress 1)
             (not (eql progress
                       (misskey-http--request-notified-progress request))))
    (setf (misskey-http--request-notified-progress request) progress)
    (funcall (misskey-http--request-progress request)
             (list :progress progress))))

(defun misskey-http--stderr-keep (process output)
  "Retain at most `misskey-http--stderr-limit' bytes of PROCESS OUTPUT."
  (when-let* ((buffer (process-buffer process)))
    (when (buffer-live-p buffer)
      (let* ((used (buffer-size buffer))
             (remaining (max 0 (- misskey-http--stderr-limit used)))
             (piece (if (> (string-bytes output) remaining)
                        (substring output 0 remaining)
                      output)))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert piece))
        (when (< (string-bytes piece) (string-bytes output))
          (process-put process 'misskey-http-truncated t))))))

(defun misskey-http--flush-stderr-pending (process)
  "Flush PROCESS's unterminated stderr suffix into the diagnostic buffer."
  (when-let* ((pending (process-get process 'misskey-http-stderr-pending)))
    (process-put process 'misskey-http-stderr-pending nil)
    (unless (or (misskey-http--curl-upload-ratio pending)
                (misskey-http--curl-progress-header-p pending))
      (misskey-http--stderr-keep process pending))))

(defun misskey-http--stderr-filter (process output)
  "Retain bounded diagnostics from PROCESS OUTPUT and report upload progress."
  (when-let* ((request (process-get process 'misskey-http-request))
              ((functionp (misskey-http--request-progress request))))
    (let* ((parsed (misskey-http--split-curl-stderr
                    (process-get process 'misskey-http-stderr-pending)
                    output))
           (diagnostics (plist-get parsed :diagnostics)))
      (process-put process 'misskey-http-stderr-pending
                   (plist-get parsed :pending))
      (when-let* ((ratio (plist-get parsed :progress)))
        (misskey-http--notify-upload-progress request ratio))
      (setq output (and diagnostics
                        (concat (mapconcat #'identity diagnostics "\n")
                                "\n")))))
  (when (and (stringp output) (not (string-empty-p output)))
    (misskey-http--stderr-keep process output)))

(defun misskey-http--curl-error-detail (process)
  "Return bounded, redacted-safe diagnostic detail for failed PROCESS."
  (let* ((stderr-process (process-get process 'misskey-http-stderr-process))
         (stderr (and (processp stderr-process)
                      (string-trim
                       (misskey-http--buffer-contents
                        (process-buffer stderr-process))))))
    (if (and stderr (not (string-empty-p stderr)))
        (concat stderr
                (when (process-get stderr-process 'misskey-http-truncated)
                  " [diagnostics truncated]"))
      (format "curl exited with code %d" (process-exit-status process)))))

(defun misskey-http--curl-sentinel (process _event)
  "Settle the JSON or multipart request owned by curl PROCESS."
  (when (memq (process-status process) '(exit signal))
    (when-let* ((request (process-get process 'misskey-http-request)))
      (when-let* ((stderr (process-get process 'misskey-http-stderr-process)))
        (misskey-http--flush-stderr-pending stderr))
      (unless (misskey-http--request-settled-p request)
        (if (and (eq (process-status process) 'exit)
                 (zerop (process-exit-status process)))
            (let ((status (process-get process 'misskey-http-status)))
              (if (integerp status)
                  (misskey-http--finish
                   request #'misskey-http--decode-status-value
                   (cons status
                         (decode-coding-string
                          (misskey-http--buffer-contents
                           (process-buffer process))
                          'utf-8)))
                (misskey-http--deliver
                 request
                 (cons 'error
                       (misskey-http--outcome-message
                        "curl returned no valid HTTP status"
                        (misskey-http--request-writep request))))))
          (misskey-http--deliver
           request
           (cons
            'error
            (misskey-http--outcome-message
             (format "Misskey request failed: %s"
                     (misskey-http--curl-error-detail process))
             (misskey-http--request-writep request)))))))))

(defun misskey-http--decode-status-value (value writep)
  "Decode (STATUS . BODY) VALUE using WRITEP outcome semantics."
  (misskey-http--decode-status-body (car value) (cdr value) writep))

(defun misskey-http--start-curl
    (request program command config data response-name stderr-name)
  "Start REQUEST with PROGRAM and COMMAND, sending CONFIG and DATA.

RESPONSE-NAME and STDERR-NAME name the bounded temporary buffers."
  (let* ((response-buffer (generate-new-buffer response-name))
         (stderr-buffer (generate-new-buffer stderr-name))
         stderr-process process)
    (dolist (buffer (list response-buffer stderr-buffer))
      (with-current-buffer buffer
        (set-buffer-multibyte nil)))
    (condition-case err
        (progn
          (setq stderr-process
                (make-pipe-process
                 :name "misskey-curl-stderr"
                 :buffer stderr-buffer
                 :coding 'binary
                 :noquery t
                 :filter #'misskey-http--stderr-filter))
          (setf (misskey-http--request-buffers request)
                (list response-buffer stderr-buffer)
                (misskey-http--request-stderr-process request) stderr-process)
          (setq process
                (make-process
                 :name "misskey-curl"
                 :buffer response-buffer
                 :stderr stderr-process
                 :command (cons program command)
                 :coding 'binary
                 :connection-type 'pipe
                 :noquery t
                 :filter (lambda (proc output)
                           (misskey-http--response-filter request proc output))
                 :sentinel #'misskey-http--curl-sentinel))
          (unless (processp process)
            (error "Curl did not return a Misskey request process"))
          (setf (misskey-http--request-process request) process
                (misskey-http--request-dispatched-p request) t)
          (process-put process 'misskey-http-request request)
          (process-put process 'misskey-http-stderr-process stderr-process)
          (process-put stderr-process 'misskey-http-request request)
          (process-send-string process config)
          (process-send-string process data)
          (process-send-eof process)
          process)
      ((error quit)
       (misskey-http--cleanup-transport request)
       (signal (car err) (cdr err))))))

(cl-defun misskey-http--start-request
    (endpoint parameters callback &key errback owner account writep)
  "POST authenticated JSON PARAMETERS to API ENDPOINT."
  (unless (functionp callback)
    (error "Misskey request callback is not callable"))
  (unless (memq writep '(nil t))
    (error "Misskey request write flag must be boolean"))
  (let*
      ((error-fn
        (or errback (lambda (message) (message "%s" message))))
       (request
         (misskey-http--request-create :callback callback :errback
                                       error-fn :writep writep))
       token)
    (unless (functionp error-fn)
      (error "Misskey request error callback is not callable"))
    (condition-case err
        (let*
            ((program
              (or (executable-find plz-curl-program)
                  (error "The curl executable is unavailable: %s"
                         plz-curl-program)))
             (request-url
              (misskey-http--endpoint-url endpoint account))
             (request-owner (or owner (misskey-app account)))
             (token-value (misskey--auth-token account))
             (data (misskey-http--json-data parameters))
             (command
              (append misskey-http--curl-args
                      (list "--show-error"
                            "--suppress-connect-headers" "--url"
                            request-url "--request" "POST" "--header"
                            "Content-Type: application/json"
                            "--header" "Accept: application/json"
                            "--header" "Expect:" "--dump-header" "-"
                            "--config" "-")))
             (config
              (concat
               (misskey-http--curl-authorization-config token-value)
               "data-binary = \"@-\"\n")))
          (setq token token-value)
          (setf (misskey-http--request-token request) token-value)
          (let ((inhibit-quit t))
            (misskey-http--start-curl request program command config
                                      data " *misskey-json-response*"
                                      " *misskey-json-stderr*")
            (unless (misskey-http--request-settled-p request)
              (setf (misskey-http--request-handle request)
                    (appkit-register-handle request-owner 'function
                                            request
                                            #'misskey-http--cancel-request))))
          request)
      ((error quit)
       (let*
           ((quitp (eq (car err) 'quit)) (inhibit-quit t)
            (dispatched-p (misskey-http--request-dispatched-p request))
            (failure
             (misskey-http--outcome-message
              (misskey-http--safe-error-message err token)
              (and dispatched-p writep))))
         (unless (misskey-http--request-settled-p request)
           (let ((process (misskey-http--request-process request)))
             (when (and (processp process) (process-live-p process))
               (delete-process process)))
           (misskey-http--deliver request (cons 'error failure)))
         (when quitp (signal 'quit nil)) nil)))))

(cl-defun misskey-http--request
    (endpoint parameters callback &key errback owner account writep)
  "Request ENDPOINT, serializing host-owned results through its Effect."
  (if (not (misskey-read-token-p owner))
      (misskey-http--start-request endpoint parameters callback
                                   :errback errback :owner owner
                                   :account account :writep writep)
    (let*
        ((surface (misskey-read-token-surface owner))
         (effect
          (appkit-effect-create :key (misskey-read-token-key owner)
                                :input (copy-tree parameters) :start
                                (lambda
                                  (_context input _observe resolve
                                            reject)
                                  (let
                                      ((request
                                         (misskey-http--start-request
                                          endpoint input resolve
                                          :errback reject :owner
                                          surface :account account
                                          :writep writep)))
                                    (when request
                                      (appkit-cancellation-create
                                       :kind 'transport :cancel
                                       (lambda ()
                                         (misskey-http-cancel request))))))
                                :success
                                (lambda (_input payload)
                                  (list :read-delivered owner callback
                                        payload))
                                :failure
                                (lambda (_input failure)
                                  (list :read-delivered owner
                                        (or errback #'ignore) failure)))))
      (misskey-dispatch surface (list :read-effect effect)) nil)))

(defun misskey-http--curl-form-file (file)
  "Return curl's quoted multipart file argument for readable FILE."
  (let ((path (expand-file-name file)))
    (unless (and (file-regular-p path) (file-readable-p path))
      (user-error "Attachment is not a readable regular file: %s" path))
    (when (string-match-p "[\0\r\n]" path)
      (user-error "Attachment path contains unsupported control characters"))
    (setq path (string-replace "\\" "\\\\" path)
          path (string-replace "\"" "\\\"" path))
    (format "file=@\"%s\"" path)))

(defun misskey-http--curl-authorization-config (token)
  "Return curl config carrying a strictly validated bearer TOKEN."
  (unless (misskey--valid-token-p token)
    (error "Misskey API token is invalid for an HTTP header"))
  (format "header = \"Authorization: Bearer %s\"\n" token))

(defun misskey-http--upload-command (url file)
  "Return bounded curl arguments uploading FILE to URL."
  (append
   misskey-http--curl-safety-args
   (list "--show-error"
         "--progress-meter"
         "--suppress-connect-headers"
         "--url" url
         "--request" "POST"
         "--header" "Accept: application/json"
         "--header" "Expect:"
         "--form" (misskey-http--curl-form-file file)
         "--dump-header" "-"
         "--config" "-")))

(defun misskey-http--buffer-contents (buffer)
  "Return BUFFER contents without text properties, or an empty string."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (buffer-substring-no-properties (point-min) (point-max)))
    ""))

(cl-defun misskey-http-upload-file
    (file callback &key errback owner account progress)
  "Upload FILE and pass its decoded response to CALLBACK.

ERRBACK receives readable failures.  OWNER defaults to ACCOUNT's Appkit
session and owns cancellation.  PROGRESS, when callable, receives a plist
with `:progress' as a 0-1 float measured from curl's upload meter.
Return the opaque request, or nil when setup fails."
  (unless (functionp callback)
    (error "Misskey upload callback is not callable"))
  (when (and progress (not (functionp progress)))
    (error "Misskey upload progress callback is not callable"))
  (let* ((error-fn (or errback (lambda (message) (message "%s" message))))
         (request (misskey-http--request-create
                   :callback callback :errback error-fn :writep t
                   :progress progress))
         (path (expand-file-name file))
         token)
    (unless (functionp error-fn)
      (error "Misskey upload error callback is not callable"))
    (condition-case err
        (let* ((program
                (or (executable-find plz-curl-program)
                    (error "The curl executable is unavailable: %s"
                           plz-curl-program)))
               (request-url
                (misskey-http--endpoint-url "drive/files/create" account))
               (request-owner (or owner (misskey-app account)))
               (token-value (misskey--auth-token account))
               (command (misskey-http--upload-command request-url path))
               (config (misskey-http--curl-authorization-config token-value)))
          (setq token token-value)
          (setf (misskey-http--request-token request) token-value)
          (let ((inhibit-quit t))
            (misskey-http--start-curl
             request program command config ""
             " *misskey-upload-response*" " *misskey-upload-stderr*")
            (unless (misskey-http--request-settled-p request)
              (setf (misskey-http--request-handle request)
                    (appkit-register-handle
                     request-owner 'function request
                     #'misskey-http--cancel-request))))
          request)
      ((error quit)
       (let* ((quitp (eq (car err) 'quit))
              (inhibit-quit t)
              (dispatched-p
               (misskey-http--request-dispatched-p request))
              (failure
               (misskey-http--outcome-message
                (misskey-http--safe-error-message err token)
                dispatched-p)))
         (when-let* ((process (misskey-http--request-process request)))
           (when (process-live-p process)
             (delete-process process)))
         (unless (misskey-http--request-settled-p request)
           (misskey-http--deliver request (cons 'error failure)))
         (when quitp
           (signal 'quit nil))
         nil)))))

(defun misskey-http--public-read-sync (endpoint parameters &optional account)
  "Synchronously send PARAMETERS to unauthenticated API ENDPOINT.

ACCOUNT selects the server origin.  Return the decoded response."
  (let* ((program
          (or (executable-find plz-curl-program)
              (error "The curl executable is unavailable: %s"
                     plz-curl-program)))
         (request-url (misskey-http--endpoint-url endpoint account))
         (data (misskey-http--json-data parameters))
         result
         (request
           (misskey-http--request-create
            :callback (lambda (payload) (setq result (cons 'success payload)))
            :errback (lambda (message) (setq result (cons 'error message)))
            :writep nil))
         (command
          (append
           misskey-http--curl-args
           (list "--show-error"
                 "--suppress-connect-headers"
                 "--url" request-url
                 "--request" "POST"
                 "--header" "Content-Type: application/json"
                 "--header" "Accept: application/json"
                 "--header" "Expect:"
                 "--dump-header" "-"
                 "--config" "-")))
         (process
          (misskey-http--start-curl
           request program command "data-binary = \"@-\"\n" data
           " *misskey-public-response*" " *misskey-public-stderr*")))
    (unwind-protect
        (progn
          (while (and (not result) (process-live-p process))
            (accept-process-output process 0.1))
          (unless result
            (accept-process-output process 0.01))
          (pcase result
            (`(success . ,payload) payload)
            (`(error . ,message) (error "%s" message))
            (_ (error "Misskey transport ended without a result"))))
      (unless (misskey-http--request-settled-p request)
        (misskey-http--cancel-request request)))))

(cl-defun misskey-http-read
    (endpoint parameters callback &key errback owner account)
  "Read authenticated API ENDPOINT with JSON PARAMETERS.

CALLBACK receives the decoded value.  ERRBACK receives a readable error
string.  OWNER defaults to ACCOUNT's Appkit session and owns cancellation.
Return the opaque in-flight request, or nil when setup fails."
  (misskey-http--request
   endpoint parameters callback
   :errback errback :owner owner :account account :writep nil))

(defun misskey-http-cancel (request)
  "Cancel opaque in-flight Misskey REQUEST exactly once."
  (unless (misskey-http--request-p request)
    (error "Invalid Misskey request"))
  (misskey-http--cancel-request request))

(cl-defun misskey-http-post
    (endpoint parameters callback &key errback owner account)
  "Create remote state through API ENDPOINT with JSON PARAMETERS.

CALLBACK receives the decoded successful object.  ERRBACK receives a readable
error string.  OWNER defaults to ACCOUNT's Appkit session and owns request
cancellation.  Every post-dispatch failure reports an unknown remote outcome.
Return the opaque in-flight request, or nil when setup fails."
  (misskey-http--request
   endpoint parameters callback
   :errback errback :owner owner :account account :writep t))

(provide 'misskey-http)

;;; misskey-http.el ends here
