;;; misskey-http.el --- Misskey JSON transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own authenticated Misskey API POST requests, bounded response decoding, and
;; Appkit lifecycle cancellation.  A dispatched write is never retried or
;; redirected, and every later failure has an unknown remote outcome.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'plz)
(require 'appkit-core)
(require 'misskey-core)

(defconst misskey-http--response-limit (* 1024 1024)
  "Maximum number of bytes accepted in one Misskey API response body.")

(defconst misskey-http--curl-args
  '("--disable" "--silent" "--max-redirs" "0" "--retry" "0")
  "Fixed curl arguments that prohibit redirects and retries.")

(cl-defstruct (misskey-http--request
               (:constructor misskey-http--request-create))
  "One Appkit-owned Misskey request lifecycle."
  callback
  errback
  writep
  process
  handle
  settled-p)

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

(defun misskey-http--decode-response (response writep)
  "Decode Plz RESPONSE using WRITEP outcome semantics.

Return a cons whose car is `success' or `error' and whose cdr is the decoded
payload or readable failure message."
  (unless (plz-response-p response)
    (error "Misskey transport returned an invalid response"))
  (let* ((status (plz-response-status response))
         (body (misskey-http--bounded-body (plz-response-body response)))
         (parsed (misskey-http--parse-body body))
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

(defun misskey-http--deliver (request result)
  "Settle REQUEST by delivering decoded RESULT exactly once."
  (unless (misskey-http--request-settled-p request)
    (setf (misskey-http--request-settled-p request) t)
    (when-let* ((handle (misskey-http--request-handle request)))
      (appkit-retire-handle handle))
    (setf (misskey-http--request-handle request) nil
          (misskey-http--request-process request) nil)
    (pcase (car result)
      ('success
       (funcall (misskey-http--request-callback request) (cdr result)))
      ('error
       (funcall (misskey-http--request-errback request) (cdr result)))
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

(cl-defun misskey-http--request
    (endpoint parameters callback &key errback owner account writep)
  "POST authenticated JSON PARAMETERS to API ENDPOINT.

CALLBACK receives the decoded successful value, including nil for a valid
empty array.  ERRBACK receives a readable error string.  OWNER defaults to
ACCOUNT's Appkit session.  WRITEP non-nil applies unknown-write-outcome
semantics after dispatch."
  (unless (functionp callback)
    (error "Misskey request callback is not callable"))
  (unless (listp parameters)
    (error "Misskey request parameters must be a plist"))
  (unless (memq writep '(nil t))
    (error "Misskey request write flag must be boolean"))
  (let* ((error-fn (or errback (lambda (message) (message "%s" message))))
         (request (misskey-http--request-create
                   :callback callback :errback error-fn :writep writep))
         token
         dispatch-attempted-p)
    (unless (functionp error-fn)
      (error "Misskey request error callback is not callable"))
    (condition-case err
        (let* ((request-url (misskey-http--endpoint-url endpoint account))
               (request-owner (or owner (misskey-app account)))
               (token-value (misskey--auth-token account))
               (data (encode-coding-string
                      (json-serialize parameters) 'utf-8)))
          (unless (executable-find plz-curl-program)
            (error "The curl executable is unavailable: %s"
                   plz-curl-program))
          (setq token token-value
                dispatch-attempted-p t)
          (let ((inhibit-quit t)
                (plz-curl-default-args misskey-http--curl-args))
            (let ((process
                   (plz 'post request-url
                     :headers
                     `(("Authorization" . ,(concat "Bearer " token-value))
                       ("Content-Type" . "application/json")
                       ("Accept" . "application/json"))
                     :body data
                     :body-type 'binary
                     :as 'response
                     :decode t
                     :noquery t
                     :then
                     (lambda (response)
                       (misskey-http--finish
                        request #'misskey-http--decode-response response))
                     :else
                     (lambda (failure)
                       (misskey-http--finish
                        request #'misskey-http--plz-error-result failure)))))
              (unless (processp process)
                (error "Plz did not return a Misskey request process"))
              (unless (misskey-http--request-settled-p request)
                (setf (misskey-http--request-process request) process
                      (misskey-http--request-handle request)
                      (appkit-register-handle
                       request-owner 'function request
                       #'misskey-http--cancel-request)))))
          request)
      ((error quit)
       (let ((quitp (eq (car err) 'quit))
             (inhibit-quit t)
             (failure
              (misskey-http--outcome-message
               (misskey-http--safe-error-message err token)
               (and dispatch-attempted-p writep))))
         (unless (misskey-http--request-settled-p request)
           (let ((process (misskey-http--request-process request)))
             (when (and (processp process) (process-live-p process))
               (delete-process process)))
           (misskey-http--deliver request (cons 'error failure)))
         (when quitp
           (signal 'quit nil))
         nil)))))

(cl-defun misskey-http-read
    (endpoint parameters callback &key errback owner account)
  "Read authenticated API ENDPOINT with JSON PARAMETERS.

CALLBACK receives the decoded value.  ERRBACK receives a readable error
string.  OWNER defaults to ACCOUNT's Appkit session and owns cancellation.
Return the opaque in-flight request, or nil when setup fails."
  (misskey-http--request
   endpoint parameters callback
   :errback errback :owner owner :account account :writep nil))

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
