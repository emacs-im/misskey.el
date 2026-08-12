;;; misskey-http.el --- Misskey JSON transport -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Own authenticated Misskey API POST requests, bounded response decoding, and
;; Appkit lifecycle cancellation.  A dispatched write is never retried.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'url-http)
(require 'url-parse)
(require 'appkit-core)
(require 'misskey-core)

(defconst misskey-http--response-limit (* 1024 1024)
  "Maximum number of bytes accepted in one Misskey API response body.")

(defvar-local misskey-http--request-handle nil
  "Appkit lifecycle handle for the current Misskey retrieval buffer.")

(defvar misskey-http--dispatch-buffer nil
  "Dynamically bound retrieval buffer allocated before write dispatch.")

(defvar misskey-http--dispatch-attempted-p nil
  "Dynamically bound non-nil once a write may have been dispatched.")

(defun misskey-http-unknown-write-outcome (message)
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

(defun misskey-http--response-body ()
  "Return the current bounded HTTP response body."
  (let* ((header-end (and (boundp 'url-http-end-of-headers)
                          url-http-end-of-headers))
         (start (cond
                 ((markerp header-end) (marker-position header-end))
                 ((integerp header-end) header-end)
                 (t (point-min))))
         (end (point-max))
         (bytes (- (position-bytes end) (position-bytes start))))
    (when (> bytes misskey-http--response-limit)
      (error "Misskey response body exceeds %d bytes"
             misskey-http--response-limit))
    (buffer-substring-no-properties start end)))

(defun misskey-http--response-status (request-status)
  "Return HTTP status from REQUEST-STATUS in the current buffer."
  (or (and (boundp 'url-http-response-status)
           (integerp url-http-response-status)
           url-http-response-status)
      (let ((error-data (plist-get request-status :error)))
        (and (listp error-data)
             (eq (nth 1 error-data) 'http)
             (integerp (nth 2 error-data))
             (nth 2 error-data)))))

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
  (when-let* ((error (and (listp payload) (alist-get 'error payload)))
              (message (and (listp error) (alist-get 'message error))))
    (let ((code (alist-get 'code error)))
      (if (and code
               (not (string-match-p
                     (format "(%s)\\'" (regexp-quote (format "%s" code)))
                     message)))
          (format "%s (%s)" message code)
        message))))

(defun misskey-http--outcome-message (message writep)
  "Return remote failure MESSAGE with WRITEP outcome semantics."
  (if writep
      (misskey-http-unknown-write-outcome message)
    message))

(defun misskey-http--decode-response (request-status writep)
  "Decode the current response for REQUEST-STATUS.

WRITEP non-nil applies unknown-write-outcome semantics.  Return a cons whose
car is `success' or `error' and whose cdr is the payload or message."
  (let* ((status (misskey-http--response-status request-status))
         (body (misskey-http--response-body))
         (parsed (misskey-http--parse-body body))
         (payload (and parsed (cdr parsed)))
         (api-error (misskey-http--api-error-message payload)))
    (cond
     ((or (not status) (< status 200) (>= status 300))
      (cons
       'error
       (misskey-http--outcome-message
        (cond
         (status
          (format "Misskey request failed (HTTP %d)%s"
                  status
                  (if api-error (format ": %s" api-error) "")))
         ((plist-get request-status :error)
          (format "Misskey request failed: %s"
                  (error-message-string
                   (plist-get request-status :error))))
         (t "Misskey request failed without an HTTP response"))
        writep)))
     ((not parsed)
      (cons
       'error
       (misskey-http--outcome-message
        "Misskey returned invalid JSON" writep)))
     (api-error
      (cons
       'error
       (misskey-http--outcome-message
        (format "Misskey request failed: %s" api-error) writep)))
     (t (cons 'success payload)))))

(defun misskey-http--retire-request-handle ()
  "Retire the current retrieval buffer's lifecycle handle."
  (when (appkit-handle-p misskey-http--request-handle)
    (appkit-retire-handle misskey-http--request-handle))
  (setq-local misskey-http--request-handle nil))

(defun misskey-http--discard-buffer (buffer)
  "Stop and kill retrieval BUFFER without delivering a callback."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local misskey-http--request-handle nil)
      (when-let* ((process (get-buffer-process buffer))
                  ((eq (process-buffer process) buffer)))
        (set-process-sentinel process nil)
        (when (process-live-p process)
          (delete-process process)))
      (kill-buffer buffer))))

(defun misskey-http--cancel-request (request)
  "Cancel the Misskey retrieval described by REQUEST."
  (let ((buffer (plist-get request :buffer))
        (errback (plist-get request :errback))
        (writep (plist-get request :writep)))
    (misskey-http--discard-buffer buffer)
    (when (functionp errback)
      (funcall
       errback
       (misskey-http--outcome-message
        "The request was canceled before a response was received"
        writep)))))

(defun misskey-http-cancel-request (request-buffer)
  "Cancel active Misskey retrieval REQUEST-BUFFER exactly once."
  (when (buffer-live-p request-buffer)
    (with-current-buffer request-buffer
      (if (and (appkit-handle-p misskey-http--request-handle)
               (appkit-handle-alive-p misskey-http--request-handle))
          (appkit-cancel-handle misskey-http--request-handle)
        (kill-buffer request-buffer)))
    t))

(defun misskey-http--post-once (request-url callback callback-args)
  "Start one non-retrying POST to REQUEST-URL.

CALLBACK and CALLBACK-ARGS follow `url-retrieve'."
  (url-do-setup)
  (let* ((url (url-generic-parse-url
               (url-encode-url (copy-sequence request-url))))
         (proxy (and (url-host url)
                     (url-find-proxy-for-url url (url-host url))))
         (proxy-url (and proxy (url-generic-parse-url proxy))))
    (when (and proxy-url (not (equal (url-type proxy-url) "http")))
      (error "Misskey request proxy scheme is unsupported"))
    (setf (url-silent url) t
          (url-asynchronous url) url-asynchronous
          (url-use-cookies url) nil)
    (setq misskey-http--dispatch-buffer
          (generate-new-buffer " *misskey write*")
          misskey-http--dispatch-attempted-p t)
    (let* ((url-current-object url)
           (url-using-proxy proxy-url)
           (started
            (url-http url callback (cons nil callback-args)
                      misskey-http--dispatch-buffer
                      (and (null proxy-url) 'tls))))
      (unless (eq started misskey-http--dispatch-buffer)
        (misskey-http--discard-buffer started)
        (error "URL transport did not retain the Misskey write buffer"))
      started)))

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
  (let ((error-fn (or errback (lambda (message) (message "%s" message))))
        (misskey-http--dispatch-buffer nil)
        (misskey-http--dispatch-attempted-p nil)
        token request-buffer handle settled-p)
    (unless (functionp error-fn)
      (error "Misskey request error callback is not callable"))
    (condition-case err
        (let* ((request-url (misskey-http--endpoint-url endpoint account))
               (request-owner (or owner (misskey-app account)))
               (token-value (misskey--auth-token account))
               (data (encode-coding-string
                      (json-serialize parameters) 'utf-8))
               (url-max-redirections 0)
               (url-http-attempt-keepalives nil)
               (url-request-method "POST")
               (url-request-data data)
               (url-request-extra-headers
                `(("Authorization" . ,(concat "Bearer " token-value))
                  ("Content-Type" . "application/json")
                  ("Accept" . "application/json")
                  ("Connection" . "close"))))
          (setq token token-value)
          (let ((inhibit-quit t))
            (setq request-buffer
                  (misskey-http--post-once
                   request-url
                   (lambda (request-status)
                     (let ((buffer (current-buffer))
                           (deliver-p
                            (or (null handle)
                                (appkit-handle-alive-p handle))))
                       (unwind-protect
                           (progn
                             (when (appkit-handle-p handle)
                               (appkit-retire-handle handle)
                               (setq-local misskey-http--request-handle nil))
                             (when deliver-p
                               (let ((result
                                      (condition-case response-error
                                          (misskey-http--decode-response
                                           request-status writep)
                                        (error
                                         (cons
                                          'error
                                          (misskey-http--outcome-message
                                           (format
                                            "Misskey response processing failed: %s"
                                            (error-message-string
                                             response-error))
                                           writep))))))
                                 (setq settled-p t)
                                 (pcase (car result)
                                   ('success
                                    (funcall callback (cdr result)))
                                   ('error
                                    (funcall error-fn (cdr result)))
                                   (_
                                    (error
                                     "Misskey response decoder returned an invalid result"))))))
                         (when (buffer-live-p buffer)
                           (kill-buffer buffer)))))
                   nil))
            (cond
             ((not request-buffer)
              (setq settled-p t)
              (funcall error-fn "Misskey did not start the HTTP request")
              nil)
             ((not (buffer-live-p request-buffer)) request-buffer)
             (t
              (setq handle
                    (appkit-register-handle
                     request-owner 'function
                     (list :buffer request-buffer
                           :errback error-fn
                           :writep writep)
                     #'misskey-http--cancel-request))
              (with-current-buffer request-buffer
                (setq-local url-max-redirections 0
                            misskey-http--request-handle handle)
                (add-hook 'kill-buffer-hook
                          #'misskey-http--retire-request-handle nil t))
              request-buffer))))
      ((error quit)
       (let ((quit-p (eq (car err) 'quit))
             (inhibit-quit t))
         (when (appkit-handle-p handle)
           (appkit-retire-handle handle))
         (misskey-http--discard-buffer
          (or request-buffer misskey-http--dispatch-buffer))
         (if settled-p
             (signal (car err) (cdr err))
           (let ((failure
                  (misskey-http--outcome-message
                   (misskey-http--safe-error-message err token)
                   (and misskey-http--dispatch-attempted-p writep))))
             (setq settled-p t)
             (unwind-protect
                 (funcall error-fn failure)
               (when quit-p (signal 'quit nil)))))
         nil)))))

(cl-defun misskey-http-read
    (endpoint parameters callback &key errback owner account)
  "Read authenticated API ENDPOINT with JSON PARAMETERS.

CALLBACK receives the decoded value.  ERRBACK receives a readable error
string.  OWNER defaults to ACCOUNT's Appkit session and owns cancellation."
  (misskey-http--request
   endpoint parameters callback
   :errback errback :owner owner :account account :writep nil))

(cl-defun misskey-http-post
    (endpoint parameters callback &key errback owner account)
  "Create remote state through API ENDPOINT with JSON PARAMETERS.

CALLBACK receives the decoded successful object.  ERRBACK receives a readable
error string.  OWNER defaults to ACCOUNT's Appkit session and owns request
cancellation.  Every post-dispatch failure reports an unknown remote outcome."
  (misskey-http--request
   endpoint parameters callback
   :errback errback :owner owner :account account :writep t))

(provide 'misskey-http)

;;; misskey-http.el ends here
