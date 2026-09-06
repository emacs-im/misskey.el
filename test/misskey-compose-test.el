;;; misskey-compose-test.el --- Tests for Misskey compose -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-compose)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(defmacro misskey-compose-test--with-buffer (&rest body)
  "Run BODY in a configured temporary Misskey compose buffer."
  (declare (indent 0) (debug t))
  (let ((buffer (make-symbol "buffer")))
    `(let ((misskey-instance-url "https://example.social")
           (,buffer (generate-new-buffer " *misskey-compose-test*")))
       (unwind-protect
           (with-current-buffer ,buffer
             (misskey-compose-mode)
             (setq-local misskey-compose--account (misskey--current-account))
             (appkit-chat-compose-setup
              :app (misskey-app misskey-compose--account)
              :context-function #'misskey-compose--context
              :status-fields-function #'misskey-compose--status-fields
              :parts-function #'misskey-compose--parts
              :footer-function #'misskey-compose--footer)
             (cl-letf (((symbol-function 'misskey-http-read)
                        (lambda (endpoint _parameters callback &rest _options)
                          (unless (equal endpoint "meta")
                            (ert-fail "Unexpected setup read"))
                          (funcall callback '((maxNoteTextLength . 3000))))))
               (misskey-compose-refresh-metadata))
             ,@body)
         (when (buffer-live-p ,buffer)
           (kill-buffer ,buffer))))))

(ert-deftest misskey-compose-renders-generated-public-note-shell ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (should (string-match-p "New note on https://example.social"
                              (appkit-chat-compose-display-string)))
      (should (string-match-p "Visibility: Public"
                              (appkit-chat-compose-display-string)))
      (should (string-match-p "C-c C-c publish"
                              (appkit-chat-compose-display-string)))
      (goto-char (appkit-chat-compose-body-start-position))
      (should (appkit-chatbuf-point-in-input-p))
      (insert "hello")
      (should (equal (appkit-chat-compose-body) "hello")))))

(ert-deftest misskey-compose-send-publishes-with-draft-view-owner ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let
          ((buffer (current-buffer)) (owner (appkit-current-surface))
           captured)
        (goto-char (appkit-chat-compose-body-start-position))
        (insert " hello world ")
        (cl-letf
            (((symbol-function 'misskey-http-post)
              (lambda (endpoint parameters callback &rest options)
                (setq captured
                      (list endpoint parameters
                            (plist-get options :owner)))
                (funcall callback '((createdNote (id . "note-1")))))))
          (misskey-compose-send)
          (should (equal (car captured) "notes/create"))
          (should
           (equal (cadr captured)
                  '(:text " hello world " :visibility "public")))
          (should (eq (nth 2 captured) owner))
          (should-not (buffer-live-p buffer)))))))

(ert-deftest misskey-compose-send-keeps-draft-after-remote-failure ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "keep me")
      (cl-letf (((symbol-function 'misskey-app)
                 (lambda (&optional _account) 'owner))
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'misskey-http-post)
                 (lambda (_endpoint _parameters _callback &rest options)
                   (funcall (plist-get options :errback) "failed"))))
        (misskey-compose-send)
        (should-not (appkit-compose-operation-active-p))
        (should (equal (appkit-chat-compose-body) "keep me"))))))

(ert-deftest misskey-compose-send-shows-inflight-state-until-callback ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "pending")
      (cl-letf (((symbol-function 'misskey-app)
                 (lambda (&optional _account) 'owner))
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'misskey-http-post)
                 (lambda (&rest _) 'request-buffer))
                ((symbol-function 'misskey-http-cancel) #'ignore))
        (misskey-compose-send)
        (should (appkit-compose-operation-active-p))
        (should (equal (appkit-chat-compose-body) "pending"))
        (goto-char (appkit-chat-compose-body-start-position))
        (should-error (delete-char 1))
        (let ((generation (appkit-compose-generation))
              (visibility misskey-compose-visibility))
          (should-error
           (misskey-compose-set-visibility
            (if (eq visibility 'followers) 'public 'followers))
           :type 'user-error)
          (should (eq misskey-compose-visibility visibility))
          (should (= generation (appkit-compose-generation))))
        (appkit-compose-cancel-operation)))))

(ert-deftest misskey-compose-send-restores-state-after-synchronous-error ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "recover")
      (cl-letf (((symbol-function 'misskey-app)
                 (lambda (&optional _account) 'owner))
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'misskey-http-post)
                 (lambda (&rest _) (error "Setup failed"))))
        (should-error (misskey-compose-send))
        (should-not (appkit-compose-operation-active-p))
        (should (equal (appkit-chat-compose-body) "recover"))
        (goto-char (appkit-chat-compose-body-end-position))
        (insert " again")
        (should (equal (appkit-chat-compose-body) "recover again"))))))

(ert-deftest misskey-compose-success-without-note-id-is-unknown-and-editable ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "unconfirmed")
      (let (reported)
        (cl-letf (((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq reported (apply #'format format-string args))))
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint _parameters callback &rest _)
                     (funcall callback '((createdNote))))))
          (misskey-compose-send)
          (should (string-match-p "outcome is unknown" reported))
          (should-not (appkit-compose-operation-active-p))
          (should (equal (appkit-chat-compose-body) "unconfirmed"))
          (goto-char (appkit-chat-compose-body-end-position))
          (insert " again")
          (should (equal (appkit-chat-compose-body) "unconfirmed again")))))))

(ert-deftest misskey-compose-send-rejects-empty-and-duplicate-send ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (should-error (misskey-compose-send) :type 'user-error)
      (appkit-compose-operation-begin 'publish :label "Publishing")
      (should-error (misskey-compose-send) :type 'user-error))))

(ert-deftest misskey-compose-send-preserves-significant-whitespace ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "  indented\n")
      (let (captured)
        (cl-letf (((symbol-function 'misskey-app)
                   (lambda (&optional _account) 'owner))
                  ((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint parameters callback &rest _)
                     (setq captured (plist-get parameters :text))
                     (funcall callback '((createdNote (id . "note-1")))))))
          (misskey-compose-send)
          (should (equal captured "  indented\n")))))))

(ert-deftest misskey-compose-add-and-remove-notes-in-the-middle ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "first")
      (misskey-compose-add-note)
      (insert "third")
      (appkit-chat-compose-goto-part 0)
      (misskey-compose-add-note)
      (insert "second")
      (should (equal (appkit-chat-compose-bodies) '("first" "second" "third")))
      (appkit-chat-compose-goto-part 1)
      (misskey-compose-remove-note)
      (should (equal (appkit-chat-compose-bodies) '("first" "third")))
      (should (eq (appkit-chat-compose-current-part-index) 1)))))

(ert-deftest misskey-compose-send-replies-later-notes-to-the-first ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let ((buffer (current-buffer))
            requests)
        (goto-char (appkit-chat-compose-body-start-position))
        (insert "first")
        (misskey-compose-add-note)
        (insert "second")
        (cl-letf (((symbol-function 'misskey-app)
                   (lambda (&optional _account) 'owner))
                  ((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (endpoint parameters callback &rest _)
                     (let ((note-id (format "note-%d"
                                            (1+ (length requests)))))
                       (push (list endpoint parameters) requests)
                       (funcall callback
                                (list (list 'createdNote
                                            (cons 'id note-id))))))))
          (misskey-compose-send)
          (setq requests (nreverse requests))
          (should (equal (nth 0 requests)
                         '("notes/create"
                           (:text "first" :visibility "public"))))
          (should (equal (car (nth 1 requests)) "notes/create"))
          (should (equal (plist-get (cadr (nth 1 requests)) :text) "second"))
          (should (equal (plist-get (cadr (nth 1 requests)) :replyId)
                         "note-1"))
          (should-not (buffer-live-p buffer)))))))

(ert-deftest misskey-compose-send-preserves-reply-target-and-visibility ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let (captured)
        (setq-local misskey-compose-reply-id "parent")
        (setq-local misskey-compose-target-label "@alice")
        (misskey-compose-set-visibility 'followers)
        (goto-char (appkit-chat-compose-body-start-position))
        (insert "reply")
        (cl-letf (((symbol-function 'misskey-app)
                   (lambda (&optional _account) 'owner))
                  ((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint parameters callback &rest _)
                     (setq captured parameters)
                     (funcall callback '((createdNote (id . "reply-1")))))))
          (misskey-compose-send)
          (should
           (equal captured
                  '(:text "reply" :visibility "followers"
                    :replyId "parent"))))))))

(ert-deftest misskey-compose-send-preserves-quote-target ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let (captured)
        (setq-local misskey-compose-renote-id "quoted")
        (goto-char (appkit-chat-compose-body-start-position))
        (insert "comment")
        (cl-letf (((symbol-function 'misskey-app)
                   (lambda (&optional _account) 'owner))
                  ((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint parameters callback &rest _)
                     (setq captured parameters)
                     (funcall callback '((createdNote (id . "quote-1")))))))
          (misskey-compose-send)
          (should (equal captured
                         '(:text "comment" :visibility "public"
                           :renoteId "quoted"))))))))

(ert-deftest misskey-compose-uploads-once-and-reuses-drive-file-after-failure ()
  (misskey-test-with-session
    (let ((file (make-temp-file "misskey-compose-attachment-")))
      (unwind-protect
          (misskey-compose-test--with-buffer
            (let ((buffer (current-buffer))
                  (uploads 0)
                  (posts 0)
                  captured)
              (goto-char (appkit-chat-compose-body-start-position))
              (insert "with attachment")
              (misskey-compose-attach-file file)
              (cl-letf (((symbol-function 'misskey-app)
                         (lambda (&optional _account) 'owner))
                        ((symbol-function 'message) #'ignore)
                        ((symbol-function 'misskey-http-upload-file)
                         (lambda (_file callback &rest _options)
                           (setq uploads (1+ uploads))
                           (funcall callback '((id . "drive-1")))))
                        ((symbol-function 'misskey-http-post)
                         (lambda (_endpoint parameters callback &rest options)
                           (setq posts (1+ posts)
                                 captured parameters)
                           (if (= posts 1)
                               (funcall (plist-get options :errback)
                                        "unknown outcome")
                             (funcall callback
                                      '((createdNote (id . "note-1"))))))))
                (misskey-compose-send)
                (should (buffer-live-p buffer))
                (should (= uploads 1))
                (should
                 (equal
                  (plist-get
                   (car
                    (plist-get (car (appkit-chat-compose-items)) :attachments))
                   :drive-id)
                  "drive-1"))
                (misskey-compose-send)
                (should (= uploads 1))
                (should (= posts 2))
                (should (equal (plist-get captured :fileIds)
                               ["drive-1"]))
                (should-not (buffer-live-p buffer)))))
        (delete-file file)))))

(ert-deftest misskey-compose-upload-progress-updates-status ()
  (misskey-test-with-session
    (let ((first (make-temp-file "misskey-compose-progress-a-"))
          (second (make-temp-file "misskey-compose-progress-b-")))
      (unwind-protect
          (misskey-compose-test--with-buffer
            (misskey-compose-attach-file first)
            (misskey-compose-attach-file second)
            (let (progress-fn)
              (cl-letf (((symbol-function 'message) #'ignore)
                        ((symbol-function 'misskey-http-upload-file)
                         (lambda (_file _callback &rest options)
                           (setq progress-fn (plist-get options :progress))
                           'upload-request))
                        ((symbol-function 'misskey-http-post)
                         (lambda (&rest _)
                           (ert-fail "Note created before upload finished")))
                        ((symbol-function 'misskey-http-cancel) #'ignore))
                (misskey-compose-send)
                (should (functionp progress-fn))
                (funcall progress-fn (list :progress 0.25))
                (should (string-match-p "Uploading"
                                        (appkit-chat-compose-display-string)))
                (should (string-match-p "1/2"
                                        (appkit-chat-compose-display-string)))
                (should (string-match-p "25%"
                                        (appkit-chat-compose-display-string)))
                (appkit-compose-cancel-operation))))
        (delete-file first)
        (delete-file second)))))

(ert-deftest misskey-compose-allows-attachment-only-note ()
  (misskey-test-with-session
    (let ((file (make-temp-file "misskey-compose-file-only-")))
      (unwind-protect
          (misskey-compose-test--with-buffer
            (let (parameters)
              (misskey-compose-attach-file file)
              (cl-letf (((symbol-function 'misskey-app)
                         (lambda (&optional _account) 'owner))
                        ((symbol-function 'message) #'ignore)
                        ((symbol-function 'misskey-http-upload-file)
                         (lambda (_file callback &rest _options)
                           (funcall callback '((id . "drive-2")))))
                        ((symbol-function 'misskey-http-post)
                         (lambda (_endpoint value callback &rest _options)
                           (setq parameters value)
                           (funcall callback
                                    '((createdNote (id . "note-2")))))))
                (misskey-compose-send)
                (should-not (plist-member parameters :text))
                (should (equal (plist-get parameters :fileIds)
                               ["drive-2"])))))
        (delete-file file)))))

(ert-deftest misskey-compose-persists-confirmed-prefix-before-retry ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let ((buffer (current-buffer))
            (post-count 0)
            requests)
        (setq-local misskey-compose-renote-id "quoted"
                    misskey-compose-target-label "@source"
                    misskey-compose-cw "Spoilers"
                    misskey-compose-local-only t
                    misskey-compose-visibility 'specified
                    misskey-compose-recipients '(("reader" . "@reader")))
        (goto-char (appkit-chat-compose-body-start-position))
        (insert "first")
        (misskey-compose-add-note)
        (insert "second")
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint parameters callback &rest options)
                     (setq post-count (1+ post-count)
                           requests (append requests (list parameters)))
                     (pcase post-count
                       (1 (funcall callback
                                   '((createdNote (id . "note-1")))))
                       (2 (funcall (plist-get options :errback)
                                   "remote outcome is unknown"))
                       (3 (funcall callback
                                   '((createdNote (id . "note-2")))))))))
          (misskey-compose-send)
          (should (buffer-live-p buffer))
          (should (equal (appkit-chat-compose-bodies) '("second")))
          (should (equal misskey-compose-reply-id "note-1"))
          (should-not misskey-compose-renote-id)
          (should-not misskey-compose-target-label)
          (should-not (appkit-compose-operation-active-p))
          (misskey-compose-send)
          (should (= post-count 3))
          (dolist (parameters requests)
            (should (equal (plist-get parameters :visibility) "specified"))
            (should (equal (plist-get parameters :visibleUserIds) ["reader"]))
            (should (equal (plist-get parameters :cw) "Spoilers"))
            (should (eq (plist-get parameters :localOnly) t)))
          (should (equal (mapcar (lambda (parameters)
                                   (plist-get parameters :text))
                                 requests)
                         '("first" "second" "second")))
          (should (equal (plist-get (nth 1 requests) :replyId) "note-1"))
          (should (equal (plist-get (nth 2 requests) :replyId) "note-1"))
          (should-not (buffer-live-p buffer)))))))

(ert-deftest misskey-compose-cancel-stops-request-and-ignores-late-success ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let ((buffer (current-buffer))
            callback
            (posts 0)
            (cancels 0))
        (goto-char (appkit-chat-compose-body-start-position))
        (insert "keep")
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint _parameters success &rest _)
                     (setq posts (1+ posts)
                           callback success)
                     'request))
                  ((symbol-function 'misskey-http-cancel)
                   (lambda (request)
                     (should (eq request 'request))
                     (setq cancels (1+ cancels)))))
          (misskey-compose-send)
          (misskey-compose-cancel)
          (should (= cancels 1))
          (should-not (buffer-live-p buffer))
          (funcall callback '((createdNote (id . "late"))))
          (should (= posts 1)))))))

(ert-deftest misskey-compose-killed-buffer-stops-chain-and-ignores-callback ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let ((buffer (current-buffer))
            callback
            (posts 0)
            (cancels 0))
        (goto-char (appkit-chat-compose-body-start-position))
        (insert "first")
        (misskey-compose-add-note)
        (insert "second")
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint _parameters success &rest _)
                     (setq posts (1+ posts)
                           callback success)
                     'request))
                  ((symbol-function 'misskey-http-cancel)
                   (lambda (_request)
                     (setq cancels (1+ cancels)))))
          (misskey-compose-send)
          (kill-buffer buffer)
          (should (= cancels 1))
          (funcall callback '((createdNote (id . "late"))))
          (should (= posts 1))
          (should-not (buffer-live-p buffer)))))))

(ert-deftest misskey-compose-stopped-owner-releases-draft-and-rejects-results ()
  (dolist (stop '(app surface))
    (misskey-test-with-session
      (misskey-compose-test--with-buffer
        (let ((buffer (current-buffer))
              (surface (appkit-current-surface))
              callback errback
              (posts 0)
              (cancels 0))
          (goto-char (appkit-chat-compose-body-start-position))
          (insert "first")
          (misskey-compose-add-note)
          (insert "second")
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey-http-post)
                     (lambda (_endpoint _parameters success &rest options)
                       (setq posts (1+ posts)
                             callback success
                             errback (plist-get options :errback))
                       'request))
                    ((symbol-function 'misskey-http-cancel)
                     (lambda (_request)
                       (setq cancels (1+ cancels))
                       ;; Cancellation may race with an already queued result.
                       (funcall callback '((createdNote (id . "racing")))))))
            (misskey-compose-send)
            (if (eq stop 'app)
                (misskey-stop)
              (appkit-surface-stop surface))
            (should (buffer-live-p buffer))
            (should-not (appkit-surface-live-p surface))
            (should-not (appkit-current-surface))
            (should-not (appkit-compose-operation-active-p))
            (should (= cancels 1))
            (should (equal (mapcar (lambda (item) (plist-get item :text))
                                   (appkit-chat-compose-items))
                           '("first" "second")))
            (goto-char (appkit-chat-compose-body-end-position))
            (insert " edited")
            (let ((generation (appkit-compose-generation))
                  (draft (appkit-chat-compose-items)))
              (funcall callback '((createdNote (id . "late"))))
              (funcall errback "late failure")
              (should (buffer-live-p buffer))
              (should (equal draft (appkit-chat-compose-items)))
              (should (= generation (appkit-compose-generation))))
            (should (equal (appkit-chat-compose-body) "second edited"))
            (should (= posts 1))
            (should-not (appkit-current-surface))
            (appkit-surface-stop surface)
            (should (= cancels 1))))))))

(ert-deftest misskey-compose-visibility-tracks-only-audience-changes ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let ((generation (appkit-compose-generation))
            (visibility (if (eq misskey-compose-visibility 'followers)
                            'public
                          'followers)))
        (should (eq (misskey-compose-set-visibility visibility) visibility))
        (should (eq misskey-compose-visibility visibility))
        (should (= (appkit-compose-generation) (1+ generation)))
        (should (buffer-modified-p))
        (set-buffer-modified-p nil)
        (misskey-compose-set-visibility visibility)
        (should (= (appkit-compose-generation) (1+ generation)))
        (should-not (buffer-modified-p))
        (should-error (misskey-compose-set-visibility 'unsupported)
                      :type 'user-error)
        (should (eq misskey-compose-visibility visibility))
        (should (= (appkit-compose-generation) (1+ generation)))
        (should-not (buffer-modified-p))))))

(ert-deftest misskey-compose-missing-upload-id-is-unknown-and-editable ()
  (misskey-test-with-session
    (let ((file (make-temp-file "misskey-compose-missing-id-")))
      (unwind-protect
          (misskey-compose-test--with-buffer
            (misskey-compose-attach-file file)
            (let (reported)
              (cl-letf (((symbol-function 'message)
                         (lambda (format-string &rest args)
                           (setq reported
                                 (apply #'format format-string args))))
                        ((symbol-function 'misskey-http-upload-file)
                         (lambda (_file callback &rest _)
                           (funcall callback '((id . "")))))
                        ((symbol-function 'misskey-http-post)
                         (lambda (&rest _)
                           (ert-fail "A note with an unconfirmed upload ran"))))
                (misskey-compose-send)
                (should (string-match-p "outcome is unknown" reported))
                (should-not (appkit-compose-operation-active-p))
                (should-not buffer-read-only)
                (should (= (length (plist-get (car (appkit-chat-compose-items))
                                              :attachments))
                           1)))))
        (delete-file file)))))

(ert-deftest misskey-compose-attachment-limit-counts-drive-and-local-files ()
  (misskey-test-with-session
    (let ((file (make-temp-file "misskey-compose-limit-"))
          (extra (make-temp-file "misskey-compose-limit-extra-")))
      (unwind-protect
          (misskey-compose-test--with-buffer
            (let ((attachments
                   (append
                    (cl-loop for index below 8
                             collect (list :path file
                                           :drive-id (format "drive-%d" index)))
                    (cl-loop repeat 8 collect (list :path file)))))
              (appkit-chat-compose-set-items
               (list (list :text "" :attachments attachments)))
              (should-error (misskey-compose-attach-file extra)
                            :type 'user-error)))
        (delete-file file)
        (delete-file extra)))))

(ert-deftest misskey-compose-rejects-remote-attachment-paths ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let ((remote "/ssh:example.social:/tmp/image.png"))
        (should-error (misskey-compose-attach-file remote) :type 'user-error)
        (appkit-chat-compose-set-items
         (list (list :text "" :attachments (list (list :path remote)))))
        (cl-letf (((symbol-function 'misskey-http-upload-file)
                   (lambda (&rest _)
                     (ert-fail "A remote attachment reached curl"))))
          (should-error (misskey-compose-send) :type 'user-error))))))

(ert-deftest misskey-compose-stale-callback-cannot-finish-retry ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let ((buffer (current-buffer))
            callbacks
            errbacks)
        (goto-char (appkit-chat-compose-body-start-position))
        (insert "retry me")
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint _parameters callback &rest options)
                     (setq callbacks (append callbacks (list callback))
                           errbacks
                           (append errbacks
                                   (list (plist-get options :errback))))
                     (intern (format "request-%d" (length callbacks))))))
          (misskey-compose-send)
          (funcall (car errbacks) "remote outcome is unknown")
          (misskey-compose-send)
          (funcall (car callbacks) '((createdNote (id . "stale"))))
          (should (buffer-live-p buffer))
          (should (appkit-compose-operation-active-p))
          (should (equal (appkit-chat-compose-body) "retry me"))
          (funcall (cadr callbacks) '((createdNote (id . "confirmed"))))
          (should-not (buffer-live-p buffer)))))))

(ert-deftest misskey-compose-private-reply-preserves-audience-and-content-controls ()
  (misskey-test-with-session
    (let (buffer parameters)
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer)
                    ((symbol-function 'misskey-http-read)
                     (lambda (endpoint _parameters callback &rest _options)
                       (should (equal endpoint "meta"))
                       (funcall callback '((maxNoteTextLength . 3000)))))
                    ((symbol-function 'misskey-http-post)
                     (lambda (_endpoint value callback &rest _options)
                       (setq parameters value)
                       (funcall callback '((createdNote (id . "reply")))))))
            (setq buffer
                  (misskey-compose-open
                   nil :reply-id "parent"
                   :target-note '((id . "parent") (visibility . "specified")
                                  (cw . "Sensitive") (localOnly . t)
                                  (visibleUserIds . ["self" "other" "author"])
                                  (user (id . "author") (username . "writer")))))
            (with-current-buffer buffer
              (should-error (misskey-compose-set-visibility 'public) :type 'user-error)
              (should-error (misskey-compose-toggle-local-only) :type 'user-error)
              (goto-char (appkit-chat-compose-body-start-position))
              (insert "reply")
              (misskey-compose-send))
            (should (equal (plist-get parameters :visibility) "specified"))
            (should (equal (plist-get parameters :visibleUserIds) ["other" "author"]))
            (should (equal (plist-get parameters :cw) "Sensitive"))
            (should (eq (plist-get parameters :localOnly) t)))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest misskey-compose-unsafe-targets-are-rejected-before-opening ()
  (misskey-test-with-session
    (cl-letf (((symbol-function 'pop-to-buffer)
               (lambda (&rest _) (ert-fail "Unsafe target opened a draft"))))
      (should-error (misskey-compose-open nil :reply-id "target")
                    :type 'user-error)
      (dolist (note '(((id . "target") (visibility . "specified")
                       (user (id . "other")))
                      ((id . "target") (visibility . "followers")
                       (user (id . "other")))))
        (should-error (misskey-compose-open nil :renote-id "target" :target-note note)
                      :type 'user-error))
      (should-error
       (misskey-compose-open
        nil :reply-id "target"
        :target-note '((id . "target") (visibility . "specified")
                       (user (id . "other"))))
       :type 'user-error))))

(ert-deftest misskey-compose-unicode-preflight-checks-all-parts-before-any-write ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (setq misskey-compose--max-note-text-length 3)
      (let ((writes 0) parameters)
        (cl-letf (((symbol-function 'misskey-http-upload-file)
                   (lambda (&rest _) (cl-incf writes)))
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint value _callback &rest options)
                     (cl-incf writes)
                     (setq parameters value)
                     (funcall (plist-get options :errback) "Retain draft"))))
          ;; Even a valid first part must not publish before validating the suffix.
          (appkit-chat-compose-set-items
           (list (list :text "ok" :attachments '((:path "missing" :drive-id "drive")))
                 (list :text "\U0001F600éx")))
          (should-error (misskey-compose-send) :type 'user-error)
          (should (= writes 0))
          ;; Astral emoji is one code point; a combining mark is its own code point.
          (appkit-chat-compose-set-items (list (list :text "\U0001F600é")))
          (should (string-match-p "3/3" (appkit-chat-compose--header-line)))
          (misskey-compose-send)
          (should (= writes 1))
          (should (equal (plist-get parameters :text) "\U0001F600é")))))))

(ert-deftest misskey-compose-missing-recipient-state-prevents-publication ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (goto-char (appkit-chat-compose-body-start-position))
      (insert "private")
      (misskey-compose-set-visibility 'specified)
      (cl-letf (((symbol-function 'misskey-http-post)
                 (lambda (&rest _) (ert-fail "Invalid recipient state published"))))
        (should-error (misskey-compose-send) :type 'user-error)
        (setq misskey-compose-recipients '(("bad id" . "Invalid")))
        (should-error (misskey-compose-send) :type 'user-error)))))

(ert-deftest misskey-compose-refresh-failure-and-late-results-remain-editing-safe ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let (callbacks errbacks)
        (cl-letf (((symbol-function 'misskey-http-read)
                   (lambda (_endpoint _parameters callback &rest options)
                     (push callback callbacks)
                     (push (plist-get options :errback) errbacks)
                     (misskey-http--request-create :callback #'ignore :errback #'ignore)))
                  ((symbol-function 'misskey-http-cancel) #'ignore)
                  ((symbol-function 'misskey-http-post)
                   (lambda (&rest _) (ert-fail "Unknown instance limit published"))))
          (let ((generation (appkit-compose-generation)))
            (misskey-compose-refresh-metadata)
            (misskey-compose-refresh-metadata)
            (funcall (cadr callbacks) '((maxNoteTextLength . 9999)))
            (should-not misskey-compose--max-note-text-length)
            (funcall (car errbacks) "Offline")
            (should (string-match-p "Offline" (appkit-chat-compose--header-line)))
            (should (= generation (appkit-compose-generation))))
          (goto-char (appkit-chat-compose-body-start-position))
          (insert "still editable")
          (should-error (misskey-compose-send) :type 'user-error)
          (misskey-compose-refresh-metadata)
          (funcall (car callbacks) '((maxNoteTextLength . 5)))
          (should (= misskey-compose--max-note-text-length 5))
          (misskey-compose-refresh-metadata)
          (appkit-surface-stop (appkit-current-surface))
          (let ((generation (appkit-compose-generation)))
            (funcall (car callbacks) '((maxNoteTextLength . 100)))
            (should-not misskey-compose--max-note-text-length)
            (should (= generation (appkit-compose-generation)))))))))

(ert-deftest misskey-compose-recipient-results-cannot-overwrite-later-audience-choices ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let (callbacks requests)
        (cl-letf (((symbol-function 'misskey-http-read)
                   (lambda (endpoint parameters callback &rest _options)
                     (should (equal endpoint "users/show"))
                     (push parameters requests)
                     (push callback callbacks)
                     (misskey-http--request-create :callback #'ignore :errback #'ignore)))
                  ((symbol-function 'misskey-http-cancel) #'ignore))
          (misskey-compose-set-visibility 'specified)
          (misskey-compose-add-recipient "@alice@remote.example")
          (should (equal (car requests) '(:username "alice" :host "remote.example")))
          (misskey-compose-add-recipient "@bob")
          (funcall (cadr callbacks) '((id . "alice") (username . "alice")))
          (should-not misskey-compose-recipients)
          (funcall (car callbacks) '((id . "bob") (username . "bob")))
          (should (equal (mapcar #'car misskey-compose-recipients) '("bob")))
          (misskey-compose-add-recipient "@alice")
          (misskey-compose-remove-recipient "bob")
          (funcall (car callbacks) '((id . "alice") (username . "alice")))
          (should-not misskey-compose-recipients)
          (misskey-compose-add-recipient "@alice")
          (misskey-compose-set-visibility 'home)
          (funcall (car callbacks) '((id . "alice") (username . "alice")))
          (should-not misskey-compose-recipients)
          (should (eq misskey-compose-visibility 'home))
          (misskey-compose-set-visibility 'specified)
          (misskey-compose-add-recipient "@alice")
          (let ((misskey-compose--account (copy-sequence misskey-compose--account)))
            (funcall (car callbacks) '((id . "alice") (username . "alice")))
            (should-not misskey-compose-recipients)))))))

(ert-deftest misskey-compose-target-defaults-never-widen-an-explicit-audience ()
  (misskey-test-with-session
    (let (buffer)
      (unwind-protect
          (cl-letf (((symbol-function 'pop-to-buffer) #'set-buffer)
                    ((symbol-function 'misskey-http-read)
                     (lambda (_endpoint _parameters callback &rest _options)
                       (funcall callback '((maxNoteTextLength . 3000))))))
            (setq buffer
                  (misskey-compose-open
                   nil :reply-id "home" :visibility 'followers
                   :target-note '((id . "home") (visibility . "home")
                                  (localOnly . :json-false)
                                  (user (id . "other") (username . "other")))))
            (with-current-buffer buffer
              (should (eq misskey-compose-visibility 'followers))
              (should-not misskey-compose-local-only)
              (should-error (misskey-compose-set-visibility 'public) :type 'user-error)
              (misskey-compose-set-visibility 'home)
              (should (eq misskey-compose-visibility 'home))))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest misskey-compose-content-controls-track-semantic-edits-only ()
  (misskey-test-with-session
    (misskey-compose-test--with-buffer
      (let ((generation (appkit-compose-generation)))
        (misskey-compose-set-cw "Spoilers")
        (should (= (appkit-compose-generation) (1+ generation)))
        (misskey-compose-set-cw "Spoilers")
        (should (= (appkit-compose-generation) (1+ generation)))
        (misskey-compose-toggle-local-only)
        (should (= (appkit-compose-generation) (+ 2 generation)))
        (misskey-compose-set-cw "")
        (should-not misskey-compose-cw)
        (should (= (appkit-compose-generation) (+ 3 generation)))
        (should-error (misskey-compose-set-cw (make-string 101 ?x)) :type 'user-error)
        (should (= (appkit-compose-generation) (+ 3 generation)))))))

(provide 'misskey-compose-test)

;;; misskey-compose-test.el ends here
