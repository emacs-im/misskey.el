;;; misskey-timeline-test.el --- Tests for Misskey timelines -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey)
(require 'misskey-timeline)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(cl-defun misskey-timeline-test--note
    (id text &key cw local-only files renote avatar-url
        (created-at "2026-08-13T00:00:00.000Z")
        (name "Alice") (username "alice"))
  "Return a normalized test note with ID and TEXT.

CW, LOCAL-ONLY, FILES, RENOTE, AVATAR-URL, CREATED-AT, NAME, and USERNAME
supply optional fields."
  `((id . ,id)
    (createdAt . ,created-at)
    (text . ,text)
    (cw . ,cw)
    (visibility . "public")
    (localOnly . ,local-only)
    (repliesCount . 1)
    (renoteCount . 2)
    (reactionCount . 3)
    (files . ,files)
    (renote . ,renote)
    (user (id . ,(concat "u-" username))
          (name . ,name)
          (username . ,username)
          (host)
          (avatarUrl . ,avatar-url))))

(defun misskey-timeline-test--file
    (id &optional sensitive type thumbnail-url url)
  "Return a test media file with ID.

SENSITIVE, TYPE, THUMBNAIL-URL, and URL customize its wire fields."
  `((id . ,id)
    (name . ,(format "%s.jpg" id))
    (type . ,(or type "image/jpeg"))
    (thumbnailUrl . ,(or thumbnail-url
                         (format "https://cdn.example/%s-thumb.webp" id)))
    (url . ,(or url (format "https://cdn.example/%s.jpg" id)))
    (isSensitive . ,sensitive)
    (comment . "Media description")))

(defun misskey-timeline-test--cleanup (view buffer)
  "Destroy test VIEW and BUFFER, then stop Misskey sessions."
  (when (and (appkit-view-p view) (appkit-view-live-p view))
    (appkit-kill-view view t))
  (when (buffer-live-p buffer)
    (kill-buffer buffer))
  (misskey-stop))

(defun misskey-timeline-test--flush (view)
  "Synchronize pending invalidations for test VIEW."
  (appkit-sync-invalidations view))

(defun misskey-timeline-test--authenticated-account (&optional account)
  "Bind provisional ACCOUNT to a stable test user identity."
  (let ((target (or account (misskey--current-account))))
    (misskey--account-create
     :origin (misskey--account-origin target)
     :auth-source-user (misskey--account-auth-source-user target)
     :remote-user-id "self")))

(ert-deftest misskey-home-renders-keyed-notes-through-installed-command ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey--apps (make-hash-table :test #'equal))
        captured view buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey--authenticated-account)
                     #'misskey-timeline-test--authenticated-account)
                    ((symbol-function 'misskey-http-read)
                     (lambda (endpoint parameters callback &rest options)
                       (setq captured
                             (list endpoint parameters
                                   (plist-get options :owner)
                                   (plist-get options :account)))
                       (funcall
                        callback
                        (list
                         (misskey-timeline-test--note
                          "n1" "hidden body"
                          :cw "Spoiler" :local-only t
                          :files '(((id . "f1")) ((id . "f2"))))
                         (misskey-timeline-test--note
                          "r1" nil :name "Bob" :username "bob"
                          :renote
                          (misskey-timeline-test--note
                           "n2" "renoted body")))))))
            (setq view (call-interactively #'misskey-home)
                  buffer (appkit-view-buffer view))
            (misskey-timeline-test--flush view)
            (let ((state (misskey-timeline--view-state view)))
              (should (equal (car captured) "notes/timeline"))
              (should (equal (cadr captured)
                             '(:limit 20 :allowPartial t)))
              (should (appkit-view-operation-p (nth 2 captured)))
              (should (eq (appkit-view-operation-view (nth 2 captured))
                          view))
              (should (equal (nth 3 captured)
                             (plist-get
                              (appkit-view-state view) :account)))
              (should (eq (plist-get state :phase) 'ready))
              (should (equal (appkit-projection-keys view)
                             '("n1" "r1"))))
            (with-current-buffer buffer
              (should (eq major-mode 'misskey-timeline-mode))
              (should buffer-read-only)
              (should (equal (buffer-name)
                             "*misskey: alice@example.social*"))
              (should
               (eq (lookup-key misskey-timeline-mode-map (kbd "TAB"))
                   #'misskey-timeline-next-kind))
              (should (eq (lookup-key misskey-timeline-mode-map (kbd "g"))
                          #'misskey-timeline-refresh))
              (should
               (eq (lookup-key misskey-timeline-mode-map (kbd "N"))
                   #'misskey-timeline-load-more))
              (should (eq (lookup-key misskey-timeline-mode-map (kbd "RET"))
                          #'misskey-render-toggle-content-warning))
              (should (eq (lookup-key misskey-timeline-mode-map (kbd "c"))
                          #'misskey-timeline-compose))
              (should (string-match-p "CW: Spoiler" (buffer-string)))
              (should (string-match-p "\\[RET to reveal\\]"
                                      (buffer-string)))
              (should-not (string-match-p "hidden body" (buffer-string)))
              (should (string-match-p "Local only" (buffer-string)))
              (should (string-match-p "2 attachments" (buffer-string)))
              (should
               (string-match-p
                "renoted by Bob @bob\nAlice @alice.*\nrenoted body"
                (buffer-string)))
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (should (equal (appkit-discussion-key-at-point) "n1"))
              (should (equal (get-text-property (point) 'misskey-note-id)
                             "n1"))
              (call-interactively
               (lookup-key misskey-timeline-mode-map (kbd "RET")))
              (misskey-timeline-test--flush view)
              (should (string-match-p "hidden body" (buffer-string)))
              (should-not (string-match-p "\\[RET to reveal\\]"
                                          (buffer-string))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-timeline-window-resize-restores-elided-heading ()
  "A widened timeline must reconstruct a formerly elided heading."
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey-timeline-show-avatars nil)
        (misskey-timeline-show-media nil)
        (misskey--apps (make-hash-table :test #'equal))
        (render-width 30)
        view buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey--authenticated-account)
                     #'misskey-timeline-test--authenticated-account)
                    ((symbol-function 'appkit-geometry-window-width)
                     (lambda (&rest _arguments) render-width))
                    ((symbol-function 'misskey-http-read)
                     (lambda (_endpoint _parameters callback &rest _options)
                       (funcall
                        callback
                        (list
                         (misskey-timeline-test--note
                          "long" "body"
                          :name "Alice Extremely Long Display Name"
                          :username "alice-identity-tail"))))))
            (setq view (misskey-home)
                  buffer (appkit-view-buffer view))
            (misskey-timeline-test--flush view)
            (with-current-buffer buffer
              (should
               (memq #'appkit-view-refresh-responsive-geometry
                     window-state-change-functions))
              (should
               (memq #'appkit-view-refresh-responsive-geometry
                     text-scale-mode-hook))
              (goto-char (point-min))
              (should (search-forward "…" nil t))
              (should-not (search-forward "identity-tail" nil t))
              (let ((heading-line (line-number-at-pos)))
                (goto-char (point-min))
                (search-forward "2026-08-13 08:00")
                (should (= heading-line (line-number-at-pos))))
              (setq render-width 160)
              (run-hook-with-args
               'window-state-change-functions
               (get-buffer-window buffer t)))
            (misskey-timeline-test--flush view)
            (with-current-buffer buffer
              (goto-char (point-min))
              (should (search-forward "identity-tail" nil t))
              (should-not (search-forward "…" nil t))
              (goto-char (point-min))
              (search-forward "Alice Extremely")
              (let ((heading-line (line-number-at-pos)))
                (search-forward "2026-08-13 08:00")
                (should (= heading-line (line-number-at-pos)))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-home-refresh-preserves-position-and-rejects-bad-data ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey--apps (make-hash-table :test #'equal))
        callbacks view buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey--authenticated-account)
                     #'misskey-timeline-test--authenticated-account)
                    ((symbol-function 'misskey-http-read)
                     (lambda (_endpoint _parameters callback &rest _options)
                       (push callback callbacks))))
            (setq view (misskey-home)
                  buffer (appkit-view-buffer view))
            (should (eq (plist-get (misskey-timeline--view-state view) :phase)
                        'initial))
            (funcall
             (car callbacks)
             (list (misskey-timeline-test--note "n1" "first")
                   (misskey-timeline-test--note "n2" "second")))
            (misskey-timeline-test--flush view)
            (with-current-buffer buffer
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (appkit-discussion-next-entry)
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (misskey-timeline-refresh)
              (should (eq (plist-get (misskey-timeline--view-state view) :phase)
                          'refresh))
              (funcall
               (car callbacks)
               (list (misskey-timeline-test--note "n0" "new")
                     (misskey-timeline-test--note "n2" "updated")
                     (misskey-timeline-test--note "n1" "first")))
              (misskey-timeline-test--flush view)
              (should (eq (plist-get (misskey-timeline--view-state view) :phase)
                          'ready))
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (should (equal (appkit-projection-keys view)
                             '("n0" "n2" "n1")))
              (misskey-timeline-refresh)
              (funcall (car callbacks) '(((id . "broken") (user))))
              (misskey-timeline-test--flush view)
              (should (eq (plist-get (misskey-timeline--view-state view) :phase)
                          'error))
              (should (string-match-p "malformed note"
                                      (plist-get (misskey-timeline--view-state view) :message)))
              (should (equal (appkit-projection-keys view)
                             '("n0" "n2" "n1")))
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (let ((misskey-timeline-limit 0)
                    (request-count (length callbacks)))
                (should-error (misskey-timeline-refresh)
                              :type 'user-error)
                (should (= (length callbacks) request-count))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-timeline-switches-canonical-states-and-revokes-request ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey--apps (make-hash-table :test #'equal))
        requests
        canceled
        view
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey--authenticated-account)
                #'misskey-timeline-test--authenticated-account)
               ((symbol-function 'misskey-http-read)
                (lambda (endpoint _parameters callback &rest options)
                  (let* ((request (make-symbol endpoint))
                         (owner (plist-get options :owner))
                         (handle
                          (appkit-register-handle
                           owner 'function request #'misskey-http-cancel)))
                    (push
                     (list
                      endpoint
                      (lambda (payload)
                        (appkit-retire-handle handle)
                        (funcall callback payload))
                      request owner)
                     requests)
                    request)))
               ((symbol-function 'misskey-http-cancel)
                (lambda (request)
                  (push request canceled))))
            (setq view (call-interactively #'misskey-home)
                  buffer (appkit-view-buffer view))
            (let ((home-state (appkit-view-state view))
                  (home-request (car requests)))
              (with-current-buffer buffer
                (call-interactively
                 (lookup-key misskey-timeline-mode-map (kbd "TAB")))
                (let ((local-state (appkit-view-state view))
                      (local-request (car requests)))
                  (should-not (eq local-state home-state))
                  (should (eq local-state
                              (misskey-timeline--feed-state
                               (appkit-view-app view) 'local)))
                  (should (equal (car local-request)
                                 "notes/local-timeline"))
                  (should (eq (car canceled) (nth 2 home-request)))
                  (funcall
                   (nth 1 home-request)
                   (list (misskey-timeline-test--note
                          "stale" "must not install")))
                  (should-not (plist-get home-state :items))
                  (funcall
                   (nth 1 local-request)
                   (list (misskey-timeline-test--note "local-1" "local")))
                  (misskey-timeline-test--flush view)
                  (goto-char (point-min))
                  (appkit-discussion-next-entry)
                  (should (equal (appkit-discussion-key-at-point)
                                 "local-1"))
                  (misskey-timeline--switch-kind view 'social)
                  (let ((social-request (car requests)))
                    (should (equal (car social-request)
                                   "notes/hybrid-timeline"))
                    (funcall
                     (nth 1 social-request)
                     (list
                      (misskey-timeline-test--note "social-1" "social")))
                    (misskey-timeline-test--flush view))
                  (let ((request-count (length requests)))
                    (misskey-timeline--switch-kind view 'local)
                    (misskey-timeline-test--flush view)
                    (should (eq (appkit-view-state view) local-state))
                    (should (= (length requests) request-count))
                    (should (equal (appkit-discussion-key-at-point)
                                   "local-1")))
                  (misskey-timeline--switch-kind view 'social)
                  (misskey-timeline-test--flush view)
                  (let ((request-count (length requests)))
                    (misskey-timeline--switch-kind view 'local t)
                    (let ((refresh-request (car requests)))
                      (should (= (length requests) (1+ request-count)))
                      (should (equal (car refresh-request)
                                     "notes/local-timeline"))
                      (funcall
                       (nth 1 refresh-request)
                       (list
                        (misskey-timeline-test--note
                         "local-1" "refreshed local")))
                      (misskey-timeline-test--flush view)
                      (should (eq (appkit-view-state view) local-state))
                      (should (equal (appkit-discussion-key-at-point)
                                     "local-1"))))
                  (should (= 1
                             (hash-table-count
                              (appkit-app-view-registry
                               (appkit-view-app view))))))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-home-loads-older-notes-with-stable-position ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey--apps (make-hash-table :test #'equal))
        requests
        view
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'misskey--authenticated-account)
                #'misskey-timeline-test--authenticated-account)
               ((symbol-function 'misskey-http-read)
                (lambda (endpoint parameters callback &rest options)
                  (push
                   (list endpoint parameters callback
                         (plist-get options :owner)
                         (plist-get options :account))
                   requests))))
            (setq view (misskey-home)
                  buffer (appkit-view-buffer view))
            (funcall
             (nth 2 (car requests))
             (list (misskey-timeline-test--note "n3" "newest")
                   (misskey-timeline-test--note "n2" "second")))
            (misskey-timeline-test--flush view)
            (with-current-buffer buffer
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (appkit-discussion-next-entry)
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (let* ((state (misskey-timeline--view-state view))
                     (oldest (car (last (plist-get state :items)))))
                (unwind-protect
                    (progn
                      (setf (alist-get 'id oldest) nil)
                      (should-error (misskey-timeline-load-more) :type 'error)
                      (should-not
                       (gethash
                        misskey-timeline--request-key
                        (appkit-view-request-table view))))
                  (setf (alist-get 'id oldest) "n2")))
              (misskey-timeline-load-more)
              (should
               (equal (cadar requests)
                      '(:limit 20 :allowPartial t :untilId "n2")))
              (should (eq (appkit-view-operation-view
                           (nth 3 (car requests)))
                          view))
              (should-error (misskey-timeline-load-more)
                            :type 'user-error)
              (funcall
               (nth 2 (car requests))
               (list (misskey-timeline-test--note "n2" "duplicate edge")
                     (misskey-timeline-test--note "n1" "oldest")))
              (misskey-timeline-test--flush view)
              (should (equal (appkit-projection-keys view)
                             '("n3" "n2" "n1")))
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (should-not
               (plist-get (misskey-timeline--view-state view) :older-exhausted-p))
              (should (string-match-p "N older" (buffer-string)))
              (misskey-timeline-refresh)
              (should
               (equal (cadar requests)
                      '(:limit 20 :allowPartial t)))
              (funcall
               (nth 2 (car requests))
               (list (misskey-timeline-test--note "n4" "refreshed")
                     (misskey-timeline-test--note "n3" "updated")))
              (misskey-timeline-test--flush view)
              (should (equal (appkit-projection-keys view)
                             '("n4" "n3" "n2" "n1")))
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (should-not
               (plist-get (misskey-timeline--view-state view) :older-exhausted-p))
              (misskey-timeline-load-more)
              (should
               (equal (cadar requests)
                      '(:limit 20 :allowPartial t :untilId "n1")))
              (funcall (nth 2 (car requests)) nil)
              (misskey-timeline-test--flush view)
              (should
               (plist-get (misskey-timeline--view-state view) :older-exhausted-p))
              (should (equal (appkit-projection-keys view)
                             '("n4" "n3" "n2" "n1")))
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (should (string-match-p "older exhausted"
                                      (buffer-string)))
              (let ((request-count (length requests)))
                (should-error (misskey-timeline-load-more)
                              :type 'user-error)
                (should (= (length requests) request-count))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-home-loads-avatar-with-stable-row-position ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey-timeline-show-avatars t)
        (appkit-media-transfer-concurrency 2)
        (misskey--apps (make-hash-table :test #'equal))
        (avatar-url "https://cdn.example/alice.png")
        (avatar-image '(image :type png :data "avatar"))
        avatar-images
        cache-file
        download-success
        requested-resource
        requested-cache-base
        view
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'appkit-media-inline-image-rendering-available-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'misskey--authenticated-account)
                #'misskey-timeline-test--authenticated-account)
               ((symbol-function 'misskey-http-read)
                (lambda (_endpoint _parameters callback &rest _options)
                  (funcall
                   callback
                   (list
                    (misskey-timeline-test--note
                     "n1" "body" :avatar-url avatar-url)))))
               ((symbol-function 'appkit-chat-avatar-prefixes)
                (lambda (image _fallback &rest _options)
                  (push image avatar-images)
                  '(:header "H " :first-body "B " :rest-body "R ")))
               ((symbol-function 'appkit-media-image-cache-existing-file)
                (lambda (_cache-base) cache-file))
               ((symbol-function 'appkit-media-file-present-p)
                (lambda (file) (and cache-file (equal file cache-file))))
               ((symbol-function 'appkit-media-circular-image-from-file)
                (lambda (file _pixel-size)
                  (and (equal file cache-file) avatar-image)))
               ((symbol-function 'appkit-media-cache-image-resource-async)
                (lambda (resource cache-base success _error &rest _options)
                  (setq requested-resource resource
                        requested-cache-base cache-base
                        download-success success)
                  'transfer))
               ((symbol-function 'appkit-media-transfer-p)
                (lambda (object) (eq object 'transfer)))
               ((symbol-function 'appkit-media-cancel-transfer) #'ignore))
            (setq view (call-interactively #'misskey-home)
                  buffer (appkit-view-buffer view))
            (misskey-timeline-test--flush view)
            (should (equal (alist-get 'url requested-resource) avatar-url))
            (should
             (string-suffix-p
              (secure-hash 'sha256 avatar-url) requested-cache-base))
            (should (functionp download-success))
            (with-current-buffer buffer
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (should (equal (appkit-discussion-key-at-point) "n1"))
              (should-not (car avatar-images))
              (setq cache-file "/tmp/misskey-avatar.png")
              (funcall download-success cache-file)
              (misskey-timeline-test--flush view)
              (should (equal (car avatar-images) avatar-image))
              (should (equal (appkit-discussion-key-at-point) "n1"))
              (let ((entry
                     (gethash
                      (list :avatar avatar-url)
                      (appkit-app-resource-store (appkit-view-app view)))))
                (should (eq (plist-get entry :status) 'ready))
                (should-not (plist-get entry :handle))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-home-loads-media-preview-with-stable-row-position ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey-timeline-show-avatars nil)
        (misskey-timeline-show-media t)
        (appkit-media-transfer-concurrency 2)
        (misskey--apps (make-hash-table :test #'equal))
        (media-file (misskey-timeline-test--file "f1"))
        (media-image '(image :type png :data "preview"))
        cache-file
        download-success
        inserted-images
        requested-resource
        requested-cache-base
        view
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'appkit-media-inline-image-rendering-available-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'misskey--authenticated-account)
                #'misskey-timeline-test--authenticated-account)
               ((symbol-function 'misskey-http-read)
                (lambda (_endpoint _parameters callback &rest _options)
                  (funcall
                   callback
                   (list
                    (misskey-timeline-test--note
                     "n1" "body" :files (list media-file))))))
               ((symbol-function 'appkit-media-image-cache-existing-file)
                (lambda (_cache-base) cache-file))
               ((symbol-function 'appkit-media-file-present-p)
                (lambda (file) (and cache-file (equal file cache-file))))
               ((symbol-function 'appkit-media-preview-image-from-file)
                (lambda (file _width _height)
                  (and (equal file cache-file) media-image)))
               ((symbol-function 'appkit-media-insert-image-slices)
                (lambda (image &rest _arguments)
                  (push image inserted-images)
                  (insert "[preview]")))
               ((symbol-function 'appkit-media-cache-image-resource-async)
                (lambda (resource cache-base success _error &rest _options)
                  (setq requested-resource resource
                        requested-cache-base cache-base
                        download-success success)
                  'transfer))
               ((symbol-function 'appkit-media-transfer-p)
                (lambda (object) (eq object 'transfer)))
               ((symbol-function 'appkit-media-cancel-transfer) #'ignore))
            (setq view (call-interactively #'misskey-home)
                  buffer (appkit-view-buffer view))
            (misskey-timeline-test--flush view)
            (should
             (equal (alist-get 'url requested-resource)
                    (alist-get 'thumbnailUrl media-file)))
            (should
             (string-suffix-p
              (secure-hash 'sha256 (alist-get 'thumbnailUrl media-file))
              requested-cache-base))
            (should (functionp download-success))
            (with-current-buffer buffer
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (should (equal (appkit-discussion-key-at-point) "n1"))
              (should (string-match-p "loading preview" (buffer-string)))
              (setq cache-file "/tmp/misskey-media.webp")
              (funcall download-success cache-file)
              (misskey-timeline-test--flush view)
              (should (equal (car inserted-images) media-image))
              (should (equal (appkit-discussion-key-at-point) "n1"))
              (let ((entry
                     (gethash
                      (list :media (alist-get 'id media-file))
                      (appkit-app-resource-store (appkit-view-app view)))))
                (should (eq (plist-get entry :status) 'ready))
                (should-not (plist-get entry :handle))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-home-reveals-sensitive-media-without-content-warning ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey-timeline-show-avatars nil)
        (misskey-timeline-show-media t)
        (misskey--apps (make-hash-table :test #'equal))
        (media-file (misskey-timeline-test--file "f1" t))
        (preview-requests 0)
        view
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'appkit-media-inline-image-rendering-available-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'misskey--authenticated-account)
                #'misskey-timeline-test--authenticated-account)
               ((symbol-function 'misskey-http-read)
                (lambda (_endpoint _parameters callback &rest _options)
                  (funcall
                   callback
                   (list
                    (misskey-timeline-test--note
                     "n1" "visible body" :files (list media-file))))))
               ((symbol-function 'appkit-media-image-cache-existing-file)
                (lambda (_cache-base) nil))
               ((symbol-function 'appkit-media-cache-image-resource-async)
                (lambda (&rest _arguments)
                  (cl-incf preview-requests)
                  nil)))
            (setq view (call-interactively #'misskey-home)
                  buffer (appkit-view-buffer view))
            (misskey-timeline-test--flush view)
            (should (= 0 preview-requests))
            (with-current-buffer buffer
              (should (string-match-p "\\[sensitive media\\]"
                                      (buffer-string)))
              (should-not (string-match-p "loading preview"
                                          (buffer-string)))
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (misskey-render-toggle-content-warning)
              (should (= 1 preview-requests))
              (misskey-timeline-test--flush view)
              (should-not (string-match-p "\\[sensitive media\\]"
                                          (buffer-string)))
              (should (string-match-p "loading preview"
                                      (buffer-string))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-media-sensitive-prefetch-uses-outer-row-reveal-identity ()
  (let* ((file (misskey-timeline-test--file "guarded" t))
         (target
          (misskey-timeline-test--note
           "target" "target body" :files (list file)))
         (pure-renote
          (misskey-timeline-test--note
           "pure-wrapper" nil :renote target))
         (quote
          (misskey-timeline-test--note
           "quote-wrapper" "comment" :renote target))
         (revealed-content (make-hash-table :test #'equal))
         (view
          (appkit-view--create
           :state (list :revealed-content revealed-content)
           :alive-p t))
         requested)
    (cl-letf (((symbol-function 'appkit-view-live-p)
               (lambda (_view) t))
              ((symbol-function 'misskey-media-request-avatar) #'ignore)
              ((symbol-function 'misskey-media-request-file)
               (lambda (_view requested-file)
                 (push requested-file requested))))
      (dolist (note (list pure-renote quote))
        (clrhash revealed-content)
        (setq requested nil)
        (puthash (misskey-note-id target) t revealed-content)
        (misskey-media-prefetch-notes view (list note))
        (should-not requested)
        (puthash (misskey-note-id note) t revealed-content)
        (misskey-media-prefetch-notes view (list note))
        (should (equal (list file) requested))))))

(ert-deftest misskey-home-shares-avatar-transfer-until-app-stops ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey-timeline-show-avatars t)
        (appkit-media-transfer-concurrency 2)
        (misskey--apps (make-hash-table :test #'equal))
        (avatar-url "https://cdn.example/alice.png")
        (cancellations 0)
        view
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'appkit-media-inline-image-rendering-available-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'misskey--authenticated-account)
                #'misskey-timeline-test--authenticated-account)
               ((symbol-function 'misskey-http-read)
                (lambda (_endpoint _parameters callback &rest _options)
                  (funcall
                   callback
                   (list
                    (misskey-timeline-test--note
                     "n1" "body" :avatar-url avatar-url)))))
               ((symbol-function 'appkit-chat-avatar-prefixes)
                (lambda (&rest _arguments)
                  '(:header "H " :first-body "B " :rest-body "R ")))
               ((symbol-function 'appkit-media-image-cache-existing-file)
                (lambda (_cache-base) nil))
               ((symbol-function 'appkit-media-cache-image-resource-async)
                (lambda (&rest _arguments) 'transfer))
               ((symbol-function 'appkit-media-transfer-p)
                (lambda (object) (eq object 'transfer)))
               ((symbol-function 'appkit-media-cancel-transfer)
                (lambda (_transfer)
                  (cl-incf cancellations))))
            (setq view (call-interactively #'misskey-home)
                  buffer (appkit-view-buffer view))
            (misskey-timeline-test--flush view)
            (let ((entry
                   (gethash
                    (list :avatar avatar-url)
                    (appkit-app-resource-store (appkit-view-app view)))))
              (should (eq (plist-get entry :status) 'pending))
              (should (plist-get entry :handle)))
            (appkit-kill-view view t)
            (should (= cancellations 0))
            (misskey-stop)
            (should (= cancellations 1))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-media-renders-audio-as-accessible-action-row ()
  (let* ((misskey-timeline-show-media t)
         (file '((id . "audio-1")
                 (name . "track.mp3")
                 (type . "audio/mpeg")
                 (url . "https://cdn.example/track.mp3")))
         (note (misskey-timeline-test--note
                "n1" "audio" :files (list file))))
    (should (equal (misskey-note-media-files note) (list file)))
    (with-temp-buffer
      (misskey-media-insert-file nil file "" nil nil)
      (should (string-match-p "\\[audio\\]" (buffer-string)))
      (should (keymapp (get-text-property (point-min) 'keymap)))
      (should (equal (get-text-property (point-min) 'help-echo)
                     "Open Misskey media")))))

(ert-deftest misskey-media-renders-disabled-preview-placeholder ()
  (let ((misskey-timeline-show-media nil)
        (file (misskey-timeline-test--file "disabled")))
    (with-temp-buffer
      (misskey-media-insert-file nil file "" nil nil)
      (should (string-match-p "\\[preview disabled\\]"
                              (buffer-string))))))

(ert-deftest misskey-media-renders-failed-preview-until-refresh ()
  (let ((misskey-timeline-show-media t)
        (file (misskey-timeline-test--file "failed")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'appkit-view-live-p)
                 (lambda (_view) t))
                ((symbol-function 'misskey-media--entry)
                 (lambda (_view _key) '(:status failed)))
                ((symbol-function 'misskey-media-preview-image)
                 (lambda (_view _file) nil)))
        (misskey-media-insert-file 'view file "" nil nil))
      (should (string-match-p "\\[preview failed; refresh to retry\\]"
                              (buffer-string))))))


(ert-deftest misskey-media-presents-generic-drive-files-as-open-actions ()
  (let* ((pdf '((id . "pdf-1")
                (name . "manual.pdf")
                (type . "application/pdf")
                (url . "https://cdn.example/manual.pdf")))
         (archive '((id . "zip-1")
                    (name . "bundle.zip")
                    (type . "application/zip")
                    (url . "https://cdn.example/bundle.zip")))
         (note (misskey-timeline-test--note
                "n1" "files" :files (list pdf archive))))
    (should (equal (list pdf archive) (misskey-note-media-files note)))
    (dolist (file (list pdf archive))
      (should-not (misskey-file-preview-url file))
      (with-temp-buffer
        (misskey-media-insert-file nil file "" nil nil)
        (should (keymapp (get-text-property (point-min) 'keymap)))
        (should (equal "Open Misskey media"
                       (get-text-property (point-min) 'help-echo)))
        (should-not (string-match-p "loading preview" (buffer-string)))))))

(ert-deftest misskey-media-rejects-unsafe-https-boundary-values ()
  (let ((urls
         (list
          "http://cdn.example/manual.pdf"
          "https://user@cdn.example/manual.pdf"
          (concat "https://cdn.example/manual.pdf\""
                  "\n--output /tmp/injected")))
        dispatched)
    (cl-letf (((symbol-function 'appkit-media-open-resource)
               (lambda (&rest _)
                 (setq dispatched t)))
              ((symbol-function 'appkit-view-live-p)
               (lambda (_view) t))
              ((symbol-function 'misskey-media--cache-base)
               (lambda (&rest _)
                 (setq dispatched t))))
      (dolist (url urls)
        (let ((file `((id . "pdf-1")
                      (name . "manual.pdf")
                      (type . "application/pdf")
                      (url . ,url))))
          (should-not (misskey-file-original-url file))
          (should-error
           (misskey-media-open-file 'view file)
           :type 'user-error)
          (should-not
           (misskey-media-request-resource
            'view '(:media "pdf-1") url "media"))))
      (let* ((malicious (car (last urls)))
             (preview `((type . "image/png")
                        (thumbnailUrl . ,malicious)))
             (note
              (misskey-timeline-test--note
               "n1" "avatar" :avatar-url malicious)))
        (should-not (misskey-file-preview-url preview))
        (should-not (misskey-note-avatar-url note)))
      (should-not dispatched))))

(ert-deftest misskey-media-open-uses-original-not-distinct-thumbnail-cache ()
  (let* ((file (misskey-timeline-test--file "original"))
         (thumbnail (alist-get 'thumbnailUrl file))
         (original (alist-get 'url file))
         (entry `(:status ready :source ,thumbnail :file "/tmp/thumbnail"))
         opened-resource)
    (cl-letf (((symbol-function 'misskey-media--entry)
               (lambda (_view _key) entry))
              ((symbol-function 'appkit-media-open-resource)
               (lambda (resource &rest _)
                 (setq opened-resource resource))))
      (misskey-media-open-file 'view file)
      (should (equal original (alist-get 'url opened-resource)))
      (should-not (alist-get 'file opened-resource))
      (setf (plist-get entry :source) original
            (plist-get entry :file) "/tmp/original")
      (misskey-media-open-file 'view file)
      (should (equal "/tmp/original"
                     (alist-get 'file opened-resource))))))

(ert-deftest misskey-media-shared-retry-invalidates-all-live-views ()
  (let* ((app (appkit-app-start 'misskey :id 'media-retry
                                :shutdown #'ignore))
         (first-buffer (generate-new-buffer " *misskey-media-first*"))
         (second-buffer (generate-new-buffer " *misskey-media-second*"))
         first-view
         second-view
         callbacks
         invalidated)
    (unwind-protect
        (progn
          (with-current-buffer first-buffer
            (setq first-view
                  (appkit-attach-view
                   :app app :id 'first :mode major-mode)))
          (with-current-buffer second-buffer
            (setq second-view
                  (appkit-attach-view
                   :app app :id 'second :mode major-mode)))
          (cl-letf
              (((symbol-function 'appkit-media-image-cache-existing-file)
                (lambda (_cache-base) nil))
               ((symbol-function 'appkit-media-cache-image-resource-async)
                (lambda (_resource _cache-base success error &rest _)
                  (push (cons success error) callbacks)
                  nil))
               ((symbol-function 'appkit-media-file-present-p)
                (lambda (file) (equal file "/tmp/retry-ready.png")))
               ((symbol-function 'appkit-request-sync)
                (lambda (view &rest _)
                  (cl-pushnew view invalidated :test #'eq))))
            (misskey-media-request-resource
             first-view '(:media "shared")
             "https://cdn.example/shared.png" "media")
            (funcall (cdr (car callbacks)) "failed")
            (setq invalidated nil)
            (misskey-media-request-resource
             first-view '(:media "shared")
             "https://cdn.example/shared.png" "media")
            (should (= 2 (length invalidated)))
            (should (memq first-view invalidated))
            (should (memq second-view invalidated))
            (setq invalidated nil)
            (funcall (car (car callbacks)) "/tmp/retry-ready.png")
            (should (= 2 (length invalidated)))
            (should (memq first-view invalidated))
            (should (memq second-view invalidated))))
      (when (appkit-app-live-p app)
        (appkit-app-close app))
      (when (buffer-live-p first-buffer)
        (kill-buffer first-buffer))
      (when (buffer-live-p second-buffer)
        (kill-buffer second-buffer)))))
(defun misskey-timeline-test--isolated-app ()
  "Return an isolated live app for renderer state tests."
  (let ((account
         (misskey--account-create
          :origin "https://example.social"
          :auth-source-user "TOKEN"
          :remote-user-id "self")))
    (appkit-app-start
     'misskey :id (list 'timeline-test (make-symbol "app"))
     :state (misskey--make-session account))))

(ert-deftest misskey-note-pure-renote-rejects-file-only-quotes ()
  (let* ((target
          (misskey-timeline-test--note
           "target" "target body" :name "Bob" :username "bob"))
         (wrapper
          (misskey-timeline-test--note
           "wrapper" nil :renote target :name "Alice" :username "alice"))
         (file-quote
          (misskey-timeline-test--note
           "quote" nil :renote target
           :files (list (misskey-timeline-test--file "file"))
           :name "Carol" :username "carol")))
    (should (misskey-note-pure-renote-p wrapper))
    (should (eq (misskey-note-display-note wrapper) target))
    (should-not (misskey-note-quoted-note wrapper))
    (should-not (misskey-note-pure-renote-p file-quote))
    (should (eq (misskey-note-display-note file-quote) file-quote))
    (should (eq (misskey-note-quoted-note file-quote) target))))

(ert-deftest misskey-render-author-properties-belong-to-visible-spans ()
  (let* ((app (misskey-timeline-test--isolated-app))
         (account (misskey--session-account (misskey--session app)))
         (target
          (misskey-timeline-test--note
           "target" "target body" :name "Bob" :username "bob"
           :created-at "2025-08-12T10:00:00.000Z"))
         (wrapper
          (misskey-timeline-test--note
           "wrapper" nil :renote target :name "Alice" :username "alice"
           :created-at "2026-08-13T11:00:00.000Z"))
         (quote
          (misskey-timeline-test--note
           "quote" nil :renote target
           :files (list (misskey-timeline-test--file "file"))
           :name "Carol" :username "carol"))
         view)
    (unwind-protect
        (with-temp-buffer
          (setq view
                (appkit-attach-view
                 :app app :id 'authors :mode major-mode
                 :state
                 (list :account account
                       :revealed-content (make-hash-table :test #'equal))))
          (cl-letf (((symbol-function 'misskey-media-avatars-enabled-p)
                     (lambda () nil))
                    ((symbol-function 'misskey-media-insert-note-files)
                     #'ignore))
            (appkit-discussion-insert-entry
             (misskey-render-note-entry view wrapper)
             :avatar-p nil)
            (appkit-discussion-insert-entry
             (misskey-render-note-entry view quote)
             :avatar-p nil))
          (should (equal (misskey-render-heading wrapper) "Bob @bob"))
          (should
           (string-match-p
            (regexp-quote (misskey-render-time target)) (buffer-string)))
          (should-not
           (string-match-p
            (regexp-quote (misskey-render-time wrapper)) (buffer-string)))
          (goto-char (point-min))
          (search-forward "renoted by Alice @alice")
          (should (= 1 (line-number-at-pos)))
          (should (equal (get-text-property
                          (1- (point)) misskey-user-id-property)
                         "u-alice"))
          (should
           (equal
            (save-excursion
              (goto-char (1- (point)))
              (misskey-user-id (misskey-actions--user-at-point)))
            "u-alice"))
          (search-forward "Bob @bob")
          (should (= 2 (line-number-at-pos)))
          (should (equal (get-text-property
                          (1- (point)) misskey-user-id-property)
                         "u-bob"))
          (should
           (equal
            (save-excursion
              (goto-char (1- (point)))
              (misskey-user-id (misskey-actions--user-at-point)))
            "u-bob"))
          (search-forward "Carol @carol")
          (should (equal (get-text-property
                          (1- (point)) misskey-user-id-property)
                         "u-carol"))
          (search-forward "Quoting Bob @bob")
          (should (equal (get-text-property
                          (1- (point)) misskey-user-id-property)
                         "u-bob"))
          (should
           (equal
            (save-excursion
              (goto-char (1- (point)))
              (misskey-user-id (misskey-actions--user-at-point)))
            "u-bob"))
          (search-forward "target body")
          (should-not
           (get-text-property (1- (point)) misskey-user-property))
          (should
           (eq (get-text-property
                (1- (point)) misskey-note-property)
               quote)))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

(ert-deftest misskey-render-deletion-propagates-into-nested-renotes ()
  (let* ((app (misskey-timeline-test--isolated-app))
         (target
          (misskey-timeline-test--note
           "target" "deleted nested body" :name "Bob" :username "bob"))
         (wrapper
          (misskey-timeline-test--note
           "wrapper" nil :renote target :name "Alice" :username "alice"))
         (quote
          (misskey-timeline-test--note
           "quote" "outer survives" :renote target
           :name "Carol" :username "carol"))
         (rows nil))
    (unwind-protect
        (progn
          (misskey-set-note-state-values app "target" :deleted-p t)
          (setq rows (misskey-render-project-notes (list wrapper quote) app))
          (should
           (equal (mapcar #'appkit-projection-row-key rows) '("quote")))
          (with-temp-buffer
            (let ((view
                   (appkit-attach-view
                    :app app :id 'deletion :mode major-mode
                    :state
                    (list :account
                          (misskey--session-account (misskey--session app))
                          :revealed-content
                          (make-hash-table :test #'equal)))))
              (cl-letf (((symbol-function 'misskey-media-avatars-enabled-p)
                         (lambda () nil))
                        ((symbol-function 'misskey-media-insert-note-files)
                         #'ignore))
                (appkit-discussion-insert-entry
                 (misskey-render-note-entry view quote) :avatar-p nil))
              (should (string-match-p "outer survives" (buffer-string)))
              (should-not
               (string-match-p "deleted nested body" (buffer-string))))))
      (when (appkit-app-live-p app)
        (appkit-app-close app)))))

(provide 'misskey-timeline-test)

;;; misskey-timeline-test.el ends here
