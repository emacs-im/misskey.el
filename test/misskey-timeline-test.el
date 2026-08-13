;;; misskey-timeline-test.el --- Tests for Misskey timelines -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey)
(require 'misskey-timeline)

(cl-defun misskey-timeline-test--note
    (id text &key cw local-only files renote avatar-url
        (name "Alice") (username "alice"))
  "Return a normalized test note with ID and TEXT.

CW, LOCAL-ONLY, FILES, RENOTE, AVATAR-URL, NAME, and USERNAME supply optional
fields."
  `((id . ,id)
    (createdAt . "2026-08-13T00:00:00.000Z")
    (text . ,text)
    (cw . ,cw)
    (visibility . "public")
    (localOnly . ,local-only)
    (repliesCount . 1)
    (renoteCount . 2)
    (reactionCount . 3)
    (files . ,files)
    (renote . ,renote)
    (user (name . ,name)
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

(ert-deftest misskey-home-renders-keyed-notes-through-installed-command ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey--apps (make-hash-table :test #'equal))
        captured view buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey-auth--ensure-token)
                     (lambda (&optional _account) "TOKEN"))
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
              (should (eq (nth 2 captured) view))
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
                          #'misskey-timeline-toggle-content-warning))
              (should (eq (lookup-key misskey-timeline-mode-map (kbd "c"))
                          #'misskey-timeline-compose))
              (should (string-match-p "CW: Spoiler" (buffer-string)))
              (should (string-match-p "\\[RET to reveal\\]"
                                      (buffer-string)))
              (should-not (string-match-p "hidden body" (buffer-string)))
              (should (string-match-p "Local only" (buffer-string)))
              (should (string-match-p "2 attachments" (buffer-string)))
              (should (string-match-p "Bob @bob renoted Alice @alice"
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

(ert-deftest misskey-home-refresh-preserves-position-and-rejects-bad-data ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey--apps (make-hash-table :test #'equal))
        callbacks view buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey-auth--ensure-token)
                     (lambda (&optional _account) "TOKEN"))
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
               ((symbol-function 'misskey-auth--ensure-token)
                (lambda (&optional _account) "TOKEN"))
               ((symbol-function 'misskey-http-read)
                (lambda (endpoint _parameters callback &rest _options)
                  (let ((request (make-symbol endpoint)))
                    (push (list endpoint callback request) requests)
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
                               (appkit-view-app view)))))))))
      (misskey-timeline-test--cleanup view buffer)))))

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
               ((symbol-function 'misskey-auth--ensure-token)
                (lambda (&optional _account) "TOKEN"))
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
              (misskey-timeline-load-more)
              (should
               (equal (cadar requests)
                      '(:limit 20 :allowPartial t :untilId "n2")))
              (should (eq (nth 3 (car requests)) view))
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
               ((symbol-function 'display-images-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'misskey-auth--ensure-token)
                (lambda (&optional _account) "TOKEN"))
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
              (should-not
               (appkit-task-queue-pending-p
                misskey-timeline--avatar-queue)))))
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
               ((symbol-function 'display-images-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'misskey-auth--ensure-token)
                (lambda (&optional _account) "TOKEN"))
               ((symbol-function 'misskey-http-read)
                (lambda (_endpoint _parameters callback &rest _options)
                  (funcall
                   callback
                   (list
                    (misskey-timeline-test--note
                     "n1" "body" :files (list media-file))))))
               ((symbol-function 'appkit-media-image-cache-existing-file)
                (lambda (_cache-base) cache-file))
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
              (should-not
               (appkit-task-queue-pending-p
                misskey-timeline--avatar-queue)))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-home-hides-sensitive-media-until-revealed ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (misskey-timeline-show-avatars nil)
        (misskey-timeline-show-media t)
        (misskey--apps (make-hash-table :test #'equal))
        (media-file (misskey-timeline-test--file "f1" t))
        view
        buffer)
    (unwind-protect
        (save-window-excursion
          (cl-letf
              (((symbol-function 'message) #'ignore)
               ((symbol-function 'display-images-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'misskey-auth--ensure-token)
                (lambda (&optional _account) "TOKEN"))
               ((symbol-function 'misskey-http-read)
                (lambda (_endpoint _parameters callback &rest _options)
                  (funcall
                   callback
                   (list
                    (misskey-timeline-test--note
                     "n1" "hidden body"
                     :cw "Spoiler" :files (list media-file))))))
               ((symbol-function 'appkit-media-image-cache-existing-file)
                (lambda (_cache-base) nil))
               ((symbol-function 'appkit-media-cache-image-resource-async)
                (lambda (&rest _arguments) nil)))
            (setq view (call-interactively #'misskey-home)
                  buffer (appkit-view-buffer view))
            (misskey-timeline-test--flush view)
            (with-current-buffer buffer
              (should (string-match-p "\\[sensitive media\\]"
                                      (buffer-string)))
              (should-not (string-match-p "loading preview"
                                          (buffer-string)))
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (misskey-timeline-toggle-content-warning)
              (misskey-timeline-test--flush view)
              (should-not (string-match-p "\\[sensitive media\\]"
                                          (buffer-string)))
              (should (string-match-p "loading preview"
                                      (buffer-string))))))
      (misskey-timeline-test--cleanup view buffer))))

(ert-deftest misskey-home-cancels-avatar-transfer-with-view ()
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
               ((symbol-function 'display-images-p)
                (lambda (&rest _arguments) t))
               ((symbol-function 'misskey-auth--ensure-token)
                (lambda (&optional _account) "TOKEN"))
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
            (with-current-buffer buffer
              (should
               (appkit-task-queue-pending-p
                misskey-timeline--avatar-queue)))
            (appkit-kill-view view t)
            (should (= cancellations 1))))
      (misskey-timeline-test--cleanup view buffer))))

(provide 'misskey-timeline-test)

;;; misskey-timeline-test.el ends here
