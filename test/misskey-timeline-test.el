;;; misskey-timeline-test.el --- Tests for Misskey home timelines -*- lexical-binding: t; -*-

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
            (let ((state (appkit-view-state view)))
              (should (equal (car captured) "notes/timeline"))
              (should (equal (cadr captured)
                             '(:limit 20 :allowPartial t)))
              (should (eq (nth 2 captured) view))
              (should (equal (nth 3 captured)
                             (plist-get state :account)))
              (should (eq (plist-get state :phase) 'ready))
              (should (equal (appkit-projection-keys view)
                             '("n1" "r1"))))
            (with-current-buffer buffer
              (should (eq major-mode 'misskey-timeline-mode))
              (should buffer-read-only)
              (should (equal (buffer-name)
                             "*misskey home: alice@example.social*"))
              (should (eq (lookup-key misskey-timeline-mode-map (kbd "g"))
                          #'misskey-timeline-refresh))
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
            (should (eq (plist-get (appkit-view-state view) :phase)
                        'initial))
            (funcall
             (car callbacks)
             (list (misskey-timeline-test--note "n1" "first")
                   (misskey-timeline-test--note "n2" "second")))
            (with-current-buffer buffer
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (appkit-discussion-next-entry)
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (misskey-timeline-refresh)
              (should (eq (plist-get (appkit-view-state view) :phase)
                          'refresh))
              (funcall
               (car callbacks)
               (list (misskey-timeline-test--note "n0" "new")
                     (misskey-timeline-test--note "n2" "updated")
                     (misskey-timeline-test--note "n1" "first")))
              (should (eq (plist-get (appkit-view-state view) :phase)
                          'ready))
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (should (equal (appkit-projection-keys view)
                             '("n0" "n2" "n1")))
              (misskey-timeline-refresh)
              (funcall (car callbacks) '(((id . "broken") (user))))
              (should (eq (plist-get (appkit-view-state view) :phase)
                          'error))
              (should (string-match-p "malformed note"
                                      (plist-get (appkit-view-state view)
                                                 :message)))
              (should (equal (appkit-projection-keys view)
                             '("n0" "n2" "n1")))
              (should (equal (appkit-discussion-key-at-point) "n2"))
              (let ((misskey-timeline-limit 0)
                    (request-count (length callbacks)))
                (should-error (misskey-timeline-refresh)
                              :type 'user-error)
                (should (= (length callbacks) request-count))))))
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
              (should (equal (car avatar-images) avatar-image))
              (should (equal (appkit-discussion-key-at-point) "n1"))
              (should-not
               (appkit-task-queue-pending-p
                (plist-get (appkit-view-state view) :avatar-queue))))))
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
              (should (equal (car inserted-images) media-image))
              (should (equal (appkit-discussion-key-at-point) "n1"))
              (should-not
               (appkit-task-queue-pending-p
                (plist-get (appkit-view-state view) :avatar-queue))))))
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
            (with-current-buffer buffer
              (should (string-match-p "\\[sensitive media\\]"
                                      (buffer-string)))
              (should-not (string-match-p "loading preview"
                                          (buffer-string)))
              (goto-char (point-min))
              (appkit-discussion-next-entry)
              (misskey-timeline-toggle-content-warning)
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
            (should
             (appkit-task-queue-pending-p
              (plist-get (appkit-view-state view) :avatar-queue)))
            (appkit-kill-view view t)
            (should (= cancellations 1))))
      (misskey-timeline-test--cleanup view buffer))))

(provide 'misskey-timeline-test)

;;; misskey-timeline-test.el ends here
