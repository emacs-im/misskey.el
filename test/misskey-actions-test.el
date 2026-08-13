;;; misskey-actions-test.el --- Tests for Misskey actions -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-actions)
(require 'misskey-compose)
(require 'misskey-search)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(defun misskey-actions-test--note ()
  "Return one valid actionable test note."
  '((id . "note-1")
    (text . "shared")
    (visibility . "public")
    (reactionCount . 1)
    (renoteCount . 2)
    (myReaction)
    (user . ((id . "u1") (username . "alice") (name . "Alice")))))

(ert-deftest misskey-actions-map-to-local-api-contracts ()
  (let ((note (misskey-actions-test--note))
        (user '((id . "u1") (username . "alice"))))
    (should (equal (misskey-actions--spec 'react note ":wave:")
                   '("notes/reactions/create"
                     (:noteId "note-1" :reaction ":wave:"))))
    (should (equal (misskey-actions--spec 'unreact note nil)
                   '("notes/reactions/delete" (:noteId "note-1"))))
    (should (equal (misskey-actions--spec 'favorite note nil)
                   '("notes/favorites/create" (:noteId "note-1"))))
    (should (equal (misskey-actions--spec 'unfavorite note nil)
                   '("notes/favorites/delete" (:noteId "note-1"))))
    (should (equal (misskey-actions--spec 'renote note nil)
                   '("notes/create" (:renoteId "note-1"))))
    (should (equal (misskey-actions--spec 'delete-note note nil)
                   '("notes/delete" (:noteId "note-1"))))
    (should (equal (misskey-actions--spec 'follow user nil)
                   '("following/create" (:userId "u1"))))
    (should (equal (misskey-actions--spec 'unfollow user nil)
                   '("following/delete" (:userId "u1"))))
    (should (equal (misskey-actions--spec 'cancel-follow user nil)
                   '("following/requests/cancel" (:userId "u1"))))))

(ert-deftest misskey-actions-success-invalidates-every-note-view ()
  (let* ((misskey--apps (make-hash-table :test #'equal))
         (misskey-search--serial 0)
         (account (misskey--account-create
                   :origin "https://example.social"
                   :auth-source-user "TOKEN"
                   :remote-user-id "self"))
         (app (misskey-actions-test--app))
         (note (misskey-actions-test--note))
         first second requests)
    (unwind-protect
        (save-window-excursion
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey-app)
                     (lambda (&optional _account) app))
                    ((symbol-function 'misskey-http-read)
                     (lambda (_endpoint _parameters callback &rest _options)
                       (funcall callback (list note))
                       'read-request)))
            (setq first (misskey-search "one" account)
                  second (misskey-search "two" account)))
          (appkit-sync-invalidations first)
          (appkit-sync-invalidations second)
          (cl-letf (((symbol-function 'message) #'ignore)
                    ((symbol-function 'misskey-app)
                     (lambda (&optional _account) app))
                    ((symbol-function 'misskey-http-post)
                     (lambda (endpoint parameters callback &rest _options)
                       (push (cons endpoint parameters) requests)
                       (funcall callback nil)
                       'write-request)))
            (misskey-actions-perform 'favorite note :account account)
            (appkit-sync-invalidations first)
            (appkit-sync-invalidations second)
            (dolist (view (list first second))
              (with-current-buffer (appkit-view-buffer view)
                (should (string-match-p "favorited" (buffer-string)))))
            (misskey-actions-perform
             'react note :value ":wave:" :account account)
            (appkit-sync-invalidations first)
            (appkit-sync-invalidations second)
            (dolist (view (list first second))
              (with-current-buffer (appkit-view-buffer view)
                (should (string-match-p "2 reactions" (buffer-string)))
                (should (string-match-p "your reaction :wave:"
                                        (buffer-string)))))
            (misskey-actions-perform 'delete-note note :account account)
            (appkit-sync-invalidations first)
            (appkit-sync-invalidations second)
            (should-not (appkit-projection-keys first))
            (should-not (appkit-projection-keys second)))
          (should (= (length requests) 3)))
      (when (appkit-view-p first) (appkit-kill-view first t))
      (when (appkit-view-p second) (appkit-kill-view second t))
      (when (appkit-app-live-p app) (appkit-stop-app app)))))

(ert-deftest misskey-actions-failure-does-not-install-state ()
  (let* ((misskey--apps (make-hash-table :test #'equal))
         (account (misskey--account-create
                   :origin "https://example.social"
                   :auth-source-user "TOKEN"
                   :remote-user-id "self"))
         (note (misskey-actions-test--note))
         (app (misskey-actions-test--app)))
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-app)
                   (lambda (&optional _account) app))
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint _parameters _callback &rest options)
                     (funcall (plist-get options :errback)
                              "Unknown remote outcome")
                     nil)))
          (misskey-actions-perform 'favorite note :account account)
          (should-not
           (misskey-note-state-value app "note-1" :favorited-p nil)))
      (when (appkit-app-live-p app) (appkit-stop-app app)))))

(ert-deftest misskey-actions-follow-installs-user-override ()
  (let* ((misskey--apps (make-hash-table :test #'equal))
         (account (misskey--account-create
                   :origin "https://example.social"
                   :auth-source-user "TOKEN"
                   :remote-user-id "self"))
         (user '((id . "u1") (username . "alice")))
         (app (misskey-actions-test--app)))
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-app)
                   (lambda (&optional _account) app))
                  ((symbol-function 'misskey-http-post)
                   (lambda (_endpoint _parameters callback &rest _options)
                     (funcall callback nil)
                     'request)))
          (misskey-actions-perform 'follow user :account account)
          (should
           (misskey-user-state-value app "u1" :following-p nil)))
      (when (appkit-app-live-p app) (appkit-stop-app app)))))

(defun misskey-actions-test--app ()
  "Return an isolated live Misskey app for mutation tests."
  (let ((account
         (misskey--account-create
          :origin "https://example.social"
          :auth-source-user "TOKEN"
          :remote-user-id "self")))
    (appkit-start-app
     'misskey :id (list 'actions-test (make-symbol "app"))
     :state (misskey--make-session account))))

(defun misskey-actions-test--capture-post (record-function)
  "Return a POST stub passing each request record to RECORD-FUNCTION."
  (lambda (endpoint parameters callback &rest options)
    (funcall
     record-function
     (list endpoint parameters callback (plist-get options :errback)))
    (make-symbol endpoint)))

(ert-deftest misskey-actions-inverse-note-writes-share-one-lane ()
  (let ((app (misskey-actions-test--app))
        (account (misskey--account-create
                  :origin "https://example.social"
                  :auth-source-user "TOKEN"
                  :remote-user-id "self"))
        (note (misskey-actions-test--note))
        requests)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-app)
                   (lambda (&optional _account) app))
                  ((symbol-function 'misskey-http-post)
                   (misskey-actions-test--capture-post
                    (lambda (record)
                      (setq requests (append requests (list record)))))))
          (misskey-actions-perform 'favorite note :account account)
          (misskey-actions-perform 'unfavorite note :account account)
          (should (= (length requests) 1))
          (funcall (nth 2 (nth 0 requests)) nil)
          (should (= (length requests) 2))
          (should (equal (mapcar #'car requests)
                         '("notes/favorites/create"
                           "notes/favorites/delete")))
          ;; A duplicated older callback cannot retire the newer inverse.
          (funcall (nth 2 (nth 0 requests)) nil)
          (should (= (length requests) 2))
          (funcall (nth 2 (nth 1 requests)) nil)
          (should-not
           (misskey-note-state-value
            app "note-1" :favorited-p 'missing)))
      (appkit-stop-app app))))

(ert-deftest misskey-actions-throwing-client-callback-cannot-strand-lane ()
  (let ((app (misskey-actions-test--app))
        (account (misskey--account-create
                  :origin "https://example.social"
                  :auth-source-user "TOKEN"
                  :remote-user-id "self"))
        (note (misskey-actions-test--note))
        requests)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-app)
                   (lambda (&optional _account) app))
                  ((symbol-function 'misskey-http-post)
                   (misskey-actions-test--capture-post
                    (lambda (record)
                      (setq requests (append requests (list record)))))))
          (misskey-actions-perform
           'favorite note :account account
           :callback (lambda (_payload) (error "Client callback failed")))
          (funcall (nth 2 (nth 0 requests)) nil)
          (misskey-actions-perform 'unfavorite note :account account)
          (should (= (length requests) 2))
          (should (equal (car (nth 1 requests))
                         "notes/favorites/delete"))
          (funcall (nth 2 (nth 1 requests)) nil)
          (should-not
           (misskey-note-state-value
            app "note-1" :favorited-p 'missing)))
      (appkit-stop-app app))))

(ert-deftest misskey-actions-latest-inverse-survives-first-write-error ()
  (let ((app (misskey-actions-test--app))
        (account (misskey--account-create
                  :origin "https://example.social"
                  :auth-source-user "TOKEN"
                  :remote-user-id "self"))
        (note (misskey-actions-test--note))
        requests)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-app)
                   (lambda (&optional _account) app))
                  ((symbol-function 'misskey-http-post)
                   (misskey-actions-test--capture-post
                    (lambda (record)
                      (setq requests (append requests (list record)))))))
          (misskey-actions-perform
           'react note :value ":wave:" :account account)
          (misskey-actions-perform 'unreact note :account account)
          (should (= (length requests) 1))
          (funcall (nth 3 (nth 0 requests)) "Unknown remote outcome")
          (should (= (length requests) 2))
          (should (equal (mapcar #'car requests)
                         '("notes/reactions/create"
                           "notes/reactions/delete")))
          (funcall (nth 2 (nth 1 requests)) nil)
          (should-not
           (misskey-note-state-value
            app "note-1" :my-reaction "unexpected"))
          (should
           (= (misskey-note-state-value
               app "note-1" :reaction-count 0)
              1)))
      (appkit-stop-app app))))

(ert-deftest misskey-actions-cancels-pending-follow-in-shared-lane ()
  (let ((app (misskey-actions-test--app))
        (account (misskey--account-create
                  :origin "https://example.social"
                  :auth-source-user "TOKEN"
                  :remote-user-id "self"))
        (user '((id . "u1") (username . "alice") (isLocked . t)))
        requests)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-app)
                   (lambda (&optional _account) app))
                  ((symbol-function 'misskey-http-post)
                   (misskey-actions-test--capture-post
                    (lambda (record)
                      (setq requests (append requests (list record)))))))
          (misskey-actions-perform 'follow user :account account)
          (misskey-actions-perform 'unfollow user :account account)
          (should (= (length requests) 1))
          (funcall
           (nth 2 (nth 0 requests))
           '((isFollowing) (hasPendingFollowRequestFromYou . t)))
          (should (= (length requests) 2))
          (should (equal (car (nth 1 requests))
                         "following/requests/cancel"))
          (funcall (nth 2 (nth 1 requests)) nil)
          (should-not
           (misskey-user-state-value app "u1" :following-p t))
          (should-not
           (misskey-user-state-value app "u1" :follow-pending-p t)))
      (appkit-stop-app app))))

(ert-deftest misskey-actions-delete-keeps-wrapper-while-compose-unwraps ()
  (let* ((display
          '((id . "display")
            (text . "body")
            (user . ((id . "bob") (username . "bob")))))
         (wrapper
          `((id . "wrapper")
            (text)
            (renoteId . "display")
            (renote . ,display)
            (user . ((id . "alice") (username . "alice")))))
         performed
         compose)
    (with-temp-buffer
      (insert
       (propertize "row" misskey-note-property wrapper))
      (goto-char (point-min))
      (should (eq (misskey-actions--note-at-point) wrapper))
      (should (eq (misskey-actions--note-at-point t) display))
      (cl-letf (((symbol-function 'yes-or-no-p)
                 (lambda (&rest _) t))
                ((symbol-function 'misskey-actions--perform-note)
                 (lambda (&rest arguments) (setq performed arguments))))
        (misskey-delete-note-at-point))
      (should (equal performed '(delete-note nil t)))
      (cl-letf (((symbol-function 'misskey-compose-open)
                 (lambda (&optional account &rest arguments)
                   (setq compose (cons account arguments)))))
        (misskey-compose-reply-at-point)
        (should (equal (plist-get (cdr compose) :reply-id) "display"))
        (should (string-match-p "bob"
                                (plist-get (cdr compose) :target-label)))
        (misskey-compose-quote-at-point)
        (should (equal (plist-get (cdr compose) :renote-id) "display"))))))

(provide 'misskey-actions-test)

;;; misskey-actions-test.el ends here
