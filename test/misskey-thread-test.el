;;; misskey-thread-test.el --- Tests for Misskey threads -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey)
(require 'misskey-thread)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(cl-defun misskey-thread-test--note
    (id text &key reply-id (created-at "2026-08-13T00:00:00.000Z"))
  "Return a normalized test note with ID, TEXT, REPLY-ID, and CREATED-AT."
  `((id . ,id)
    (replyId . ,reply-id)
    (createdAt . ,created-at)
    (text . ,text)
    (visibility . "public")
    (localOnly . ,json-false)
    (repliesCount . 0)
    (renoteCount . 0)
    (reactionCount . 0)
    (files)
    (user (id . ,(concat "u-" id))
          (name . "Alice")
          (username . "alice")
          (host)
          (avatarUrl))))

(defun misskey-thread-test--cleanup (view)
  "Destroy test VIEW and stop Misskey sessions."
  (when (and (appkit-view-p view) (appkit-view-live-p view))
    (appkit-kill-view view t))
  (misskey-stop))

(ert-deftest misskey-thread-loads-chain-and-pages-direct-replies ()
  (let* ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (misskey-timeline-show-avatars nil)
         (misskey-timeline-show-media nil)
         (misskey--apps (make-hash-table :test #'equal))
         (root (misskey-thread-test--note "a1" "root"))
         (parent (misskey-thread-test--note "a2" "parent" :reply-id "a1"))
         (focus (misskey-thread-test--note "f" "focus" :reply-id "a2"))
         (first (misskey-thread-test--note "r1" "first" :reply-id "f"))
         (second (misskey-thread-test--note "r2" "second" :reply-id "f"))
         (reply-requests 0)
         requests
         view)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-read)
                   (lambda (endpoint parameters callback &rest _options)
                     (push (cons endpoint parameters) requests)
                     (pcase endpoint
                       ("notes/show" (funcall callback focus))
                       ("notes/conversation"
                        (funcall callback (list parent root)))
                       ("notes/replies"
                        (cl-incf reply-requests)
                        (funcall callback
                                 (pcase reply-requests
                                   (1 (list first))
                                   (2 (list second))
                                   (_ nil)))))
                     nil)))
          (setq view (misskey-thread-open "f"))
          (appkit-sync-invalidations view)
          (should (equal (appkit-projection-keys view)
                         '("a1" "a2" "f" "r1")))
          (with-current-buffer (appkit-view-buffer view)
            (should (equal (get-text-property
                            (point) appkit-discussion-key-property)
                           "f")))
          (should-not (appkit-view-pending-events-snapshot view))
          (should
           (equal (appkit-projection-row-context
                   (appkit-projection-row view "a2"))
                  '(:parent-key "a1" :depth 1 :connector continue)))
          (should
           (equal (appkit-projection-row-context
                   (appkit-projection-row view "f"))
                  '(:parent-key "a2" :depth 2 :connector end)))
          (should
           (equal (appkit-projection-row-context
                   (appkit-projection-row view "r1"))
                  '(:parent-key "f" :depth 3)))
          (with-current-buffer (appkit-view-buffer view)
            (goto-char (point-min))
            (let ((match
                   (text-property-search-forward
                    appkit-discussion-key-property "r1" #'equal)))
              (should match)
              (goto-char (prop-match-beginning match)))
            (appkit-view-enqueue-event view (list :position 'preserve))
            (appkit-request-sync
             view :structure t :part 'frame :position t))
          (appkit-sync-invalidations view)
          (with-current-buffer (appkit-view-buffer view)
            (should
             (equal (get-text-property
                     (point) appkit-discussion-key-property)
                    "r1")))
          (should-not (appkit-view-pending-events-snapshot view))
          (with-current-buffer (appkit-view-buffer view)
            (should (string-match-p "root" (buffer-string)))
            (should (string-match-p "focus" (buffer-string)))
            (misskey-thread-load-more))
          (appkit-sync-invalidations view)
          (should (equal (appkit-projection-keys view)
                         '("a1" "a2" "f" "r1" "r2")))
          (should
           (cl-find-if
            (lambda (request)
              (equal (plist-get (cdr request) :untilId) "r1"))
            requests))
          (with-current-buffer (appkit-view-buffer view)
            (misskey-thread-load-more))
          (appkit-sync-invalidations view)
          (should (plist-get (appkit-view-state view)
                             :replies-exhausted-p))
          (with-current-buffer (appkit-view-buffer view)
            (should-error (misskey-thread-load-more) :type 'user-error)))
      (misskey-thread-test--cleanup view))))

(ert-deftest misskey-thread-malformed-final-stage-fails-atomically ()
  (let* ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (misskey-timeline-show-avatars nil)
         (misskey-timeline-show-media nil)
         (misskey--apps (make-hash-table :test #'equal))
         (parent (misskey-thread-test--note "p" "parent"))
         (focus (misskey-thread-test--note "f" "focus" :reply-id "p"))
         callbacks
         view)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-read)
                   (lambda (endpoint _parameters callback &rest _options)
                     (push (cons endpoint callback) callbacks)
                     (list endpoint))))
          (setq view (misskey-thread-open "f"))
          (let ((state (appkit-view-state view)))
            (funcall (cdr (assoc "notes/show" callbacks)) focus)
            (should-not (plist-get state :focus))
            (funcall (cdr (assoc "notes/conversation" callbacks))
                     (list parent))
            (should-not (plist-get state :focus))
            (should-not (plist-get state :ancestors))
            (funcall (cdr (assoc "notes/replies" callbacks))
                     '(((text . "missing identity"))))
            (should (eq (plist-get state :phase) 'error))
            (should-not (plist-get state :request-token))
            (should-not (plist-get state :stage-token))
            (should-not (plist-get state :focus))
            (should-not (plist-get state :ancestors))
            (should-not (plist-get state :replies))
            (should-not
             (gethash misskey-thread--request-key
                      (appkit-view-request-table view)))))
      (misskey-thread-test--cleanup view))))

(ert-deftest misskey-thread-older-requires-existing-reply-cursor ()
  (let* ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (misskey-timeline-show-avatars nil)
         (misskey-timeline-show-media nil)
         (misskey--apps (make-hash-table :test #'equal))
         (focus (misskey-thread-test--note "f" "focus"))
         (reads 0)
         view)
    (unwind-protect
        (cl-letf (((symbol-function 'message) #'ignore)
                  ((symbol-function 'misskey-http-read)
                   (lambda (endpoint _parameters callback &rest _options)
                     (cl-incf reads)
                     (funcall callback
                              (pcase endpoint
                                ("notes/show" focus)
                                (_ nil)))
                     nil)))
          (setq view (misskey-thread-open "f"))
          (setf (plist-get (appkit-view-state view) :replies-exhausted-p)
                nil)
          (with-current-buffer (appkit-view-buffer view)
            (should-error (misskey-thread-load-more) :type 'user-error))
          (should (= reads 3))
          (should-not (plist-get (appkit-view-state view) :request-token)))
      (misskey-thread-test--cleanup view))))

(provide 'misskey-thread-test)

;;; misskey-thread-test.el ends here
