;;; misskey-test-helper.el --- Isolated Misskey test credentials -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'auth-source)
(require 'misskey-core)

(defmacro misskey-test-with-session (&rest body)
  "Run BODY with synthetic credentials and isolated application ownership.
Unexpected credential persistence or transport is an error, never authorization."
  (declare (indent 0) (debug t))
  `(let ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (misskey--apps (make-hash-table :test #'equal))
         (misskey-timeline-show-avatars nil)
         (misskey-timeline-show-media nil))
     (cl-letf (((symbol-function 'misskey--stored-credential)
                (lambda (&optional account)
                  (let ((target (or account (misskey--current-account-locator))))
                    (unless (and (equal (misskey--account-origin target) "https://example.social")
                                 (member (misskey--account-auth-source-user target)
                                         '("alice" "TOKEN" "misskey.el" "credential-label")))
                      (error "Unconfigured synthetic Misskey account: %S" target))
                    (misskey--credential-create :token "TOKEN" :user-id "self"))))
               ((symbol-function 'auth-source-search)
                (lambda (&rest _) (error "Unexpected credential lookup in host fixture")))
               ((symbol-function 'misskey-auth-authorize)
                (lambda (&rest _) (error "Unexpected authorization in host fixture")))
               ((symbol-function 'misskey-auth--store-credential)
                (lambda (&rest _) (error "Unexpected credential persistence in host fixture")))
               ((symbol-function 'misskey-http--start-request)
                (lambda (&rest _) (error "Unmocked Misskey host transport")))
               ((symbol-function 'misskey-http-upload-file)
                (lambda (&rest _) (error "Unmocked Misskey upload transport")))
               ((symbol-function 'appkit-media-cache-image-resource-async)
                (lambda (&rest _) (error "Unmocked Misskey preview transport")))
               ((symbol-function 'misskey-http--public-read-sync)
                (lambda (&rest _) (error "Unmocked public Misskey transport"))))
       (unwind-protect (progn ,@body)
         (misskey-stop)))))

(defun misskey-test-visible-note-keys (surface)
  "Return the ordered note identities displayed in SURFACE."
  (with-current-buffer (appkit-surface-buffer surface)
    (let ((position (point-min)) keys)
      (while (< position (point-max))
        (when-let*
            ((key
              (get-text-property position
                                 appkit-discussion-key-property)))
          (unless (member key keys) (push key keys)))
        (setq position
              (next-single-property-change position
                                           appkit-discussion-key-property
                                           nil (point-max))))
      (nreverse keys))))

(cl-defun misskey-test-surface (&key app identity input (mode #'fundamental-mode) buffer)
  "Mount a canonical test Surface without a client presentation cache."
  (appkit-open-generated-surface
   (appkit-surface-type-create
    :name 'misskey-test :mode mode
    :init (lambda (context model)
            (appkit-next :model (plist-put model :address
                                           (appkit-transition-context-owner-address context))
                         :render appkit-render-none))
    :update #'misskey-surface-update
    :renderer-factory #'misskey-renderer-create)
   :app (or app (misskey-app)) :identity identity :input input :buffer (or buffer (current-buffer))))

(defun misskey-test-drain (&optional surface)
  "Drain pending canonical work for SURFACE and registered test Apps."
  (let ((apps (delete-dups (delq nil (cons (and surface (appkit-surface-app surface))
                                           (hash-table-values misskey--apps)))))
        (remaining 256) pending)
    (while (progn
             (setq pending nil)
             (dolist (app apps)
               (when (appkit-app-live-p app)
                 (let ((loops (cons (appkit-app-loop app)
                                    (mapcar (lambda (entry) (appkit-surface-loop (cdr entry)))
                                            (hash-table-values (appkit-app-surfaces app))))))
                   (dolist (loop loops)
                     (when (> (appkit-loop-pending-count loop) 0)
                       (setq pending t)
                       (appkit-loop-run-pass loop))))))
             (and pending (> (cl-decf remaining) 0))))
    (unless (> remaining 0) (error "Misskey fixture did not quiesce"))))

(provide 'misskey-test-helper)

;;; misskey-test-helper.el ends here
