;;; misskey-media-test.el --- Preview scheduling regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'misskey-media)
(require 'misskey-test-helper)

(ert-deftest misskey-media-bounds-previews-and-advances-until-app-stop ()
  "Slow previews must not exhaust the App Effect runtime or stall its FIFO."
  (misskey-test-with-session
    (let* ((app (misskey-app))
           (account (misskey--session-account (appkit-app-model app)))
           (file (make-temp-file "misskey-preview-" nil ".png" "cached"))
           (starts 0) (active 0) (cancelled 0) transfers)
      (unwind-protect
          (with-temp-buffer
            (let ((surface (misskey-test-surface
                            :app app :identity 'preview-capacity
                            :input (list :account account :items nil
                                         :revealed-content (make-hash-table :test #'equal)))))
              (cl-letf
                  (((symbol-function 'appkit-media-image-cache-existing-file)
                    (lambda (_base) nil))
                   ((symbol-function 'appkit-media-cache-image-resource-async)
                    (lambda (_resource _base success failure &rest _options)
                      (let ((transfer (list :test-preview t :success success
                                            :failure failure :active t)))
                        (cl-incf starts) (cl-incf active)
                        (push transfer transfers)
                        transfer)))
                   ((symbol-function 'appkit-media-transfer-p)
                    (lambda (transfer) (plist-get transfer :test-preview)))
                   ((symbol-function 'appkit-media-cancel-transfer)
                    (lambda (transfer)
                      (when (plist-get transfer :active)
                        (setf (plist-get transfer :active) nil)
                        (cl-incf cancelled) (cl-decf active)))))
                (unwind-protect
                    (cl-labels
                        ((request (number)
                           (misskey-media-request-resource
                            surface (list :media number)
                            (format "https://cdn.example/%s.png" number) "media"))
                         (settle (transfer failure)
                           (setf (plist-get transfer :active) nil)
                           (cl-decf active)
                           (funcall (plist-get transfer (if failure :failure :success))
                                    (if failure "controlled failure" file))))
                      (dotimes (number 40) (request number))
                      
                      (misskey-test-drain surface)
                      (should (appkit-app-live-p app))
                      (should (appkit-surface-live-p surface))
                      (should (= starts 6))
                      (should (= active 6))
                      (settle (car (last transfers)) nil)
                      (misskey-test-drain surface)
                      (should (= starts 7))
                      (should (eq 'ready (plist-get
                                          (gethash '(:media 0) (misskey-resource-store app))
                                          :status)))
                      (settle (nth 5 transfers) t)
                      (misskey-test-drain surface)
                      (should (= starts 8))
                      (should (eq 'failed (plist-get
                                           (gethash '(:media 1) (misskey-resource-store app))
                                           :status)))
                      (while (> active 0)
                        (settle (cl-find-if (lambda (transfer) (plist-get transfer :active))
                                            transfers) nil)
                        (misskey-test-drain surface)
                        (should (<= active 6)))
                      (should (= starts 40))
                      (dotimes (number 40)
                        (should (memq (plist-get (gethash (list :media number)
                                                          (misskey-resource-store app))
                                                 :status)
                                      '(ready failed))))
                      (dotimes (number 40) (request (+ number 40)))
                      (misskey-test-drain surface)
                      (should (= starts 46))
                      (should (appkit-app-live-p app))
                      (should (appkit-surface-live-p surface))
                      (let ((stale (nth 4 transfers)))
                        (misskey-media-request-resource
                         surface '(:media 41) "https://cdn.example/replaced.png" "media")
                        (misskey-test-drain surface)
                        (should (= starts 47))
                        (funcall (plist-get stale :success) file)
                        (misskey-test-drain surface)
                        (should (eq 'pending (plist-get
                                              (gethash '(:media 41) (misskey-resource-store app))
                                              :status))))
                      (misskey-dispatch app '(:preview-cancel (:media 40)))
                      (misskey-test-drain surface)
                      (should (= starts 48))
                      (should (eq 'failed (plist-get
                                           (gethash '(:media 40) (misskey-resource-store app))
                                           :status)))
                      (request 40)
                      (misskey-test-drain surface)
                      (should (eq 'pending (plist-get
                                            (gethash '(:media 40) (misskey-resource-store app))
                                            :status)))
                  ;; Replacing a queued demand with a cache hit must retire it.
                      (cl-letf (((symbol-function 'appkit-media-image-cache-existing-file)
                                 (lambda (_base) file)))
                        (misskey-media-request-resource
                         surface '(:media 79) "https://cdn.example/cached.png" "media")
                        (misskey-test-drain surface))
                      (should (= starts 48))
                      (should (eq 'ready (plist-get
                                          (gethash '(:media 79) (misskey-resource-store app))
                                          :status)))
                      (misskey-stop)
                      (should (= cancelled 8))
                      (should (= active 0))
                  ;; Deliver every stale transport callback after cancellation.
                      (dolist (transfer transfers)
                        (funcall (plist-get transfer :success) file))
                      (should (= starts 48))
                      (should-not (appkit-app-live-p app)))
                  (misskey-stop)))))
        (delete-file file)))))

(provide 'misskey-media-test)
;;; misskey-media-test.el ends here
