;;; misskey-compose-test.el --- Tests for Misskey compose -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey-compose)

(defmacro misskey-compose-test--with-buffer (&rest body)
  "Run BODY in a configured temporary Misskey compose buffer."
  (declare (indent 0) (debug t))
  `(let ((misskey-instance-url "https://example.social"))
     (with-temp-buffer
       (misskey-compose-mode)
       (appkit-compose-setup
        :context-function #'misskey-compose--context
        :status-fields-function #'misskey-compose--status-fields
        :footer-function #'misskey-compose--footer)
       ,@body)))

(ert-deftest misskey-compose-renders-generated-public-note-shell ()
  (misskey-compose-test--with-buffer
    (should (string-match-p "New note on https://example.social"
                            (buffer-string)))
    (should (string-match-p "Visibility: Public" (buffer-string)))
    (should (string-match-p "C-c C-c publish" (buffer-string)))
    (goto-char (point-min))
    (should (get-text-property (point) 'read-only))
    (goto-char (appkit-compose-body-start-position))
    (insert "hello")
    (should (equal (appkit-compose-body) "hello"))))

(ert-deftest misskey-compose-send-publishes-public-text-and-closes-on-id ()
  (misskey-compose-test--with-buffer
    (let ((buffer (current-buffer)) captured)
      (goto-char (appkit-compose-body-start-position))
      (insert " hello world ")
      (cl-letf (((symbol-function 'misskey-app)
                 (lambda (&optional _account) 'owner))
                ((symbol-function 'misskey-http-post)
                 (lambda (endpoint parameters callback &rest options)
                   (setq captured
                         (list endpoint parameters
                               (plist-get options :owner)))
                   (funcall callback
                            '((createdNote (id . "note-1")))))))
        (misskey-compose-send)
        (should (equal captured
                       '("notes/create"
                         (:text " hello world " :visibility "public")
                         owner)))
        (should-not (buffer-live-p buffer))))))

(ert-deftest misskey-compose-send-keeps-draft-after-remote-failure ()
  (misskey-compose-test--with-buffer
    (goto-char (appkit-compose-body-start-position))
    (insert "keep me")
    (cl-letf (((symbol-function 'misskey-app)
               (lambda (&optional _account) 'owner))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'misskey-http-post)
               (lambda (_endpoint _parameters _callback &rest options)
                 (funcall (plist-get options :errback) "failed"))))
      (misskey-compose-send)
      (should-not misskey-compose--sending)
      (should (equal (appkit-compose-body) "keep me")))))

(ert-deftest misskey-compose-send-shows-inflight-state-until-callback ()
  (misskey-compose-test--with-buffer
    (goto-char (appkit-compose-body-start-position))
    (insert "pending")
    (cl-letf (((symbol-function 'misskey-app)
               (lambda (&optional _account) 'owner))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'misskey-http-post)
               (lambda (&rest _) 'request-buffer)))
      (misskey-compose-send)
      (should misskey-compose--sending)
      (should (string-match-p "State: Publishing" (buffer-string)))
      (should (string-match-p "wait for the server response"
                              (buffer-string)))
      (should (equal (appkit-compose-body) "pending"))
      (goto-char (appkit-compose-body-start-position))
      (should-error (delete-char 1) :type 'text-read-only))))

(ert-deftest misskey-compose-send-restores-state-after-synchronous-error ()
  (misskey-compose-test--with-buffer
    (goto-char (appkit-compose-body-start-position))
    (insert "recover")
    (cl-letf (((symbol-function 'misskey-app)
               (lambda (&optional _account) 'owner))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'misskey-http-post)
               (lambda (&rest _) (error "setup failed"))))
      (should-error (misskey-compose-send))
      (should-not misskey-compose--sending)
      (should (string-match-p "State: Draft" (buffer-string)))
      (should (equal (appkit-compose-body) "recover"))
      (goto-char (appkit-compose-body-end-position))
      (insert " again")
      (should (equal (appkit-compose-body) "recover again")))))

(ert-deftest misskey-compose-success-without-note-id-keeps-draft ()
  (misskey-compose-test--with-buffer
    (goto-char (appkit-compose-body-start-position))
    (insert "unconfirmed")
    (cl-letf (((symbol-function 'misskey-app)
               (lambda (&optional _account) 'owner))
              ((symbol-function 'message) #'ignore)
              ((symbol-function 'misskey-http-post)
               (lambda (_endpoint _parameters callback &rest _)
                 (funcall callback '((createdNote))))))
      (misskey-compose-send)
      (should-not misskey-compose--sending)
      (should (equal (appkit-compose-body) "unconfirmed"))
      (goto-char (appkit-compose-body-end-position))
      (insert " again")
      (should (equal (appkit-compose-body) "unconfirmed again")))))

(ert-deftest misskey-compose-error-still-reports-after-buffer-dies ()
  (let ((buffer (generate-new-buffer " *misskey-compose-dead*"))
        reported)
    (kill-buffer buffer)
    (cl-letf (((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq reported (apply #'format format-string args)))))
      (misskey-compose--handle-error buffer "unknown outcome")
      (should (equal reported "unknown outcome")))))

(ert-deftest misskey-compose-send-rejects-empty-and-duplicate-send ()
  (misskey-compose-test--with-buffer
    (should-error (misskey-compose-send) :type 'user-error)
    (setq-local misskey-compose--sending t)
    (should-error (misskey-compose-send) :type 'user-error)))

(ert-deftest misskey-compose-send-preserves-significant-whitespace ()
  (misskey-compose-test--with-buffer
    (goto-char (appkit-compose-body-start-position))
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
        (should (equal captured "  indented\n"))))))

(ert-deftest misskey-compose-cancel-refuses-inflight-write ()
  (misskey-compose-test--with-buffer
    (setq-local misskey-compose--sending t)
    (should-error (misskey-compose-cancel) :type 'user-error)))

(provide 'misskey-compose-test)

;;; misskey-compose-test.el ends here
