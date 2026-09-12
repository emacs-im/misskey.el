;;; misskey-evil-test.el --- Modal interaction regressions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey)
(require 'misskey-evil)
(require 'misskey-test-helper)
(require 'evil nil t)

(defun misskey-evil-test--press (keys)
  "Invoke the effective binding of KEYS in the current buffer."
  (let ((command (key-binding (kbd keys))))
    (should (commandp command))
    (call-interactively command)))

(defmacro misskey-evil-test--with-state (state &rest body)
  "Run BODY with buffer-local Evil in STATE."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (featurep 'evil))
     (let ((appkit-evil-enable-integration t))
       (misskey-evil-setup))
     (evil-local-mode 1)
     (evil-change-state ,state)
     ,@body))

(ert-deftest misskey-evil-note-views-navigate-and-copy-in-both-states ()
  (skip-unless (featurep 'evil))
  (misskey-test-with-session
    (dolist (mode '(misskey-timeline-mode misskey-thread-mode
                    misskey-profile-mode misskey-search-mode))
      (dolist (state '(normal motion))
        (with-temp-buffer
          (let* ((account (misskey--current-account))
                 (view (misskey-test-surface
                        :identity (list mode state) :mode mode
                        :input (list :account account)))
                 (url "https://remote.social/notes/target")
                 (note `((id . "note") (text . ,url) (visibility . "public")
                         (user . ((id . "u1") (username . "alice")))))
                 opened browsed)
            (let ((inhibit-read-only t))
              (insert (misskey-navigation-propertize url note) "
")
              (add-text-properties (point-min) (point-max)
                                   (misskey-render-note-properties note)))
            (goto-char (point-min))
            (misskey-evil-test--with-state state
              (cl-letf (((symbol-function 'misskey-navigation-open-url)
                         (lambda (target owner) (setq opened (list target owner))))
                        ((symbol-function 'browse-url)
                         (lambda (target &rest _) (setq browsed target))))
                (dolist (key '("RET" "<return>"))
                  (setq opened nil)
                  (misskey-evil-test--press key)
                  (should (equal opened (list url view))))
                (misskey-evil-test--press "g x")
                (should (equal browsed url))
                (let ((kill-ring nil) (interprogram-cut-function nil) (interprogram-paste-function nil))
                  (misskey-evil-test--press "Z l")
                  (should (equal (current-kill 0) url))))
              (when (eq state 'normal)
                (let ((before (buffer-string)))
                  (condition-case nil (misskey-evil-test--press "x")
                    (buffer-read-only nil))
                  (should (equal (buffer-string) before)))))))))))

(ert-deftest misskey-evil-directory-keys-activate-and-page-their-own-surface ()
  (skip-unless (featurep 'evil))
  (misskey-test-with-session
    (dolist (kind '(notifications followers))
      (dolist (state '(normal motion))
        (let* ((account (misskey--current-account))
               (subject '((id . "owner") (username . "owner")))
               (user '((id . "u1") (username . "alice")))
               (endpoint (if (eq kind 'notifications)
                             "i/notifications" "users/followers"))
               requests opened view)
          (unwind-protect
              (cl-letf
                  (((symbol-function 'misskey-http-read)
                    (lambda (path parameters callback &rest _)
                      (push (cons path parameters) requests)
                      (funcall callback
                               (if (eq kind 'notifications)
                                   `(((id . "notification-1") (type . "mention")
                                      (createdAt . "2026-08-13T00:00:00.000Z")
                                      (user . ,user)
                                      (note . ((id . "note-1") (text . "hello")
                                               (user . ,user)))))
                                 `(((id . "relationship-1") (follower . ,user)))))
                      nil))
                   ((symbol-function 'misskey-thread-open)
                    (lambda (&rest args) (setq opened (cons 'thread args))))
                   ((symbol-function 'misskey-profile-open)
                    (lambda (&rest args) (setq opened (cons 'profile args)))))
                (setq view (if (eq kind 'notifications)
                               (misskey-notifications account)
                             (misskey-directory-open 'followers subject account)))
                (misskey-test-drain view)
                (with-current-buffer (appkit-surface-buffer view)
                  (goto-char (point-min))
                  (should (text-property-search-forward
                           misskey-user-id-property "u1" #'equal))
                  (backward-char)
                  (misskey-evil-test--with-state state
                    (dolist (key '("RET" "<return>"))
                      (setq opened nil)
                      (misskey-evil-test--press key)
                      (if (eq kind 'notifications)
                          (should (equal opened (list 'thread "note-1" account)))
                        (should (eq (car opened) 'profile))
                        (should (equal (nth 2 opened) account))))
                    (setq requests nil)
                    (misskey-evil-test--press "g n")
                    (misskey-test-drain view)
                    (should (equal (caar requests) endpoint))
                    (should (equal (plist-get (cdar requests) :untilId)
                                   (if (eq kind 'notifications)
                                       "notification-1" "relationship-1")))
                    (setq requests nil)
                    (misskey-evil-test--press "g r")
                    (misskey-test-drain view)
                    (should (equal (caar requests) endpoint))
                    (should-not (plist-get (cdar requests) :untilId)))))
            (when (and view (buffer-live-p (appkit-surface-buffer view)))
              (kill-buffer (appkit-surface-buffer view)))))))))

(ert-deftest misskey-evil-compose-enters-input-and-keeps-draft-controls ()
  (skip-unless (featurep 'evil))
  (misskey-test-with-session
    (let ((appkit-evil-enable-integration t))
      (misskey-evil-setup))
    (with-temp-buffer
      (misskey-compose-mode)
      (setq-local misskey-compose--account (misskey--current-account))
      (appkit-chat-compose-setup :app (misskey-app misskey-compose--account))
      (evil-local-mode 1)
      (should (eq evil-state 'insert))
      (evil-normal-state)
      (goto-char (point-min))
      (misskey-evil-test--press "i")
      (should (eq evil-state 'insert))
      (should (= (point) (appkit-chatbuf-input-logical-end-position)))
      (insert "A draft")
      (should (equal (appkit-chat-compose-body) "A draft"))
      (dolist (state '(insert normal motion))
        (evil-change-state state)
        (let ((before misskey-compose-local-only))
          (misskey-evil-test--press "C-c C-l")
          (should (eq misskey-compose-local-only (not before))))
        (should (eq (key-binding (kbd "C-c C-o")) #'misskey-compose-menu)))
      (should (equal (appkit-chat-compose-body) "A draft")))))

(ert-deftest misskey-evil-preserves-user-motions-and-global-maps ()
  (skip-unless (featurep 'evil))
  (let ((normal-before (copy-keymap evil-normal-state-map))
        (motion-before (copy-keymap evil-motion-state-map))
        (insert-before (copy-keymap evil-insert-state-map)))
    (unwind-protect
        (let ((evil-normal-state-map (copy-keymap evil-normal-state-map))
              (evil-motion-state-map (copy-keymap evil-motion-state-map)))
          (dolist (map (list evil-normal-state-map evil-motion-state-map))
            (dolist (key '("n" "p" "w" "B"))
              (define-key map (kbd key) #'backward-word)))
          (dolist (state '(normal motion))
            (with-temp-buffer
              (misskey-timeline-mode)
              (let ((inhibit-read-only t)) (insert "one two"))
              (misskey-evil-test--with-state state
                (dolist (key '("n" "p" "w" "B"))
                  (goto-char (point-max))
                  (misskey-evil-test--press key)
                  (should (= (point) 5)))
                (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))))))
      (should (equal evil-normal-state-map normal-before))
      (should (equal evil-motion-state-map motion-before))
      (should (equal evil-insert-state-map insert-before)))))

(ert-deftest misskey-evil-quit-closes-window-without-destroying-buffer ()
  (skip-unless (featurep 'evil))
  (let ((buffer (generate-new-buffer " *misskey-evil-quit*")) killed)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (misskey-search-mode)
          (add-hook 'kill-buffer-hook (lambda () (setq killed t)) nil t)
          (misskey-evil-test--with-state 'normal
            (misskey-evil-test--press "q")
            (should (buffer-live-p buffer))
            (should-not killed)
            (should-not (eq (window-buffer) buffer))))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest misskey-evil-reply-and-quote-open-distinct-editable-drafts ()
  (skip-unless (featurep 'evil))
  (misskey-test-with-session
    (save-window-excursion
      (with-temp-buffer
        (let* ((source (current-buffer))
               (account (misskey--current-account))
               (note '((id . "target") (text . "Original")
                       (visibility . "public")
                       (user . ((id . "u1") (username . "alice"))))))
          (misskey-test-surface :identity 'evil-reply :mode #'misskey-search-mode
                                :input (list :account account))
          (let ((inhibit-read-only t))
            (insert (propertize "Original" misskey-note-property note)))
          (goto-char (point-min))
          (cl-letf (((symbol-function 'misskey-http-read)
                     (lambda (endpoint _parameters callback &rest _)
                       (should (equal endpoint "meta"))
                       (funcall callback '((maxNoteTextLength . 3000))))))
            (dolist (state '(normal motion))
              (dolist (binding '(("r" misskey-compose-reply-at-point)
                                 ("Q" misskey-compose-quote-at-point)))
                (with-current-buffer source
                  (misskey-evil-test--with-state state
                    (should (eq (key-binding (kbd (car binding))) (cadr binding))))
                  (let ((draft (misskey-evil-test--press (car binding))))
                    (unwind-protect
                        (with-current-buffer draft
                          (should (derived-mode-p 'misskey-compose-mode))
                          (should (equal misskey-compose--account account))
                          (should (equal misskey-compose-reply-id
                                         (and (equal (car binding) "r") "target")))
                          (should (equal misskey-compose-renote-id
                                         (and (equal (car binding) "Q") "target")))
                          (appkit-evil-chatbuf-enter-input)
                          (insert "My response")
                          (should (equal (appkit-chat-compose-body) "My response")))
                      (when (buffer-live-p draft) (kill-buffer draft)))))))))))))

(provide 'misskey-evil-test)
;;; misskey-evil-test.el ends here
