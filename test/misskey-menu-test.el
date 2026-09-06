;;; misskey-menu-test.el --- Misskey menu context regressions -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'misskey-menu)
(require 'misskey-render)
(require 'misskey-test-helper
         (expand-file-name "misskey-test-helper"
                           (file-name-directory (or load-file-name buffer-file-name))))

(defmacro misskey-menu-test--with-scope (prefix buffer &rest body)
  "Run BODY with PREFIX scoped to BUFFER, without opening a global menu."
  (declare (indent 2) (debug t))
  `(let ((transient-current-prefix
          (clone (get ,prefix 'transient--prefix) :scope ,buffer)))
     ,@body))

(defmacro misskey-menu-test--with-draft (&rest body)
  "Run BODY with an isolated live `draft', without network or window changes."
  (declare (indent 0) (debug t))
  `(misskey-test-with-session
     (save-window-excursion
       (let (draft)
         (unwind-protect
             (progn
               (cl-letf (((symbol-function 'misskey-http-read)
                          (lambda (endpoint _parameters callback &rest _options)
                            (unless (equal endpoint "meta")
                              (ert-fail "Unexpected draft setup read"))
                            (funcall callback '((maxNoteTextLength . 3000))))))
                 (setq draft (misskey-compose-open)))
               ,@body)
           (when (buffer-live-p draft)
             (with-current-buffer draft (set-buffer-modified-p nil))
             (kill-buffer draft)))))))

(ert-deftest misskey-menu-actions-use-original-view-not-menu-buffer ()
  (misskey-test-with-session
    (with-temp-buffer
      (let* ((source (current-buffer))
             (account (misskey--current-account))
             (note '((id . "source-note") (text . "Source")
                     (user . ((id . "author") (username . "author")))))
             (kill-ring nil) (interprogram-cut-function nil) (interprogram-paste-function nil))
        (misskey-test-surface :identity 'menu-source
                              :input (list :type 'timeline :account account))
        (insert "Source")
        (add-text-properties (point-min) (point-max)
                             (misskey-render-note-properties note))
        (goto-char (point-min))
        (misskey-menu-test--with-scope 'misskey-menu source
          (with-temp-buffer
            ;; A foreign buffer can carry plausible properties, but it must
            ;; neither supply the menu's context nor receive its commands.
            (insert (propertize "Decoy" misskey-note-property
                                '((id . "wrong-note"))))
            (goto-char (point-min))
            (should (equal (misskey-note-id (misskey-menu--note)) "source-note"))
            (should (misskey-menu--user-p))
            (misskey-menu--in-source #'misskey-navigation-copy-link)
            (should (equal (current-kill 0)
                           "https://example.social/notes/source-note"))))))))

(ert-deftest misskey-menu-rejects-foreign-and-dead-context ()
  (with-temp-buffer
    (let ((source (current-buffer)))
      (insert (propertize "Not a view" misskey-note-property '((id . "decoy"))
                          misskey-user-property '((id . "decoy-user"))
                          misskey-navigation-target-property '(url "https://example.org")))
      (goto-char (point-min))
      (misskey-menu-test--with-scope 'misskey-menu source
        (should-not (misskey-menu--note))
        (should-not (misskey-menu--raw-note-p))
        (should-not (misskey-menu--user-p))
        (should-not (misskey-menu--link-p))
        (should-not (misskey-menu--activation-p))
        (kill-buffer source)
        (should-error
         (misskey-menu--in-source (lambda () (ert-fail "Ran without its source")))
         :type 'user-error)))))

(ert-deftest misskey-menu-draft-controls-update-live-source-without-publishing ()
  (misskey-menu-test--with-draft
    (let ((generation (with-current-buffer draft (appkit-compose-generation))))
      (misskey-menu-test--with-scope 'misskey-compose-menu draft
        (with-temp-buffer
          (setq-local misskey-compose-cw "Wrong draft")
          (cl-letf (((symbol-function 'misskey-http-post)
                     (lambda (&rest _) (ert-fail "An option attempted publication")))
                    ((symbol-function 'misskey-http-upload-file)
                     (lambda (&rest _) (ert-fail "An option attempted upload")))
                    ((symbol-function 'read-string)
                     (lambda (_prompt initial &rest _)
                       (should-not initial)
                       "Source warning")))
            ;; :advice* applies this source binding to the interactive spec too.
            (misskey-menu--in-source #'call-interactively #'misskey-compose-set-cw)
            (misskey-menu--in-source #'misskey-compose-set-visibility 'specified)
            (misskey-menu--in-source #'call-interactively
                                     #'misskey-compose-toggle-local-only)
            (should (string-match-p "Source warning" (misskey-menu--cw-label)))
            (should (string-match-p "Specified" (misskey-menu--visibility-label)))
            (should (misskey-menu--specified-p))
            (should (equal misskey-compose-cw "Wrong draft"))
            (with-current-buffer draft
              (should (= (appkit-compose-generation) (+ generation 3)))
              (should misskey-compose-local-only)
              (should-not (appkit-compose-operation-active-p)))
            ;; A second semantic edit is visible immediately; there is no
            ;; detached Transient option value to overwrite the real draft.
            (misskey-menu--in-source #'misskey-compose-set-cw "Later warning")
            (should (string-match-p "Later warning" (misskey-menu--draft-status)))
            (should-not (string-match-p "Source warning" (misskey-menu--cw-label)))
            (misskey-menu--in-source #'misskey-compose-set-visibility 'home)
            (should-not (misskey-menu--specified-p))))))))

(ert-deftest misskey-menu-draft-status-follows-async-limit-failure ()
  (misskey-menu-test--with-draft
    (misskey-menu-test--with-scope 'misskey-compose-menu draft
      (with-temp-buffer
        (let (fail)
          (cl-letf (((symbol-function 'misskey-http-read)
                     (lambda (_endpoint _parameters _callback &rest options)
                       (setq fail (plist-get options :errback))
                       nil)))
            (misskey-menu--in-source #'misskey-compose-refresh-metadata))
          (funcall fail "Instance metadata unavailable")
          (should (string-match-p "Instance metadata unavailable"
                                  (misskey-menu--draft-status)))
          (should (misskey-menu--draft-idle-p))
          (cl-letf (((symbol-function 'misskey-http-post)
                     (lambda (&rest _) (ert-fail "Published without a limit"))))
            (should-error (misskey-menu--in-source #'misskey-compose-send)
                          :type 'user-error)))))))

(provide 'misskey-menu-test)
;;; misskey-menu-test.el ends here
