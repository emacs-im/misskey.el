;;; misskey-auth-test.el --- Tests for Misskey authorization -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey)

(ert-deftest misskey-auth-builds-scoped-miauth-url ()
  (let* ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (account (misskey--current-account))
         (session "12345678-1234-4abc-8def-1234567890ab"))
    (should
     (equal
      (misskey-auth--authorization-url account session)
      (concat
       "https://example.social/miauth/12345678-1234-4abc-8def-1234567890ab"
       "?name=misskey.el&permission=read:account,write:notes")))))

(ert-deftest misskey-auth-session-id-uses-random-uuid-program ()
  (cl-letf (((symbol-function 'executable-find)
             (lambda (program)
               (and (equal program "uuidgen") "/usr/bin/uuidgen")))
            ((symbol-function 'call-process)
             (lambda (_program _infile destination _display &rest _args)
               (should (eq destination t))
               (insert "12345678-1234-4ABC-8DEF-1234567890AB\n")
               0)))
    (should
     (equal (misskey-auth--session-id)
            "12345678-1234-4abc-8def-1234567890ab"))))

(ert-deftest misskey-auth-session-id-requires-uuidgen ()
  (cl-letf (((symbol-function 'executable-find) (lambda (_program) nil)))
    (should-error (misskey-auth--session-id))))

(ert-deftest misskey-authorize-runs-miauth-and-persists-token ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice")
        (auth-sources '("/tmp/misskey-auth-test.gpg"))
        (session "12345678-1234-4abc-8def-1234567890ab")
        searches authorization-url prompt endpoint parameters request-account
        creation-sources ignore-non-existing-p saved-p forgotten-spec)
    (cl-letf (((symbol-function 'misskey-auth--session-id)
               (lambda () session))
              ((symbol-function 'browse-url)
               (lambda (url &rest _)
                 (setq authorization-url url)))
              ((symbol-function 'read-string)
               (lambda (text &rest _)
                 (setq prompt text)
                 ""))
              ((symbol-function 'misskey-http--public-read-sync)
               (lambda (requested-endpoint requested-parameters account)
                 (setq endpoint requested-endpoint
                       parameters requested-parameters
                       request-account account)
                 '((ok . t)
                   (token . "NEW-TOKEN")
                   (user (username . "alice")))))
              ((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 (push args searches)
                 (when (or saved-p (plist-get args :create))
                   (when (plist-get args :create)
                     (setq creation-sources auth-sources
                           ignore-non-existing-p
                           auth-source-ignore-non-existing-file))
                   (list
                    (list :user "alice"
                          :port "misskey"
                          :secret (lambda () "NEW-TOKEN")
                          :save-function (lambda () (setq saved-p t)))))))
              ((symbol-function 'auth-source-forget)
               (lambda (spec) (setq forgotten-spec spec)))
              ((symbol-function 'message) #'ignore))
      (should (equal (call-interactively #'misskey-authorize) "NEW-TOKEN"))
      (should
       (equal authorization-url
              (concat
               "https://example.social/miauth/" session
               "?name=misskey.el&permission=read:account,write:notes")))
      (should (string-match-p "press RET" prompt))
      (should (equal endpoint (format "miauth/%s/check" session)))
      (should (hash-table-p parameters))
      (should (= (hash-table-count parameters) 0))
      (should (equal (misskey--account-origin request-account)
                     "https://example.social"))
      (let ((create-spec
             (cl-find-if (lambda (spec) (plist-get spec :create)) searches)))
        (should create-spec)
        (should (equal (plist-get create-spec :host) "example.social"))
        (should (equal (plist-get create-spec :user) "alice"))
        (should (equal (plist-get create-spec :port) "misskey"))
        (should (equal (plist-get create-spec :secret) "NEW-TOKEN")))
      (should saved-p)
      (should (equal creation-sources
                     '("/tmp/misskey-auth-test.gpg")))
      (should-not ignore-non-existing-p)
      (should
       (equal forgotten-spec
              '(:host "example.social" :user "alice" :port "misskey"
                :require (:secret :port) :max 1))))))

(ert-deftest misskey-auth-storage-source-selects-encrypted-file ()
  (let ((auth-sources
         '(password-store "~/.authinfo" "~/.authinfo.gpg")))
    (should (equal (misskey-auth--storage-source) "~/.authinfo.gpg")))
  (let ((auth-sources '(password-store "~/.authinfo")))
    (should-error (misskey-auth--storage-source) :type 'user-error)))

(ert-deftest misskey-authorize-reuses-stored-token-without-browser ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice"))
    (cl-letf (((symbol-function 'misskey--stored-auth-token)
               (lambda (&optional _account) "EXISTING"))
              ((symbol-function 'misskey-auth--request-token)
               (lambda (&rest _) (ert-fail "MiAuth must not start"))))
      (should (equal (call-interactively #'misskey-authorize) "EXISTING")))))

(ert-deftest misskey-auth-refuses-unapproved-session-before-storage ()
  (let ((misskey-instance-url "https://example.social")
        (misskey-auth-source-user "alice"))
    (cl-letf (((symbol-function 'misskey--stored-auth-token)
               (lambda (&optional _account) nil))
              ((symbol-function 'browse-url) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'misskey-http--public-read-sync)
               (lambda (&rest _) '((ok . nil))))
              ((symbol-function 'misskey-auth--store-token)
               (lambda (&rest _) (ert-fail "Token must not be stored"))))
      (should-error (misskey-auth--ensure-token) :type 'user-error))))

(ert-deftest misskey-auth-redacts-token-from-storage-errors ()
  (let* ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (auth-sources '("/tmp/misskey-auth-test.gpg"))
         (account (misskey--current-account))
         failure)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) (error "Backend exposed SECRET"))))
      (condition-case err
          (misskey-auth--store-token account "SECRET")
        (error (setq failure (error-message-string err))))
      (should (string-match-p "\\[REDACTED\\]" failure))
      (should-not (string-match-p "SECRET" failure)))))

(ert-deftest misskey-public-entry-points-authorize-before-opening ()
  (let (calls)
    (cl-letf (((symbol-function 'misskey-auth--ensure-token)
               (lambda (&optional _account) (push 'authorize calls)))
              ((symbol-function 'misskey-compose-open)
               (lambda (&rest _) (push 'compose calls)))
              ((symbol-function 'misskey-timeline-open)
               (lambda (&rest _) (push 'home calls))))
      (call-interactively #'misskey-compose)
      (should (equal (nreverse calls) '(authorize compose)))
      (setq calls nil)
      (call-interactively #'misskey-home)
      (should (equal (nreverse calls) '(authorize home))))))

(ert-deftest misskey-authorize-persists-through-new-auth-source-file ()
  (let* ((file (make-temp-name
                (expand-file-name "misskey-auth-test-" temporary-file-directory)))
         (auth-sources (list file))
         (auth-source-ignore-non-existing-file t)
         (auth-source-save-behavior t)
         (misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice"))
    (unwind-protect
        (cl-letf (((symbol-function 'misskey-auth--storage-source)
                   (lambda () file))
                  ((symbol-function 'misskey-auth--request-token)
                   (lambda (_account) "TEST-TOKEN"))
                  ((symbol-function 'message) #'ignore))
          (should-not (file-exists-p file))
          (should (equal (misskey-auth--ensure-token) "TEST-TOKEN"))
          (should (file-exists-p file))
          (auth-source-forget-all-cached)
          (should (equal (misskey--auth-token) "TEST-TOKEN"))
          (with-temp-buffer
            (insert-file-contents file)
            (should
             (string-match-p
              "machine example.social login alice port misskey password TEST-TOKEN"
              (buffer-string)))))
      (auth-source-forget-all-cached)
      (when (file-exists-p file)
        (delete-file file)))))

(provide 'misskey-auth-test)

;;; misskey-auth-test.el ends here
