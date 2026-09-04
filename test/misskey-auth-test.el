;;; misskey-auth-test.el --- Tests for Misskey authorization -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'misskey)
(require 'misskey-test-helper
         (expand-file-name
          "misskey-test-helper"
          (file-name-directory
           (or load-file-name
               (and (boundp 'byte-compile-current-file)
                    byte-compile-current-file)
               buffer-file-name))))

(ert-deftest misskey-auth-builds-scoped-miauth-url ()
  (let* ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (account (misskey--current-account-locator))
         (session "12345678-1234-4abc-8def-1234567890ab"))
    (should
     (equal
      (misskey-auth--authorization-url account session)
      (concat
       "https://example.social/miauth/12345678-1234-4abc-8def-1234567890ab"
       "?name=misskey.el&permission=read:account,read:notifications,write:notes,write:reactions,write:favorites,write:following,write:notifications,write:drive")))))

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

(ert-deftest misskey-auth-miauth-returns-token-and-stable-user-id ()
  (let ((misskey-instance-url "https://example.social")
        (session "12345678-1234-4abc-8def-1234567890ab"))
    (cl-letf (((symbol-function 'misskey-auth--session-id)
               (lambda () session))
              ((symbol-function 'browse-url) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'misskey-http--public-read-sync)
               (lambda (&rest _)
                 '((ok . t) (token . "TOKEN-1")
                   (user (id . "bob-id") (username . "bob"))))))
      (let ((credential
             (misskey-auth--request-credential
              (misskey--current-account-locator))))
        (should (equal (misskey--credential-token credential) "TOKEN-1"))
        (should (equal (misskey--credential-user-id credential) "bob-id"))))))

(ert-deftest misskey-auth-rejects-hostile-miauth-token ()
  (let ((misskey-instance-url "https://example.social"))
    (cl-letf (((symbol-function 'browse-url) #'ignore)
              ((symbol-function 'read-string) (lambda (&rest _) ""))
              ((symbol-function 'misskey-http--public-read-sync)
               (lambda (&rest _)
                 '((ok . t) (token . "safe\"\nheader = \"evil")
                   (user (id . "bob-id"))))))
      (should-error
       (misskey-auth--request-credential
        (misskey--current-account-locator))
       :type 'user-error))))

(ert-deftest misskey-auth-storage-source-selects-first-encrypted-file ()
  (let ((auth-sources
         '(password-store
           (:source "~/.authinfo.gpg" :host t)
           "~/.netrc.gpg")))
    (should
     (equal (misskey-auth--storage-source)
            '(:source "~/.authinfo.gpg" :host t))))
  (let ((auth-sources '(password-store "~/.authinfo")))
    (should-error (misskey-auth--storage-source) :type 'user-error)))

(ert-deftest misskey-test-helper-preserves-interactive-auth-sources ()
  (let ((auth-sources '(password-store "~/.authinfo.gpg")))
    (misskey-test-with-session
      (should (equal (misskey--credential-user-id
                      (misskey--stored-credential (misskey--current-account-locator)))
                     "self"))
      (should-error (auth-source-search :host "example.social")))
    (should (equal auth-sources '(password-store "~/.authinfo.gpg")))))

(ert-deftest misskey-auth-real-netrc-upsert-leaves-one-replacement ()
  (let* ((password-data (make-hash-table :test #'equal))
         (auth-source-netrc-cache nil)
         (auth-source-do-cache nil)
         (file (make-temp-file "misskey-auth-upsert-"))
         (misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (account (misskey--current-account-locator))
         (credential
          (misskey--credential-create :token "NEW-TOKEN" :user-id "bob-id"))
         (secret (misskey--credential-string credential)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# preserve this comment\n"
                    "machine other.social login other port misskey password OTHER\n"
                    "machine example.social\n"
                    "login alice\n"
                    "port misskey\n"
                    "password OLD # preserve inline comment\n"
                    "# preserve comment between duplicates\n"
                    "machine example.social login alice port misskey password OLDER\n"))
          (cl-letf (((symbol-function 'misskey-auth--storage-source)
                     (lambda () (list :source file))))
            (should (equal
                     (misskey-auth--store-credential account credential)
                     credential)))
          (auth-source-forget-all-cached)
          (let* ((auth-sources (list (list :source file)))
                 (matches
                  (auth-source-search
                   :host "example.social" :user "alice" :port "misskey"
                   :require '(:secret :port) :max 10)))
            (should (= (length matches) 1))
            (should (equal (auth-info-password (car matches)) secret)))
          (with-temp-buffer
            (insert-file-contents file)
            (should (string-match-p "# preserve this comment"
                                    (buffer-string)))
            (should (string-match-p "machine other.social"
                                    (buffer-string)))
            (should (string-match-p "# preserve inline comment"
                                    (buffer-string)))
            (should (string-match-p "# preserve comment between duplicates"
                                    (buffer-string)))))
      (auth-source-forget-all-cached)
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest misskey-auth-upsert-failure-leaves-original-file ()
  (let* ((file (make-temp-file "misskey-auth-atomic-"))
         (spec '(:host "example.social" :user "alice" :port "misskey"))
         (original
          "machine example.social login alice port misskey password OLD\n"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert original))
          (cl-letf (((symbol-function 'misskey-auth--verify-netrc-candidate)
                     (lambda (&rest _) (error "candidate rejected"))))
            (should-error
             (misskey-auth--upsert-netrc-file file spec "NEW")))
          (with-temp-buffer
            (insert-file-contents file)
            (should (equal (buffer-string) original))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest misskey-authorize-rebinds-label-from-alice-to-bob ()
  (let* ((misskey-instance-url "https://example.social")
         (misskey-auth-source-user "alice")
         (target (misskey--current-account-locator))
         (old (misskey--credential-create
               :token "OLD-TOKEN" :user-id "alice-id"))
         (new (misskey--credential-create
               :token "NEW-TOKEN" :user-id "bob-id"))
         stored replaced)
    (cl-letf (((symbol-function 'misskey--stored-credential)
               (lambda (&optional _account) old))
              ((symbol-function 'misskey-auth--request-credential)
               (lambda (account)
                 (should (equal account target))
                 new))
              ((symbol-function 'misskey-auth--store-credential)
               (lambda (account credential)
                 (setq stored (list account credential))))
              ((symbol-function 'misskey-auth--replace-session)
               (lambda (account before after)
                 (setq replaced (list account before after))))
              ((symbol-function 'message) #'ignore))
      (should (equal (misskey-auth-authorize target) "NEW-TOKEN"))
      (should (equal stored (list target new)))
      (should (equal replaced (list target old new)))
      (should (equal (misskey--credential-user-id new) "bob-id")))))

(ert-deftest misskey-auth-replaces-live-identity-session ()
  (let*
      ((account
        (misskey--account-create :origin "https://example.social"
                                 :auth-source-user "label"))
       (old
        (misskey--credential-create :token "OLD" :user-id "alice-id"))
       (new
        (misskey--credential-create :token "NEW" :user-id "bob-id"))
       (old-key '("https://example.social" "alice-id"))
       (old-app (appkit-app-start misskey--app-type :identity old-key))
       recreated)
    (puthash old-key old-app misskey--apps)
    (unwind-protect
        (cl-letf
            (((symbol-function 'misskey-app)
              (lambda (bound) (setq recreated bound) 'new-app)))
          (misskey-auth--replace-session account old new)
          (should-not (appkit-app-live-p old-app))
          (should-not (gethash old-key misskey--apps))
          (should
           (equal (misskey--account-remote-user-id recreated) "bob-id")))
      (remhash old-key misskey--apps)
      (when (appkit-app-live-p old-app) (appkit-app-close old-app)))))

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

(ert-deftest misskey-auth-redacts-token-from-storage-errors ()
  (let* ((misskey-instance-url "https://example.social")
         (auth-sources '("credentials.gpg"))
         (account (misskey--current-account-locator))
         (credential
          (misskey--credential-create :token "SECRET" :user-id "user-id"))
         (secret (misskey--credential-string credential))
         failure)
    (cl-letf (((symbol-function 'misskey-auth--upsert-netrc-file)
               (lambda (&rest _)
                 (error "Backend exposed SECRET and %s" secret))))
      (condition-case err
          (misskey-auth--store-credential account credential)
        (error (setq failure (error-message-string err))))
      (should (string-match-p "\\[REDACTED\\]" failure))
      (should-not (string-match-p "SECRET" failure))
      (should-not (string-match-p (regexp-quote secret) failure)))))

(provide 'misskey-auth-test)

;;; misskey-auth-test.el ends here
