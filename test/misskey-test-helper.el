;;; misskey-test-helper.el --- Isolated Misskey test credentials -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Code:

(require 'auth-source)
(require 'misskey-core)

(defvar misskey-test--auth-file nil
  "Temporary authinfo file installed for Misskey tests.")

(defvar misskey-test--original-auth-sources nil
  "User auth sources temporarily replaced by the test fixture.")

(defvar misskey-test--auth-source-installed-p nil
  "Non-nil while the isolated test source is installed.")

(defun misskey-test--cleanup-auth-source ()
  "Remove the temporary credential source and restore user configuration."
  (auth-source-forget-all-cached)
  (when (and misskey-test--auth-file
             (file-exists-p misskey-test--auth-file))
    (delete-file misskey-test--auth-file))
  (setq misskey-test--auth-file nil)
  (when misskey-test--auth-source-installed-p
    (setq auth-sources misskey-test--original-auth-sources
          misskey-test--original-auth-sources nil
          misskey-test--auth-source-installed-p nil)))

(defun misskey-test-install-auth-source ()
  "Install an isolated validated credential source for this Emacs process."
  (misskey-test--cleanup-auth-source)
  (setq misskey-test--original-auth-sources (copy-tree auth-sources)
        misskey-test--auth-source-installed-p t)
  (condition-case err
      (let* ((credential
              (misskey--credential-create :token "TOKEN" :user-id "self"))
             (secret (misskey--credential-string credential)))
        (unless (and (misskey--valid-token-p
                      (misskey--credential-token credential))
                     (misskey--valid-user-id-p
                      (misskey--credential-user-id credential)))
          (error "Invalid Misskey test credential"))
        (setq misskey-test--auth-file
              (make-temp-file "misskey-test-auth-" nil ".authinfo"))
        (with-temp-file misskey-test--auth-file
          (dolist (user '("alice" "TOKEN" "misskey.el" "credential-label"))
            (insert
             (format
              "machine example.social login %s port misskey password %s\n"
              user secret))))
        (setq auth-sources (list misskey-test--auth-file))
        (auth-source-forget-all-cached))
    (error
     (misskey-test--cleanup-auth-source)
     (signal (car err) (cdr err)))))

(defun misskey-test--enable-auth-source ()
  "Install isolated credentials only in a batch test process."
  (when noninteractive
    (misskey-test-install-auth-source)
    (add-hook 'kill-emacs-hook #'misskey-test--cleanup-auth-source)))

(misskey-test--enable-auth-source)

(provide 'misskey-test-helper)

;;; misskey-test-helper.el ends here
