;;; misskey.el --- Browse and publish on Misskey-compatible servers -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;; Author: 0WD0
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (appkit "0.2.8") (plz "0.9.1"))
;; Keywords: convenience, comm
;; URL: https://github.com/0WD0/misskey.el

;;; Commentary:

;; misskey.el is an Emacs client for Misskey-compatible servers.  It provides
;; an authenticated home timeline and standalone note composition.

;;; Code:

(eval-and-compile
  (let ((dir (file-name-directory
              (or load-file-name
                  (buffer-file-name)
                  default-directory))))
    (add-to-list 'load-path (expand-file-name "lisp" dir))))

(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-auth)
(require 'misskey-compose)
(require 'misskey-timeline)

;;;###autoload
(defun misskey-authorize ()
  "Authorize the configured account and store its scoped API token."
  (interactive)
  (misskey-auth--ensure-token))

;;;###autoload
(defun misskey-compose ()
  "Open a buffer for composing a new Misskey note."
  (interactive)
  (misskey-auth--ensure-token)
  (misskey-compose-open))

;;;###autoload
(defun misskey-home ()
  "Open the authenticated Misskey home timeline."
  (interactive)
  (misskey-auth--ensure-token)
  (misskey-timeline-open))

(provide 'misskey)

;;; misskey.el ends here
