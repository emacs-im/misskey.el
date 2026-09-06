;;; misskey.el --- Browse and publish on Misskey-compatible servers -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;; Author: 0WD0
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0") (appkit "0.3.0") (plz "0.9.1") (transient "0.13.7"))
;; Keywords: convenience, comm
;; URL: https://github.com/emacs-im/misskey.el

;;; Commentary:

;; misskey.el is an Emacs client for Misskey-compatible servers.  It provides
;; Appkit-backed timelines, threads, profiles, search, notifications, media,
;; compose drafts, and explicit note and relationship actions.

;;; Code:

(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-auth)
(require 'misskey-compose)
(require 'misskey-timeline)
(require 'misskey-thread)
(require 'misskey-profile)
(require 'misskey-directory)
(require 'misskey-search)
(require 'misskey-notifications)

;;;###autoload
(defun misskey-authorize ()
  "Replace the configured identity-bound credential through MiAuth."
  (interactive)
  (misskey-auth-authorize))

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

;;;###autoload
(defun misskey-thread (note-id)
  "Open the authenticated Misskey thread rooted at NOTE-ID."
  (interactive "sMisskey note ID: ")
  (misskey-auth--ensure-token)
  (misskey-thread-open note-id))

(require 'misskey-menu)
(require 'misskey-evil)

(provide 'misskey)

;;; misskey.el ends here
