;;; misskey-search.el --- Search Misskey notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Open each note search in an independent Appkit-backed feed with its own
;; request token, result set, cursor, content-warning state, and lifecycle.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'misskey-actions)
(require 'appkit-discussion)
(require 'misskey-compose)
(require 'misskey-auth)
(require 'misskey-core)
(require 'misskey-feed)
(require 'misskey-render)
(require 'misskey-thread)

(defcustom misskey-search-limit 20
  "Maximum number of notes requested for each search page."
  :type 'integer
  :group 'misskey)

(defvar misskey-search--serial 0
  "Serial used to identify independent search views.")

(defvar-keymap misskey-search-mode-map
  :doc "Keymap for `misskey-search-mode'."
  :parent special-mode-map
  "g" #'misskey-search-refresh
  "N" #'misskey-search-load-more
  "n" #'appkit-discussion-next-entry
  "p" #'appkit-discussion-previous-entry
  "RET" #'misskey-render-toggle-content-warning
  "t" #'misskey-thread-at-point
  "a" misskey-actions-map
  "r" #'misskey-compose-reply-at-point
  "q" #'misskey-compose-quote-at-point)

(define-derived-mode misskey-search-mode special-mode "Misskey-Search"
  "Major mode for one independent Misskey note search."
  (setq-local header-line-format nil)
  (setq-local line-spacing 0))

(defun misskey-search--current-view ()
  "Return the current live Misskey search view, or nil."
  (misskey-feed-current-view 'search))

(defun misskey-search--footer (state)
  "Return the generated search footer for STATE."
  (concat "\nQuery: " (plist-get state :query)
          (misskey-feed-default-footer state)))

(defun misskey-search--setup-view (view)
  "Initialize search VIEW and start its first request."
  (misskey-feed-setup-view view)
  (misskey-feed-request view 'initial))

(defun misskey-search-refresh ()
  "Refresh the current Misskey note search."
  (interactive)
  (if-let* ((view (misskey-search--current-view)))
      (misskey-feed-refresh view)
    (user-error "Current buffer is not a Misskey note search")))

(defun misskey-search-load-more ()
  "Load one older page in the current Misskey note search."
  (interactive)
  (if-let* ((view (misskey-search--current-view)))
      (misskey-feed-load-more view)
    (user-error "Current buffer is not a Misskey note search")))

;;;###autoload
(defun misskey-search (query &optional account)
  "Search Misskey notes for QUERY under ACCOUNT.

Each invocation creates a fresh view whose pagination is independent of every
other search and timeline.  ACCOUNT defaults to current customization."
  (interactive "sSearch Misskey notes: ")
  (unless (and (stringp query) (string-match-p "[^[:space:]]" query))
    (user-error "Misskey search query cannot be empty"))
  (let*
      ((target (or account (misskey--current-account)))
       (_token
        (and (called-interactively-p 'interactive)
             (misskey-auth--ensure-token target)))
       (serial (cl-incf misskey-search--serial))
       (state
        (misskey-feed-make-state :type 'search :account target :title
                                 (format "Search: %s" query) :endpoint
                                 "notes/search" :parameters
                                 (list :query query) :limit
                                 misskey-search-limit :footer-function
                                 #'misskey-search--footer
                                 :empty-message "No matching notes.")))
    (setf (plist-get state :query) query)
    (misskey-open-surface :app (misskey-app target) :identity
                          (list 'search serial) :mode
                          #'misskey-search-mode :buffer-name
                          (format "*misskey search %d: %s*" serial
                                  query)
                          :input state :setup
                          #'misskey-search--setup-view :select t)))

(provide 'misskey-search)

;;; misskey-search.el ends here
