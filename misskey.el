;;; misskey.el --- Publish notes to Misskey-compatible servers -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;; Author: 0WD0
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (appkit "0.2.4"))
;; Keywords: convenience, comm
;; URL: https://github.com/0WD0/misskey.el

;;; Commentary:

;; misskey.el is an Emacs client for Misskey-compatible servers.  The initial
;; release provides authenticated plain-text note composition and publishing.

;;; Code:

(eval-and-compile
  (let ((dir (file-name-directory
              (or load-file-name
                  (buffer-file-name)
                  default-directory))))
    (add-to-list 'load-path (expand-file-name "lisp" dir))))

(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-compose)

;;;###autoload
(defun misskey-compose ()
  "Open a buffer for composing a new Misskey note."
  (interactive)
  (misskey-compose-open))

(provide 'misskey)

;;; misskey.el ends here
