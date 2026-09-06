;;; misskey-navigation.el --- Note text navigation -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'browse-url)
(require 'misskey-core)
(require 'misskey-http)
(require 'misskey-note)
(require 'button)
(require 'appkit-ui)

(declare-function misskey-thread-at-point "misskey-thread" ())
(declare-function misskey-render-note-at-point "misskey-render")
(declare-function misskey-thread-open "misskey-thread")
(declare-function misskey-profile-open "misskey-profile")
(declare-function misskey-search-tag "misskey-search")

(defconst misskey-navigation-target-property 'misskey-navigation-target
  "Text property containing a link target, independent of row identity.")

(defvar-keymap misskey-navigation-link-map
  :doc "Exact-span activation; deliberately leaves TAB to the view."
  "RET" #'misskey-navigation-activate
  "<mouse-2>" #'misskey-navigation-mouse-activate)

(defun misskey-navigation--url (text)
  "Parse safe HTTP(S) TEXT, returning its URL object or signaling an error."
  (let ((case-fold-search t))
    (unless (and (stringp text)
                 (string-match-p "\\`https?://\\(?:[A-Za-z0-9.-]+\\|\\[[A-Fa-f0-9:]+\\]\\)\\(?::[0-9]+\\)?\\(?:/\\|[?#]\\|\\'\\)" text)
                 (not (string-match-p "[[:space:]\\\\\x00-\x1f\x7f]" text)))
      (user-error "Link must be an HTTP(S) URL without credentials"))
    (let* ((url (url-generic-parse-url text)) (host (url-host url)))
      (unless (and (member (downcase (url-type url)) '("http" "https"))
                   (stringp host) (not (string-empty-p host))
                   (if (string-prefix-p "[" host)
                       (string-match-p "\\`\\[[A-Fa-f0-9]*:[A-Fa-f0-9:]+\\]\\'" host)
                     (cl-every (lambda (label)
                                 (string-match-p "\\`[A-Za-z0-9]\\(?:[A-Za-z0-9-]*[A-Za-z0-9]\\)?\\'" label))
                               (split-string (string-remove-suffix "." host) "\\." nil)))
                   (null (url-user url)) (null (url-password url))
                   (> (url-port url) 0) (<= (url-port url) 65535))
        (user-error "Invalid HTTP(S) link host"))
      url)))

(defun misskey-navigation--origin (url)
  "Return URL's normalized origin for exact same-instance comparison."
  (list (downcase (url-type url)) (downcase (url-host url)) (url-port url)))

(defun misskey-navigation--route (text account)
  "Classify TEXT for ACCOUNT as a local view, remote object, or browser URL.
Only recognized object paths trigger federation resolution.  Decoding cannot
introduce separators, query syntax, escapes, or dot-segment traversal."
  (let* ((url (misskey-navigation--url text))
         (raw (car (split-string (url-filename url) "?")))
         (path (decode-coding-string (url-unhex-string raw) 'utf-8))
         (own (equal (misskey-navigation--origin url)
                     (misskey-navigation--origin
                      (misskey-navigation--url (misskey--account-origin account)))))
         (case-fold-search nil))
    (when (or (string-match-p "%\\(?:2[fF]\\|5[cC]\\|3[fF]\\|23\\|25\\|0[0-9a-fA-F]\\|1[0-9a-fA-F]\\|7[fF]\\)" raw)
              (string-match-p "\\(?:\\`\\|/\\)\\.\\.?\\(?:/\\|\\'\\)" path))
      (user-error "Ambiguous encoded or relative URL path"))
    (cond
     ((string-match "\\`/notes/\\([A-Za-z0-9_-]+\\)/?\\'" path)
      (if own (list 'note (match-string 1 path)) (list 'remote text)))
     ((string-match "\\`/@\\([A-Za-z0-9_]+\\(?:@[A-Za-z0-9.-]+\\(?::[0-9]+\\)?\\)?\\)/?\\'" path)
      (if own (list 'user (concat "@" (match-string 1 path))) (list 'remote text)))
     ((string-match "\\`/users/\\([A-Za-z0-9_-]+\\)/?\\'" path)
      (if own (list 'user (match-string 1 path)) (list 'remote text)))
     ((and (not own)
           (or (string-match-p "\\`/@[A-Za-z0-9_]+/[A-Za-z0-9_-]+/?\\'" path)
               (string-match-p "\\`/users/[A-Za-z0-9_]+/statuses/[A-Za-z0-9_-]+/?\\'" path)))
      (list 'remote text))
     (t (list 'browser text)))))

(defun misskey-navigation--boundary-p (text start)
  "Whether START in TEXT can begin a mention or hashtag."
  (or (= start 0)
      (not (string-match-p "[[:alnum:]_@#./:%+\\\\-]"
                           (substring text (1- start) start)))))

(defun misskey-navigation--trim-url (text)
  "Trim sentence punctuation and unmatched closing delimiters from TEXT."
  (setq text (string-trim-right text "[.,!?;:]+"))
  (dolist (pair '((?\) . ?\() (?\] . ?\[) (?\} . ?\{)))
    (while (and (> (length text) 0)
                (= (aref text (1- (length text))) (car pair))
                (> (cl-count (car pair) text) (cl-count (cdr pair) text)))
      (setq text (substring text 0 -1))))
  text)

(defun misskey-navigation-propertize (text note)
  "Return TEXT unchanged except ordered navigation properties for NOTE.
Backtick code and plain literals remain inert.  Bare mentions inherit the
actual text author's host, including in quoted Notes."
  (let ((result (copy-sequence text)) (position 0) (case-fold-search t)
        (host (alist-get 'host (misskey-note-user note))))
    (while (string-match
            "`+\\|<plain>\\|[A-Za-z][A-Za-z0-9+.-]*://[^[:space:]<>`\"]+\\|@[A-Za-z0-9_]+\\(?:@[A-Za-z0-9]+\\(?:[.-][A-Za-z0-9-]*[A-Za-z0-9]\\)*\\(?::[0-9]+\\)?\\)?\\|#[[:alnum:]_]+"
            text position)
      (let* ((start (match-beginning 0)) (end (match-end 0))
             (token (match-string 0 text)) target)
        (setq position end)
        (cond
         ((string-prefix-p "`" token)
          (setq position (if (string-match (regexp-quote token) text end)
                             (match-end 0) (length text))))
         ((equal token "<plain>")
          (setq position (if (string-match "</plain>" text end)
                             (match-end 0) (length text))))
         ((string-match-p "\\`[A-Za-z][A-Za-z0-9+.-]*://" token)
          (when (string-match-p "\\`https?://" token)
            (setq token (misskey-navigation--trim-url token) end (+ start (length token)))
            (when (condition-case nil (misskey-navigation--url token) (error nil))
              (setq target (list 'url token)))))
         ((and (misskey-navigation--boundary-p text start)
               (or (= end (length text))
                   (not (string-match-p "[[:alnum:]_@]" (substring text end (1+ end))))))
          (setq target
                (if (string-prefix-p "#" token) (list 'tag (substring token 1))
                  (list 'mention
                        (if (and (stringp host) (not (string-empty-p host))
                                 (not (string-match-p "@" (substring token 1))))
                            (concat token "@" host) token))))))
        (when target
          (add-text-properties start end
                               (list misskey-navigation-target-property target
                                     'face 'link 'mouse-face 'highlight
                                     'help-echo "RET/mouse-2: open; B: browser; w: copy link"
                                     'keymap misskey-navigation-link-map 'rear-nonsticky t)
                               result))))
    result))

(defun misskey-navigation--view ()
  "Return the initiating live view with an account."
  (let ((view (appkit-current-surface)))
    (unless (and (appkit-surface-live-p view)
                 (misskey--account-p (plist-get (appkit-surface-model view) :account)))
      (user-error "Open links from a live Misskey view"))
    view))

(defun misskey-navigation--open-native (kind value account)
  "Open native KIND with VALUE using ACCOUNT."
  (pcase kind
    ('note (require 'misskey-thread) (misskey-thread-open value account))
    ('user (require 'misskey-profile) (misskey-profile-open value account))
    ('tag (require 'misskey-search) (misskey-search-tag value account))))

(defun misskey-navigation--resolve (url view account)
  "Resolve remote URL on ACCOUNT's instance, owned exactly by VIEW."
  (let ((operation (misskey-read-begin view 'navigation))
        (snapshot (copy-misskey--account account)))
    (misskey-http-read
     "ap/show" (list :uri url)
     (lambda (payload)
       (when (and (misskey-read-finish operation)
                  (equal snapshot (plist-get (appkit-surface-model view) :account))
                  (equal snapshot (misskey--session-account
                                   (misskey--session (appkit-surface-app view)))))
         (let* ((object (alist-get 'object payload)) (id (alist-get 'id object))
                (kind (pcase (alist-get 'type payload) ("Note" 'note) ("User" 'user))))
           (if (and kind (misskey--valid-user-id-p id))
               (misskey-navigation--open-native kind id account)
             (message "Unrecognized federated object; B opens the link in your browser")))))
     :errback (lambda (failure)
                (when (and (misskey-read-finish operation)
                           (equal snapshot (plist-get (appkit-surface-model view) :account)))
                  (message "%s; B opens the link in your browser" failure)))
     :account account :owner operation)))

(defun misskey-navigation-open-url (url &optional view)
  "Open URL natively when recognized, otherwise explicitly in the browser.
VIEW supplies the account and owns any asynchronous federation resolution."
  (let ((owner (or view (misskey-navigation--view))))
    (unless (and (appkit-surface-live-p owner)
                 (misskey--account-p (plist-get (appkit-surface-model owner) :account)))
      (user-error "Open links from a live Misskey view"))
    (let* ((account (plist-get (appkit-surface-model owner) :account))
           (route (misskey-navigation--route url account)))
      (misskey-read-cancel owner 'navigation)
      (pcase (car route)
        ('remote (misskey-navigation--resolve url owner account))
        ('browser (browse-url url))
        (_ (misskey-navigation--open-native (car route) (cadr route) account))))))

(defun misskey-navigation-activate nil
  "Activate the exact link, action, button, or author, or open the note thread."
  (interactive)
  (cond
   ((get-text-property (point) misskey-navigation-target-property)
    (let*
        ((target
          (get-text-property (point)
                             misskey-navigation-target-property))
         (view (misskey-navigation--view))
         (account (plist-get (appkit-surface-model view) :account)))
      (unless (eq (car target) 'url)
        (misskey-read-cancel view 'navigation))
      (pcase (car target)
        ('url (misskey-navigation-open-url (cadr target) view))
        ('mention
         (misskey-navigation--open-native 'user (cadr target) account))
        ('tag
         (misskey-navigation--open-native 'tag (cadr target) account)))))
   ((get-text-property (point) appkit-ui-action-property)
    (appkit-ui-activate-at))
   ((button-at (point)) (button-activate (button-at (point))))
   ((get-text-property (point) misskey-user-property)
    (let*
        ((view (misskey-navigation--view))
         (account (plist-get (appkit-surface-model view) :account)))
      (misskey-read-cancel view 'navigation)
      (misskey-navigation--open-native 'user
                                       (get-text-property (point)
                                                          misskey-user-property)
                                       account)))
   (t (misskey-read-cancel (misskey-navigation--view) 'navigation)
      (require 'misskey-thread) (misskey-thread-at-point))))


(defun misskey-navigation-mouse-activate (event)
  "Activate the exact text span clicked by EVENT."
  (interactive "e")
  (mouse-set-point event)
  (misskey-navigation-activate))

(defun misskey-navigation--note-url (view)
  "Return the contextual Note URL in VIEW, never a text span target."
  (let* ((note (misskey-note-display-note (misskey-render-note-at-point)))
         (account (plist-get (appkit-surface-model view) :account))
         (id (and note (misskey-note-id note))))
    (unless id (user-error "No Misskey note at point"))
    (or (alist-get 'url note) (alist-get 'uri note)
        (concat (misskey--account-origin account) "/notes/" (url-hexify-string id)))))

(defun misskey-navigation--link-url (view)
  "Return the exact link at point, or the contextual Note URL in VIEW."
  (let* ((target (get-text-property (point) misskey-navigation-target-property))
         (account (plist-get (appkit-surface-model view) :account)))
    (pcase (car target)
      ('url (cadr target))
      ('tag (concat (misskey--account-origin account) "/tags/" (url-hexify-string (cadr target))))
      ('mention
       (let* ((parts (split-string (substring (cadr target) 1) "@")) (host (cadr parts)))
         (concat (if host (concat "https://" host) (misskey--account-origin account))
                 "/@" (url-hexify-string (car parts)))))
      (_ (misskey-navigation--note-url view)))))

(defun misskey-navigation-open-note-url (url)
  "Open a Note or profile URL using the initiating Misskey view's account."
  (interactive "sNote or profile URL: ")
  (misskey-navigation-open-url url))

(defun misskey-navigation-browse ()
  "Open the exact link or contextual Note in the browser without resolution."
  (interactive)
  (let* ((view (misskey-navigation--view)) (url (misskey-navigation--link-url view)))
    (misskey-navigation--url url)
    (misskey-read-cancel view 'navigation)
    (browse-url url)))

(defun misskey-navigation-copy-link ()
  "Copy the exact link or contextual Note URL without text properties."
  (interactive)
  (let ((url (misskey-navigation--link-url (misskey-navigation--view))))
    (misskey-navigation--url url)
    (kill-new (substring-no-properties url))
    (message "Copied Misskey link")))

(provide 'misskey-navigation)
;;; misskey-navigation.el ends here
