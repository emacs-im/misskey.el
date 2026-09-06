;;; misskey-navigation-test.el --- Navigation regressions -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'misskey-search)
(require 'misskey-timeline)
(require 'misskey-test-helper
         (expand-file-name "misskey-test-helper"
                           (file-name-directory (or load-file-name buffer-file-name))))

(defun misskey-navigation-test--note (id text &optional host)
  "Return a Note fixture with ID, TEXT, and optional author HOST."
  `((id . ,id) (text . ,text) (visibility . "public")
    (user . ((id . ,(concat "user-" id)) (username . "author") (host . ,host)))))

(defun misskey-navigation-test--targets (text)
  "Return ordered distinct link spans in propertized TEXT."
  (let ((position 0) targets)
    (while (< position (length text))
      (when-let* ((target (get-text-property position misskey-navigation-target-property text)))
        (push target targets))
      (setq position (next-single-property-change position misskey-navigation-target-property
                                                  text (length text))))
    (nreverse targets)))

(ert-deftest misskey-navigation-code-email-and-url-boundaries ()
  (let* ((text "@alice @bob@elsewhere.social. #日本語 email+tag@example.social `@code #code https://code.test` ```\n#fenced @fenced\n``` <plain>@plain #plain</plain> https://remote.social/@url#fragment ftp://host/@ftp#tag (https://example.org/a(b)).")
         (rendered (misskey-navigation-propertize
                    text (misskey-navigation-test--note "n" text "author.social"))))
    (should (equal (substring-no-properties rendered) text))
    (should (equal (misskey-navigation-test--targets rendered)
                   '((mention "@alice@author.social") (mention "@bob@elsewhere.social")
                     (tag "日本語") (url "https://remote.social/@url#fragment")
                     (url "https://example.org/a(b)"))))
    (should-not (misskey-navigation-test--targets
                 (misskey-navigation-propertize "`unclosed @hidden #hidden"
                                                (misskey-navigation-test--note "n" ""))))))

(ert-deftest misskey-navigation-cw-quote-and-action-identities ()
  (misskey-test-with-session
    (with-temp-buffer
      (let* ((primary (misskey-navigation-test--note "outer" "@local"))
             (quote (misskey-navigation-test--note "quote" "@remote #inside" "far.social"))
             (_ (push '(cw . "guarded") quote))
             (_ (push (cons 'renote quote) primary))
             (view (misskey-test-surface
                    :identity 'navigation-render
                    :input (list :account (misskey--current-account)
                                 :revealed-content (make-hash-table :test #'equal))))
             (properties (misskey-render-note-properties primary)))
        (misskey-render--insert-body view primary nil "" properties)
        (should (equal (misskey-navigation-test--targets (buffer-string)) '((mention "@local"))))
        (should-not (string-match-p "@remote" (buffer-string)))
        (erase-buffer)
        (misskey-render--insert-body view primary t "" properties)
        (should (equal (misskey-navigation-test--targets (buffer-string))
                       '((mention "@local") (mention "@remote@far.social") (tag "inside"))))
        (goto-char (point-min)) (search-forward "@remote") (backward-char)
        (should (eq (misskey-render-note-at-point) primary))
        (should-not (get-text-property (point) misskey-user-property))))))

(ert-deftest misskey-navigation-url-authority-and-decoded-boundaries ()
  (let ((account (misskey--account-create :origin "https://example.social"
                                          :auth-source-user "TOKEN" :remote-user-id "self")))
    (should (equal (misskey-navigation--route "https://EXAMPLE.social:443/notes/abc" account)
                   '(note "abc")))
    (should (equal (misskey-navigation--route "https://example.social/@alice" account)
                   '(user "@alice")))
    (should (equal (car (misskey-navigation--route "https://example.social.evil/notes/abc" account)) 'remote))
    (should (equal (car (misskey-navigation--route "https://example.social:444/notes/abc" account)) 'remote))
    (should (equal (car (misskey-navigation--route "https://remote.social/article?url=/notes/abc" account)) 'browser))
    (should (equal (car (misskey-navigation--route "https://example.social/NOTES/abc" account)) 'browser))
    (dolist (url '("https://example.social@evil.test/notes/abc"
                   "https://evil.test\\@example.social/notes/abc"
                   "javascript:alert(1)" "https://example.social/notes/a%2Fb"
                   "https://example.social/notes/a%2fb" "https://example.social/%2e%2e/notes/a"
                   "https://example.social/notes/a%252fb"))
      (should-error (misskey-navigation--route url account) :type 'user-error))))

(ert-deftest misskey-navigation-exact-span-activation-and-browser-escape ()
  (misskey-test-with-session
    (with-temp-buffer
      (let* ((account (misskey--current-account))
             (view (misskey-test-surface :identity 'activation :input (list :account account)))
             (note (misskey-navigation-test--note "note" "https://remote.social/notes/remote"))
             opened browsed toggled)
        (insert (misskey-navigation-propertize (alist-get 'text note) note) " ")
        (add-text-properties (point-min) (point-max) (misskey-render-note-properties note))
        (cl-letf (((symbol-function 'misskey-navigation-open-url)
                   (lambda (url owner) (setq opened (list url owner))))
                  ((symbol-function 'misskey-render-toggle-content-warning)
                   (lambda () (setq toggled t)))
                  ((symbol-function 'browse-url) (lambda (url &rest _) (setq browsed url))))
          (goto-char (point-min))
          (misskey-navigation-activate)
          (should (equal opened (list "https://remote.social/notes/remote" view)))
          (misskey-navigation-browse)
          (should (equal browsed "https://remote.social/notes/remote"))
          (let ((kill-ring nil) (interprogram-cut-function nil) (interprogram-paste-function nil))
            (misskey-navigation-copy-link)
            (should (equal (current-kill 0) "https://remote.social/notes/remote")))
          (goto-char (1- (point-max)))
          (misskey-navigation-activate)
          (should toggled)
          (misskey-navigation-browse)
          (should (equal browsed "https://example.social/notes/note")))))))

(ert-deftest misskey-navigation-resolution-owner-account-and-replacement-fences ()
  (misskey-test-with-session
    (let ((account (misskey--current-account)) requests opened view)
      (unwind-protect
          (cl-letf (((symbol-function 'misskey-http-read)
                     (lambda (endpoint parameters callback &rest options)
                       (push (list endpoint parameters callback options) requests)))
                    ((symbol-function 'misskey-navigation--open-native)
                     (lambda (kind value target) (push (list kind value target) opened))))
            (setq view (misskey-search "owner" account))
            (misskey-navigation-open-url "https://remote.social/notes/first" view)
            (let* ((first (car requests)) (options (nth 3 first))
                   (token (plist-get options :owner)))
              (should (equal (car first) "ap/show"))
              (should (equal (plist-get options :account) account))
              (should (eq (misskey-read-token-surface token) view))
              (misskey-navigation-open-url "https://remote.social/@second" view)
              (funcall (nth 2 first) '((type . "Note") (object . ((id . "stale")))))
              (should-not opened)
              (funcall (nth 2 (car requests)) '((type . "User") (object . ((id . "local-user")))))
              (should (equal opened (list (list 'user "local-user" account)))))
            (setq opened nil)
            (misskey-navigation-open-url "https://remote.social/notes/account" view)
            (setf (plist-get (appkit-surface-model view) :account)
                  (misskey--account-create :origin "https://other.social"
                                           :auth-source-user "other" :remote-user-id "other"))
            (funcall (nth 2 (car requests)) '((type . "Note") (object . ((id . "wrong-account")))))
            (should-not opened)
            (setf (plist-get (appkit-surface-model view) :account) account)
            (misskey-navigation-open-url "https://remote.social/notes/closed" view)
            (kill-buffer (appkit-surface-buffer view))
            (funcall (nth 2 (car requests)) '((type . "Note") (object . ((id . "closed")))))
            (should-not opened))
        (when (and view (buffer-live-p (appkit-surface-buffer view)))
          (kill-buffer (appkit-surface-buffer view)))))))

(provide 'misskey-navigation-test)
