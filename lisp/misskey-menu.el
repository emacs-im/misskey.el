;;; misskey-menu.el --- Contextual Misskey Transient menus -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "29.1") (transient "0.13.7"))

;;; Commentary:
;; Menus operate on the invoking buffer.  Draft controls call the compose
;; commands directly, so generation tracking and publication guards stay there.

;;; Code:

(require 'transient)
(require 'misskey-compose)
(require 'misskey-navigation)

(declare-function misskey-home "misskey" ())
(declare-function misskey-compose "misskey" ())
(declare-function misskey-notifications "misskey-notifications" (&optional account))
(declare-function misskey-search "misskey-search" (query &optional account))
(declare-function misskey-search-tag "misskey-search" (tag &optional account))
(declare-function misskey-thread "misskey" (note-id))
(declare-function misskey-thread-at-point "misskey-thread" ())
(declare-function misskey-profile-open-at-point "misskey-profile" ())
(declare-function misskey-profile-followers "misskey-profile" ())
(declare-function misskey-profile-following "misskey-profile" ())
(declare-function misskey-profile-next-mode "misskey-profile" ())
(declare-function misskey-timeline-next-kind "misskey-timeline" ())
(declare-function misskey-react-at-point "misskey-actions" (reaction))
(declare-function misskey-unreact-at-point "misskey-actions" ())
(declare-function misskey-favorite-at-point "misskey-actions" ())
(declare-function misskey-unfavorite-at-point "misskey-actions" ())
(declare-function misskey-renote-at-point "misskey-actions" ())
(declare-function misskey-delete-note-at-point "misskey-actions" ())
(declare-function misskey-follow-at-point "misskey-actions" ())
(declare-function misskey-unfollow-at-point "misskey-actions" ())

(defun misskey-menu--source ()
  "Return the originating buffer of the current Misskey menu, if live."
  (let ((buffer (transient-scope '(misskey-menu misskey-compose-menu))))
    (and (buffer-live-p buffer) buffer)))

(defun misskey-menu--in-source (function &rest arguments)
  "Call FUNCTION with ARGUMENTS in the originating buffer.
Used as Transient advice for both interactive argument reading and execution."
  (let ((buffer (misskey-menu--source)))
    (unless buffer
      (user-error "The originating Misskey buffer is no longer live"))
    (with-current-buffer buffer
      (apply function arguments))))

(defun misskey-menu--view-state ()
  "Return the live originating Misskey view's model, or nil."
  (when-let* ((buffer (misskey-menu--source)))
    (with-current-buffer buffer
      (when-let* ((view (appkit-current-surface))
                  ((appkit-surface-live-p view))
                  (state (appkit-surface-model view))
                  ((misskey--account-p (plist-get state :account))))
        state))))

(defun misskey-menu--note ()
  "Return the displayed note in the originating live view, or nil."
  (when (misskey-menu--view-state)
    (misskey-menu--in-source
     (lambda ()
       (let ((note (get-text-property (point) misskey-note-property)))
         (and (consp note) (misskey-note-display-note note)))))))

(defun misskey-menu--raw-note-p ()
  "Whether the originating view has an actual note row."
  (and (misskey-menu--view-state)
       (misskey-menu--in-source
        (lambda ()
          (consp (get-text-property (point) misskey-note-property))))))

(defun misskey-menu--user-p ()
  "Whether the originating view has a user or Note author at point."
  (and (misskey-menu--view-state)
       (misskey-menu--in-source
        (lambda ()
          (consp (or (get-text-property (point) misskey-user-property)
                     (misskey-note-user (misskey-menu--note))))))))

(defun misskey-menu--link-p ()
  "Whether the originating view has a text link or displayed note."
  (and (misskey-menu--view-state)
       (or (misskey-menu--note)
           (misskey-menu--in-source
            (lambda ()
              (get-text-property (point) misskey-navigation-target-property))))))

(defun misskey-menu--activation-p ()
  "Whether point in the originating view has a link or content warning."
  (and (misskey-menu--view-state)
       (or (misskey-menu--in-source
            (lambda ()
              (get-text-property (point) misskey-navigation-target-property)))
           (alist-get 'cw (misskey-menu--note)))))

(defun misskey-menu--profile-p ()
  "Whether the originating view has loaded a profile."
  (let ((state (misskey-menu--view-state)))
    (and (eq (plist-get state :type) 'profile)
         (plist-get state :profile-user))))

(defun misskey-menu--timeline-p ()
  "Whether the originating view is a timeline."
  (eq (plist-get (misskey-menu--view-state) :type) 'timeline))

(defun misskey-menu--notifications-p ()
  "Whether the originating view is the notification inbox."
  (eq (plist-get (misskey-menu--view-state) :type) 'notifications))

(defun misskey-menu--draft-p ()
  "Whether the originating buffer is a Misskey draft."
  (when-let* ((buffer (misskey-menu--source)))
    (with-current-buffer buffer
      (derived-mode-p 'misskey-compose-mode))))

(defun misskey-menu--draft-idle-p ()
  "Whether the originating draft can currently be edited."
  (and (misskey-menu--draft-p)
       (misskey-menu--in-source
        (lambda () (not (appkit-compose-operation-active-p))))))

(defun misskey-menu--local-editable-p ()
  "Whether the originating draft permits changing localOnly."
  (and (misskey-menu--draft-idle-p)
       (misskey-menu--in-source
        (lambda () (not misskey-compose--target-local-only)))))

(defun misskey-menu--specified-p ()
  "Whether the originating idle draft accepts recipients."
  (and (misskey-menu--draft-idle-p)
       (misskey-menu--in-source
        (lambda () (eq misskey-compose-visibility 'specified)))))

(defun misskey-menu--recipients-p ()
  "Whether the originating idle draft has removable recipients."
  (and (misskey-menu--draft-idle-p)
       (misskey-menu--in-source (lambda () misskey-compose-recipients))))

(defun misskey-menu--extra-parts-p ()
  "Whether the originating idle draft has an extra note to remove."
  (and (misskey-menu--draft-idle-p)
       (misskey-menu--in-source
        (lambda () (> (length (appkit-chat-compose-items)) 1)))))

(defun misskey-menu--attachments-p ()
  "Whether the originating idle draft part has removable attachments."
  (and (misskey-menu--draft-idle-p)
       (misskey-menu--in-source
        (lambda ()
          (plist-get (nth (or (appkit-chat-compose-current-part-index) 0)
                          (appkit-chat-compose-items))
                     :attachments)))))

(defun misskey-menu--draft-status ()
  "Describe the original draft using its live compose status fields."
  (if (not (misskey-menu--draft-p))
      "Draft no longer available"
    (misskey-menu--in-source
     (lambda ()
       (mapconcat (lambda (field)
                    (format "%s: %s" (plist-get field :label)
                            (plist-get field :value)))
                  (misskey-compose--status-fields) "\n")))))

(defun misskey-menu--visibility-label ()
  "Describe the original draft's current visibility."
  (if (misskey-menu--draft-p)
      (misskey-menu--in-source
       (lambda ()
         (format "Visibility: %s"
                 (alist-get misskey-compose-visibility
                            misskey-compose--visibility-choices))))
    "Visibility"))

(defun misskey-menu--cw-label ()
  "Describe the original draft's current content warning."
  (if (misskey-menu--draft-p)
      (misskey-menu--in-source
       (lambda () (format "CW: %s" (or misskey-compose-cw "None"))))
    "CW"))

(defun misskey-menu--local-label ()
  "Describe the original draft's current localOnly flag."
  (if (misskey-menu--draft-p)
      (misskey-menu--in-source
       (lambda ()
         (format "localOnly: %s" (if misskey-compose-local-only "yes" "no"))))
    "localOnly"))

;;;###autoload (autoload 'misskey-menu "misskey-menu" nil t)
(transient-define-prefix misskey-menu ()
  "Browse Misskey views and act on the originating view's context.
Outside a live Misskey view, only global open commands are available."
  [["Open" :advice* misskey-menu--in-source ("h" "Home" misskey-home)
    ("n" "Notifications" misskey-notifications)
    ("s" "Search notes" misskey-search)
    ("#" "Hashtag" misskey-search-tag)
    ("t" "Thread by ID" misskey-thread)
    ("o" "Note URL" misskey-navigation-open-note-url :inapt-if-not
     misskey-menu--view-state)
    ("c" "Compose" misskey-compose)]
   ["Context" :advice* misskey-menu--in-source
    ("RET" "Link / CW" misskey-navigation-activate :inapt-if-not
     misskey-menu--activation-p)
    ("T" "Note thread" misskey-thread-at-point :inapt-if-not
     misskey-menu--note)
    ("u" "User profile" misskey-profile-open-at-point :inapt-if-not
     misskey-menu--user-p)
    ("b" "Browser" misskey-navigation-browse :inapt-if-not
     misskey-menu--link-p)
    ("y" "Copy link" misskey-navigation-copy-link :inapt-if-not
     misskey-menu--link-p)]
   ["View" :advice* misskey-menu--in-source
    ("M" "Mark notifications read" misskey-notifications-mark-all-read :inapt-if-not misskey-menu--notifications-p)
    ("v" "Next timeline" misskey-timeline-next-kind :inapt-if-not
     misskey-menu--timeline-p)
    ("V" "Next profile view" misskey-profile-next-mode :inapt-if-not
     misskey-menu--profile-p)
    ("[" "Followers" misskey-profile-followers :inapt-if-not
     misskey-menu--profile-p)
    ("]" "Following" misskey-profile-following :inapt-if-not
     misskey-menu--profile-p)]]
  [["Note" :advice* misskey-menu--in-source
    ("r" "Reply" misskey-compose-reply-at-point :inapt-if-not
     misskey-menu--note)
    ("Q" "Quote" misskey-compose-quote-at-point :inapt-if-not
     misskey-menu--note)
    ("R" "Renote" misskey-renote-at-point :inapt-if-not
     misskey-menu--note)
    ("d" "Delete exact row" misskey-delete-note-at-point :inapt-if-not
     misskey-menu--raw-note-p)]
   ["React / favorite" :advice* misskey-menu--in-source
    ("a" "React" misskey-react-at-point :inapt-if-not
     misskey-menu--note)
    ("A" "Remove reaction" misskey-unreact-at-point :inapt-if-not
     misskey-menu--note)
    ("f" "Favorite" misskey-favorite-at-point :inapt-if-not
     misskey-menu--note)
    ("F" "Unfavorite" misskey-unfavorite-at-point :inapt-if-not
     misskey-menu--note)]
   ["User" :advice* misskey-menu--in-source
    ("+" "Follow" misskey-follow-at-point :inapt-if-not
     misskey-menu--user-p)
    ("-" "Unfollow" misskey-unfollow-at-point :inapt-if-not
     misskey-menu--user-p)]]
  (interactive) (require 'misskey)
  (transient-setup 'misskey-menu nil nil :scope (current-buffer)))

;;;###autoload (autoload 'misskey-compose-menu "misskey-menu" nil t)
(transient-define-prefix misskey-compose-menu ()
  "Edit the originating Misskey draft's live controls; publish only with C-c C-c."
  :refresh-suffixes t
  [:description misskey-menu--draft-status
                ["Audience / content" :advice* misskey-menu--in-source
                 ("v" misskey-menu--visibility-label
                  misskey-compose-set-visibility :transient t
                  :inapt-if-not misskey-menu--draft-idle-p)
                 ("w" misskey-menu--cw-label misskey-compose-set-cw
                  :transient t :inapt-if-not
                  misskey-menu--draft-idle-p)
                 ("l" misskey-menu--local-label
                  misskey-compose-toggle-local-only :transient t
                  :inapt-if-not misskey-menu--local-editable-p)
                 ("r" "Add recipient" misskey-compose-add-recipient
                  :transient t :inapt-if-not misskey-menu--specified-p)
                 ("R" "Remove recipient"
                  misskey-compose-remove-recipient :transient t
                  :inapt-if-not misskey-menu--recipients-p)]
                ["Files / parts" :advice* misskey-menu--in-source
                 ("a" "Attach file" misskey-compose-attach-file
                  :transient t :inapt-if-not
                  misskey-menu--draft-idle-p)
                 ("d" "Remove attachment"
                  misskey-compose-remove-attachment :transient t
                  :inapt-if-not misskey-menu--attachments-p)
                 ("+" "Add note" misskey-compose-add-note :transient t
                  :inapt-if-not misskey-menu--draft-idle-p)
                 ("-" "Remove note" misskey-compose-remove-note
                  :transient t :inapt-if-not
                  misskey-menu--extra-parts-p)]
                ["Publish" :advice* misskey-menu--in-source
                 ("g" "Refresh instance limit"
                  misskey-compose-refresh-metadata :transient t
                  :inapt-if-not misskey-menu--draft-idle-p)
                 ("C-c C-c" "Publish" misskey-compose-send
                  :inapt-if-not misskey-menu--draft-idle-p)]]
  (interactive)
  (unless (derived-mode-p 'misskey-compose-mode)
    (user-error "Open this menu from a Misskey draft"))
  (transient-setup 'misskey-compose-menu nil nil :scope
                   (current-buffer)))

(provide 'misskey-menu)
;;; misskey-menu.el ends here
