;;; misskey-profile.el --- Browse Misskey user profiles -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Resolve one Misskey user and present profile details plus switchable note,
;; reply-inclusive, and media feeds in one Appkit view.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-discussion)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'misskey-actions)
(require 'misskey-auth)
(require 'misskey-compose)
(require 'misskey-core)
(require 'misskey-feed)
(require 'misskey-http)
(require 'misskey-note)
(require 'misskey-render)
(require 'misskey-thread)

(declare-function misskey-profile-open-followers "misskey-directory"
                  (user &optional account))
(declare-function misskey-profile-open-following "misskey-directory"
                  (user &optional account))

(defcustom misskey-profile-note-limit 20
  "Maximum number of notes requested for each profile page."
  :type 'integer
  :group 'misskey)

(defconst misskey-profile--mode-specs
  '((notes "Notes" (:withReplies :json-false))
    (replies "Notes + replies" (:withReplies t))
    (media "Media" (:withFiles t)))
  "Profile note modes with labels and `users/notes' parameters.")

(defconst misskey-profile--request-key 'profile-user
  "Operation key for the active profile lookup.")

(declare-function misskey-menu "misskey-menu" nil)

(defvar-keymap misskey-profile-mode-map
  :doc "Keymap for `misskey-profile-mode'." :parent special-mode-map
  "TAB" #'misskey-profile-next-mode "g" #'misskey-profile-refresh "N"
  #'misskey-profile-load-more "n" #'appkit-discussion-next-entry "p"
  #'appkit-discussion-previous-entry "RET"
  #'misskey-navigation-activate "<mouse-2>"
  #'misskey-navigation-mouse-activate "O"
  #'misskey-navigation-open-note-url "B" #'misskey-navigation-browse
  "w" #'misskey-navigation-copy-link "t" #'misskey-thread-at-point "a"
  misskey-actions-map "r" #'misskey-compose-reply-at-point "q"
  #'misskey-compose-quote-at-point "f" #'misskey-profile-followers "F"
  #'misskey-profile-following "?" #'misskey-menu)

(define-derived-mode misskey-profile-mode special-mode "Misskey-Profile"
  "Major mode for one Misskey user profile."
  (setq-local header-line-format '(:eval (misskey-profile--header-line)))
  (setq-local line-spacing 0)
  (visual-line-mode 1))

(defun misskey-profile--current-view ()
  "Return the current live Misskey profile view, or nil."
  (misskey-feed-current-view 'profile))

(defun misskey-profile--state (view)
  "Return VIEW's validated profile state."
  (let ((state (misskey-feed-view-state view 'profile)))
    (unless (and (assq (plist-get state :profile-mode)
                       misskey-profile--mode-specs)
                 (listp (plist-get state :profile-reference)))
      (error "Invalid Misskey profile view state"))
    state))

(defun misskey-profile--mode-spec (mode)
  "Return the profile mode specification for MODE."
  (or (assq mode misskey-profile--mode-specs)
      (error "Invalid Misskey profile mode: %S" mode)))

(defun misskey-profile--next-mode (mode)
  "Return the profile mode following MODE."
  (let ((tail (memq (misskey-profile--mode-spec mode)
                    misskey-profile--mode-specs)))
    (car (or (cadr tail) (car misskey-profile--mode-specs)))))

(defun misskey-profile--reference (user)
  "Return normalized API reference parameters for USER.

USER may be a Misskey user object, a user ID, or `@username@host'."
  (cond
   ((consp user)
    (let ((id (misskey-user-id user)))
      (unless (and (stringp id) (not (string-empty-p id)))
        (user-error "Misskey user has no stable ID"))
      (list :userId id)))
   ((and (stringp user) (string-prefix-p "@" user))
    (let* ((parts (split-string (substring user 1) "@" t))
           (username (car parts))
           (host (cadr parts)))
      (unless (and username (<= (length parts) 2))
        (user-error "Use @username or @username@host"))
      (append (list :username username)
              (and host (list :host host)))))
   ((and (stringp user) (not (string-empty-p user)))
    (list :userId user))
   (t
    (user-error "Misskey user must be an ID or @username[@host]"))))

(defun misskey-profile--reference-key (reference)
  "Return a stable view key for normalized REFERENCE."
  (or (plist-get reference :userId)
      (concat "@" (plist-get reference :username)
              (if-let* ((host (plist-get reference :host)))
                  (concat "@" host)
                ""))))

(defun misskey-profile--validate-user (payload)
  "Return validated profile user PAYLOAD."
  (unless (and (consp payload)
               (stringp (misskey-user-id payload))
               (not (string-empty-p (misskey-user-id payload)))
               (stringp (alist-get 'username payload)))
    (error "Misskey returned a malformed user profile"))
  payload)

(defun misskey-profile--count (user key label)
  "Return USER's nonnegative count at KEY formatted with LABEL."
  (let ((count (alist-get key user)))
    (when (and (integerp count) (>= count 0))
      (format "%d %s" count label))))

(defun misskey-profile--frame (state)
  "Return the generated profile frame for STATE."
  (let*
      ((user (plist-get state :profile-user))
       (phase (plist-get state :phase))
       (items (plist-get state :items))
       (message (plist-get state :message))
       (view (appkit-current-surface))
       (app
        (and (appkit-surface-live-p view) (appkit-surface-app view))))
    (if (not user)
        (concat (propertize "Misskey profile" 'face 'bold) "\n"
                (if (eq phase 'error)
                    (format "Unable to load profile.\n%s\n\n" message)
                  "Loading user...\n\n"))
      (let*
          ((description (alist-get 'description user))
           (user-id (misskey-user-id user))
           (following-p
            (if (appkit-app-live-p app)
                (misskey-user-state-value app user-id :following-p
                                          (eq
                                           (alist-get 'isFollowing
                                                      user)
                                           t))
              (eq (alist-get 'isFollowing user) t))))
        (concat
         (propertize (misskey-user-label user) 'face 'bold
                     misskey-user-property user
                     misskey-user-id-property user-id)
         "  "
         (propertize (misskey-user-handle user) 'face 'shadow
                     misskey-user-property user
                     misskey-user-id-property user-id)
         "\n"
         (when
             (and (stringp description)
                  (not (string-empty-p description)))
           (concat description "\n"))
         (string-join
          (delq nil
                (list (and following-p "following")
                      (misskey-profile--count user 'notesCount "notes")
                      (misskey-profile--count user 'followersCount
                                              "followers")
                      (misskey-profile--count user 'followingCount
                                              "following")))
          " · ")
         "\n\n"
         (pcase phase
           ('initial "Loading notes...\n\n")
           ('refresh "Refreshing notes...\n\n")
           ('older "Loading older notes...\n\n")
           ('error (format "Unable to load profile.\n%s\n\n" message))
           (_ (if items "" "No notes returned.\n\n"))))))))

(defun misskey-profile--footer (state)
  "Return the generated profile footer for STATE."
  (concat
   "\nTAB next profile mode   f followers   F following"
   (misskey-feed-default-footer state)))

(defun misskey-profile--header-line ()
  "Return the current profile mode header line."
  (when-let* ((view (misskey-profile--current-view)))
    (let ((active (plist-get (misskey-profile--state view) :profile-mode)))
      (concat
       " "
       (string-join
        (mapcar
         (lambda (spec)
           (propertize (cadr spec)
                       'face (if (eq active (car spec))
                                 'mode-line-emphasis
                               'shadow)))
         misskey-profile--mode-specs)
        "   ")))))

(defun misskey-profile--note-parameters (state mode)
  "Return `users/notes' parameters for profile STATE and MODE."
  (let* ((user (plist-get state :profile-user))
         (parameters (copy-sequence
                      (caddr (misskey-profile--mode-spec mode)))))
    (plist-put parameters :userId (misskey-user-id user))))

(defun misskey-profile--handle-error (view state failure)
  "Install profile FAILURE in VIEW STATE."
  (setf (plist-get state :profile-loading-p) nil
        (plist-get state :phase) 'error (plist-get state :message)
        failure)
  (misskey-dispatch view
                    (list :render
                          (appkit-projection-change-create :frame-p t
                                                           :position
                                                           'preserve)))
  (message "%s" failure))

(defun misskey-profile--handle-user (view state observation payload)
  "Install profile PAYLOAD in VIEW STATE using OBSERVATION."
  (condition-case err
      (let ((user (misskey-profile--validate-user payload)))
        (misskey-merge-user-state (appkit-surface-app view) user
                                  observation)
        (setf (plist-get state :profile-loading-p) nil
              (plist-get state :profile-user) user
              (plist-get state :title) (misskey-user-label user))
        (misskey-feed-reset-query view "users/notes"
                                  (misskey-profile--note-parameters
                                   state
                                   (plist-get state :profile-mode))))
    (error
     (misskey-profile--handle-error view state
                                    (error-message-string err)))))

(defun misskey-profile--request-user (view)
  "Resolve and load VIEW's profile user."
  (let ((state (misskey-profile--state view)))
    (misskey-feed-cancel-request view)
    (let*
        ((observation
          (misskey-state-observe (appkit-surface-app view)))
         (operation
          (misskey-read-begin view misskey-profile--request-key)))
      (setf (plist-get state :profile-loading-p) t
            (plist-get state :phase) 'initial
            (plist-get state :message) nil)
      (misskey-dispatch view
                        (list :render
                              (appkit-projection-change-create
                               :frame-p t :position 'preserve)))
      (misskey-http-read "users/show"
                         (plist-get state :profile-reference)
                         (lambda (payload)
                           (when (misskey-read-finish operation)
                             (misskey-profile--handle-user view state
                                                           observation
                                                           payload)))
                         :errback
                         (lambda (failure)
                           (when (misskey-read-finish operation)
                             (misskey-profile--handle-error view state
                                                            failure)))
                         :account (plist-get state :account) :owner
                         operation))))

(defun misskey-profile--setup-view (view)
  "Initialize profile VIEW and request its user."
  (misskey-feed-setup-view view)
  (misskey-profile--request-user view))

(defun misskey-profile-switch-mode (mode)
  "Switch the current profile view to note MODE."
  (interactive
   (list
    (intern
     (completing-read
      "Profile mode: "
      (mapcar (lambda (spec) (symbol-name (car spec)))
              misskey-profile--mode-specs)
      nil t))))
  (if-let* ((view (misskey-profile--current-view)))
      (let ((state (misskey-profile--state view)))
        (misskey-profile--mode-spec mode)
        (unless (eq mode (plist-get state :profile-mode))
          (setf (plist-get state :profile-mode) mode)
          (unless (plist-get state :profile-loading-p)
            (unless (plist-get state :profile-user)
              (user-error "The current profile has not loaded a user"))
            (misskey-feed-reset-query
             view "users/notes"
             (misskey-profile--note-parameters state mode)))
          (force-mode-line-update)))
    (user-error "Current buffer is not a Misskey profile")))

(defun misskey-profile-next-mode ()
  "Switch to the next profile note mode."
  (interactive)
  (if-let* ((view (misskey-profile--current-view)))
      (misskey-profile-switch-mode
       (misskey-profile--next-mode
        (plist-get (misskey-profile--state view) :profile-mode)))
    (user-error "Current buffer is not a Misskey profile")))

(defun misskey-profile-refresh ()
  "Refresh the current Misskey profile and its active note mode."
  (interactive)
  (if-let* ((view (misskey-profile--current-view)))
      (misskey-profile--request-user view)
    (user-error "Current buffer is not a Misskey profile")))

(defun misskey-profile-load-more ()
  "Load one older page in the current Misskey profile."
  (interactive)
  (if-let* ((view (misskey-profile--current-view)))
      (let ((state (misskey-profile--state view)))
        (when (plist-get state :profile-loading-p)
          (user-error "The Misskey profile is still loading its user"))
        (unless (plist-get state :profile-user)
          (user-error "The current profile has not loaded a user"))
        (unless (plist-get state :items)
          (user-error "The Misskey profile has no notes"))
        (misskey-feed-load-more view))
    (user-error "Current buffer is not a Misskey profile")))

(defun misskey-profile-followers ()
  "Open the current profile user's followers directory."
  (interactive)
  (if-let* ((view (misskey-profile--current-view))
            (state (misskey-profile--state view))
            (user (plist-get state :profile-user)))
      (misskey-profile-open-followers user (plist-get state :account))
    (user-error "The current profile has not loaded a user")))

(defun misskey-profile-following ()
  "Open the users followed by the current profile user."
  (interactive)
  (if-let* ((view (misskey-profile--current-view))
            (state (misskey-profile--state view))
            (user (plist-get state :profile-user)))
      (misskey-profile-open-following user (plist-get state :account))
    (user-error "The current profile has not loaded a user")))

(defun misskey-profile-open-at-point ()
  "Open the Misskey user carried at point in the source view's account."
  (interactive)
  (if-let*
      ((user (get-text-property (point) misskey-user-property))
       (view (appkit-current-surface)) ((appkit-surface-live-p view))
       (account (plist-get (appkit-surface-model view) :account)))
      (misskey-profile-open user account)
    (user-error "No Misskey user at point")))

;;;###autoload
(defun misskey-profile-open (user &optional account)
  "Open USER's Misskey profile for ACCOUNT.

USER is a user object, stable ID, or `@username[@host]'.  ACCOUNT defaults to
the account selected by current customization."
  (interactive "sMisskey user ID or @username[@host]: ")
  (let*
      ((target (or account (misskey--current-account)))
       (_token
        (and (called-interactively-p 'interactive)
             (misskey-auth--ensure-token target)))
       (app (misskey-app target))
       (reference (misskey-profile--reference user))
       (reference-key (misskey-profile--reference-key reference))
       (id (list 'profile reference-key))
       (existing (appkit-app-surface app id))
       (state
        (or (and existing (appkit-surface-model existing))
            (let
                ((feed
                  (misskey-feed-make-state :type 'profile :account
                                           target :title "Profile"
                                           :limit
                                           misskey-profile-note-limit
                                           :header-function
                                           #'misskey-profile--frame
                                           :footer-function
                                           #'misskey-profile--footer)))
              (setf (plist-get feed :profile-reference) reference
                    (plist-get feed :profile-user) nil
                    (plist-get feed :profile-mode) 'notes
                    (plist-get feed :profile-loading-p) nil)
              feed)))
       (view
        (misskey-open-surface :app app :identity id :mode
                              #'misskey-profile-mode :buffer-name
                              (format "*misskey profile %s*"
                                      reference-key)
                              :input state :setup
                              #'misskey-profile--setup-view :select t)))
    (unless (plist-get state :profile-user)
      (unless (plist-get state :profile-loading-p)
        (misskey-profile--request-user view)))
    view))

(provide 'misskey-profile)

;;; misskey-profile.el ends here
