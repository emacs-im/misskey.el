;;; misskey-render.el --- Shared Misskey note rendering -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: MIT

;;; Commentary:

;; Project Misskey notes by stable identity and render them through Appkit's
;; protocol-neutral discussion geometry.  Views own only ordering, paging,
;; request state, and the set of locally revealed content warnings.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'time-date)
(require 'appkit-core)
(require 'appkit-discussion)
(require 'appkit-projection)
(require 'appkit-ui)
(require 'appkit-presentation)
(require 'misskey-core)
(require 'misskey-media)
(require 'misskey-note)
(require 'misskey-navigation)

(defun misskey-render-width ()
  "Return the current Misskey view render width in columns."
  (or (appkit-geometry-window-width) 80))

(defun misskey-render--revealed-content (view)
  "Return VIEW's content-warning reveal table."
  (let
      ((table
        (and (appkit-surface-p view) (appkit-surface-alive-p view)
             (plist-get (appkit-surface-model view) :revealed-content))))
    (unless (hash-table-p table)
      (error "Misskey note view has no content-warning state"))
    table))

(defun misskey-render-revealed-p (view note-id)
  "Return non-nil when VIEW reveals NOTE-ID's guarded content."
  (gethash note-id (misskey-render--revealed-content view)))

(defun misskey-render--positive-value (value label)
  "Return positive integer VALUE formatted with LABEL."
  (when (and (integerp value) (> value 0))
    (format "%d %s" value label)))

(defun misskey-render--note-state-value
    (app note property response-key)
  "Return APP's NOTE PROPERTY, falling back to RESPONSE-KEY."
  (let ((fallback (alist-get response-key note)))
    (if (appkit-app-live-p app)
        (misskey-note-state-value
         app (misskey-note-id note) property fallback)
      fallback)))

(defun misskey-render--display-user (note)
  "Return NOTE's displayed author, or nil."
  (when-let* ((display-note (misskey-note-display-note note)))
    (misskey-note-user display-note)))

(defun misskey-render-heading (note)
  "Return the discussion heading for NOTE without text properties."
  (if-let* ((user (misskey-render--display-user note)))
      (misskey-user-label user)
    "(deleted note)"))

(defun misskey-render--user-label (user)
  "Return USER's label carrying only USER's author properties."
  (propertize (misskey-user-label user)
              misskey-user-property user
              misskey-user-id-property (misskey-user-id user)))

(defun misskey-render--insert-heading (note)
  "Insert NOTE's displayed author heading."
  (if-let* ((user (misskey-render--display-user note)))
      (insert (misskey-render--user-label user))
    (insert "(deleted note)")))

(defun misskey-render-time (note)
  "Return NOTE's compact creation time."
  (let ((created-at (alist-get 'createdAt note)))
    (if (not (stringp created-at))
        ""
      (condition-case nil
          (format-time-string "%Y-%m-%d %H:%M" (date-to-time created-at))
        (error created-at)))))

(defun misskey-render-footer (note &optional app)
  "Return a compact metadata footer for NOTE under optional APP state."
  (let* ((display-note (misskey-note-display-note note))
         (visibility (alist-get 'visibility display-note))
         (files (alist-get 'files display-note))
         (file-count (if (listp files) (length files) 0))
         (my-reaction
          (misskey-render--note-state-value
           app display-note :my-reaction 'myReaction))
         (favorited-p
          (and (appkit-app-live-p app)
               (misskey-note-state-value
                app (misskey-note-id display-note) :favorited-p nil))))
    (string-join
     (delq nil
           (list
            (and (stringp visibility) (capitalize visibility))
            (and (eq (alist-get 'localOnly display-note) t)
                 "Local only")
            (misskey-render--positive-value
             (alist-get 'repliesCount display-note) "replies")
            (misskey-render--positive-value
             (misskey-render--note-state-value
              app display-note :renote-count 'renoteCount)
             "renotes")
            (misskey-render--positive-value
             (misskey-render--note-state-value
              app display-note :reaction-count 'reactionCount)
             "reactions")
            (and (stringp my-reaction)
                 (not (string-empty-p my-reaction))
                 (format "your reaction %s" my-reaction))
            (and favorited-p "favorited")
            (and (> file-count 0)
                 (format "%d attachment%s"
                         file-count
                         (if (= file-count 1) "" "s")))))
     " · ")))

(defun misskey-render--insert-content (note revealed prefix properties)
  "Insert NOTE content using REVEALED, PREFIX, and PROPERTIES."
  (let* ((warning (alist-get 'cw note))
         (guarded (and (stringp warning) (not (string-empty-p warning))))
         (text (alist-get 'text note)))
    (when guarded
      (let ((start (point)))
        (appkit-ui-insert-prefixed-lines
         prefix (format "CW: %s" warning)
         :face 'warning :properties properties)
        (unless revealed
          (appkit-ui-insert-prefixed-lines
           prefix "[RET to reveal]" :face 'shadow :properties properties))
        (appkit-ui-add-action
         start (1- (point)) #'misskey-render-toggle-content-warning
         :help-echo "Toggle content warning")))
    (unless (and guarded (not revealed))
      (appkit-ui-insert-prefixed-lines
       prefix
       (if (and (stringp text) (not (string-empty-p text)))
           (misskey-navigation-propertize text note)
         "(no text)")
       :properties properties))))

(defun misskey-render--insert-body
    (view note revealed prefix properties)
  "Insert NOTE body for VIEW using REVEALED, PREFIX, and PROPERTIES."
  (let*
      ((primary (misskey-note-display-note note))
       (candidate (misskey-note-quoted-note note))
       (app (appkit-surface-app view))
       (quoted
        (and candidate (not (misskey-note-deleted-p app candidate))
             candidate)))
    (when primary
      (misskey-render--insert-content primary revealed prefix
                                      properties)
      (misskey-media-insert-note-files view primary revealed prefix
                                       properties))
    (when quoted
      (appkit-ui-insert-prefixed-lines prefix
                                       (concat "Quoting "
                                               (misskey-render--user-label
                                                (misskey-note-user
                                                 quoted)))
                                       :face 'shadow :properties
                                       properties)
      (misskey-render--insert-content quoted revealed prefix
                                      properties)
      (misskey-media-insert-note-files view quoted revealed prefix
                                       properties))))

(defun misskey-render-note-properties (note)
  "Return durable row properties for NOTE.

User properties belong only to the visible span naming that user."
  (list misskey-note-property note
        misskey-note-id-property (misskey-note-id note)))

(cl-defun misskey-render-note-entry
    (view note &key parent-key (depth 0) connector)
  "Return an Appkit discussion entry rendering NOTE in VIEW.

PARENT-KEY, DEPTH, and CONNECTOR describe optional thread geometry."
  (unless (and (appkit-surface-p view) (appkit-surface-alive-p view))
    (error "Cannot render a Misskey note into a dead view"))
  (let*
      ((key (misskey-note-id note))
       (revealed (misskey-render-revealed-p view key))
       (properties (misskey-render-note-properties note)))
    (appkit-discussion-entry-create :key key :parent-key parent-key
                                    :depth depth :connector connector
                                    :avatar
                                    (misskey-media-avatar-image view note)
                                    :avatar-fallback "@" :context
                                    (and
                                     (misskey-note-pure-renote-p note)
                                     (concat "renoted by "
                                             (misskey-render--user-label
                                              (misskey-note-user note))))
                                    :context-face 'shadow
                                    :heading-inserter
                                    (lambda ()
                                      (misskey-render--insert-heading
                                       note))
                                    :heading-face 'bold :time
                                    (misskey-render-time
                                     (misskey-note-display-note note))
                                    :body-inserter
                                    (lambda (prefix row-properties)
                                      (misskey-render--insert-body
                                       view note revealed prefix
                                       row-properties))
                                    :footer
                                    (misskey-render-footer note
                                                           (appkit-surface-app
                                                            view))
                                    :properties properties)))

(defun misskey-render-insert-row (row)
  "Insert one projected Misskey note ROW at point."
  (let
      ((view (appkit-current-surface))
       (context (appkit-projection-row-context row)))
    (unless (and (appkit-surface-p view) (appkit-surface-alive-p view))
      (error "No live Appkit view while rendering a Misskey note"))
    (appkit-discussion-insert-entry
     (misskey-render-note-entry view
                                (appkit-projection-row-payload row)
                                :parent-key
                                (plist-get context :parent-key) :depth
                                (or (plist-get context :depth) 0)
                                :connector
                                (plist-get context :connector))
     :width (misskey-render-width) :avatar-p
     (misskey-media-avatars-enabled-p))))

(defun misskey-render-project-notes (notes &optional app)
  "Project visible NOTES into stable dependency-indexed Appkit rows.

When APP is live, omit locally deleted notes, pure wrappers around deleted
targets, and pure wrappers whose target payload is absent."
  (appkit-projection-project
   (if (appkit-app-live-p app)
       (cl-remove-if
        (lambda (note)
          (or (misskey-note-deleted-p app note)
              (and (misskey-note-pure-renote-p note)
                   (let ((display-note (misskey-note-display-note note)))
                     (or (null display-note)
                         (misskey-note-deleted-p app display-note))))))
        notes)
     notes)
   #'misskey-note-id
   :dependencies-function #'misskey-note-presentation-dependencies))

(defun misskey-render-note-at-point ()
  "Return the Misskey note at point, or nil."
  (get-text-property (point) misskey-note-property))

(defun misskey-render-toggle-content-warning ()
  "Toggle guarded content for the Misskey note at point."
  (interactive)
  (let* ((view (appkit-current-surface))
         (note (and (appkit-surface-live-p view) (misskey-render-note-at-point)))
         (key (and note (misskey-note-id note))))
    (unless (and key (or (misskey-note-content-warning-p note)
                         (misskey-note-sensitive-media-p note)))
      (user-error "Current note has no guarded content"))
    (if (eq (plist-get (appkit-surface-model view) :type) 'timeline)
        (misskey-dispatch view (list :timeline-reveal key))
      (let ((revealed (misskey-render--revealed-content view)))
        (if (gethash key revealed) (remhash key revealed)
          (puthash key t revealed)
          (misskey-media-prefetch-notes view (list note)))
        (misskey-dispatch view
                          (list :render (appkit-projection-change-create
                                         :keys (list key) :position key)))))))

(provide 'misskey-render)

;;; misskey-render.el ends here
