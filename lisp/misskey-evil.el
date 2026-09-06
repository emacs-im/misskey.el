;;; misskey-evil.el --- Optional modal Misskey bindings -*- lexical-binding: t; -*-

;;; Commentary:

;; Client-local Evil bindings using Appkit's optional integration.  Load after
;; the Misskey view modules; Evil itself need not be installed or loaded.

;;; Code:

(require 'appkit-evil)

(declare-function misskey-menu "misskey-menu" ())
(declare-function misskey-compose-menu "misskey-menu" ())
(declare-function misskey-compose "misskey" ())

(defconst misskey-evil--views
  '((misskey-timeline-mode misskey-timeline-mode-map
     misskey-timeline-refresh misskey-timeline-load-more
     misskey-timeline-next-kind)
    (misskey-thread-mode misskey-thread-mode-map
                         misskey-thread-refresh misskey-thread-load-more)
    (misskey-profile-mode misskey-profile-mode-map
                          misskey-profile-refresh misskey-profile-load-more
                          misskey-profile-next-mode)
    (misskey-search-mode misskey-search-mode-map
                         misskey-search-refresh misskey-search-load-more)
    (misskey-notifications-mode misskey-notifications-mode-map
                                misskey-notifications-refresh misskey-notifications-load-more
                                appkit-directory-tab-dwim)
    (misskey-directory-mode misskey-directory-mode-map
                            misskey-directory-refresh misskey-directory-load-more
                            appkit-directory-tab-dwim))
  "Browsing modes, their real maps, refresh, paging, and optional TAB commands.")

;;;###autoload
(defun misskey-evil-setup ()
  "Install optional Evil bindings in Misskey buffers.
Honor `appkit-evil-enable-integration' and leave global Evil maps untouched.
Safe to call repeatedly, including after Evil loads or view buffers exist."
  (interactive)
  (when (and appkit-evil-enable-integration (featurep 'evil))
    (let ((modes (mapcar #'car misskey-evil--views)))
      (appkit-evil-set-initial-states modes 'normal)
      (appkit-evil-set-initial-states '(misskey-compose-mode) 'insert)
      (dolist (spec misskey-evil--views)
        (pcase-let ((`(,mode ,map ,refresh ,more . ,cycle) spec))
          (appkit-evil-define-readonly-keys map)
          (appkit-evil-define-keys '(normal motion) map
            "?" #'misskey-menu
            "g r" refresh
            "g n" more
            "a" misskey-actions-map
            "c" (if (eq mode 'misskey-timeline-mode)
                    #'misskey-timeline-compose
                  #'misskey-compose))
          (when (eq mode 'misskey-notifications-mode)
            (appkit-evil-define-keys '(normal motion) map
              "M" #'misskey-notifications-mark-all-read))
          (when (car cycle)
            (appkit-evil-define-keys '(normal motion) map
              "TAB" (car cycle)
              "<tab>" (car cycle)))
          (if (memq mode '(misskey-notifications-mode misskey-directory-mode))
              (appkit-evil-define-keys '(normal motion) map
                "RET" #'appkit-directory-activate
                "<return>" #'appkit-directory-activate
                "g j" #'appkit-directory-next-item
                "g k" #'appkit-directory-previous-item)
            (appkit-evil-define-keys '(normal motion) map
              "RET" #'misskey-navigation-activate
              "<return>" #'misskey-navigation-activate
              "g j" #'appkit-discussion-next-entry
              "g k" #'appkit-discussion-previous-entry
              "g o" #'misskey-navigation-browse
              "g O" #'misskey-navigation-open-note-url
              "g y" #'misskey-navigation-copy-link
              "r" #'misskey-compose-reply-at-point
              "Q" #'misskey-compose-quote-at-point))))
      (appkit-evil-define-keys '(normal motion) 'misskey-compose-mode-map
        "i" #'appkit-evil-chatbuf-enter-input)
      (dolist (mode (cons 'misskey-compose-mode modes))
        (add-hook (intern (concat (symbol-name mode) "-hook"))
                  #'appkit-evil-normalize-keymaps))
      (appkit-evil-normalize-buffers (cons 'misskey-compose-mode modes)))))

(with-eval-after-load 'evil
  (misskey-evil-setup))

(provide 'misskey-evil)
;;; misskey-evil.el ends here
