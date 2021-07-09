;;; aar-better-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; gcmh
(aar/maybe-install-package 'gcmh)

(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 16 1024 1024))
(setq read-process-output-max (* (* 1024 1024) 8))
(setq gcmh-verbose init-file-debug)
(require 'gcmh)
(gcmh-mode 1)

;; Auto-save transforms
(setq auto-save-list-file-prefix (aar/expand-cache-file-name "autosave/")
      auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ; Prefix tramp auto-saves to prevent conflicts
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))

;; Set file for custom.el to use
(setq custom-file (aar/expand-etc-file-name "custom.el"))

;;; Dashboard
(aar/maybe-install-package 'page-break-lines)
(aar/maybe-install-package 'dashboard)

(setq dashboard-set-heading-icons nil)
(setq dashboard-set-file-icons nil)
(setq dashboard-set-navigator nil)
(setq dashboard-set-init-info t)
(setq dashboard-set-footer nil)
(setq dashboard-banner-logo-title "Hi Ahnaf, welcome to Emacs")
(setq dashboard-startup-banner 'logo)
(setq dashboard-center-content nil)
(setq dashboard-projects-backend 'project-el)
(setq dashboard-items '((recents  . 10)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
(setq initial-buffer-choice (lambda ()
                              (get-buffer "*dashboard*")))
(require 'dashboard)

;; Hooks for dashboard-mode
(defun aar/dashboard-h ()
  (setq-local show-trailing-whitespace nil))

(add-hook 'dashboard-mode-hook #'aar/dashboard-h)

(dashboard-setup-startup-hook)

(provide 'aar-better-init)
;;; aar-better-init.el ends here
