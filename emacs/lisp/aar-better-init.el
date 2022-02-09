;;; aar-better-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; gcmh
(straight-use-package 'gcmh)
(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 50 1024 1024))
(setq gcmh-verbose init-file-debug)
(gcmh-mode 1)

;; (setq read-process-output-max (* (* 1024 1024) 8))
(setq-default read-process-output-max (* 1024 1024))

;;; Dashboard
(straight-use-package 'dashboard)

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
