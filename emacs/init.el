;;; init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Personal details
(setq user-full-name "Ahnaf Rafi")
(setq user-mail-address "ahnaf.al.rafi@gmail.com")

;; UTF-8
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq selection-coding-system 'utf-8)

;; Get rid of startup messages
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message user-login-name)
(setq initial-scratch-message nil)
(advice-add #'display-startup-echo-area-message :override #'ignore)

;; yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Clipboard/kill-ring
(setq kill-do-not-save-duplicates t)

;; Disable the alarm bell
(setq ring-bell-function 'ignore)

;; Auto-saves, backups and lockfiles
;; Auto-save transforms
(setq auto-save-default t)
(setq auto-save-list-file-prefix (aar/expand-cache-file-name "autosave/"))
(setq auto-save-file-name-transforms
      (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                  ; Prefix tramp auto-saves to prevent conflicts
                  (concat auto-save-list-file-prefix "tramp-\\2") t)
            (list ".*" auto-save-list-file-prefix t)))
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Bookmarks
(setq bookmark-default-file (aar/expand-cache-file-name "bookmarks.el"))

;; Set file for custom.el to use
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;;  Load configuration files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'aar-packages)
(require 'aar-better-init)
(require 'aar-keybindings)
(require 'aar-visuals)
(require 'aar-bufwinframes)
(require 'aar-completion)
(require 'aar-files)
(require 'aar-help)

(require 'aar-editor)
(require 'aar-search)
(require 'aar-code)
(require 'aar-project-vc)
(require 'aar-spelling)
(require 'aar-apps)

(require 'aar-cc)
(require 'aar-ess)
(require 'aar-julia)
(require 'aar-python)
(require 'aar-langs-config)
(require 'aar-langs-extra)

(require 'aar-latex)
(require 'aar-bib)
(require 'aar-markdown)
(require 'aar-org)

(provide 'init)
;;; init.el ends here
