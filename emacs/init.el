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

;; Cursor, tooltip and dialog box
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(setq visible-cursor nil)
(setq use-dialog-box nil)
(setq x-gtk-use-system-tooltips nil)

;; Frame title: tell me if I am in daemon mode
(setq frame-title-format (if (daemonp)
                             '("AAR Daemacs - %b")
                           '("AAR Emacs - %b")))

;; Clipboard/kill-ring
(setq kill-do-not-save-duplicates t)

;; Auto-saves, backups and lockfiles
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; Scrolling
(setq-default scroll-margin 0)
(setq-default scroll-step 1)
(setq-default scroll-preserve-screen-position nil)
(setq-default scroll-conservatively 10000)
(setq-default auto-window-vscroll nil)

;; New lines at EOF
(setq-default require-final-newline nil)
(setq-default mode-require-final-newline t)
(setq-default log-edit-require-final-newline nil)

;; Long lines
(setq-default fill-column 80)
(setq-default word-wrap t)
(setq-default truncate-lines t)

;; Indentation widths
(setq-default standard-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160)
(setq split-height-threshold nil)

;;;  Load configuration files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'aar-packages)
(require 'aar-better-init)
(require 'aar-keybindings)
(require 'aar-visuals)
(require 'aar-completion)
(require 'aar-files)
(require 'aar-buffers)

(provide 'init)
;;; init.el ends here
