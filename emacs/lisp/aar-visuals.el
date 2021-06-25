;;; aar-visuals.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-10")
(set-frame-font "JetBrainsMono Nerd Font-10" nil t)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-10"))

;;; Display line numbers
(setq display-line-numbers-type 'visual)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;;; Display fill-column-indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

;;; Theme
(setq modus-themes-org-blocks 'grayscale)
(setq modus-themes-no-mixed-fonts t)
(load-theme 'modus-vivendi t)
(define-key aar/leader-map (kbd "t t") #'load-theme)

;;; Icons
(aar/maybe-install-package 'all-the-icons)
(require 'all-the-icons)

;;; Modeline
;;;;;; Base modeline settings
(size-indication-mode t)
(line-number-mode t)
(column-number-mode t)

;;;;;; doom-modeline
(aar/maybe-install-package 'doom-modeline)
(setq doom-modeline-height 13)
(setq doom-modeline-bar-width 3)
(setq doom-modeline-window-width-limit fill-column)

(doom-modeline-mode t)
(add-hook 'after-init-hook #'doom-modeline-mode)

;;;;;; hide-mode-line
(aar/maybe-install-package 'hide-mode-line)
(require 'hide-mode-line)
(add-hook 'completion-list-mode-hook #'hide-mode-line-mode)

(provide 'aar-visuals)
;;; aar-visuals.el ends here
