;;; aar-visuals.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Font
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-10.5")
(set-frame-font "JetBrainsMono Nerd Font-10.5" nil t)
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-10.5"))

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

;;; hl-todo: additional highlighting for TODO keywords
(aar/maybe-install-package 'hl-todo)
(dolist (hook '(prog-mode-hook tex-mode-hook markdown-mode-hook))
  (add-hook hook #'hl-todo-mode))

;; Stolen from doom-emacs: modules/ui/hl-todo/config.el
(setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(;; For things that need to be done, just not today.
          ("TODO" warning bold)
          ;; For problems that will become bigger problems later if not
          ;; fixed ASAP.
          ("FIXME" error bold)
          ;; For tidbits that are unconventional and not intended uses of the
          ;; constituent parts, and may break in a future update.
          ("HACK" font-lock-constant-face bold)
          ;; For things that were done hastily and/or hasn't been thoroughly
          ;; tested. It may not even be necessary!
          ("REVIEW" font-lock-keyword-face bold)
          ;; For especially important gotchas with a given implementation,
          ;; directed at another user other than the author.
          ("NOTE" success bold)
          ;; For things that just gotta go and will soon be gone.
          ("DEPRECATED" font-lock-doc-face bold)
          ;; For a known bug that needs a workaround
          ("BUG" error bold)
          ;; For warning about a problematic or misguiding code
          ("XXX" font-lock-constant-face bold)))

(provide 'aar-visuals)
;;; aar-visuals.el ends here
