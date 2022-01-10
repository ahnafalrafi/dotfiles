;;; aar-visuals.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Font
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-11.5"))
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-11.5")

;;; Dealing with Xressources - i.e. don't bother, ignore.
(setq inhibit-x-resources t)

;;; Cursor, tooltip and dialog box
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))
(setq visible-cursor nil)
(setq use-dialog-box nil)
(setq x-gtk-use-system-tooltips nil)

;;; Display line numbers and fill-column indicator
(setq display-line-numbers-type 'visual)
(dolist (hook '(prog-mode-hook text-mode-hook))
  (add-hook hook #'display-line-numbers-mode)
  (add-hook hook #'display-fill-column-indicator-mode))

;;; Theme
(straight-use-package 'doom-themes)

(defun aar/load-theme ()
  "Wrapper function around commands for loading theme."
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)

  ;; (load-theme 'doom-dracula t)
  ;; (set-face-attribute 'vertical-border nil
  ;;                     :foreground "#6272a4"
  ;;                     :background "#6272a4")

  ;; (load-theme 'doom-vibrant t)
  ;; (setq doom-vibrant-brighter-comments t)
  ;; (setq doom-vibrant-brighter-modeline t)

  ;; (setq doom-tokyo-night-comment-bg t)
  ;; (setq doom-tokyo-night-brighter-comments t)
  ;; (setq doom-tokyo-night-brighter-modeline t)
  ;; (load-theme 'doom-tokyo-night t)

  (setq doom-gruvbox-dark-variant "hard")
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  ;; Enable custom treemacs theme (all-the-icons must be installed!)
  (if aar/use-treemacs
      (doom-themes-treemacs-config)))

(defun aar/load-theme-after-frame-h (frame)
  "Load theme after the frame has been made.
Useful for loading themes properly in daemon mode"
  (with-selected-frame frame
    (aar/load-theme)))

(add-hook 'after-init-hook #'aar/load-theme)
(add-hook 'after-make-frame-functions #'aar/load-theme-after-frame-h)

;;; Icons
(straight-use-package 'all-the-icons)

;;; Modeline
;; Base modeline settings
(size-indication-mode t)
(line-number-mode t)
(column-number-mode t)

;; doom-modeline
(straight-use-package 'doom-modeline)
(setq doom-modeline-height 12)
(setq doom-modeline-bar-width 3)
(setq doom-modeline-window-width-limit fill-column)
(add-hook 'after-init-hook #'doom-modeline-mode)

;; hide-mode-line
(straight-use-package 'hide-mode-line)
(add-hook 'completion-list-mode-hook #'hide-mode-line-mode)

;;; hl-todo: additional highlighting for TODO keywords
(straight-use-package 'hl-todo)
(dolist (hook '(prog-mode-hook tex-mode-hook markdown-mode-hook))
  (add-hook hook #'hl-todo-mode))

;; Stolen from doom-emacs: modules/ui/hl-todo/config.el
(setq hl-todo-highlight-punctuation ":")
(setq  hl-todo-keyword-faces
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
