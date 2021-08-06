;;; aar-visuals.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Font
(add-to-list 'default-frame-alist '(font . "JetBrainsMono Nerd Font-10.5"))
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font-10.5")

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

;;; Display line numbers
(setq display-line-numbers-type 'visual)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;;; Display fill-column-indicator
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'text-mode-hook #'display-fill-column-indicator-mode)

;;; Theme
(defvar aar/modus-vivendi t
  "Indicate whether modus-vivendi (the dark variant) should be loaded.")

(defun aar/load-modus-theme ()
  "Load modus theme variant according to value of `aar-modus-vivendi'."
  (setq modus-themes-no-mixed-fonts t)
  (setq modus-themes-paren-match '(bold intense))
  (setq modus-themes-org-blocks 'gray-background)
  (if aar/modus-vivendi
      (load-theme 'modus-vivendi t)
    (load-theme 'modus-operandi t)))

(defun aar/load-modus-theme-after-frame-h (frame)
  "Load modus theme after the frame has been made.
Useful for loading the themes properly in daemon mode"
  (with-selected-frame frame
    (aar/load-modus-theme)))

(if (daemonp)
    (add-hook 'after-make-frame-functions #'aar/load-modus-theme-after-frame-h)
  (aar/load-modus-theme))

;;; Icons
(aar/maybe-install-package 'all-the-icons)
(require 'all-the-icons)

;;; Modeline
;;;;;; Base modeline settings
(size-indication-mode t)
(line-number-mode t)
(column-number-mode t)

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
