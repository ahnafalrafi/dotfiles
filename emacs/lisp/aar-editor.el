;;; aar-editor.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Undo/Redo
(if (>= emacs-major-version 28)
    (evil-set-undo-system 'undo-redo)

  (straight-use-package 'undo-fu)
  (straight-use-package 'undo-fu-session)
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'"
                                             "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode 1)
  (evil-set-undo-system 'undo-fu))

;; Indentation widths
(setq-default standard-indent 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)
(setq-default evil-shift-width standard-indent)

;; New lines at EOF
(setq-default require-final-newline nil)
(setq-default mode-require-final-newline t)
(setq-default log-edit-require-final-newline nil)

;; Long lines
(setq-default fill-column 80)
(setq-default word-wrap t)
(setq-default truncate-lines t)

;; electric-indent
;; See: https://emacs.stackexchange.com/questions/20896/change-the-behaviour-of-ret-with-electric-indent-to-only-indent-the-new-line
(setq electric-indent-inhibit t)

;; adaptive-wrap
(straight-use-package 'adaptive-wrap)
(setq-default adaptive-wrap-extra-indent 0)

;; paren
(setq show-paren-delay 0)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(add-hook 'after-init-hook #'show-paren-mode)

;; elec-pair
(add-hook 'after-init-hook #'electric-pair-mode)

;; evil-surround
(straight-use-package 'evil-surround)
(add-hook 'after-init-hook #'global-evil-surround-mode)

;; evil-matchit
;; (straight-use-package 'evil-matchit)
;; (add-hook 'after-init-hook #'global-evil-matchit-mode)

;; rainbow-delimiters
(straight-use-package 'rainbow-delimiters)

;; Whitespaces
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Snippets
(straight-use-package 'yasnippet)
(setq yas-indent-line 'auto)
(add-hook 'after-init-hook #'yas-global-mode)
(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets/"
                                                   user-emacs-directory))
  (defvaralias '% 'yas-selected-text))

(provide 'aar-editor)
;;; aar-editor.el ends here
