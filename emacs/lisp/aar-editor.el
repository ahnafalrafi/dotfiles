;;; aar-editor.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Undo/Redo
(if (>= emacs-major-version 28)
    (evil-set-undo-system 'undo-redo)

  (aar/maybe-install-package 'undo-fu)
  (aar/maybe-install-package 'undo-fu-session)
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'"
                                             "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode 1)
  (evil-set-undo-system 'undo-fu))

;;; electric-indent
;; See: https://emacs.stackexchange.com/questions/20896/change-the-behaviour-of-ret-with-electric-indent-to-only-indent-the-new-line
(setq electric-indent-inhibit t)

;;; adaptive-wrap
(aar/maybe-install-package 'adaptive-wrap)
(setq-default adaptive-wrap-extra-indent 0)

;;; paren
(setq show-paren-delay 0)
(setq show-paren-highlight-openparen t)
(setq show-paren-when-point-inside-paren t)
(setq show-paren-when-point-in-periphery t)
(show-paren-mode 1)

;;; elec-pair
(electric-pair-mode 1)

;;; evil-surround
(aar/maybe-install-package 'evil-surround)
(require 'evil-surround)
(global-evil-surround-mode 1)

;;; evil-matchit
(aar/maybe-install-package 'evil-matchit)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;;; rainbow-delimiters
(aar/maybe-install-package 'rainbow-delimiters)

;;; Whitespaces
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;; Snippets
(aar/maybe-install-package 'yasnippet)
(setq yas-indent-line 'auto)
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets/"
                                                 user-emacs-directory))
(yas-global-mode t)
(defvaralias '% 'yas-selected-text)

(provide 'aar-editor)
;;; aar-editor.el ends here
