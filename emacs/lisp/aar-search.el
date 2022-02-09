;;; aar-search.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; <leader> search map
(define-prefix-command 'aar/leader-search-map)
(define-key aar/leader-map (kbd "s") 'aar/leader-search-map)
(which-key-add-keymap-based-replacements aar/leader-map "s" "search")

;; evil-snipe
(straight-use-package 'evil-snipe)
(setq evil-snipe-smart-case t)
(setq evil-snipe-scope 'line)
(setq evil-snipe-repeat-scope 'visible)
(setq evil-snipe-char-fold t)
(add-hook 'after-init-hook #'evil-snipe-mode)
(add-hook 'after-init-hook #'evil-snipe-override-mode)

;;; evil-traces
(straight-use-package 'evil-traces)
(require 'evil-traces)
(evil-traces-use-diff-faces)
(evil-traces-mode)

;; anzu
(straight-use-package 'anzu)
(add-hook 'after-init-hook #'global-anzu-mode)

(global-set-key [remap query-replace]        #'anzu-query-replace)
(global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)

;; evil-anzu
(straight-use-package 'evil-anzu)
(with-eval-after-load 'evil
  (require 'evil-anzu))

;; Keybindings
(define-key aar/leader-search-map (kbd "c") #'evil-ex-nohighlight)
(define-key aar/leader-search-map (kbd "p") #'consult-ripgrep)
(define-key aar/leader-search-map (kbd "l") #'consult-line)

(provide 'aar-search)
;;; aar-search.el ends here
