;;; aar-search.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; <leader> search map
(define-prefix-command 'aar/leader-search-map)
(define-key aar/leader-map (kbd "s") 'aar/leader-search-map)
(which-key-add-keymap-based-replacements aar/leader-map "s" "search")

;;; evil-snipe
(aar/maybe-install-package 'evil-snipe)
(setq evil-snipe-smart-case t)
(setq evil-snipe-scope 'line)
(setq evil-snipe-repeat-scope 'visible)
(setq evil-snipe-char-fold t)
(require 'evil-snipe)
(evil-snipe-mode 1)
(evil-snipe-override-mode 1)

;;; anzu
(aar/maybe-install-package 'anzu)
(global-anzu-mode 1)

(global-set-key [remap query-replace]        #'anzu-query-replace)
(global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp)

;;; evil-anzu
(aar/maybe-install-package 'evil-anzu)
(with-eval-after-load 'evil
  (require 'evil-anzu))

;;; Keybindings
(define-key aar/leader-search-map (kbd "c") #'evil-ex-nohighlight)
(define-key aar/leader-search-map (kbd "p") #'consult-ripgrep)
(define-key aar/leader-search-map (kbd "l") #'consult-line)

(provide 'aar-search)
;;; aar-search.el ends here
