;;; aar-code.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; <leader> code map
(define-prefix-command 'aar/leader-code-map)
(define-key aar/leader-map (kbd "c") 'aar/leader-code-map)
(which-key-add-keymap-based-replacements aar/leader-map "c" "code")

;;; <leader> code bindings
(define-key aar/leader-code-map (kbd "i") #'imenu)

;;; eldoc
(setq eldoc-echo-area-use-multiline-p 2)

;;; flymake
(evil-define-key '(normal motion) 'flymake-mode-map
  (kbd "] e") #'flymake-goto-next-error
  (kbd "[ e") #'flymake-goto-prev-error)

;;; tree-sitter
(aar/maybe-install-package 'tree-sitter)
(aar/maybe-install-package 'tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;; eglot
(aar/maybe-install-package 'eglot)
(setq eglot-autoshutdown t)

(provide 'aar-code)
;;; aar-code.el ends here
