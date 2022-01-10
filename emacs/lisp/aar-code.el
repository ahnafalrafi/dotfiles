;;; aar-code.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; <leader> code map
(define-prefix-command 'aar/leader-code-map)
(define-key aar/leader-map (kbd "c") 'aar/leader-code-map)
(which-key-add-keymap-based-replacements aar/leader-map "c" "code")

;; <leader> code bindings
(define-key aar/leader-code-map (kbd "i") #'imenu)

;; flycheck
(straight-use-package 'flycheck)

(evil-define-key '(normal visual motion) 'flycheck-mode-map
  (kbd "] e") #'flycheck-next-error
  (kbd "[ e") #'flycheck-previous-error)

;; I hate LaTeX syntax checking and so will disable this preemptively.
(setq-default flycheck-disabled-checkers '(tex-chktex tex-lacheck))

;; tree-sitter
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(add-hook 'after-init-hook
          (lambda ()
            (require 'tree-sitter)
            (require 'tree-sitter-langs)
            (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

;; lsp-mode
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)

(setq lsp-session-file (aar/expand-etc-file-name "lsp-session"))
(setq lsp-server-install-dir (aar/expand-cache-file-name "lsp/"))
(setq lsp-keymap-prefix "C-c l")
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-ignore-duplicate t)
(setq lsp-ui-sideline-diagnostic-max-lines 10)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-ui-doc-show-with-mouse nil)

(with-eval-after-load 'lsp-mode
  (define-key aar/leader-code-map (kbd "l") lsp-command-map))

;; consult-lsp
(straight-use-package 'consult-lsp)

;; lsp-treemacs
(if aar/use-treemacs
    (straight-use-package 'lsp-treemacs))

;;;###autoload
(defun aar/lsp-mode-h ()
  (lsp-enable-which-key-integration)
  (lsp-ui-mode)
  (if aar/use-treemacs
      (lsp-treemacs-sync-mode 1))
  (consult-lsp-marginalia-mode 1)
  (evil-local-set-key 'normal (kbd "K") #'lsp-describe-thing-at-point)
  (evil-local-set-key 'normal (kbd "g d") #'lsp-find-definition)
  (evil-local-set-key 'normal (kbd "g r") #'lsp-find-references))

(add-hook 'lsp-mode-hook #'aar/lsp-mode-h)

(with-eval-after-load 'lsp-mode
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(provide 'aar-code)
;;; aar-code.el ends here
