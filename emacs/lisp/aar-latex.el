;;; aar-latex.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Built-in tex-mode - just a fallback, auctex is the goto
(setq tex-fontify-script nil)

;; auctex
(straight-use-package 'auctex)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(setq TeX-save-query nil)
(with-eval-after-load 'tex
  (setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Zathura"))

(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-start-server t)
(setq TeX-electric-sub-and-superscript t)
(setq TeX-electric-math (cons "\\(" "\\)"))
(setq LaTeX-indent-environment-list nil)
(setq LaTeX-electric-left-right-brace t)
(setq LaTeX-section-hook '(LaTeX-section-heading
                           LaTeX-section-title
                           LaTeX-section-toc
                           LaTeX-section-section
                           LaTeX-section-label))
(setq LaTeX-fill-break-at-separators nil)
(setq LaTeX-item-indent 0)
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)
(with-eval-after-load 'font-latex
  (set-face-foreground 'font-latex-script-char-face nil))

(add-hook 'TeX-after-compilation-finished-functions-hook
          #'TeX-revert-document-buffer)

;; <localleader> LaTeX-mode bindings
(define-prefix-command 'aar/localleader-LaTeX-mode-map)
(evil-define-key '(normal visual motion) LaTeX-mode-map
  (kbd aar/localleader-key) 'aar/localleader-LaTeX-mode-map)
(evil-define-key '(insert emacs) LaTeX-mode-map
  (kbd aar/localleader-alt-key) 'aar/localleader-LaTeX-mode-map)

(define-key aar/localleader-LaTeX-mode-map (kbd "a") #'TeX-command-run-all)
(define-key aar/localleader-LaTeX-mode-map (kbd "v") #'TeX-view)
(define-key aar/localleader-LaTeX-mode-map (kbd "c") #'TeX-clean)

;; previews
(define-prefix-command 'aar/LaTeX-preview-map)
(define-key aar/localleader-LaTeX-mode-map (kbd "p") 'aar/LaTeX-preview-map)

(define-key aar/LaTeX-preview-map (kbd "b")   #'preview-buffer)
(define-key aar/LaTeX-preview-map (kbd "c b") #'preview-clearout-buffer)
(define-key aar/LaTeX-preview-map (kbd "d")   #'preview-document)
(define-key aar/LaTeX-preview-map (kbd "c d") #'preview-clearout-document)
(define-key aar/LaTeX-preview-map (kbd "e")   #'preview-environment)
(define-key aar/LaTeX-preview-map (kbd "p")   #'preview-at-point)
(define-key aar/LaTeX-preview-map (kbd "c p") #'preview-clearout-at-point)
(define-key aar/LaTeX-preview-map (kbd "r")   #'preview-region)
(define-key aar/LaTeX-preview-map (kbd "c r") #'preview-clearout)
(define-key aar/LaTeX-preview-map (kbd "s")   #'preview-section)
(define-key aar/LaTeX-preview-map (kbd "c s") #'preview-clearout-section)
(define-key aar/LaTeX-preview-map (kbd "f")   #'preview-cache-preamble)
(define-key aar/LaTeX-preview-map (kbd "c f") #'preview-cache-preamble-off)
(define-key aar/LaTeX-preview-map (kbd "w")   #'preview-copy-region-as-mml)
(define-key aar/LaTeX-preview-map (kbd "TAB") #'preview-goto-info-page)

;; auctex-latexmk
(straight-use-package 'auctex-latexmk)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

(with-eval-after-load 'tex
  (require 'auctex-latexmk)
  (auctex-latexmk-setup))

;; evil-tex
(straight-use-package 'evil-tex)
(setq evil-tex-toggle-override-m nil)
(setq evil-tex-toggle-override-t t)

;; reftex
(setq reftex-plug-into-AUCTeX t)
(setq reftex-toc-split-windows-fraction 0.3)
(add-hook 'reftex-mode-hook #'evil-normalize-keymaps)

(define-key aar/localleader-LaTeX-mode-map (kbd ";") #'reftex-toc)

;; Automatically compile on save
;;;###autoload
(defun aar/latex-default-compile-on-master ()
  "Run `TeX-command-default' on `TeX-master' for current buffer."
  (TeX-command TeX-command-default #'TeX-master-file))

;;;###autoload
(defun aar/latex-compile-after-save-on ()
  (interactive)
  (add-hook 'after-save-hook #'aar/latex-default-compile-on-master 0 t))

;;;###autoload
(defun aar/latex-compile-after-save-off ()
  (interactive)
  (remove-hook 'after-save-hook #'aar/latex-default-compile-on-master t))

(define-key aar/localleader-LaTeX-mode-map (kbd "ll") #'aar/latex-compile-after-save-on)
(define-key aar/localleader-LaTeX-mode-map (kbd "lL") #'aar/latex-compile-after-save-off)

;; Hook for tex modes
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(tex-mode . ("texlab"))))

;;;###autoload
(defun aar/latex-mode-h ()
  (setq-local fill-nobreak-predicate nil)
  (setq-local spell-fu-faces-exclude '(font-lock-function-name-face
                                       font-lock-keyword-face
                                       font-lock-constant-face
                                       font-lock-variable-name-face
                                       font-latex-math-face
                                       font-latex-sedate-face
                                       font-latex-warning-face
                                       font-lock-type-face
                                       button
                                       lsp-face-highlight-write))
  (setq-local TeX-command-default "LatexMk")
  (visual-line-mode)
  (auto-fill-mode)
  (adaptive-wrap-prefix-mode)
  (evil-tex-mode)
  (reftex-mode)
  (eglot-ensure)
  (font-lock-add-keywords nil  '(("\\(\\\\citep\\)\\s-*{" 1 font-lock-keyword-face t)))
  (font-lock-add-keywords nil  '(("\\(\\\\citet\\)\\s-*{" 1 font-lock-keyword-face t)))
  (font-latex-add-keywords '(("citep" "*[[{")) 'reference)
  (font-latex-add-keywords '(("citet" "*[[{")) 'reference))

(add-hook 'TeX-mode-hook #'aar/latex-mode-h)

(provide 'aar-latex)
;;; aar-latex.el ends here
