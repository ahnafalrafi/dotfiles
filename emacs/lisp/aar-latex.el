;;; aar-latex.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Built-in tex-mode - just a fallback, auctex is the goto
;; This is just configuration for a fallback
(setq tex-fontify-script nil)

;;; auctex
(aar/maybe-install-package 'auctex)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

(setq TeX-save-query nil)
(setq TeX-view-program-selection '((output-pdf "PDF Tools")
                                   (output-pdf "Zathura")))
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-mode 'synctex)
(setq TeX-source-correlate-start-server t)
(setq TeX-electric-sub-and-superscript t)
(setq LaTeX-indent-environment-list nil)
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

;;; <localleader> LaTeX-mode bindings
(define-prefix-command 'aar/localleader-LaTeX-mode-map)
(evil-define-key '(normal visual motion) LaTeX-mode-map
  (kbd aar/localleader-key) 'aar/localleader-LaTeX-mode-map)
(evil-define-key '(insert emacs) LaTeX-mode-map
  (kbd aar/localleader-alt-key) 'aar/localleader-LaTeX-mode-map)

(define-key aar/localleader-LaTeX-mode-map (kbd "a")     #'TeX-command-run-all)
(define-key aar/localleader-LaTeX-mode-map (kbd "v")     #'TeX-view)
(define-key aar/localleader-LaTeX-mode-map (kbd "c")     #'TeX-clean)
(define-key aar/localleader-LaTeX-mode-map (kbd "p b")   #'preview-buffer)
(define-key aar/localleader-LaTeX-mode-map (kbd "p c b") #'preview-clearout-buffer)

;;; auctex-latexmk
(aar/maybe-install-package 'auctex-latexmk)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)
(setq TeX-command-default "LatexMk")

;;; evil-tex
(aar/maybe-install-package 'evil-tex)
(setq evil-tex-toggle-override-m nil)
(setq evil-tex-toggle-override-t t)

;;; reftex
(setq reftex-plug-into-AUCTeX t)
(setq reftex-toc-split-windows-fraction 0.3)
(add-hook 'reftex-mode-hook #'evil-normalize-keymaps)

(define-key aar/localleader-LaTeX-mode-map (kbd ";") #'reftex-toc)

;;; bibtex
(setq bibtex-dialect 'biblatex)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 20)
(with-eval-after-load 'bibtex
  (define-key bibtex-mode-map (kbd "C-c \\") #'bibtex-fill-entry))

;;; eglot configuration for latex
(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   `((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab"))))

;;; Hook for tex modes
(defun aar/latex-mode-h ()
  (setq-local fill-nobreak-predicate nil)
  (setq-local preivew-scale-function 1.25)
  (setq-local spell-fu-faces-exclude '(font-lock-function-name-face
                                       font-lock-keyword-face
                                       font-lock-constant-face
                                       font-lock-variable-name-face
                                       font-latex-math-face
                                       font-latex-sedate-face
                                       font-latex-warning-face
                                       button))

  (visual-line-mode)
  (auto-fill-mode)
  (adaptive-wrap-prefix-mode)
  (require 'auctex-latexmk)
  (auctex-latexmk-setup)
  (evil-tex-mode)
  (reftex-mode)
  (eglot-ensure))

(add-hook 'LaTeX-mode-hook #'aar/latex-mode-h)

(provide 'aar-latex)
;;; aar-latex.el ends here
