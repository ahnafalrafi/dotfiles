;;; aar-latex.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; auctex
(aar/maybe-install-package 'auctex)

(setq-default font-latex-fontify-script nil
              font-latex-fontify-sectioning 'color
              fill-nobreak-predicate nil
              LaTeX-indent-environment-list nil
              TeX-command-default "LatexMk"
              TeX-electric-sub-and-superscript t
              TeX-save-query nil
              TeX-view-program-selection '((output-pdf "PDF Tools")
                                           (output-pdf "Zathura"))
              TeX-source-correlate-mode t
              TeX-source-correlate-method 'synctex
              TeX-source-correlate-start-server t
              LaTeX-section-hook '(LaTeX-section-heading
                                   LaTeX-section-title
                                   LaTeX-section-toc
                                   LaTeX-section-section
                                   LaTeX-section-label)
              LaTeX-fill-break-at-separators nil
              LaTeX-item-indent 0)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
             `((tex-mode context-mode texinfo-mode bibtex-mode) . ("texlab"))))

(with-eval-after-load 'font-latex
  (set-face-foreground 'font-latex-script-char-face nil))

(defun aar/LaTeX-mode-settings-h ()
  (setq-local preivew-scale-function 1.25)
  (setq-local spell-fu-faces-exclude '(font-latex-math-face
                                       font-latex-sedate-face
                                       font-latex-warning-face
                                       font-lock-function-name-face
                                       font-lock-keyword-face
                                       font-lock-constant-face
                                       font-lock-variable-name-face
                                       button))
  (visual-line-mode)
  (auto-fill-mode)
  (adaptive-wrap-prefix-mode)
  (eglot-ensure))

(add-hook 'LaTeX-mode-hook #'aar/LaTeX-mode-settings-h)
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

;;;###autoload
(defun aar/auctex-latexmk-h ()
  (require 'auctex-latexmk)
  (auctex-latexmk-setup))
(add-hook 'LaTeX-mode-hook #'aar/auctex-latexmk-h)

;;; evil-tex
(aar/maybe-install-package 'evil-tex)
(setq evil-tex-toggle-override-m nil)
(setq evil-tex-toggle-override-t t)

(add-hook 'LaTeX-mode-hook #'evil-tex-mode)

;;; reftex
(setq reftex-plug-into-AUCTeX t)
(setq reftex-toc-split-windows-fraction 0.3)
(add-hook 'LaTeX-mode-hook #'reftex-mode)
(add-hook 'reftex-mode-hook #'evil-normalize-keymaps)

(define-key aar/localleader-LaTeX-mode-map (kbd ";") #'reftex-toc)

;;; bibtex
(setq bibtex-dialect 'biblatex
      bibtex-align-at-equal-sign t
      bibtex-text-indentation 20)
(with-eval-after-load 'bibtex
  (define-key bibtex-mode-map (kbd "C-c \\") #'bibtex-fill-entry))

(provide 'aar-latex)
;;; aar-latex.el ends here
