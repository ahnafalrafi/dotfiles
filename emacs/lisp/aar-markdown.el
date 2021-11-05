;;; aar-markdown.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(straight-use-package 'markdown-mode)

;;;###autoload
(defun aar/markdown-mode-h ()
  (setq-local spell-fu-faces-exclude '(markdown-code-face
                                       font-lock-keyword-face
                                       markdown-html-attr-name-face
                                       markdown-html-attr-value-face
                                       markdown-html-tag-name-face
                                       markdown-link-face
                                       markdown-markup-face
                                       markdown-reference-face
                                       markdown-url-face)))

(add-hook 'markdown-mode-hook #'aar/markdown-mode-h)

(provide 'aar-markdown)
;;; aar-markdown.el ends here
