;;; aar-cc.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;;###autoload
(defun aar/cc-h ()
  (tree-sitter-mode)
  (lsp-deferred))

(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook #'aar/cc-h))

(provide 'aar-cc)
;;; aar-cc.el ends here
