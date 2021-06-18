;;; aar-python.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun aar/python-h ()
  (tree-sitter-mode)
  (rainbow-delimiters-mode)
  (eglot-ensure))

(add-hook 'python-mode-hook #'aar/python-h)

;; Use IPython when available or fall back to regular Python
(cond
 ((executable-find "ipython")
  (setq python-shell-buffer-name "IPython")
  (setq python-shell-interpreter "ipython")
  (setq python-shell-interpreter-args "-i --simple-prompt"))
 ((executable-find "python3")
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-i")))

(provide 'aar-python)
;;; aar-python.el ends here
