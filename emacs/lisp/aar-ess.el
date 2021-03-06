;;; aar-ess.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(straight-use-package 'ess)

(setq ess-eval-visibly 'nowait)
(setq ess-offset-continued 'straight)
(setq ess-use-flymake t)
(setq ess-nuke-trailing-whitespace-p t)
(setq ess-style 'DEFAULT)
(setq ess-history-directory (aar/expand-etc-file-name "ess-history/"))

(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))

;; Inferior ess repl hooks
;;;###autoload
(defun aar/ess-inferior-mode-h ()
  (setq-local show-trailing-whitespace nil))

(add-hook 'inferior-ess-mode-hook #'aar/ess-inferior-mode-h)

(provide 'aar-ess)
;;; aar-ess.el ends here
