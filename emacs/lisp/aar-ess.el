;;; aar-ess.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(aar/maybe-install-package 'ess)

(setq ess-eval-visibly 'nowait)
(setq ess-offset-continued 'straight)
(setq ess-use-flymake t)
(setq ess-nuke-trailing-whitespace-p t)
(setq ess-style 'DEFAULT)
(setq ess-history-directory (no-littering-expand-etc-file-name "ess-history/"))

(load "ess-autoloads")
(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))

(provide 'aar-ess)
;;; aar-ess.el ends here
