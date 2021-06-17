;;; aar-help.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; <leader> bindings for help
(define-key aar/leader-map (kbd "h") help-map)

;;; helpful
(aar/maybe-install-package 'helpful)
(add-hook 'helpful-mode-hook #'display-line-numbers-mode)
(add-hook 'helpful-mode-hook #'visual-line-mode)

(global-set-key [remap describe-key]      #'helpful-key)
(global-set-key [remap describe-command]  #'helpful-command)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-symbol]   #'helpful-symbol)

(provide 'aar-help)
;;; aar-help.el ends here
