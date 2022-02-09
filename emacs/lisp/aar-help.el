;;; aar-help.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; <leader> bindings for help
(define-key aar/leader-map (kbd "h") help-map)

;; elisp-demos
(straight-use-package 'elisp-demos)
(advice-add 'describe-function-1
            :after #'elisp-demos-advice-describe-function-1)

;; Hooks
;;;###autoload
(defun aar/help-mode-h ()
  (setq-local show-trailing-whitespace nil)

  (visual-line-mode)
  (display-line-numbers-mode))

(dolist (hook '(Info-mode-hook help-mode-hook))
  (add-hook hook #'aar/help-mode-h))

(provide 'aar-help)
;;; aar-help.el ends here
