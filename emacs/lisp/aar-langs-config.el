;;; aar-langs-config.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; conf-mode
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-fill-column-indicator-mode)

(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.timer\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.target\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.mount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.automount\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.slice\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.path\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.netdev\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.network\\'" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.link\\'" . conf-unix-mode))

;; elisp
;;;###autoload
(defun aar/elisp-mode-h ()
  (rainbow-delimiters-mode +1)
  (setq-local tab-width 8)
  (setq-local mode-name "Elisp"))

(add-hook 'emacs-lisp-mode-hook #'aar/elisp-mode-h)

;; yaml
(straight-use-package 'yaml-mode)

(provide 'aar-langs-config)
;;; aar-langs-config.el ends here
