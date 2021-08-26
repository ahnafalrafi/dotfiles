;;; aar-langs-extra.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; ado-mode for stata source files
(aar/maybe-install-package 'ado-mode)
(require 'ado-mode)
(add-to-list 'auto-mode-alist '("\\.ado\\'" . ado-mode))
(add-to-list 'auto-mode-alist '("\\.do\\'" . ado-mode))

;; matlab-mode
(aar/maybe-install-package 'matlab-mode)

(defun aar/matlab-mode-h ()
  (display-line-numbers-mode)
  (display-fill-column-indicator-mode))

(add-hook 'matlab-mode-hook #'aar/matlab-mode-h)

(provide 'aar-langs-extra)
;;; aar-langs-extra.el ends here
