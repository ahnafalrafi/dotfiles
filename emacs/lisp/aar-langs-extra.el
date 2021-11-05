;;; aar-langs-extra.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; matlab-mode
(straight-use-package 'matlab-mode)

;;;###autoload
(defun aar/matlab-mode-h ()
  (display-line-numbers-mode)
  (display-fill-column-indicator-mode))

(add-hook 'matlab-mode-hook #'aar/matlab-mode-h)

(provide 'aar-langs-extra)
;;; aar-langs-extra.el ends here
