;;; aar-better-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; gcmh
(aar/maybe-install-package 'gcmh)

(setq gcmh-idle-delay 5)
(setq gcmh-high-cons-threshold (* 16 1024 1024))
(setq read-process-output-max (* (* 1024 1024) 8))
(setq gcmh-verbose init-file-debug)
(require 'gcmh)
(gcmh-mode 1)

;;; no-littering
(aar/maybe-install-package 'no-littering)
(require 'no-littering)

;; Auto-save transforms
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Set file for custom.el to use
(setq custom-file (no-littering-expand-etc-file-name "custom.el"))

(provide 'aar-better-init)
;;; aar-better-init.el ends here
