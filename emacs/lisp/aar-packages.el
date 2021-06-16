;;; aar-packages.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; native-comp
(when (boundp 'comp-async-report-warnings-errors)
  (setq comp-async-report-warnings-errors nil))

;; Initialize package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun aar/maybe-install-package (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(provide 'aar-packages)
;;; aar-packages.el ends here
