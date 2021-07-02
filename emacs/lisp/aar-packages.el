;;; aar-packages.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; native-comp
(when (boundp 'comp-async-report-warnings-errors)
  (setq comp-async-report-warnings-errors nil))

;;; Initialize package sources
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(defun aar/maybe-install-package (pkg)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg))
  (add-to-list 'package-selected-packages pkg 'append))

(provide 'aar-packages)
;;; aar-packages.el ends here
