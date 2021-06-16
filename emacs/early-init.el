;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Defer loading package.el until called explicitly
(setq package-enable-at-startup nil)

;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer t)

;; Disable gui elements before they are initialized.
(push '(menu-bar-lines . 0) default-frame-alist)
(setq menu-bar-mode nil)

(push '(tool-bar-lines . 0) default-frame-alist)
(setq tool-bar-mode nil)

(push '(vertical-scroll-bars) default-frame-alist)
(setq scroll-bar-mode nil)

(provide 'early-init)
;;; early-init.el ends here
