;;; early-init.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Defer loading package.el until called explicitly
(setq package-enable-at-startup nil)

;; Defer garbage collection
(setq gc-cons-threshold (* 80 1024 1024))

;; Disable gui elements before they are initialized.
(when (featurep 'menu-bar)
  (push '(menu-bar-lines . 0) default-frame-alist)
  (setq menu-bar-mode nil))

(when (featurep 'tool-bar)
  (push '(tool-bar-lines . 0) default-frame-alist)
  (setq tool-bar-mode nil))

(when (featurep 'scroll-bar)
  (push '(vertical-scroll-bars) default-frame-alist)
  (setq scroll-bar-mode nil))

;; Directories for keeping user-emacs-directory clean
(defconst aar/etc-dir (expand-file-name
                       (convert-standard-filename "emacs/")
                       (or (getenv "XDG_DATA_HOME")
                           (expand-file-name "~/.local/share/")))
  "Directory for non-volatile local storage.

Used for files that don't change much. Must end with a slash.")

(defconst aar/cache-dir (expand-file-name
                         (convert-standard-filename "emacs/")
                         (or (getenv "XDG_CACHE_HOME")
                             (expand-file-name "~/.cache/")))
  "Directory for volatile local storage.

Used for files that change often. Must end with a slash.")

;; Make above directories if they don't exist
(dolist (dir (list aar/etc-dir
                   aar/cache-dir))
  (unless (file-directory-p dir)
    (make-directory dir)))

(defun aar/expand-etc-file-name (file)
  "Expand filename FILE relative to `aar/etc-dir'."
  (expand-file-name (convert-standard-filename file) aar/etc-dir))

(defun aar/expand-cache-file-name (file)
  "Expand filename FILE relative to `aar/cache-dir'."
  (expand-file-name (convert-standard-filename file) aar/cache-dir))

(when (featurep 'native-compile)
  ;; Silence compiler warnings as they can be pretty disruptive
  (setq native-comp-async-report-warnings-errors nil)

  ;; Set the right directory to store the native comp cache
  (setcar native-comp-eln-load-path (aar/expand-cache-file-name "eln-cache/")))

(provide 'early-init)
;;; early-init.el ends here
