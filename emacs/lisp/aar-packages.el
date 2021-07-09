;;; aar-packages.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; native-comp
(when (boundp 'comp-async-report-warnings-errors)
  (setq comp-async-report-warnings-errors nil))

;; Enable native compilation of packages when it's available
(when (featurep 'native-compile)
  (setq package-native-compile t))

;; Initialize package sources
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

;;; Upgrade installed packages
;; Inspired from paradox.el, extracted from:
;; https://www.reddit.com/r/emacs/comments/3m5tqx/can_usepackage_upgrade_installed_packages/
(defun aar/package-upgrade-packages (&optional no-fetch)
  "Upgrade all packages.  No questions asked.
This function is equivalent to `list-packages', followed by a
`package-menu-mark-upgrades'     and a `package-menu-execute'.      Except
the user isn't asked to confirm deletion of packages.
The NO-FETCH prefix argument is passed to `list-packages'.  It
prevents re-download of information about new versions.  It does
not prevent downloading the actual packages (obviously)."
  (interactive "P")
  (let ((package-menu-async nil)) ; This variable was introduced in emacs 25.0
    (save-window-excursion
      (package-list-packages no-fetch)
      (package-menu-mark-upgrades)
      (package-menu-execute 'noquery))))

;;; vendor-lisp: Packages from non-(M)ELPA sources
(defconst aar/vendor-lisp-dir
  (expand-file-name "vendor-lisp/" user-emacs-directory)
  "Directory containing lisp files from non-(M)ELPA sources.")

(provide 'aar-packages)
;;; aar-packages.el ends here
