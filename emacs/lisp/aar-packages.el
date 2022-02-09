;;; aar-packages.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; Set up directory for straight.el and tell it to pull from the develop branch
(setq straight-base-dir aar/cache-dir)
(setq straight-repository-branch "develop")
(setq straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" aar/cache-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(provide 'aar-packages)
;;; aar-packages.el ends here
