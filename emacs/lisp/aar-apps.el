;;; aar-apps.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; <leader> applications map
(define-prefix-command 'aar/leader-apps-map)
(define-key aar/leader-map (kbd "a") 'aar/leader-apps-map)
(which-key-add-keymap-based-replacements aar/leader-map "a" "apps")

;;; vterm
(straight-use-package 'vterm)
(setq vterm-always-compile-module t)
(setq vterm-buffer-name-string "vterm: %s")
(setq vterm-copy-exclude-prompt t)
(setq vterm-kill-buffer-on-exit t)
(setq vterm-max-scrollback 5000)

;; vterm helper functions
;;;###autoload
(defun aar/vterm-cd-if-remote ()
  "When `default-directory` is remote, use the corresponding
method to prepare vterm at the corresponding remote directory."
  (when (and (featurep 'tramp)
             (tramp-tramp-file-p default-directory))
    (message "default-directory is %s" default-directory)
    (with-parsed-tramp-file-name default-directory path
                                 (let ((method (cadr (assoc `tramp-login-program
                                                            (assoc path-method tramp-methods)))))
                                   (vterm-send-string
                                    (concat method " "
                                            (when path-user (concat path-user "@")) path-host))
                                   (vterm-send-return)
                                   (vterm-send-string
                                    (concat "cd " path-localname))
                                   (vterm-send-return)))))

;;;###autoload
(defun aar/vterm-here-other-window ()
  (interactive)
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  ;; This hack forces vterm to redraw, fixing strange artefacting in the tty.
  (save-window-excursion
    (pop-to-buffer "*scratch*"))
  (require 'vterm)
  (vterm-other-window)
  (aar/vterm-cd-if-remote))

;;;###autoload
(defun aar/vterm-here ()
  (interactive)
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  ;; This hack forces vterm to redraw, fixing strange artefacting in the tty.
  (save-window-excursion
    (pop-to-buffer "*scratch*"))
  (require 'vterm)
  (vterm)
  (aar/vterm-cd-if-remote))

;; vterm hook function
;;;###autoload
(defun aar/vterm-h ()
  (setq-local show-trailing-whitespace nil))

(add-hook 'vterm-mode-hook #'aar/vterm-h)

;; vterm keybindings
(define-key aar/leader-apps-map (kbd "t") #'aar/vterm-here-other-window)
(define-key aar/leader-apps-map (kbd "T") #'aar/vterm-here)

(provide 'aar-apps)
;;; aar-apps.el ends here
