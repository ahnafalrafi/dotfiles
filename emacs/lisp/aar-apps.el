;;; aar-apps.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; <leader> apps map
(define-prefix-command 'aar/leader-apps-map)
(define-key aar/leader-map (kbd "a") 'aar/leader-apps-map)
(which-key-add-keymap-based-replacements aar/leader-map "a" "apps")

;;; vterm
(aar/maybe-install-package 'vterm)
(setq vterm-always-compile-module t)
(setq vterm-buffer-name-string "vterm: %s")
(setq vterm-copy-exclude-prompt t)
(setq vterm-kill-buffer-on-exit t)
(setq vterm-max-scrollback 5000)

;;;;;; vterm helper functions
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

;;;;;; vterm hook function
(defun aar/vterm-h ()
  (setq-local show-trailing-whitespace nil))

(add-hook 'vterm-mode-hook #'aar/vterm-h)

;;;;;; vterm keybindings
(define-key aar/leader-apps-map (kbd "t") #'aar/vterm-here-other-window)
(define-key aar/leader-apps-map (kbd "T") #'aar/vterm-here)

;;; jupyter
(aar/maybe-install-package 'jupyter)
(setq jupyter-repl-echo-eval-p t)
(setq jupyter-repl-allow-RET-when-busy t)

(define-key aar/leader-apps-map (kbd "j") #'jupyter-run-repl)

;;; pdf-tools
(aar/maybe-install-package 'pdf-tools)
(pdf-loader-install)
(add-to-list 'auto-mode-alist '("\\.[pP][dD][fF]\\'" . pdf-view-mode))
(setq-default pdf-view-display-size 'fit-width
              pdf-view-resize-factor 1.05)
(push 'pdf-view-mode evil-snipe-disabled-modes)

(defun aar/pdf-view-mode-h ()
  (setq-local evil-normal-state-cursor (list nil))
  (pdf-sync-minor-mode)
  (pdf-links-minor-mode))

(add-hook 'pdf-view-mode-hook #'aar/pdf-view-mode-h)

;; On scroll, move to beginning of page.
(advice-add #'pdf-view-previous-page-command :after #'image-bob)
(advice-add #'pdf-view-next-page-command :after #'image-bob)

(evil-define-key 'normal 'pdf-view-mode-map
  (kbd "s")     #'pdf-view-fit-width-to-window
  (kbd "a")     #'pdf-view-fit-height-to-window
  (kbd "H")     #'image-bob
  (kbd "J")     #'pdf-view-next-page-command
  (kbd "K")     #'pdf-view-previous-page-command
  (kbd "L")     #'image-eob
  (kbd "o")     #'pdf-outline
  (kbd "TAB")   #'pdf-outline
  (kbd "<tab>") #'pdf-outline
  (kbd "C-r")   #'pdf-view-midnight-minor-mode)

(if (fboundp 'doom-modeline-def-modeline)
    (doom-modeline-def-modeline 'pdf
      '(bar window-number matches pdf-pages buffer-info)
      '(misc-info major-mode process vcs)))

(provide 'aar-apps)
;;; aar-apps.el ends here
