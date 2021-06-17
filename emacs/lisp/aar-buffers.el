;;; aar-buffers.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; <leader> buffer map
(define-prefix-command 'aar/leader-buffer-map)
(define-key aar/leader-map (kbd "b") 'aar/leader-buffer-map)
(which-key-add-keymap-based-replacements aar/leader-map "b" "buffer")

(defun aar/revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer t t t))

(define-key aar/leader-buffer-map (kbd "b") #'switch-to-buffer)
(define-key aar/leader-buffer-map (kbd "i") #'ibuffer)
(define-key aar/leader-buffer-map (kbd "r") #'aar/revert-buffer-no-confirm)
(define-key aar/leader-buffer-map (kbd "d") #'kill-current-buffer)
(define-key aar/leader-buffer-map (kbd "k") #'kill-buffer)
(define-key aar/leader-buffer-map (kbd "p") #'previous-buffer)
(define-key aar/leader-buffer-map (kbd "n") #'next-buffer)
(define-key aar/leader-buffer-map (kbd "[") #'previous-buffer)
(define-key aar/leader-buffer-map (kbd "]") #'next-buffer)

;;; autorevert
(global-auto-revert-mode t)

;;; ibuffer-vc
(aar/maybe-install-package 'ibuffer-vc)

(setq ibuffer-formats `((mark modified read-only locked " "
                              (name 18 18 :left :elide)
                              " " (size 9 -1 :right)
                              " " (mode 16 16 :left :elide)
                              ,@(when (require 'ibuffer-vc nil t)
                                  '(" " (vc-status 12 :left)))
                              " " project-relative-file)
                        (mark " " (name 16 -1) " " filename)))

;;; all-the-icons-ibuffer
(aar/maybe-install-package 'all-the-icons-ibuffer)

(setq all-the-icons-ibuffer-icon-size 1.0)
(setq all-the-icons-ibuffer-icon-v-adjust 0.0)
(setq all-the-icons-ibuffer-human-readable-size t)

(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

(provide 'aar-buffers)
;;; aar-buffers.el ends here
