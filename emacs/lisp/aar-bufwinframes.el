;;; aar-bufwinframes.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; Buffers
;;;;;; <leader> buffer map
(define-prefix-command 'aar/leader-buffer-map)
(define-key aar/leader-map (kbd "b") 'aar/leader-buffer-map)
(which-key-add-keymap-based-replacements aar/leader-map "b" "buffer")

;;;;;; Functions
(defun aar/revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer t t t))

;;;;;; Leader bindings for buffer map
(define-key aar/leader-buffer-map (kbd "b") #'switch-to-buffer)
(define-key aar/leader-buffer-map (kbd "i") #'ibuffer)
(define-key aar/leader-buffer-map (kbd "r") #'aar/revert-buffer-no-confirm)
(define-key aar/leader-buffer-map (kbd "d") #'kill-current-buffer)
(define-key aar/leader-buffer-map (kbd "k") #'kill-buffer)
(define-key aar/leader-buffer-map (kbd "p") #'previous-buffer)
(define-key aar/leader-buffer-map (kbd "n") #'next-buffer)
(define-key aar/leader-buffer-map (kbd "[") #'previous-buffer)
(define-key aar/leader-buffer-map (kbd "]") #'next-buffer)

;;;;;; autorevert
(global-auto-revert-mode t)

;;;;;; ibuffer-vc
(aar/maybe-install-package 'ibuffer-vc)
(setq ibuffer-formats `((mark modified read-only locked " "
                              (name 18 18 :left :elide)
                              " " (size 9 -1 :right)
                              " " (mode 16 16 :left :elide)
                              ,@(when (require 'ibuffer-vc nil t)
                                  '(" " (vc-status 12 :left)))
                              " " project-relative-file)
                        (mark " " (name 16 -1) " " filename)))

;;;;;; all-the-icons-ibuffer
(aar/maybe-install-package 'all-the-icons-ibuffer)
(setq all-the-icons-ibuffer-icon-size 1.0)
(setq all-the-icons-ibuffer-icon-v-adjust 0.0)
(setq all-the-icons-ibuffer-human-readable-size t)
(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

;;; Windows
;;;;;; <leader> bindings for managing windows
(define-key aar/leader-map (kbd "w") 'evil-window-map)

(define-key evil-window-map (kbd "C-h") #'evil-window-left)
(define-key evil-window-map (kbd "C-j") #'evil-window-down)
(define-key evil-window-map (kbd "C-k") #'evil-window-up)
(define-key evil-window-map (kbd "C-l") #'evil-window-right)
(define-key evil-window-map (kbd "C-q") #'evil-quit)
(define-key evil-window-map (kbd "d")   #'evil-quit)
(define-key evil-window-map (kbd "x")   #'kill-buffer-and-window)
(define-key evil-window-map (kbd "f")   #'ffap-other-window)
(define-key evil-window-map (kbd "C-f") #'ffap-other-window)

;;; Frames
;;;;;; <leader> frame map
(define-prefix-command 'aar/leader-frame-map)
(define-key aar/leader-map (kbd "F") 'aar/leader-frame-map)
(which-key-add-keymap-based-replacements aar/leader-map "F" "frame")

;;;;;; Functions
(defun aar/delete-frame-or-kill-emacs ()
  "Delete the current frame if it is non-unique in session."
  (interactive)
  (if (cdr (frame-list))
      (delete-frame)
    (save-buffers-kill-emacs)))

;;;;;; Leader bindings for frame
(define-key aar/leader-frame-map (kbd "o") #'make-frame)
(define-key aar/leader-frame-map (kbd "q") #'aar/delete-frame-or-kill-emacs)

;;; Quitting emacs
;;;;;; Don't require confirmation every time when quitting.
(setq confirm-kill-emacs nil)

;;;;;; <leader> quit map
(define-prefix-command 'aar/leader-quit-map)
(define-key aar/leader-map (kbd "q") 'aar/leader-quit-map)
(which-key-add-keymap-based-replacements aar/leader-map "q" "quit")

;;;;;; Leader bindings for quit map
(define-key aar/leader-quit-map (kbd "K") #'save-buffers-kill-emacs)

(provide 'aar-bufwinframes)
;;; aar-bufwinframes.el ends here
