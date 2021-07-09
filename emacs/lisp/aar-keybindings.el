;;; aar-keybindings.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; which-key
(aar/maybe-install-package 'which-key)
(setq which-key-idle-delay 0.3)
(setq which-key-allow-evil-operators t)
(which-key-mode)
(which-key-setup-side-window-bottom)

;;; key-chord
(aar/maybe-install-package 'key-chord)
(require 'key-chord)
(key-chord-mode 1)

;;; evil
(aar/maybe-install-package 'evil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(setq evil-want-C-u-delete t)
(setq evil-want-C-i-jump nil)
(setq evil-want-visual-char-semi-exclusive t)
(setq evil-ex-search-vim-style-regexp t)
(setq evil-ex-visual-char-range t)
(setq evil-respect-visual-line-mode t)
(setq evil-mode-line-format 'nil)
(setq evil-symbol-word-search t)
(setq evil-ex-interactive-search-highlight 'selected-window)
(setq evil-kbd-macro-suppress-motion-error t)
(setq evil-split-window-below t)
(setq evil-vsplit-window-right t)
(setq evil-flash-timer nil)
(setq evil-complete-all-buffers nil)
(require 'evil)
(evil-mode 1)
(evil-set-initial-state 'messages-buffer-mode 'normal)

;;;;;; evil-collection
(aar/maybe-install-package 'evil-collection)
(setq evil-collection-outline-bind-tab-p nil)
(setq evil-collection-want-unimpaired-p nil)
(add-hook 'emacs-startup-hook #'evil-collection-init)

;;;;;; evil-traces
(aar/maybe-install-package 'evil-traces)
(require 'evil-traces)
(evil-traces-use-diff-faces)
(evil-traces-mode)

;;; Keybinding definitions

;;;;;; Better escape
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;;;;;; Text scale and zoom
(global-set-key (kbd "C-+") #'text-scale-increase)
(global-set-key (kbd "C-_") #'text-scale-decrease)
(global-set-key (kbd "C-)") #'text-scale-adjust)

;;;;;; Key chords for reverting to normal mode
(key-chord-define evil-insert-state-map  "fd" #'evil-normal-state)
(key-chord-define evil-replace-state-map "fd" #'evil-normal-state)
(key-chord-define evil-emacs-state-map   "fd" #'evil-normal-state)

;;;;;; Universal arguments with evil
(global-set-key (kbd "C-M-u") 'universal-argument)

;;;;;; Visual indent/dedent
(defun aar/evil-visual-dedent ()
  "Equivalent to vnoremap < <gv."
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(defun aar/evil-visual-indent ()
  "Equivalent to vnoremap > >gv."
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

(evil-define-key 'visual 'global
  (kbd "<") #'aar/evil-visual-dedent
  (kbd ">") #'aar/evil-visual-indent)

;;;;;; Jumping to function and method definitions
(defun aar/next-beginning-of-method (count)
  "Jump to the beginning of the COUNT-th method/function after point."
  (interactive "p")
  (beginning-of-defun (- count)))

(defun aar/previous-beginning-of-method (count)
  "Jump to the beginning of the COUNT-th method/function before point."
  (interactive "p")
  (beginning-of-defun count))

(defalias #'aar/next-end-of-method #'end-of-defun
  "Jump to the end of the COUNT-th method/function after point.")

(defun aar/previous-end-of-method (count)
  "Jump to the end of the COUNT-th method/function before point."
  (interactive "p")
  (end-of-defun (- count)))

(evil-define-key '(normal visual motion) 'global
  (kbd "] m") #'aar/next-beginning-of-method
  (kbd "[ m") #'aar/previous-beginning-of-method
  (kbd "] M") #'aar/next-end-of-method
  (kbd "[ M") #'aar/previous-end-of-method)

;;;;;; <leader> and <localleader> implementations
;;;;;;;;; Override mode and map to trigger <leader> keys
(defvar aar/leader-override-mode-map (make-sparse-keymap)
  "Override Keymap to trigger \"leader key\" shortcuts.")

(define-minor-mode aar/leader-override-mode
  "Leader mode"
  :lighter ""
  :global t
  :keymap aar/leader-override-mode-map)

(defvar-local aar/maps-alist
  `((aar/leader-override-mode . ,aar/leader-override-mode-map))
  "Holds the (mode . keymap) pairs for leader override mode")

(put 'aar/maps-alist 'permanent-local t)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists 'aar/maps-alist)

;;;;;;;;; <leader> key definitions, keymap and trigger bindings
(defconst aar/leader-key     "SPC")
(defconst aar/leader-alt-key "M-SPC")
(defconst aar/localleader-key     "SPC m")
(defconst aar/localleader-alt-key "M-SPC m")

(which-key-add-key-based-replacements aar/localleader-key "<localleader>")
(which-key-add-key-based-replacements aar/localleader-alt-key "<localleader>")

(defvar aar/leader-map (make-sparse-keymap)
  "Keymap for \"leader key\" shortcuts.")

(evil-define-key '(normal visual motion) 'aar/leader-override-mode-map
  (kbd aar/leader-key) aar/leader-map)

(evil-define-key '(insert emacs) 'aar/leader-override-mode-map
  (kbd aar/leader-alt-key) aar/leader-map)

;;;;;; Some basic <leader> keybindings
(define-key aar/leader-map (kbd ":") #'pp-eval-expression)
(define-key aar/leader-map (kbd ";") #'execute-extended-command)
(define-key aar/leader-map (kbd "&") #'async-shell-command)
(define-key aar/leader-map (kbd "u") #'universal-argument)

(provide 'aar-keybindings)
;;; aar-keybindings.el ends here
