;;; aar-completion.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; vertico
(straight-use-package 'vertico)
(setq vertico-cycle t)
(add-hook 'after-init-hook #'vertico-mode)

;; savehist-mode: persist history over Emacs restarts.
(setq savehist-file (aar/expand-cache-file-name "savehist.el"))
(setq savehist-save-minibuffer-history t)
(add-hook 'after-init-hook #'savehist-mode)

;; consult
(straight-use-package 'consult)
(setq consult-preview-key 'nil)

(global-set-key [remap apropos]            #'consult-apropos)
(global-set-key [remap bookmark-jump]      #'consult-bookmark)
(global-set-key [remap evil-show-marks]    #'consult-mark)
(global-set-key [remap imenu]              #'consult-imenu)
(global-set-key [remap load-theme]         #'consult-theme)
(global-set-key [remap locate]             #'consult-locate)
(global-set-key [remap recentf-open-files] #'consult-recent-file)
(global-set-key [remap yank-pop]           #'consult-yank-pop)

;; marginalia
(straight-use-package 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy
                              marginalia-annotators-light))
(add-hook 'after-init-hook #'marginalia-mode)

;; orderless
(straight-use-package 'orderless)
(setq completion-styles '(orderless))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles . (partial-completion)))))

;; corfu
(straight-use-package 'corfu)
(setq corfu-auto t)
(setq corfu-cycle t)
(setq corfu-preselect-first nil)
(setq corfu-quit-at-boundary t)
(setq corfu-quit-no-match t)
(add-hook 'after-init-hook #'corfu-global-mode)

;; cape
(straight-use-package 'cape)

(with-eval-after-load 'corfu
  ;; See https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (evil-make-overriding-map corfu-map)
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-define-key 'insert corfu-map
    (kbd "C-n")     #'corfu-next
    (kbd "C-p")     #'corfu-previous
    (kbd "C-j")     #'corfu-next
    (kbd "C-k")     #'corfu-previous
    (kbd "TAB")     #'corfu-next
    (kbd "S-TAB")   #'corfu-previous
    (kbd "C-x C-o") #'completion-at-point
    (kbd "C-x C-f") #'cape-file))

(provide 'aar-completion)
;;; aar-completion.el ends here
