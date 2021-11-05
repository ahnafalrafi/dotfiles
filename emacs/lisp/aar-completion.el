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

;; company-mode
(straight-use-package 'company)

(with-eval-after-load 'company
  (evil-define-key 'insert company-mode-map
    (kbd "C-n")     #'company-select-next
    (kbd "C-p")     #'company-select-previous
    (kbd "C-x C-o") #'company-manual-begin
    (kbd "C-x C-f") #'company-files)

  (evil-define-key 'insert company-active-map
    (kbd "C-n") #'company-select-next-or-abort
    (kbd "C-p") #'company-select-previous-or-abort
    (kbd "C-j") #'company-select-next
    (kbd "C-k") #'company-select-previous))


(defvar aar/company-backends-global '(company-capf
                                      company-files
                                      company-keywords
                                      company-yasnippet)
  "Values for `company-backends' used everywhere.")

(setq company-backends aar/company-backends-global)
(setq company-minimum-prefix-length 2)
(setq company-idle-delay 0.0)
(setq company-show-numbers nil)
(setq company-selection-wrap-around t)
(setq company-tooltip-idle-delay 0.0)
(setq company-tooltip-limit 15)
(setq company-minimum-prefix-length 1)
(setq company-tooltip-align-annotations t)
(setq company-transformers '(company-sort-by-backend-importance))

(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'company-tng-mode)

(provide 'aar-completion)
;;; aar-completion.el ends here
