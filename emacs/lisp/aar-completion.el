;;; aar-completion.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; vertico
(aar/maybe-install-package 'vertico)
(setq vertico-cycle t)
(vertico-mode)

;;; savehist-mode: persist history over Emacs restarts.
;; Vertico sorts by history position.
(setq savehist-save-minibuffer-history t)
(savehist-mode)

;;; consult
(aar/maybe-install-package 'consult)
(setq consult-preview-key 'nil)

(require 'consult)

(global-set-key [remap apropos]            #'consult-apropos)
(global-set-key [remap bookmark-jump]      #'consult-bookmark)
(global-set-key [remap evil-show-marks]    #'consult-mark)
(global-set-key [remap imenu]              #'consult-imenu)
(global-set-key [remap load-theme]         #'consult-theme)
(global-set-key [remap locate]             #'consult-locate)
(global-set-key [remap recentf-open-files] #'consult-recent-file)
(global-set-key [remap yank-pop]           #'consult-yank-pop)

;;; marginalia
(aar/maybe-install-package 'marginalia)
(setq marginalia-annotators '(marginalia-annotators-heavy
                              marginalia-annotators-light))
(marginalia-mode)

;;; orderless
(aar/maybe-install-package 'orderless)
(require 'orderless)
(setq completion-styles '(orderless))
(setq completion-category-defaults nil)
(setq completion-category-overrides '((file (styles . (partial-completion)))))

;;; company-mode
(aar/maybe-install-package 'company)

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

;; (dolist (hook '(prog-mode-hook text-mode-hook))
;;   (add-hook hook #'company-mode))
(add-hook 'after-init-hook #'global-company-mode)
(add-hook 'after-init-hook #'company-tng-mode)

(provide 'aar-completion)
;;; aar-completion.el ends here
