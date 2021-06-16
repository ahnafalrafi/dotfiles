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

(provide 'aar-completion)
;;; aar-completion.el ends here
