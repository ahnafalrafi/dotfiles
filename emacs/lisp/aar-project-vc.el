;;; aar-project-vc.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; <leader> project map
(define-key aar/leader-map (kbd "SPC") #'project-find-file)
(define-key aar/leader-map (kbd "p")   project-prefix-map)

;;; vc
(setq vc-follow-symlinks t)
(setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                   vc-ignore-dir-regexp
                                   tramp-file-name-regexp))

(define-key aar/leader-map (kbd "v") #'vc-prefix-map)
(define-key vc-prefix-map  (kbd "c") #'vc-create-repo)

;;; magit
(aar/maybe-install-package 'magit)
(setq magit-completing-read-function #'consult--read)

;; A <leader> git map and bindings
(define-prefix-command 'aar/leader-git-map)
(define-key aar/leader-map (kbd "g") 'aar/leader-git-map)
(which-key-add-keymap-based-replacements aar/leader-map "g" "git")

(define-key aar/leader-git-map (kbd "g") #'magit-status)

(provide 'aar-project-vc)
;;; aar-project-vc.el ends here
