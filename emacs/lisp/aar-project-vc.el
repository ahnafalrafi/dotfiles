;;; aar-project-vc.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; project.el
(setq project-list-file (aar/expand-etc-file-name "project-list.el"))

;; Adapted from Manuel Uberti's config.
;; Declare directories with ".project" as a project
(cl-defmethod project-root ((project (head local)))
  (cdr project))

;;;###autoload
(defun aar/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'aar/project-try-local))

;; <leader> project map
(define-key aar/leader-map (kbd "SPC") #'project-find-file)
(define-key aar/leader-map (kbd "p")   project-prefix-map)

;; vc
(setq vc-follow-symlinks t)
(setq vc-ignore-dir-regexp (format "\\(%s\\)\\|\\(%s\\)"
                                   vc-ignore-dir-regexp
                                   tramp-file-name-regexp))

(define-key aar/leader-map (kbd "v") #'vc-prefix-map)
(define-key vc-prefix-map  (kbd "c") #'vc-create-repo)

;; magit
(straight-use-package 'magit)

;; settings for transient (a magit dependency)
(setq transient-levels-file  (aar/expand-cache-file-name "transient/levels"))
(setq transient-values-file  (aar/expand-cache-file-name "transient/values"))
(setq transient-history-file (aar/expand-cache-file-name "transient/history"))

;; gitgutter
(straight-use-package 'git-gutter)
(add-hook 'after-init-hook #'global-git-gutter-mode)

;; A <leader> git map and bindings
(define-prefix-command 'aar/leader-git-map)
(define-key aar/leader-map (kbd "g") 'aar/leader-git-map)
(which-key-add-keymap-based-replacements aar/leader-map "g" "git")

(define-key aar/leader-git-map (kbd "g") #'magit-status)
(define-key aar/leader-git-map (kbd "i") #'magit-init)

(provide 'aar-project-vc)
;;; aar-project-vc.el ends here
