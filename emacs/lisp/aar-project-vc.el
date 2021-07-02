;;; aar-project-vc.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; project.el
;; Adapted from Manuel Uberti's config.
;; Declare directories with ".project" as a project
(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun aar/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'local root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'aar/project-try-local))

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
(define-key aar/leader-git-map (kbd "i") #'magit-init)

(provide 'aar-project-vc)
;;; aar-project-vc.el ends here
