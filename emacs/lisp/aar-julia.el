;;; aar-julia.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; julia-mode
(straight-use-package 'julia-mode)

;; <localleader> map for julia
(define-prefix-command 'aar/localleader-julia-mode-map)
(evil-define-key '(normal visual motion) julia-mode-map
  (kbd aar/localleader-key) 'aar/localleader-julia-mode-map)
(evil-define-key '(insert emacs) julia-mode-map
  (kbd aar/localleader-alt-key) 'aar/localleader-julia-mode-map)

;; julia-repl
(straight-use-package 'julia-repl)
(define-key aar/localleader-julia-mode-map (kbd "r") #'julia-repl)
(define-key aar/localleader-julia-mode-map
  (kbd "a") #'julia-repl-send-region-or-line)
(define-key aar/localleader-julia-mode-map (kbd "l") #'julia-repl-send-line)

(with-eval-after-load 'julia-repl
  (julia-repl-set-terminal-backend 'vterm))

;; project.el integration - shamelessly stolen from eglot-jl
;; Make project.el aware of Julia projects
;;;###autoload
(defun aar/project-try-julia (dir)
  "Return project instance if DIR is part of a julia project.
Otherwise returns nil"
  (let ((root (or (locate-dominating-file dir "JuliaProject.toml")
                  (locate-dominating-file dir "Project.toml"))))
    (and root (cons 'julia root))))

(cl-defmethod project-roots ((project (head julia)))
  (list (cdr project)))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'aar/project-try-julia))

;; julia hook function
;;;###autoload
(defun aar/julia-mode-h ()
  (visual-line-mode)
  (adaptive-wrap-prefix-mode)
  (setq-local adaptive-wrap-extra-indent 2)
  (setq-local evil-shift-width julia-indent-offset)
  (julia-repl-mode)
  (lsp-deferred))

(with-eval-after-load 'julia-mode
  (require 'lsp-mode)
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("julia"
                                                            "--startup-file=no"
                                                            "--history-file=no"
                                                            "/home/ahnaf/dotfiles/julia/lang_server_invoke.jl"))
                    :major-modes '(julia-mode ess-julia-mode)
                    :server-id 'julia-ls
                    :multi-root t)))

(add-hook 'julia-mode-hook #'aar/julia-mode-h)

(provide 'aar-julia)
;;; aar-julia.el ends here
