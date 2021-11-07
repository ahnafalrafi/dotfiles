;;; aar-julia.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; julia-mode
(straight-use-package 'julia-mode)

;;; <localleader> map for julia
(define-prefix-command 'aar/localleader-julia-mode-map)
(evil-define-key '(normal visual motion) julia-mode-map
  (kbd aar/localleader-key) 'aar/localleader-julia-mode-map)
(evil-define-key '(insert emacs) julia-mode-map
  (kbd aar/localleader-alt-key) 'aar/localleader-julia-mode-map)

;;; julia-repl
(straight-use-package 'julia-repl)
(define-key aar/localleader-julia-mode-map (kbd "r") #'julia-repl)
(define-key aar/localleader-julia-mode-map
  (kbd "a") #'julia-repl-send-region-or-line)
(define-key aar/localleader-julia-mode-map (kbd "l") #'julia-repl-send-line)

(with-eval-after-load 'julia-repl
  (julia-repl-set-terminal-backend 'vterm))

;;; project.el integration - shamelessly stolen from eglot-jl
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

;;; lsp-julia
(straight-use-package 'lsp-julia)

(defconst aar/julia-sysimage (expand-file-name "~/.cache/julia/julials.so")
  "Path to system image with precompiled LanguageServer.jl and friends.")

(defconst aar/julia-language-server-project
  (directory-file-name
   (file-name-directory
    (with-temp-buffer
      (insert-file-contents (expand-file-name "julia/lang_server_locate"
                                              (or (getenv "XDG_CACHE_HOME")
                                                  "~/.cache")))
      (buffer-string))))
  "Julia project to run language server from.
The project should have LanguageServer and SymbolServer packages
available.")

(setq lsp-julia-package-dir nil)
(setq lsp-julia-default-environment
      (car (last (file-expand-wildcards "~/.julia/environments/v*" t))))
(setq lsp-julia-flags `("--startup-file=no"
                        "--history-file=no"
                        ,(concat "--project="
                                 aar/julia-language-server-project)
                        ,(if (file-exists-p aar/julia-sysimage)
                             (concat "--sysimage=" aar/julia-sysimage))
                        "--sysimage-native-code=yes"))

;;; julia hook function
;;;###autoload
(defun aar/julia-mode-h ()
  (julia-repl-mode)
  (require 'lsp-julia)
  (lsp-deferred))
(add-hook 'julia-mode-hook #'aar/julia-mode-h)

(provide 'aar-julia)
;;; aar-julia.el ends here
