;;; aar-julia.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; julia-mode
(aar/maybe-install-package 'julia-mode)

;;; <localleader> map for julia
(define-prefix-command 'aar/localleader-julia-mode-map)
(evil-define-key '(normal visual motion) julia-mode-map
  (kbd aar/localleader-key) 'aar/localleader-julia-mode-map)
(evil-define-key '(insert emacs) julia-mode-map
  (kbd aar/localleader-alt-key) 'aar/localleader-julia-mode-map)

;;; ess-julia-mode
(add-to-list 'auto-mode-alist '("\\.jl\\'" . ess-julia-mode))

(with-eval-after-load 'ess-julia
  (evil-define-key '(normal visual motion) ess-julia-mode-map
    (kbd aar/localleader-key) 'aar/localleader-julia-mode-map)
  (evil-define-key '(insert emacs) ess-julia-mode-map
    (kbd aar/localleader-alt-key) 'aar/localleader-julia-mode-map)

  (define-key ess-julia-mode-map (kbd "TAB") #'julia-latexsub-or-indent))

;;; julia-repl
(aar/maybe-install-package 'julia-repl)
(define-key aar/localleader-julia-mode-map (kbd "r") #'julia-repl)
(define-key aar/localleader-julia-mode-map
  (kbd "a") #'julia-repl-send-region-or-line)
(define-key aar/localleader-julia-mode-map (kbd "l") #'julia-repl-send-line)

(with-eval-after-load 'julia-repl
  (julia-repl-set-terminal-backend 'vterm))

;;; project.el integration - shamelessly stolen from eglot-jl
;; Make project.el aware of Julia projects
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

;;; eglot configuration - also obviously shamelessly stolen from eglot-jl
(defgroup eglot-julia nil
  "Interaction with LanguageServer.jl LSP server via eglot"
  :prefix "eglot-julia-"
  :group 'applications)

(defcustom eglot-julia-sysimage (expand-file-name "~/.cache/julia/julials.so")
  "Path to system image with precompiled LanguageServer.jl and friends."
  :type 'string)

(defcustom eglot-julia-flags `("--startup-file=no"
                               "--history-file=no"
                               ,(if (file-exists-p eglot-julia-sysimage)
                                    (concat "--sysimage=" eglot-julia-sysimage))
                               "--sysimage-native-code=yes")
  "Extra flags to pass to the Julia executable."
  :type '(repeat string))

(defcustom eglot-julia-language-server-project (getenv "JULIA_LSP")
  "Julia project to run language server from.
The project should have LanguageServer and SymbolServer packages
available."
  :type 'string)

(defcustom eglot-julia-language-server-script
  (expand-file-name "~/.config/julia/lang_server_invoke.jl")
  "Julia script used to invoke language server and capabilities."
  :type 'string)

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               ;; function instead of strings to find project dir at runtime
               `((julia-mode ess-julia-mode)
                 .
                 ("julia"
                  ,(concat "--project=" eglot-julia-language-server-project)
                  ,@eglot-julia-flags
                  ,eglot-julia-language-server-script))))

;;; julia hook function
(defun aar/julia-mode-h ()
  (julia-repl-mode)
  (setq-local eglot-connect-timeout 300)
  (eglot-ensure))

(dolist (hook '(julia-mode-hook ess-julia-mode-hook))
  (add-hook hook #'aar/julia-mode-h))

(provide 'aar-julia)
;;; aar-julia.el ends here
