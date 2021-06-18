;;; aar-julia.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; julia-mode
(aar/maybe-install-package 'julia-mode)
(require 'julia-mode)

;;; <localleader> map for julia
(define-prefix-command 'aar/localleader-julia-mode-map)
(evil-define-key '(normal visual motion) julia-mode-map
  (kbd aar/localleader-key) 'aar/localleader-julia-mode-map)
(evil-define-key '(insert emacs) julia-mode-map
  (kbd aar/localleader-alt-key) 'aar/localleader-julia-mode-map)

;;; julia-repl
(aar/maybe-install-package 'julia-repl)
(evil-define-key nil aar/localleader-julia-mode-map
   (kbd "r") #'julia-repl
   (kbd "a") #'julia-repl-send-region-or-line
   (kbd "l") #'julia-repl-send-line)

(with-eval-after-load 'julia-repl
  (julia-repl-set-terminal-backend 'vterm))

;;; eglot configuration - shamelessly stolen from eglot-jl
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
               `(julia-mode . ("julia"
                               ,(concat "--project="
                                        eglot-julia-language-server-project)
                               ,@eglot-julia-flags
                               ,eglot-julia-language-server-script))))
;;; julia hook function
(defun aar/julia-mode-h ()
  (julia-repl-mode)
  (tree-sitter-mode)
  (setq-local eglot-connect-timeout 300)
  (eglot-ensure))

(add-hook 'julia-mode-hook #'aar/julia-mode-h)

(provide 'aar-julia)
;;; aar-julia.el ends here
