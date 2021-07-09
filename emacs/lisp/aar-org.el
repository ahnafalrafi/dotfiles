;;; aar-org.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(aar/maybe-install-package 'org)

(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "inbox.org"))
(setq org-indent-mode nil)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-startup-indented nil)
(setq org-adapt-indentation nil)
(setq org-hide-leading-stars nil)
(setq org-preview-latex-image-directory (aar/expand-cache-file-name
                                         "org-latex-previews/"))
(setq org-latex-prefer-user-labels t)
(setq org-src-preserve-indentation t)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-return-follows-link t)
(setq org-use-sub-superscripts '{})
(setq org-highlight-latex-and-related '(native latex script entities))
(setq org-confirm-babel-evaluate #'aar/org-confirm-babel-evaluate)

(defun aar/org-confirm-babel-evaluate (lang body)
  (not (string= lang "jupyter-python")))

;;; evil and org
;;;;;; Better tab: cycle only the current subtree
;;;###autoload
(defun aar/org-cycle-only-current-subtree-h (&optional arg)
  "Toggle the local fold at the point, and no deeper.
`org-cycle's standard behavior is to cycle between three levels: collapsed,
subtree and whole document. This is slow, especially in larger org buffer. Most
of the time I just want to peek into the current subtree -- at most, expand
,*only* the current subtree.

All my (performant) foldings needs are met between this and `org-show-subtree'
(on zO for evil users), and `org-cycle' on shift-TAB if I need it."
  (interactive "P")
  (unless (eq this-command 'org-shifttab)
    (save-excursion
      (org-beginning-of-line)
      (let (invisible-p)
        (when (and (org-at-heading-p)
                   (or org-cycle-open-archived-trees
                       (not (member org-archive-tag (org-get-tags))))
                   (or (not arg)
                       (setq invisible-p (outline-invisible-p (line-end-position)))))
          (unless invisible-p
            (setq org-cycle-subtree-status 'subtree))
          (org-cycle-internal-local)
          t)))))

;;;;;; evil-org
(aar/maybe-install-package 'evil-org)

;;; cdlatex
(aar/maybe-install-package 'cdlatex)
(setq cdlatex-sub-super-scripts-outside-math-mode nil)
(setq cdlatex-use-dollar-to-ensure-math nil)
(setq cdlatex-simplify-sub-super-scripts nil)

;;; toc-org
(aar/maybe-install-package 'toc-org)
(setq toc-org-hrefify-default "gh")

;;; <localleader> org mode map
(define-prefix-command 'aar/localleader-org-mode-map)
(evil-define-key '(normal visual motion) org-mode-map
  (kbd aar/localleader-key) 'aar/localleader-org-mode-map)
(evil-define-key '(insert emacs) org-mode-map
  (kbd aar/localleader-alt-key) 'aar/localleader-org-mode-map)

(define-key aar/localleader-org-mode-map (kbd ".") #'consult-outline)
(define-key aar/localleader-org-mode-map (kbd "p") #'org-preview-latex-fragment)
(define-key aar/localleader-org-mode-map
  (kbd "l t") #'org-toggle-link-display)
(define-key aar/localleader-org-mode-map
  (kbd "l i") #'org-toggle-inline-images)
(define-key aar/localleader-org-mode-map (kbd "e") #'org-export-dispatch)
(define-key aar/localleader-org-mode-map (kbd "a") #'org-latex-export-to-pdf)

(evil-define-key '(normal visual motion) org-mode-map
  (kbd "[ h") #'org-backward-heading-same-level
  (kbd "] h") #'org-forward-heading-same-level
  (kbd "[ l") #'org-previous-link
  (kbd "] l") #'org-next-link
  (kbd "[ c") #'org-babel-previous-src-block
  (kbd "] c") #'org-babel-next-src-block)

;;; Hook for org-mode
(defun aar/org-mode-h ()
  (setq-local spell-fu-faces-exclude '(org-block
                                       org-block-begin-line
                                       org-block-end-line
                                       org-code
                                       font-lock-keyword-face
                                       org-date
                                       org-formula
                                       org-latex-and-related
                                       org-link
                                       org-meta-line
                                       org-property-value
                                       org-ref-cite-face
                                       org-special-keyword
                                       org-tag
                                       org-todo
                                       org-todo-keyword-done
                                       org-todo-keyword-habt
                                       org-todo-keyword-kill
                                       org-todo-keyword-outd
                                       org-todo-keyword-todo
                                       org-todo-keyword-wait
                                       org-verbatim))

  (visual-line-mode)
  (auto-fill-mode)
  (adaptive-wrap-prefix-mode)

  (require 'evil-org)
  (setq evil-org-retain-visual-state-on-shift t)
  (add-hook 'evil-org-mode-hook #'evil-org-set-key-theme)
  (evil-org-mode 1)

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  (toc-org-enable)
  (turn-on-org-cdlatex))

(add-hook 'org-mode-hook #'aar/org-mode-h)
(add-hook 'org-tab-first #'aar/org-cycle-only-current-subtree-h)
(add-hook 'org-capture-mode-hook #'evil-insert-state)

;;; ob-async
(aar/maybe-install-package 'ob-async)

;;; ob-jupyter
(with-eval-after-load 'org-src
  (dolist (lang '(python julia R))
    (cl-pushnew (cons (format "jupyter-%s" lang) lang)
                org-src-lang-modes :key #'car)))
(with-eval-after-load 'ob-async
  (dolist (lang '(python julia R))
    (cl-pushnew (format "jupyter-%s" lang)
                ob-async-no-async-languages-alist)))

(setq org-babel-default-header-args:jupyter-python '((:async   . "yes")
                                                     (:session . "py")
                                                     (:kernel  . "python3")))

;;; ob-julia
;; (add-to-list 'load-path (expand-file-name "ob-julia" aar/vendor-lisp-dir))

;;; with-eval-after-load statements
(with-eval-after-load 'org
  (plist-put org-format-latex-options :scale 1.75)
  (plist-put org-format-latex-options :background 'default)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (R . t)
     (python . t)
     ;; (julia . t)
     (jupyter . t))))

(with-eval-after-load 'ox-latex
  ;; Configuration
  (setq org-latex-listings t)
  (setq org-latex-hyperref-template nil)

  (setq org-latex-listings-langs '((jupyter-python "Python")
                                   (emacs-lisp "common-lisp")
                                   (elisp "common-lisp")
                                   (cc "c++")
                                   (shell-script "bash")))

  (add-to-list 'org-latex-classes
               `("aar-article"
                 ,(concat "\\documentclass[letterpaper,11pt]{article}\n"
                          "[NO-DEFAULT-PACKAGES]\n"
                          ;; "[NO-PACKAGES]\n"
                          "[EXTRA]")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("book-noparts"
                 "\\documentclass[11pt]{book}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
  (add-to-list 'org-latex-classes
               '("report"
                 "\\documentclass[11pt]{report}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (setq org-latex-default-class "aar-article"))

(provide 'aar-org)
;;; aar-org.el ends here
