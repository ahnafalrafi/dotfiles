;;; aar-bib.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; bibtex-mode
(setq bibtex-dialect 'biblatex)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 20)

;; bibtex-mode hooks
(defun aar/bibtex-mode-h ()
  (visual-line-mode))

(add-hook 'bibtex-mode-hook #'aar/bibtex-mode-h)

;; bibtex-autokey stuff
(setq bibtex-autokey-year-length 4)
(setq bibtex-autokey-names 1)
(setq bibtex-autokey-titlewords 3)
(setq bibtex-autokey-titlewords-stretch 0)
(setq bibtex-autokey-titleword-length nil)
(setq bibtex-autokey-titleword-case-convert-function #'s-upper-camel-case)
(setq bibtex-autokey-titleword-separator "")

(defun aar/bibtex-generate-autokey ()
  "Generate a key for a BibTeX entry using the author, year and title fields.
This uses the same components as `bibtex-generate-autokey' but combines them in
a different order. `bibtex-generate-autokey' combines components according to
(name, year, title). Here, the combination is (year, name, title). See the
documentation for `bibtex-generate-autokey' for more details."
  (interactive)
  (let* ((names (bibtex-autokey-get-names))
         (year (bibtex-autokey-get-year))
         (title (bibtex-autokey-get-title))
         (autokey (concat year names title)))
    autokey))

;;; ebib
(aar/maybe-install-package 'ebib)
(setq ebib-default-directory "~/Dropbox/research/")
(setq ebib-bibtex-dialect 'biblatex)
;; Open files in pdf-tools
(setq ebib-file-associations nil)
(setq ebib-bib-search-dirs '("~/Dropbox/research/"
                             "~/Dropbox/research/ongoing/"))
(setq ebib-preload-bib-files '("bibliography.bib"
                               "2021_semipar_eff_car/2021_semipar_eff_car.bib"))

(setq ebib-truncate-file-names nil)
(setq ebib-file-search-dirs '("~/Dropbox/research/readings/"))

(defun aar/ebib-entry-mode-h ()
  (setq-local show-trailing-whitespace nil)
  (visual-line-mode))

(add-hook 'ebib-entry-mode-hook #'aar/ebib-entry-mode-h)

;;;;;; Keybindings
;; <leader> keybinding to launch ebib
(define-key aar/leader-apps-map (kbd "b") #'ebib)

;; <localleader> bindings for ebib-index-mode
(with-eval-after-load 'ebib
  (define-prefix-command 'aar/localleader-ebib-index-mode-map)
  (evil-define-key '(normal visual motion) ebib-index-mode-map
    (kbd aar/localleader-key) 'aar/localleader-ebib-index-mode-map)
  (evil-define-key '(insert emacs) ebib-index-mode-map
    (kbd aar/localleader-alt-key) 'aar/localleader-ebib-index-mode-map)

  ;; <localleader> jump to entry
  (define-key aar/localleader-ebib-index-mode-map
    (kbd "j") #'ebib-jump-to-entry)

  ;; Remap save-buffer ebib-save-current-database in ebib-index-mode-map. This is
  ;; what C-x C-s does in ebib-index-mode-map anyway.
  (define-key ebib-index-mode-map
    [remap save-buffer] #'ebib-save-current-database))

(provide 'aar-bib)
;;; aar-bib.el ends here
