;;; aar-bib.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;; bibtex-mode
(setq bibtex-dialect 'biblatex)
(setq bibtex-align-at-equal-sign t)
(setq bibtex-text-indentation 20)

(add-to-list 'auto-mode-alist '("\\.bibtex\\'" . bibtex-mode))

;; bibtex-mode hooks
(defun aar/bibtex-mode-h ()
  (display-line-numbers-mode)
  (visual-line-mode))

(add-hook 'bibtex-mode-hook #'aar/bibtex-mode-h)

;; bibtex-autokey stuff
(setq bibtex-autokey-year-length 4)
(setq bibtex-autokey-names 1)
(setq bibtex-autokey-titlewords 3)
(setq bibtex-autokey-titlewords-stretch 0)
(setq bibtex-autokey-titleword-length nil)
(setq bibtex-autokey-titleword-ignore '("A"       "a"
                                        "An"      "an"
                                        "The"     "the"
                                        "Above"   "above"
                                        "About"   "about"
                                        "Across"  "across"
                                        "Against" "against"
                                        "Along"   "along"
                                        "Among"   "among"
                                        "Around"  "around"
                                        "At"      "at"
                                        "Before"  "before"
                                        "Behind"  "behind"
                                        "Below"   "below"
                                        "Beneath" "beneath"
                                        "Beside"  "beside"
                                        "Between" "between"
                                        "Beyond"  "beyond"
                                        "By"      "by"
                                        "Down"    "down"
                                        "During"  "during"
                                        "Except"  "except"
                                        "For"     "for"
                                        "From"    "from"
                                        "In"      "in"
                                        "Inside"  "inside"
                                        "Into"    "into"
                                        "Like"    "like"
                                        "Near"    "near"
                                        "Of"      "of"
                                        "Off"     "off"
                                        "On"      "on"
                                        "Onto"    "onto"
                                        "Since"   "since"
                                        "To"      "to"
                                        "Toward"  "toward"
                                        "Through" "through"
                                        "Under"   "under"
                                        "Until"   "until"
                                        "Up"      "up"
                                        "Upon"    "upon"
                                        "With"    "with"
                                        "Within"  "within"
                                        "Without" "without"
                                        "And"     "and"
                                        "But"     "but"
                                        "For"     "for"
                                        "Nor"     "nor"
                                        "Or"      "or"
                                        "So"      "so"
                                        "Yet"     "yet"))
(setq bibtex-autokey-titleword-case-convert-function #'s-upper-camel-case)
(setq bibtex-autokey-titleword-separator "")

(defun aar/bibtex-generate-autokey ()
  "Generate a citation key for a BibTeX entry using the author, year and title.
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

(defun aar/insert-bibtex-autokey ()
  "Insert autogenerated citation key for BibTeX entry at point."
  (interactive)
  (insert (aar/bibtex-generate-autokey)))

(define-prefix-command 'aar/localleader-bibtex-mode-map)
(evil-define-key '(normal visual motion) bibtex-mode-map
  (kbd aar/localleader-key) 'aar/localleader-bibtex-mode-map)
(evil-define-key '(insert emacs) bibtex-mode-map
  (kbd aar/localleader-alt-key) 'aar/localleader-bibtex-mode-map)

(define-key aar/localleader-bibtex-mode-map
  (kbd "k") #'aar/insert-bibtex-autokey)

;;; ebib
(aar/maybe-install-package 'ebib)
(setq ebib-default-directory "~/Dropbox/research/")
(setq ebib-bibtex-dialect 'biblatex)
;; Open files in pdf-tools
(setq ebib-file-associations nil)
(setq ebib-bib-search-dirs '("~/Dropbox/research/"
                             "~/Dropbox/research/ongoing/"))
(setq ebib-preload-bib-files '("bibliography.bib"))

(setq ebib-truncate-file-names nil)
(setq ebib-file-search-dirs '("~/Dropbox/research/readings/"))

;; Some layout tweaks
(setq ebib-layout 'custom)
(setq ebib-width 0.5)

;; Reading list
;; Needs to load my org settings first, since org-directory is defined there.
(with-eval-after-load 'aar-org
  (setq ebib-reading-list-file (concat org-directory "readinglist.org")))

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
    (kbd ".") #'ebib-jump-to-entry)

  ;; <localleader> merge bibtex file
  (define-key aar/localleader-ebib-index-mode-map
    (kbd "m") #'ebib-merge-bibtex-file)

  ;; Remap save-buffer ebib-save-current-database in ebib-index-mode-map. This is
  ;; what C-x C-s does in ebib-index-mode-map anyway.
  (define-key ebib-index-mode-map
    [remap save-buffer] #'ebib-save-current-database))

(provide 'aar-bib)
;;; aar-bib.el ends here