;;; aar-files.el --- -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; <leader> file map
(define-prefix-command 'aar/leader-file-map)
(define-key aar/leader-map (kbd "f") 'aar/leader-file-map)
(which-key-add-keymap-based-replacements aar/leader-map "f" "file")

;; Functions
;; Insert file names from minibuffer
;;;###autoload
(defun aar/insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.
Prefixed with \\[universal-argument], expand the file name to its fully
canocalized path.  See `expand-file-name'.

Prefixed with \\[negative-argument], use relative path to file name from current
directory, `default-directory'.  See `file-relative-name'.

The default with no prefix is to insert the file name exactly as it appears in
the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
         (insert (file-relative-name filename)))
        ((not (null args))
         (insert (expand-file-name filename)))
        (t
         (insert filename))))

;; Find file in config
;;;###autoload
(defun aar/find-file-in-config ()
  "Find files in configuration directory using project.el"
  (interactive)
  (if (not (featurep 'project))
      (require 'project))
  (let* ((pr (project--find-in-directory (file-truename
                                          user-emacs-directory)))
         (dirs (list (project-root pr))))
    (project-find-file-in (thing-at-point 'filename) dirs pr)))

;; Copy file path
;;;###autoload
(defun aar/yank-buffer-file-path ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name
                        (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find file path in current buffer")))

;; Copy path to directory containing file
;;;###autoload
(defun aar/yank-buffer-dir-path ()
  "Copy the current buffer's directory path to the kill ring."
  (interactive)
  (if-let (dir-name (or default-directory
                        (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name dir-name)))
    (error "Couldn't find directory path in current buffer")))

;; Copy file name
;; TODO: adjust for final child node of a directory path.
;;;###autoload
(defun aar/yank-buffer-file-name ()
  "Copy the current buffer's non-directory name to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name
                        (bound-and-true-p list-buffers-directory)))
      (message (kill-new (file-name-nondirectory
                          (abbreviate-file-name filename))))
    (error "Couldn't find file name in current buffer")))

;; Basic keybindings
(define-key aar/leader-map (kbd ".") #'find-file)
(define-key aar/leader-file-map (kbd "f")   #'find-file)
(define-key aar/leader-file-map (kbd "n")   #'rename-file)
(define-key aar/leader-file-map (kbd "s")   #'save-buffer)
(define-key aar/leader-file-map (kbd "d")   #'dired)
(define-key aar/leader-file-map (kbd "j")   #'dired-jump)
(define-key aar/leader-file-map (kbd "i")   #'aar/insert-file-name)
(define-key aar/leader-file-map (kbd "p")   #'aar/find-file-in-config)
(define-key aar/leader-file-map (kbd "y y") #'aar/yank-buffer-file-path)
(define-key aar/leader-file-map (kbd "y d") #'aar/yank-buffer-dir-path)
(define-key aar/leader-file-map (kbd "y n") #'aar/yank-buffer-file-name)

;; dired
(setq dired-listing-switches "-agho --group-directories-first")
(add-hook 'dired-mode-hook #'display-line-numbers-mode)
(add-hook 'dired-mode-hook #'display-fill-column-indicator-mode)

(define-key aar/leader-file-map (kbd "d") #'dired)
(define-key aar/leader-file-map (kbd "j") #'dired-jump)

(with-eval-after-load 'wdired
  (define-key wdired-mode-map [remap save-buffer] #'wdired-finish-edit))

;; dired-single and dired-x
(straight-use-package 'dired-single)
(with-eval-after-load 'dired
  (require 'dired-x)
  (define-key dired-mode-map [remap dired-find-file] #'dired-single-buffer)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    #'dired-single-buffer-mouse)
  (define-key dired-mode-map [remap dired-up-directory]
    #'dired-single-up-directory)
  (evil-define-key '(normal visual motion) dired-mode-map
    (kbd "h") #'dired-single-up-directory
    (kbd "l") #'dired-single-buffer))

;; Open certain files in external processes.
;;;###autoload
(defun aar/xdg-open (filename)
  "Open the file FILENAME in exter-externallynal application using `xdg-open'."
  (interactive "fFilename: ")
  (let ((process-connection-type nil))
    ;; (start-process "" nil "xdg-open" (expand-file-name filename))
    (call-process "xdg-open" nil 0 nil (expand-file-name filename))))

(defvar aar/xdg-open-filetypes '("\\.pdf\\'" "\\.docx?\\'"))

;;;###autoload
(defun aar/find-file-auto (orig-fun &rest args)
  "Advice for `find-file': if file has extension in `aar/xdg-open-filetypes',
then open in external application using `aar/xdg-open'. Otherwise, go with
default behavior."
  (let ((filename (car args)))
    (if (cl-find-if
         (lambda (regexp) (string-match regexp filename))
         aar/xdg-open-filetypes)
        (aar/xdg-open filename)
      (apply orig-fun args))))

(advice-add 'find-file :around #'aar/find-file-auto)

;; all-the-icons-dired
(straight-use-package 'all-the-icons-dired)
(add-hook 'dired-mode-hook #'all-the-icons-dired-mode)

;; recentf
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-save-file (aar/expand-cache-file-name "recentf-save.el"))
(setq recentf-max-saved-items 50)
(setq recentf-max-menu-items 15)
(setq recentf-auto-cleanup (if (daemonp) 300))
(add-to-list 'recentf-exclude aar/cache-dir)
(add-to-list 'recentf-exclude aar/etc-dir)
(add-hook 'find-file-hook #'recentf-save-list)
(add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")

(define-key aar/leader-file-map (kbd "r") #'recentf-open-files)

(provide 'aar-files)
;;; aar-files.el ends here
