(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("28a34dd458a554d34de989e251dc965e3dc72bace7d096cdc29249d60f395a82" default))
 '(ispell-dictionary nil)
 '(package-selected-packages
   '(company-pollen pollen-mode geiser-racket rainbow-delimiters paredit magit use-package racket-mode smartparens)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'smartparens-config)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; for Racket support
(use-package racket-mode
  :ensure t)

(require 'pollen-mode)

;; for git
(use-package magit
  :ensure t)

;; tool for handling tasks involving parentheses and delimiters
(use-package paredit
  :ensure t
  :config
  (add-hook 'racket-mode-hook #'enable-paredit-mode))

;; colorizes delimiters for easier reading
(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Make buffer names more readable (e.g. they look more like a filepath)
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward))

;; for toggling panes quicker
(global-set-key (kbd "M-o") 'other-window)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'zenburn t)

;; Disabled Things
;; --------------------------------------------------------
(tool-bar-mode -1)
