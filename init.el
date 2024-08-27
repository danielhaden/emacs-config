(require 'package)    ; initialize package sources

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on  non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package  command-log-mode)

(use-package ivy
  :diminish                                       ;; diminish: don't show this package in mode line
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
:config
(ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
	   ("C-." . counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package flycheck)

(use-package yasnippet
:config (yas-global-mode))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package lsp-treemacs)

(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)

(use-package company)

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general)
(general-create-definer drh/leader-keys
  :prefix "C-;")

(drh/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme"))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(drh/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package key-chord
  :ensure nil
  :load-path "~/.emacs.d/packages/key-chord.el"
  :config
  (setq key-chord-one-key-delay 0.2)
  (setq key-chord-two-keys-delay 0.07))
(key-chord-mode 1)
(key-chord-define-global "xf" 'counsel-find-file)
(key-chord-define-global "df" 'ivy-switch-buffer)
(key-chord-define-global "xs" 'save-buffer)
(key-chord-define-global "aj" 'other-window)
(key-chord-define-global "au" 'delete-other-windows)
(key-chord-define-global "ai" 'split-window-below)
(key-chord-define-global "ao" 'split-window-right)
(key-chord-define-global "we" 'eval-region)
(key-chord-define-global "ci" 'org-roam-node-insert)
(key-chord-define-global "cf" 'org-roam-node-find)
(key-chord-define-global "qw" 'counsel-switch-buffer)
(key-chord-define-global "eb" 'eval-buffer)
(key-chord-define-global "fn" 'make-frame-command)
(key-chord-define-global "fo" 'other-frame)
(key-chord-define-global "zh" 'ellama-chat)
(key-chord-define-global "tt" 'org-babel-tangle)
(key-chord-define-global "sh" 'shell)
(key-chord-define-global "lq" 'drh/back-8-lines)
(key-chord-define-global "lw" 'drh/back-32-lines)
(key-chord-define-global "le" 'drh/back-128-lines)
(key-chord-define-global ";q" 'drh/jump-8-lines)
(key-chord-define-global ";w" 'drh/jump-32-lines)
(key-chord-define-global ";e" 'drh/jump-128-lines)
(key-chord-define-global ";;" 'goto-line)
(key-chord-define-global "sk" 'kill-current-buffer)
(key-chord-define-global "]d" 'org-agenda)

(defun drh/jump-multiple-lines-forward (n)
  (forward-line n))

(defun drh/back-8-lines ()
  (interactive)
  (drh/jump-multiple-lines-forward -8))

(defun drh/back-32-lines ()
  (interactive)
  (drh/jump-multiple-lines-forward -32))

(defun drh/back-128-lines ()
  (interactive)
  (drh/jump-multiple-lines-forward -128))

(defun drh/jump-8-lines ()
  (interactive)
  (drh/jump-multiple-lines-forward 8))

(defun drh/jump-32-lines ()
  (interactive)
  (drh/jump-multiple-lines-forward 32))

(defun drh/jump-128-lines ()
  (interactive)
  (drh/jump-multiple-lines-forward 128))

(use-package racket-mode
  :ensure t)

(use-package geiser-racket
:ensure t
:config
(setq geiser-racket-binary "/Applications/Racket v8.12/bin/racket"))

(use-package pollen-mode)

(use-package tuareg
    :ensure t
    :mode (("\\.ocamlinit\\'" . taureg-mode)))

  ;; Major mode for Dune project files
  (use-package dune
    :ensure t)

;; Merlin provides advanced IDE features
(use-package merlin
  :ensure t
  :config
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  (add-hook 'merlin-mode-hook #'company-mode)
  ;; we're using flycheck instead
  (setq merlin-error-after-save nil))

(defun drh/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Cardo"))))
   '(fixed-pitch ((t (:family "Fira Code Retina")))))


  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.4)
                  (org-level-2 . 1.3)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.05)
                  (org-level-6 . 1.05)
                  (org-level-7 . 1.05)
                  (org-level-8 . 1.05)))
    (set-face-attribute (car face) nil :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil    :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil    :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch)
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch))

(defun drh/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package ellama
  :init
  ;; setup key bindings
  (setopt ellama-keymap-prefix "C-c e")
  ;; language you want ellama to translate to
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   ;; this model should be pulled to use it
	   ;; value should be the same as you print in terminal during pull
	   :chat-model "llama3.1:latest"))

  ;; Predefined llm providers for interactive switching.
  ;; You shouldn't add ollama providers here - it can be selected interactively
  ;; without it. It is just example.
  (setopt ellama-providers
		    '(("zephyr" . (make-llm-ollama
				   :chat-model "zephyr:7b-beta-q6_K"
				   :embedding-model "zephyr:7b-beta-q6_K"))
		      ("mistral" . (make-llm-ollama
				    :chat-model "mistral:7b-instruct-v0.2-q6_K"
				    :embedding-model "mistral:7b-instruct-v0.2-q6_K"))
		      ("mixtral" . (make-llm-ollama
				    :chat-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"
				    :embedding-model "mixtral:8x7b-instruct-v0.1-q3_K_M-4k"))))
  ;; Naming new sessions with llm
  (setopt ellama-naming-provider
	  (make-llm-ollama
	   :chat-model "llama3.1:latest"
	   :embedding-model "nomic-embed-text"
	   :default-chat-non-standard-params '(("stop" . ("\n")))))
  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
  ;; Translation llm provider
  (setopt ellama-translation-provider (make-llm-ollama
				       :chat-model "phi3:14b-medium-128k-instruct-q6_K"
				       :embedding-model "nomic-embed-text")))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . drh/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
     (setq org-hide-emphasis-markers t)  ;; hides markup elements like * and /
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/zettelkasten/tasks.org"
          "~/zettelkasten/habits.org"
          "~/zettelkasten/archive.org"))

     (setq org-archive-location '( "~/zettelkasten/tasks.org::"))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

     ;; See here for org toggle commands
  (setq org-todo-keywords
    '((sequence "TODO" "ACTIVE" "ON HOLD" "DONE" "CANCELLED" "ARCHIVE")))

  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (drh/org-font-setup))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/zettelkasten")))

(require 'org-tempo)  ;; Needed as of Org 9.2

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(org-babel-do-load-languages
'org-babel-load-languages
'((python . t)
  (shell . t)
  (emacs-lisp . t)))

(setq org-babel-python-command "python3")

(setq python-shell-interpreter "~/anaconda3/bin/python")

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package projectile)

(add-to-list 'exec-path "/usr/local/sbin")
(add-to-list 'exec-path "/usr/local/bin")

(defvar drh/default-font-size 150)
  (defvar drh/default-variable-font-size 150)

  (set-face-attribute 'default nil  :height drh/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :height drh/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :height drh/default-variable-font-size :weight 'regular)

(defvar drh/frame-transparency '(94 . 94))

(set-frame-parameter (selected-frame) 'alpha drh/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,drh/frame-transparency))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package all-the-icons
  :if (display-graphic-p))

(setq inhibit-startup-message t)
(scroll-bar-mode -1)            ; Disable visible scrollbar
(tool-bar-mode -1)              ; Disable the toolbar
(tooltip-mode -1)               ; Disable tooltips

(column-number-mode)                       ;; display column number at point
(global-display-line-numbers-mode t)  ;; display line numbers by default

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-fringe-mode 10)

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package almost-mono-themes
:config
;; (load-theme 'almost-mono-black t)
;; (load-theme 'almost-mono-gray t)
;; (load-theme 'almost-mono-white t)
(load-theme 'almost-mono-cream t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
