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

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(use-package interaction-log)

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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(treemacs-start-on-boot)

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm
  :config (helm-mode))
(use-package lsp-treemacs)

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

(use-package hydra )

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(drh/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(use-package org-noter
 :ensure t)

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
;; org, org-roam
(key-chord-define-global "cp" 'org-id-get-create)
(key-chord-define-global "ci" 'org-roam-node-insert)
(key-chord-define-global "cf" 'org-roam-node-find)
(key-chord-define-global "cl" 'org-roam-db-sync)
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
(key-chord-define-global "lt" 'leo-translate-word)
(key-chord-define-global "mf" 'make-frame)
(key-chord-define-global "jq" 'google-translate-at-point)

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

(use-package org-translate
:ensure t)

(use-package google-translate
  :ensure t
  :config
  (setq google-translate-default-source-language "ru")
  (setq google-translate-default-target-language "en"))

(use-package leo
  :ensure nil
  :load-path "~/.emacs.d/packages/emacs-leo"
  :config
  (setq leo-language "en"))

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

  (setq org-archive-location "~/zettelkasten/archive.org::")
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

(use-package org-drill
:ensure t)

(require 'org-tempo)  ;; Needed as of Org 9.2

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("jv" . "src java"))
(add-to-list 'org-structure-template-alist '("clj" . "src clojure"))

(org-babel-do-load-languages
'org-babel-load-languages
'((python . t)
  (shell . t)
  (emacs-lisp . t)
  (clojure .t )))

(setq org-babel-python-command "python3")

(setq python-shell-interpreter "/Users/dhadenx6/.pyenv/shims/python")

(use-package clojure-mode
:ensure t
:mode (("\\.clj\\'" . clojure-mode)
       ("\\.edn\\'" . clojure-mode))
:init
(add-hook 'clojure-mode-hook #'yas-minor-mode)         
(add-hook 'clojure-mode-hook #'linum-mode)             
(add-hook 'clojure-mode-hook #'subword-mode)           
(add-hook 'clojure-mode-hook #'smartparens-mode)       
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'eldoc-mode)             
(add-hook 'clojure-mode-hook #'idle-highlight-mode))

(use-package cider
  :ensure t
  :defer t
  :init (add-hook 'cider-mode-hook #'clj-refactor-mode)
  :diminish subword-mode
  :config
  (setq nrepl-log-messages t                  
        cider-repl-display-in-current-window t
        cider-repl-use-clojure-font-lock t    
        cider-prompt-save-file-on-load 'always-save
        cider-font-lock-dynamically '(macro core function var)
        nrepl-hide-special-buffers t            
        cider-overlays-use-font-lock t)         
  (cider-repl-toggle-pretty-printing))

(use-package cider-eval-sexp-fu
  :defer t)

(use-package clj-refactor
  :defer t
  :ensure t
  :diminish clj-refactor-mode
  :config (cljr-add-keybindings-with-prefix "C-c C-m"))

(use-package smartparens
  :defer t
  :ensure t
  :diminish smartparens-mode
  :init
  (setq sp-override-key-bindings
        '(("C-<right>" . nil)
          ("C-<left>" . nil)
          ("C-)" . sp-forward-slurp-sexp)
          ("M-<backspace>" . nil)
          ("C-(" . sp-forward-barf-sexp)))
  :config
  (use-package smartparens-config)
  (sp-use-smartparens-bindings)
  (sp--update-override-key-bindings)
  :commands (smartparens-mode show-smartparens-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package projectile
  :diminish projectile-mode
     :custom ((projectile-completion-system 'ivy)) 
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-discover-projects-in-directory "~/projects/" 1))

(use-package exec-path-from-shell
:load-path "~/.emacs.d/packages/exec-path-from-shell.el")

(add-to-list 'exec-path "/usr/local/sbin")
(add-to-list 'exec-path "/usr/plocal/bin")
(add-to-list 'exec-path "/opt/homebrew/bin")

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
