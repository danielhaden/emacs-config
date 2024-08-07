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
  (setq key-chord-two-keys-delay 0.05))

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

(use-package racket-mode
  :ensure t)

(use-package geiser-racket
:ensure t
:config
(setq geiser-racket-binary "/Applications/Racket v8.12/bin/racket"))

(use-package pollen-mode)

(defun drh/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
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

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
        '("~/projects/zettelkasten/tasks.org"
	  "~/projects/active-projects/new-project-under-Dan.org"
          "~/projects/zettelkasten/habits.org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

  ;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Projects/Code/emacs-from-scratch/OrgFiles/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree "~/Projects/Code/emacs-from-scratch/OrgFiles/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline "~/Projects/Code/emacs-from-scratch/OrgFiles/Metrics.org" "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

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

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects/Code")
    (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; (use-package exwm
;;   :config
;;   (setq exwm-workspace-number 5) ;; Set the default number of workspaces
;;   (exwm-enable))

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
