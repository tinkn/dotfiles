;; NOTE: init.el is now generated from Emacs.org.  Please edit that file
;;       in Emacs and init.el will be generated automatically!

;; You will most likely need to adjust this font size for your system!
(defvar efs/default-font-size 120)
(defvar efs/default-variable-font-size 150)

;; Make frame transparency overridable
;;(defvar efs/frame-transparency '(90 . 90))

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe))

(defun my-minibuffer-setup-hook () (setq gc-cons-threshold (* 640 1024 1024)))

(defun my-minibuffer-exit-hook () (setq gc-cons-threshold (* 32 1024 1024)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)

(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

(setq org-src-fontify-natively t)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally) ; side-by-side (default)

(use-package company
  :ensure t
  :config
  ;; Enable company-mode globally
  (global-company-mode 1))
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 5)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
;;(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

;; Set frame transparency
;;(set-frame-parameter (selected-frame) 'alpha efs/frame-transparency)
;;(add-to-list 'default-frame-alist `(alpha . ,efs/frame-transparency))
;;(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :font "Fira Code Retina" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Inconsolata" :height efs/default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :family "Roboto" :height efs/default-variable-font-size :weight 'regular)

(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(use-package command-log-mode)

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 10)))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
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

;;(use-package ivy-rich
;;  :init
;;  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))


(defun efs/org-font-setup ()
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
    (set-face-attribute (car face) nil :font "FiraSans-Regular" :weight 'regular :height (cdr face)))

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

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  ;; :pin org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        '("~/org/work.org"
          "~/org/personal.org"
          "~/org/inbox.org"))
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "IN-PROGRESS(i)" "REVIEW(v)" "|" "COMPLETED(c!)" "CANCELLED(k!)")))
  (setq org-refile-targets
    '(("archive.org" :maxlevel . 1)
      ("personal.org" :maxlevel . 1)
      ("work.org" :maxlevel . 1)))
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
    ("w" "Work Tasks" tags-todo "+work-email")
    ("s" "Workflow Status"
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
             (org-agenda-files org-agenda-files))))))
  
   ;; Keep A/B/C for input, but display as flags
  (setq org-priority-faces
        '((?A . (:foreground "red" :weight bold))
          (?B . (:foreground "orange"))
          (?C . (:foreground "gray"))))
  
  ;; Display priorities as flags in agenda
  (setq org-agenda-fontify-priorities t) 
  (efs/org-font-setup))

(setq org-default-notes-file "~/org/inbox.org")

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c v") 'dirvish)
(global-set-key (kbd "C-c e") 'mu4e)

;; Define a prefix map under C-x x
(define-prefix-command 'my-xref-map)
(define-key global-map (kbd "C-c x") 'my-xref-map)

;; Bind individual xref commands to single letters
(define-key my-xref-map (kbd "d") #'xref-find-definitions)
(define-key my-xref-map (kbd "r") #'xref-find-references)
(define-key my-xref-map (kbd "b") #'xref-pop-marker-stack)
(define-key my-xref-map (kbd "a") #'xref-find-apropos)

(setq org-capture-templates
      '(
        ("g" "Personal/General" entry
         (file+headline "~/org/personal.org" "General")
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n ")

        ("h" "Personal/Health+Fitness" entry
         (file+headline "~/org/personal.org" "Health & Fitness")
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n ")

        ("w" "Work Task" entry
         (file+headline "~/org/work.org" "Cosdata") 
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n ")

        ("i" "Inbox (for later triage)" entry
         (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n ")

        ("e" "Email follow-up" entry
         (file+headline "~/org/inbox.org" "Email Tasks")
         "* TODO Follow up with %:fromname on %:subject\n%a\n")))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;;(defun efs/org-mode-visual-fill ()
;;  (setq visual-fill-column-width 100
;;        visual-fill-column-center-text t)
;;  (visual-fill-column-mode 1))

;;(use-package visual-fill-column
;;  :hook (org-mode . efs/org-mode-visual-fill))
(setopt python-shell-interpreter "/home/nithin/my-python/bin/python"
	 python-shell-interpreter-args "-i" )

(org-babel-do-load-languages
  'org-babel-load-languages
    '((python . t)
     (d2 . t)))

(require 'ob-rust)

;; Add cargo as a rust execution path for org babel
;; (setq org-babel-rust-command "cargo script")

(push '("conf-unix" . conf-unix) org-src-lang-modes)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))


(use-package eglot
  :hook ((rust-mode nix-mode) . eglot-ensure)
  :config (add-to-list 'eglot-server-programs
                       `(rust-mode . ("rust-analyzer" :initializationOptions
                                     ( :procMacro (:enable t)
                                       :cargo ( :buildScripts (:enable t)
                                                :features "all"))))))

(use-package flycheck :ensure)


(use-package treemacs-evil)
(use-package treemacs-projectile)



(use-package pyvenv
  :config
  (pyvenv-mode 1))


(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/")
    (setq projectile-project-search-path '("~/")))
  (setq projectile-switch-project-action #'projectile-dired))

(add-to-list 'load-path "~/")
(require 'projectile)
(projectile-global-mode) ;; to enable in all buffers


(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
(use-package forge)

(use-package magit
  :ensure t)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package term
  :config
  (setq explicit-shell-file-name "bash") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)


  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
    )

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("56044c5a9cc45b6ec45c0eb28df100d3f0a576f18eef33ff8ff5d32bac2d9700"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "5f19cb23200e0ac301d42b880641128833067d341d22344806cdad48e6ec62f6"
     "6c531d6c3dbc344045af7829a3a20a09929e6c41d7a7278963f7d3215139f6a7"
     "c4063322b5011829f7fdd7509979b5823e8eea2abf1fe5572ec4b7af1dd78519"
     default))
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((mu4e-thread-folding :vc-backend Git :url
			  "https://github.com/rougier/mu4e-thread-folding"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mu4e-thread-folding-child-face ((t (:extend t :background "#2a2a2a"))))
 '(mu4e-thread-folding-line-face ((t (:foreground "#555555"))))
 '(mu4e-thread-folding-root-folded-face ((t (:extend t :background "#3a2a1a"))))
 '(mu4e-thread-folding-root-unfolded-face ((t (:extend t :background "#1a3a3a"))))
 '(mu4e-thread-folding-single-face ((t (:extend t :background "#2a2a30")))))

(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-<tab>") 'switch-to-last-buffer)

(desktop-save-mode t)
;;(tab-bar-mode nil)

(setq show-paren-delay 0)
(show-paren-mode 1)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional, but helpful if you use other packages that integrate with evil
  :config
  (evil-mode 1))

(global-set-key (kbd "C-c a") 'org-agenda)

(require 'general)

(use-package general
  :ensure t
  :config
  (general-evil-setup t)
  (general-create-definer my-leader-def
    :keymaps '(normal insert visual emacs)
    :prefix ","
    :global-prefix "C-,"
    :prefix-command 'my-leader-prefix
    :prefix-map 'my-leader-map
    :prefix-doc '(("o" . "Contains org commands")
                  ("p" . "Contains projectile commands")
                  ("b" . "Contains babel commands")
                  ("c" . "Compilation commands")
                  ("s" . "Search and navigate commands")
                  ("w" . "Window management commands")
                  ("m" . "Magit commands")
                  ("l" . "Language and tools commands")
                  ("x" . "Cross-reference commands")
                  ("n" . "Navigation commands")
                  ("h" . "Code folding commands")
                  ("d" . "Dired commands")
                  ("t" . "Basic commands")))

  (my-leader-def
    "t"  '(:ignore t :which-key "Buffer cmds.")
    "t b" 'switch-to-buffer
    "t s" 'save-buffer
    "t f" 'find-file
    "t k" 'kill-buffer

    "b"  '(:ignore b :which-key "Babel cmds.")
    "b t" 'org-babel-tangle
    "b d" 'org-babel-detangle
    "b s" 'org-babel-execute-src-block
    "c" 'compile
    "s" 'swiper 

    "o"  '(:ignore o :which-key "Org cmds.")
    "o l" 'org-cycle
    "o g" 'org-global-cycle
    "o h" 'org-previous-visible-heading
    "o p" 'org-previous-block
    "o n" 'org-next-visible-heading
    "o N" 'org-next-block
    "o i" 'org-toggle-inline-images
    "o o" 'org-open-at-point
    "o m" 'org-mark-ring-goto
    "o e" 'org-export-dispatch
    "o a" 'org-agenda
    "o t" 'org-todo
    "o s" 'org-schedule
    "o d" 'org-deadline
    "o ," 'org-priority
    "o c" 'org-capture


    "r"  '(:ignore o :which-key "Roam cmds.")
    "r f" 'org-roam-node-find
    "r i" 'org-roam-node-insert

    "w"  '(:ignore w :which-key "Window mgmt.")
    "w o" 'other-window
    "w d" 'delete-window
    "w r" 'delete-other-windows
    "w h" 'split-window-right
    "w v" 'split-window-below
    
    "p"  '(:ignore p :which-key "Projectile")
    "p p" 'projectile-switch-project
    "p c" 'counsel-projectile
    "p r" 'projectile-ripgrep
    "p i" 'projectile-ibuffer
    "p k" 'projectile-kill-buffers
    "p b" 'projectile-switch-to-buffer
    
    "m" 'magit
    "d" 'dired
    "v" 'dirvish
    
    "l"  '(:ignore l :which-key "Lang & tools")
    "l t" 'treemacs
    "l s" 'ispell-word
    "l f" 'flyspell-mode
    "l b" 'flyspell-buffer
    "l e" 'eglot-format-buffer

    "x"  '(:ignore x :which-key "Cross reference")
    "x f" 'xref-find-references
    "x g" 'xref-quit-and-goto-xref
    "x d" 'flymake-show-project-diagnostics
    
    "n"  '(:ignore n :which-key "Navigation")
    "n i" 'imenu
    "n n" 'my-next-function
    "n p" 'my-previous-function

    "h"  '(:ignore h :which-key "Code folding")
    "h t" 'hs-toggle-hiding
    "h e" 'hs-show-block
    "h s" 'hs-show-all
    "h c" 'hs-hide-block
    "h h" 'hs-hide-all))

(with-eval-after-load 'evil
  (general-add-hook 'after-init-hook
                    (lambda (&rest _)
                      (when-let ((messages-buffer (get-buffer "*Messages*")))
                        (with-current-buffer messages-buffer
                          (evil-normalize-keymaps))))
                    nil
                    nil
                    t))

(use-package htmlize)
(require 'htmlize)
;;This is the default setting. It highlights the code according to the current Emacs theme you are using. It directly applies color to the code with inline styles, e.g., int.
;;The problem is that the highlight theme depends on the Emacs theme. If you use a dark theme in your Emacs but a light theme (usually we like light themed web pages) web pages, the exported code are hardly illegible due to the light font color, or vice versa.
;;(setq org-html-htmlize-output-type 'inline-css)

(setq org-html-checkbox-type 'html)

;;This configuration disables highlighting by htmlize.
;;(setq org-html-htmlize-output-type nil)


;;This is my preferred way. If you use my org.css, then set this option in your init file and you are all set.
;;This is similar to the first option, instead of using inline styles, this will assign classes to each component of the code, e.g., <span class="org-type">int</span>, and you could create your own stylesheet for .org-type.
;;To obtain a list of all supported org classes, run M-x org-html-htmlize-generate-css. This will create a buffer containing all the available org style class names in the current Emacs session (refer to src/css/htmlize.css for an example).
(setq org-html-htmlize-output-type 'css)

;; (setq org-html-htmlize-font-prefix "") ;; default
(setq org-html-htmlize-font-prefix "org-")

;; to instruct org-mode to embed all the css from my stylesheet into a single HTML file, rather than including a link to it as it does by default

(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/.config/my-css-themes/leuven.css" path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)


(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/org")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; rustic is an extension of rust-mode which adds a number of useful features (see the its github readme) to it. It is the core of the setup and you can use just it without any other Emacs packages (and without rust-analyzer) if you just want code highlighting, compilation and cargo commands bound to emacs shortcuts, and a few other features.

(use-package rustic
  :ensure t
  :config
  ;; uncomment for less flashiness
 ;;  (setq lsp-eldoc-hook nil)
 ;;  (setq lsp-enable-symbol-highlighting nil)
  (setq rustic-lsp-client 'eglot)  ;; You can also use lsp-mode if you prefer
  (setq lsp-signature-auto-activate nil)
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'company-mode))

(defun hs-rustic-mode-hook ()
  "Custom hook for `rustic-mode'."
  (hs-minor-mode 1)
  ;; Other customizations...
  )

(add-hook 'rustic-mode-hook 'hs-rustic-mode-hook)

(defun my-next-function ()
  "Move cursor to the next function definition."
  (interactive)
  (re-search-forward "fn " nil t)
  (beginning-of-line))

(defun my-previous-function ()
  "Move cursor to the previous function definition."
  (interactive)
  (re-search-backward "fn " nil t)
  (beginning-of-line))

;;(setq dired-dwim-target t)

(dirvish-override-dired-mode)

;; Enable Flyspell for Org mode
(add-hook 'org-mode-hook 'flyspell-mode)

;; Enable Flyspell for Text mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; Optionally, you might want to enable Flyspell for programming comments and strings
(defun my-prog-mode-flyspell-setup ()
  (flyspell-prog-mode))

(add-hook 'prog-mode-hook 'my-prog-mode-flyspell-setup)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "en_US")  ;; Set default dictionary to "en_US"


(setq org-link-frame-setup '((file . find-file)))


(use-package evil-colemak-basics
  :after evil
  :init
  (setq evil-colemak-basics-layout-mod 'mod-dh)
  :config
  (global-evil-colemak-basics-mode))

(evil-set-initial-state 'xref--xref-buffer-mode 'emacs)
;; Add eglot's xref backend
(add-hook 'xref-backend-functions #'eglot-xref-backend nil t)

(use-package evil-collection
  :ensure t
  :after (evil evil-colemak-basics)
  :config
  (evil-collection-init))

;; Mu4e identity first (before use-package)
(setq mail-user-agent 'mu4e-user-agent
      user-full-name "Nithin Mani"
      user-mail-address "nithin@cosdata.io")

(use-package mu4e
  :ensure nil
  :config
  ;; Basic mu4e settings
  (setq mu4e-change-filenames-when-moving t)
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  
  ;; Mail directory - matches your mbsync MaildirStore Path
  (setq mu4e-maildir "~/mail/zoho/")
  
  ;; Folder configuration - matches your mbsync setup
  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")  
  (setq mu4e-trash-folder  "/Trash")
  (setq mu4e-refile-folder "/INBOX")

  ;; Prefer HTML when available
  (setq mu4e-view-prefer-html t)
  ;; Use built-in HTML renderer instead of w3m
  (setq mu4e-html2text-command 'mu4e-shr2text)

  ;; Enable images
  (setq mu4e-view-show-images t)
  (setq mu4e-view-image-max-width 800)
  
  ;;(setq mu4e-compose-keep-self-cc nil)

  (setq mu4e-compose-format-flowed t)
  (setq fill-flowed-encode-column 9999)
  (setq message-fill-column 9999)

  ;; Add this hook inside the mu4e config too
  (add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (auto-fill-mode -1)
            (setq fill-column 9999)))
  (setq mu4e-compose-dont-reply-to-self t)

  ;;(setq fill-flowed-encode-column 998)
  (setq message-kill-buffer-on-exit t)   ; You already have this
  (setq mu4e-compose-in-new-frame nil)   ; Don't use new frames for compose
  ;; Configure shr for better rendering
  (setq shr-color-visible-luminance-min 60)
  (setq shr-use-colors nil)
  (setq shr-width 80)
  ;; Maildir shortcuts
  (setq mu4e-maildir-shortcuts
      '(("/INBOX"    . ?i)
        ("/Sent"     . ?s)
        ("/Trash"    . ?t)
        ("/Drafts"   . ?d)))

  (setq mu4e-headers-show-threads t) ; Enable threading
  (add-to-list 'mu4e-headers-actions
             '("Capture to Org" . mu4e-action-capture-message) t)
  (add-to-list 'mu4e-view-actions
             '("Capture to Org" . mu4e-action-capture-message) t)

  (setq mu4e-headers-fields
      '((:human-date . 12)
        (:flags      . 6)
        (:from       . 22)
        (:subject    . nil)))
  ;; SMTP configuration for sending mail
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtppro.zoho.in"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls
        smtpmail-auth-credentials "~/.authinfo.gpg")

  ;; Additional useful settings
  (setq mu4e-compose-signature-auto-include nil
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-attachment-dir "~/Downloads"
        mu4e-compose-format-flowed t
        message-kill-buffer-on-exit t)
  ;;(setq mu4e-compose-format-flowed nil) ; <-- Ensure this is off for HTML email
  )
 
;; put this near the end of your init after evil & mu4e are loaded
(with-eval-after-load 'mu4e
  (defun my/mu4e-view-nav-keys ()
    "Colemak-friendly next/prev message keys in mu4e‑view."
    (evil-define-key 'normal mu4e-view-mode-map
      (kbd "C-n") #'mu4e-view-headers-next
      (kbd "C-e") #'mu4e-view-headers-prev))
  (add-hook 'mu4e-view-mode-hook #'my/mu4e-view-nav-keys))

;; Install and require mu4e-thread-folding
(unless (package-installed-p 'mu4e-thread-folding)
  (package-vc-install "https://github.com/rougier/mu4e-thread-folding"))

(use-package mu4e-thread-folding
  :after mu4e
  :config
  ;; Enable visual indicators
  (setq mu4e-thread-folding-show-folding-lines t)

  (custom-set-faces
 ;; Child messages in threads
 '(mu4e-thread-folding-child-face ((t (:extend t :background "#2a2a2a"))))
 
 ;; Root messages of unfolded threads
 '(mu4e-thread-folding-root-unfolded-face ((t (:extend t :background "#1a3a3a"))))
 
 ;; Root messages of folded threads
 '(mu4e-thread-folding-root-folded-face ((t (:extend t :background "#3a2a1a"))))
 
 ;; Single emails - slightly different color to distinguish from threaded
 '(mu4e-thread-folding-single-face ((t (:extend t :background "#2a2a30"))))
 
 ;; Thread folding lines
 '(mu4e-thread-folding-line-face ((t (:foreground "#555555")))))

    ;; Hook function to set up keybindings after Evil loads
  (defun my/mu4e-thread-folding-setup ()
    "Set up thread folding with Evil-compatible keybindings."
    (mu4e-thread-folding-mode 1)
    
    ;; Use Evil's define-key to override the bindings
    (evil-define-key 'normal mu4e-headers-mode-map
      (kbd "f") #'mu4e-headers-toggle-at-point
      (kbd "zf") #'mu4e-headers-fold-at-point        ; Use z prefix like Vim
      (kbd "zF") #'mu4e-headers-fold-all
      (kbd "zo") #'mu4e-headers-unfold-at-point
      (kbd "zO") #'mu4e-headers-unfold-all)
    
    ;; Force Evil to recognize the new bindings
    (evil-normalize-keymaps))
  (add-hook 'mu4e-headers-mode-hook 'my/mu4e-thread-folding-setup))

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    (kbd "RET") 'org-open-at-point))



