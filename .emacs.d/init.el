

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


;; Define your preferred sizes
(defvar my/default-font-size 120)
(defvar my/variable-font-size 140)

;; Main font for code and general UI
(set-face-attribute 'default nil
                    :font "FiraCode Nerd Font Mono"
                    :height my/default-font-size)

;; Fixed-pitch font (for code blocks inside org-mode, tables, etc.)
(set-face-attribute 'fixed-pitch nil
                    :font "FiraCode Nerd Font Mono"
                    :height my/default-font-size)

;; Variable-pitch font (for prose in org-mode)
(set-face-attribute 'variable-pitch nil
                    :font "Fira Sans"
                    :height my/variable-font-size
                    :weight 'regular)

;; Enable variable-pitch for Org mode
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

;; 1. Vertico: completion UI in minibuffer
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; 2. Orderless: fuzzy, space-separated matching
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; 3. Marginalia: annotations (like docs, file info) in minibuffer
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(require 'project)

;; ---------------------------
;; Project + Consult Integration
;; ---------------------------
(require 'project)

(use-package consult
  :ensure t)

(use-package consult-project-extra
  :ensure t)
;; Optional keybinding similar to Projectile
(global-set-key (kbd "C-c p f") 'consult-project-extra-find)
(global-set-key (kbd "C-c p o") 'consult-project-extra-find-other-window)

(global-set-key (kbd "C-c w w") 'other-window)
(global-set-key (kbd "C-c w d") 'delete-window)
(global-set-key (kbd "C-c w o") 'delete-other-windows)

(defun my/pick-font (candidates)
  "Return the first available font from CANDIDATES list, or nil if none found."
  (seq-find (lambda (f) (member f (font-family-list))) candidates))

(defun efs/org-font-setup ()
  "Configure fonts and bullets for Org mode."

  ;; Pick fonts safely
  (let* ((default-font     (my/pick-font '("FiraCode Nerd Font Mono" "Fira Mono" "Monospace")))
         (heading-font     (my/pick-font '("Fira Sans" "Sans Serif")))
         (variable-font    (my/pick-font '("Fira Sans" "Sans Serif")))
         (fixed-font       (my/pick-font '("FiraCode Nerd Font Mono" "Fira Mono" "Monospace"))))

    ;; Replace list hyphen with bullet •
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-+*]\\) "
                               (0 (prog1 ()
                                    (compose-region (match-beginning 1)
                                                    (match-end 1) "•"))))))

    ;; Set heading fonts (variable pitch)
    (when heading-font
      (dolist (face-height '((org-level-1 . 1.5)
                             (org-level-2 . 1.25)
                             (org-level-3 . 1.12)
                             (org-level-4 . 1.06)
                             (org-level-5 . 1.0)
                             (org-level-6 . 1.0)))
        (set-face-attribute (car face-height) nil
                            :font heading-font
                            :weight 'regular
                            :height (cdr face-height))))

    ;; Fixed-pitch faces (for code, tables, etc.)
    (when fixed-font
      (dolist (face '(org-block org-table org-formula
                      org-code org-verbatim org-special-keyword
                      org-meta-line org-checkbox
                      line-number line-number-current-line))
        (set-face-attribute face nil
                            :inherit 'fixed-pitch
                            :font fixed-font
                            :foreground 'unspecified)))

    ;; Variable-pitch for prose
    (when variable-font
      (set-face-attribute 'variable-pitch nil
                          :font variable-font
                          :weight 'regular
                          :height 1.1))))

;; Run after Org is loaded
(with-eval-after-load 'org
  (efs/org-font-setup))


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
  (setq org-agenda-fontify-priorities t) )

(setq org-default-notes-file "~/org/inbox.org")

(global-set-key (kbd "C-c t") 'org-capture)
(global-set-key (kbd "C-c v") 'dirvish)
(global-set-key (kbd "C-c e") 'notmuch)

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
     ))

(use-package ob-rust
  :ensure t
  :after org)

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


(use-package pyvenv
  :config
  (pyvenv-mode 1))


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



(defun switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-<tab>") 'switch-to-last-buffer)

(desktop-save-mode t)
;;(tab-bar-mode nil)

(setq show-paren-delay 0)
(show-paren-mode 1)

(global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c S") 'org-caldav-sync)
(global-set-key (kbd "C-c m") 'magit-status)

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
  :bind (("C-c r l" . org-roam-buffer-toggle)
         ("C-c r f" . org-roam-node-find)
         ("C-c r i" . org-roam-node-insert)
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

;; Add eglot's xref backend
(add-hook 'xref-backend-functions #'eglot-xref-backend nil t)

;; Or set notmuch as your default mail client
(setq mail-user-agent 'notmuch-user-agent)
(setq user-full-name "Nithin Mani"
      user-mail-address "nithin@cosdata.io")

(use-package notmuch
  :ensure t
  :init
  (setq notmuch-search-oldest-first nil)
  (setq notmuch-tree-oldest-first nil)
  :config
  ;; Basic notmuch settings
  (setq notmuch-show-all-tags-list t)
  (setq notmuch-hello-thousands-separator ",")
  
  (setq message-default-fcc nil)                 ;; Disable fallback prompts
  (setq notmuch-fcc-dirs "Sent/ -inbox -unread +sent")

  (setq notmuch-fcc-dirs
      '(("nithin@cosdata.io" . "nithin@cosdata.io/Sent/ -inbox -unread +sent +work")
        ("nithin@oodal.in" . "nithin@oodal.in/Sent/ -inbox -unread +sent +personal")))

  (setq notmuch-address-command "~/notmuch-address-complete")
  (setq notmuch-address-internal-completion nil)

  (require 'notmuch-address)
  (notmuch-address-setup)
  (setq notmuch-address-use-company t)

  (define-key notmuch-tree-mode-map (kbd "SPC") 'scroll-up-command)
  (define-key notmuch-tree-mode-map (kbd "DEL") 'scroll-down-command)
 
   ;; Search configuration
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i" :search-type tree)
          (:name "unread" :query "tag:unread" :key "u" :search-type tree)
          (:name "sent" :query "tag:sent" :key "s" :search-type tree)
          (:name "drafts" :query "tag:draft" :key "d" :search-type tree)
          (:name "cosdata inbox" :query "tag:inbox and (to:nithin@cosdata.io or from:nithin@cosdata.io)" :key "c" :search-type tree)
          (:name "oodal inbox" :query "tag:inbox and (to:nithin@oodal.in or from:nithin@oodal.in)" :key "o" :search-type tree)
          (:name "all mail" :query "*" :key "a" :search-type tree)))
  ;; Message display
  (setq notmuch-show-logo t)
  (setq notmuch-message-headers '("Subject" "To" "Cc" "Date"))
  (setq notmuch-message-headers-visible t)
  
  (setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header
      message-sendmail-envelope-from 'header)
  
  ;; HTML email handling
  (setq mm-text-html-renderer 'shr)
  (setq shr-color-visible-luminance-min 60)
  (setq shr-use-colors nil)
  (setq shr-width 120)

  (setq message-sendmail-envelope-from 'header)
  (setq notmuch-always-prompt-for-sender t)
  ;; Set up multiple identities - notmuch will auto-select for replies
  (setq notmuch-identities '("Nithin Mani <nithin@cosdata.io>"
                          "Nithin Mani <nithin@oodal.in>"))

  ;; For replies, notmuch will automatically choose the identity that matches
  ;; the To/Cc field of the original message
  (setq notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
  ;; Composition settings
  (setq message-kill-buffer-on-exit t)
  (setq notmuch-mua-compose-in 'current-window)

  (define-key notmuch-tree-mode-map (kbd "p") 'notmuch-tree-resume-message)
  (define-key notmuch-tree-mode-map (kbd "e") 'notmuch-tree-prev-matching-message)
  ;; Archive instead of delete
  (define-key notmuch-search-mode-map "d" 
    (lambda () (interactive) (notmuch-search-add-tag '("+deleted" "-inbox" "-sent"))))
  (define-key notmuch-show-mode-map "d"
    (lambda () (interactive) (notmuch-show-add-tag '("+deleted" "-inbox" "-sent"))))
  
  ;; Mail polling
  (defun notmuch-poll-and-refresh-this-buffer ()
    "Poll for new mail using mbsync and refresh."
    (interactive)
    (start-process "mbsync" "*mbsync*" "mbsync" "-a")
    (set-process-sentinel
     (get-process "mbsync")
     (lambda (process event)
       (when (string= event "finished\n")
         (start-process "notmuch-new" "*notmuch-new*" "notmuch" "new")
         (set-process-sentinel
          (get-process "notmuch-new")
          (lambda (_proc _event)
            (notmuch-refresh-this-buffer)))))))
  ;; Keep your update interval concept with a hook
  (run-at-time nil (* 5 60) 'notmuch-poll-and-refresh-this-buffer))

 
(use-package org-msg
  :ensure t
  :config
  ;; Org-msg basic setup
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
        org-msg-startup "hidestars indent inlineimages"
        org-msg-greeting-fmt "\nHi %s,\n\n"
        org-msg-greeting-name-limit 3
        org-msg-default-alternatives '((new . (text html))
                                       (reply-to-html . (text html))
                                       (reply-to-text . (text)))
        org-msg-convert-citation t
        org-msg-separator "^--$\\|^--- $\\|^___$"
        ;; Start with empty signature, will be updated dynamically
        org-msg-signature "")

  ;; Enable org-msg mode
  (org-msg-mode)

  ;; Function to dynamically update signature based on From
  (defun my/org-msg-set-signature ()
    "Update org-msg signature based on the current From header."
    (let ((from (or (message-fetch-field "from") "")))
      (setq org-msg-signature
            (cond
             ((string-match "nithin@cosdata.io" from)
              "
#+begin_signature
--
Nithin Mani | Founder, Cosdata
📬 nithin@cosdata.io | 🌐 https://cosdata.io
#+end_signature
")
             ((string-match "nithin@oodal.in" from)
              "
#+begin_signature
--
Nithin Mani
📬 nithin@oodal.in
#+end_signature
")
             (t "")))
      (when (fboundp 'org-msg-update-signature)
        (org-msg-update-signature))))

  ;; Hook to set signature when composing a message
  (add-hook 'message-setup-hook #'my/org-msg-set-signature))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass[presentation]{beamer}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}"))))
;; Force notmuch address completion in all message-related modes
(with-eval-after-load 'org-msg
  (add-hook 'org-msg-edit-mode-hook 'notmuch-address-setup))

(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

(setq org-latex-toc-command "\\tableofcontents \\clearpage")


(use-package org-caldav
  :ensure t
  :after org
  :config
  ;; 1. CalDAV Settings
  (setq org-caldav-url "https://calendar.zoho.in/caldav"
        org-caldav-calendar-id "zz0802123050d1399af9e26ef6ca91d05331a07de8958c1980c1f4696165b31b05bec0e5340f1ffa384cf75c418b110e3e68db480b/events"
        org-caldav-use-file-authinfo t
        org-caldav-inbox "~/org/appointments.org"
        org-caldav-files '("~/org/work.org" "~/org/inbox.org")
        org-caldav-username "nithin@cosdata.io"
        org-caldav-debug-level 2)
  
  ;; Optional: schedule periodic sync every 10 minutes
  ;; (run-at-time "5 min" 300 'org-caldav-sync)
)


(use-package ol-notmuch
  :ensure t
  :after notmuch
  :config

  ;; Customize capture contexts for Org-capture with email links.
  (setq org-capture-templates-contexts
        '(("e" ((in-mode . "notmuch-search-mode")
                (in-mode . "notmuch-show-mode")
                (in-mode . "notmuch-tree-mode")))))

)


(defun my/meow-colemak-setup ()
  ;; Leader key (Space)
(meow-leader-define-key
 '("SPC" . execute-extended-command)
 '("f"   . find-file)
 '("b"   . switch-to-buffer)
 '("k"   . kill-buffer)
 '("s"   . save-buffer))

  ;; Normal mode Colemak bindings
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("." . meow-inner-of-thing)
   '("," . meow-bounds-of-thing)
   '("(" . meow-beginning-of-thing)
   '(")" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-kill-word)
   '("D" . meow-backward-kill-word)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-mark-word)
   '("H" . meow-mark-symbol)
   '("n" . meow-next)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("K" . meow-kill-whole-line)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-left)
   '("M" . meow-left-expand)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-indent)
   '("'" . repeat)
   '("<escape>" . ignore)
  ;; '("<backspace>" . ignore)
  ;; '("<return>" . ignore)
   )
  )


(use-package meow
  :ensure t
  :config
  (my/meow-colemak-setup)
  (add-hook 'notmuch-hello-mode-hook (lambda () (meow-mode -1)))
  (add-hook 'notmuch-search-mode-hook (lambda () (meow-mode -1)))
  (add-hook 'notmuch-show-mode-hook (lambda () (meow-mode -1)))
  (add-hook 'notmuch-tree-mode-hook (lambda () (meow-mode -1)))
  (add-hook 'minibuffer-setup-hook (lambda () (meow-mode -1)))
  (meow-global-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
