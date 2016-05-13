(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(defconst android-p
  (string-equal system-configuration "arm-unknown-linux-androideabi"))

(setq use-package-always-ensure t)
;(setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package evil
  :config
  (evil-mode 1)

  (defun rasen/quit-other ()
    (interactive)
    (other-window 1)
    (quit-window))

  (defun nmap (key action)
    (define-key evil-normal-state-map (kbd key) action))
  (defun vmap (key action)
    (define-key evil-visual-state-map (kbd key) action))
  (defun imap (key action)
    (define-key evil-insert-state-map (kbd key) action))
  (defun mmap (key action)
    (define-key evil-motion-state-map (kbd key) action))
  (defmacro rasen/hard-way (key)
    `(lambda () (interactive) (error "Don't use this key! Use %s instead" ,key)))
  (defun rasen/helm-projectile-grep-headers ()
    (interactive)
    (helm-do-grep-1 (list (projectile-project-root)) t nil '("*.h")))

  (nmap "j"       'evil-next-visual-line)
  (nmap "k"       'evil-previous-visual-line)
  (nmap "gj"      'evil-next-line)
  (nmap "gk"      'evil-previous-line)

  (nmap "C-h"     'windmove-left)
  (nmap "C-j"     'windmove-down)
  (nmap "C-k"     'windmove-up)
  (nmap "C-l"     'windmove-right)
  (mmap "C-h"     'windmove-left)
  (mmap "C-j"     'windmove-down)
  (mmap "C-k"     'windmove-up)
  (mmap "C-l"     'windmove-right)

  (nmap "SPC"     nil)
  (vmap "SPC"     nil)
  (mmap "SPC"     nil)
  (nmap "SPC SPC" 'save-buffer)
  (nmap "H"       'evil-first-non-blank)
  (vmap "H"       'evil-first-non-blank)
  (mmap "H"       'evil-first-non-blank)
  (nmap "L"       'evil-end-of-line)
  (vmap "L"       'evil-end-of-line)
  (mmap "L"       'evil-end-of-line)

  (nmap "-"       'hs-toggle-hiding)

  (nmap "SPC ,"   'previous-error)
  (nmap "SPC ."   'next-error)
  (nmap "M-,"     'previous-error)
  (nmap "M-."     'next-error)

  (nmap "SPC x"   'helm-M-x)
  (vmap "SPC x"   'helm-M-x)
  (mmap "SPC x"   'helm-M-x)
  (nmap "M-x"     (rasen/hard-way "SPC x"))
  (nmap "SPC ;"   (lookup-key (current-global-map) (kbd "M-:")))

  (nmap "\\ \\"   (lookup-key (current-global-map) (kbd "C-x")))
  (vmap "\\ \\"   (lookup-key (current-global-map) (kbd "C-x")))
  (nmap "\\ x"    (rasen/hard-way "\\ \\"))
  (vmap "\\ x"    (rasen/hard-way "\\ \\"))

  (nmap "SPC f"   'helm-find-files)
  (nmap "SPC p p" 'projectile-switch-project)
  (nmap "SPC p f" (rasen/hard-way "U"))
  (nmap "SPC p d" 'helm-projectile-find-dir)
  ;; like "global search"
  (nmap "g /"     'helm-projectile-grep)
  (nmap "SPC p g" (rasen/hard-way "g /"))
  (nmap "g r"     (rasen/hard-way "g /"))
  (nmap "g h"     'rasen/helm-projectile-grep-headers)
  (nmap "SPC p &" 'projectile-run-async-shell-command-in-root)
  (nmap "SPC p !" 'projectile-run-shell-command-in-root)

  ;; That works much better than the default
  (nmap "g f"     'helm-projectile-find-file-dwim)
  ;; Save default just in case
  (nmap "g F"     'find-file-at-point)
  ;; g F was bound to `evil-find-file-at-point-with-line'
  ;; I've never used it though

  (nmap "SPC q"   'rasen/quit-other)
  (nmap "SPC w"   (lambda () (interactive) (save-buffers-kill-terminal t)))
  (nmap "U"       'helm-projectile-find-file)
  (nmap "C-]"     'helm-semantic-or-imenu)

  ;; Swap . and ;
  (nmap "."       'evil-repeat-find-char)
  (nmap ";"       'evil-repeat)
  (nmap "C-;"     'evil-repeat-pop)
  (nmap "g."      'goto-last-change)

  (nmap "<f3>"    'projectile-test-project)
  (nmap "<f4>"    'projectile-compile-project)
  (nmap "<f5>"    'projectile-run-project)
  (mmap "<f3>"    'projectile-test-project)
  (mmap "<f4>"    'projectile-compile-project)
  (mmap "<f5>"    'projectile-run-project)

  (imap "C-n"     #'company-complete-common-or-cycle)
  (imap "C-p"     #'company-select-previous)

  (nmap "C-\\"    'evil-execute-in-emacs-state)

  ;; esc quit anything
  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
the it takes a second \\[keyboard-quit]] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (use-package smartrep
    :commands (smartrep-read-event-loop))
  (defun isearch-exit-chord-worker (&optional arg)
    (interactive "p")
    (isearch-delete-char)
    (isearch-cancel))

  (defun isearch-exit-chord (arg)
    (interactive "p")
    (isearch-printing-char)
    (run-at-time 0.3 nil '(lambda () (signal 'quit nil)))
    (condition-case nil
        (smartrep-read-event-loop
         '(("k" . isearch-exit-chord-worker)))
      (quit nil)))
  (define-key isearch-mode-map "j" 'isearch-exit-chord)

  ;; Some highlighting for f, F, t, T commands
  (use-package evil-quickscope
    :defer 2
    :config
    (global-evil-quickscope-mode))

  (use-package key-chord
    :if (not android-p)
    :config

    ;; This must be called before `key-chord-mode' as key-chord must
    ;; override input-method to work properly.
    (set-input-method "ukrainian-computer")
    ;; I still want English be default
    (toggle-input-method)

    (setq key-chord-two-keys-delay 0.2)
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-visual-state-map "jk" 'evil-change-to-previous-state)
    (key-chord-define evil-replace-state-map "jk" 'evil-normal-state)
    (imap "<escape>" (rasen/hard-way "jk"))
    (vmap "<escape>" (rasen/hard-way "jk")))

  (use-package evil-numbers
    :commands (evil-numbers/inc-at-pt
               evil-numbers/dec-at-pt)
    :init
    (nmap "C-a" 'evil-numbers/inc-at-pt)
    (nmap "C-x" 'evil-numbers/dec-at-pt)))

(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w"))) ; _ is a part of word

(tool-bar-mode 0)
(menu-bar-mode 0)
(unless android-p
  (scroll-bar-mode 0))
(column-number-mode 1)
(show-paren-mode 1)

;; It doesn't work well on android, but is enabled by default there
(if android-p
    (xterm-mouse-mode -1)
  (xterm-mouse-mode 1))

(global-hl-line-mode)

(setq-default indent-tabs-mode nil)

;; `scroll-conservatively' should be greater than 100 to never recenter point.
;; Value 1 helps, but eventually recenters cursor if you scroll too fast
(setq scroll-margin 3
      scroll-conservatively 101)

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :config
  (setq-default whitespace-line-column 120
                whitespace-style '(face
                                   tab-mark
                                   empty
                                   trailing
                                   lines-tail))
  (global-whitespace-mode t))

(use-package clean-aindent-mode
  :init
  (setq clean-aindent-is-simple-indent t)
  :config
  (clean-aindent-mode t))
;(define-key global-map (kbd "RET") 'newline-and-indent)

;(use-package smartparens
;  :diminish smartparens-mode
;  :config
;  (sp-local-pair '(c-mode rust-mode) "{" "}" :post-handlers '(("\n||\n[i]" "RET")))
;  (require 'smartparens-config)
;  (sp-local-pair '(c-mode rust-mode) "{" "}" :post-handlers '(("\n||\n[i]" "RET")))
;  (sp-local-pair 'c-mode "/*" "*/" :post-handlers '(("| " "SPC")
;                                                    ("||\n[i]" "RET")
;                                                    (" ||\n[i]" "*")))
;  (smartparens-global-mode))

(defun set-tab-width (width)
  "Set tab width to WIDTH and generate tab stops"
  (setq tab-width width)
  (setq tab-stop-list (number-sequence width 120 width)))

(set-tab-width 2)

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

;; Open config (this file)
(global-set-key (kbd "<f12>") (lambda () (interactive) (find-file user-init-file)))

(defun font-exists-p (font)
  (not (and (display-graphic-p) (null (x-list-fonts font)))))

(cond ((font-exists-p "Terminus-12")
       (set-face-attribute 'default nil :font "Terminus-12"))
      ((font-exists-p "Terminess Powerline-12")
       (set-face-attribute 'default nil :font "Terminess Powerline-12")))

(setq inhibit-startup-screen t)

;; Here is how to preserve current working directory
;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (setq default-directory command-line-default-directory)))
;; or put this in your project root (.dir-locals.el):
;; ((nil . ((default-directory . "project/directory/"))))

(defvar dotfiles-directory
  (file-name-as-directory (expand-file-name ".." (file-name-directory (file-truename user-init-file))))
  "The path to the dotfiles directory")

(setq compilation-scroll-output t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'molokai t)

(use-package helm-projectile
  :commands (helm-projectile-switch-to-buffer
             helm-projectile-find-dir
             helm-projectile-dired-find-dir
             helm-projectile-recentf
             helm-projectile-find-file
             helm-projectile-grep
             helm-projectile
             helm-projectile-switch-project)
  :config
  (helm-projectile-on))

(use-package helm
  :commands (helm-M-x
             helm-mini
             helm-find-files
             helm-command-prefix
             helm-google-suggest)
  :diminish helm-mode
  :defer 6

  :init
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-b") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-h") 'helm-command-prefix)
  (global-set-key (kbd "C-c h g") 'helm-google-suggest)

  :config
  (require 'helm-config)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z") 'helm-select-action)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-move-to-line-cycle-in-source t
        helm-ff-search-library-in-sexp t
        helm-scroll-amount 8
        helm-ff-file-name-history-use-recentf t
        helm-M-x-fuzzy-match t)

  (helm-mode 1))

(use-package xclip
  :if (and (not window-system) (not android-p))
  :config
  (xclip-mode 1)
  (defun my-x-set-selection (f type data)
    (xclip-set-selection type data))
  (advice-add 'x-set-selection :around #'my-x-set-selection)

  (defun my-x-selection-value-internal (f type)
    (xclip-selection-value))
  (advice-add 'x-selection-value-internal :around #'my-x-selection-value-internal))

(use-package magit
  :bind ("C-c m" . magit-status)
  :diminish auto-revert-mode
  :defer 6
  :config
  (use-package evil-magit
    :init
    (setq evil-magit-use-y-for-yank t)))

(use-package gitconfig-mode
  :mode "^\\.gitconfig$")

(use-package diff-hl
  :defer 3
  :config
  (global-diff-hl-mode t)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (unless (display-graphic-p)
    (diff-hl-margin-mode t))
  (diff-hl-flydiff-mode t))

(use-package yasnippet
  :defer 5
  :diminish yas-minor-mode
  :init
  (make-directory "~/.emacs.d/snippets" t)
  :config
  (yas-global-mode 1)
  (yas-load-directory "~/.emacs.d/snippets")
  (add-hook 'term-mode-hook (lambda ()
                              (setq yas-dont-activate t))))

(use-package projectile
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-run-project
             projectile-dired
             projectile-grep
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-find-test-file
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-register-project-type
             projectile-replace
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :diminish projectile-mode
  :config
  ;; Use prefix arg, is you want to change compilation command
  (setq compilation-read-command nil)

  (projectile-global-mode)
  (setq projectile-completion-system 'helm))

(use-package powerline)

(use-package airline-themes
  :init
  (require 'cl)
  (load-theme 'airline-molokai t))

(use-package company
  :diminish company-mode
  :config
  (global-company-mode))

(use-package ycmd
  :defer t
  :commands (ycmd-hook ycmd-mode)
  :config
  (set-variable 'ycmd-server-command (list "ycmd"))
  (set-variable 'ycmd-global-config
                (concat dotfiles-directory ".nvim/.ycm_extra_conf.py")))

(use-package flycheck
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

(use-package flycheck-ycmd
  :config
  (flycheck-ycmd-setup))

(use-package company-ycmd
  :commands (company-ycmd)
  :init
  (defun c-c++-hook ()
    (push 'company-ycmd company-backends)
    (ycmd-mode))

  (add-hook 'c-mode-hook 'c-c++-hook)
  (add-hook 'c++-mode-hook 'c-c++-hook))

(use-package haskell-mode
  :mode "\\.hs$"
  :init
  (setq company-ghc-show-info t)
  (setq flycheck-ghc-stack-use-nix t)
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

  ;; For unknown reason, local value of `adaptive-fill-mode' was set
  ;; to nil in haskell files. That broke refill in comments.
  ;;
  ;; Setting it back to t fixes refill.
  (add-hook 'haskell-mode-hook (lambda ()
                                 (setq-local adaptive-fill-mode t)))

  (setq haskell-compile-cabal-build-command "cd %s && stack build")
  (setq haskell-compile-cabal-build-command-alt "cd %s && cabal build --ghc-options=-ferror-spans")

  ;; Use Nix for stack ghci
  (add-to-list 'haskell-process-args-stack-ghci "--nix")
  (add-to-list 'haskell-process-args-stack-ghci "--test")

  ;; Use Nix for default build/test command
  (projectile-register-project-type 'haskell-stack
                                    '("stack.yaml")
                                    "stack build --nix"
                                    "stack build --nix --test")

  (define-key haskell-mode-map [f8] 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c v c") 'haskell-cabal-visit-file)

  ; haskell-interactive-mode
  (define-key haskell-mode-map (kbd "C-x C-d") nil)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c M-.") nil)
  (define-key haskell-mode-map (kbd "C-c C-d") nil)

  ;; Disable popups (i.e., report errors in the interactive shell).
  (setq haskell-interactive-popup-errors nil)

  (setq haskell-process-suggest-remove-import-lines t
        haskell-process-auto-import-loaded-modules t)

  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode)))))

  (use-package ghc
    :init
    (add-hook 'haskell-mode-hook 'ghc-init)
    :config
    ; (setq ghc-ghc-options '("-fdefer-type-errors" "-XNamedWildCards"))
    (autoload 'ghc-init "ghc" nil t)
    (autoload 'ghc-debug "ghc" nil t))

  (use-package company-ghc
    :config
    (add-to-list 'company-backends 'company-ghc)))

(use-package hideshow
  :commands (hs-minor-mode)
  :diminish hs-minor-mode
  :init
  (add-hook 'c-mode-hook 'hs-minor-mode))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

(use-package color-identifiers-mode
  :defer 3
  :diminish color-identifiers-mode
  :config
  (global-color-identifiers-mode))

(use-package yaml-mode
  :mode ("\\.\\(yml\\|yaml\\)$" . yaml-mode))

(use-package markdown-mode
  :mode ("\\.\\(markdown\\|mdown\\|md\\)$" . markdown-mode))

(use-package undo-tree
  :diminish undo-tree-mode)

(use-package cmake-mode
  :mode ("^CMakeLists\\.txt$" . cmake-mode)
  :config
  (use-package cmake-font-lock)
  (use-package company-cmake))

(use-package idris-mode
  :disabled t
  :mode ("\\.idr$" . idris-mode))

(use-package eldoc
  :commands (eldoc-mode)
  :diminish eldoc-mode)

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode)
  :config
  (use-package racer
    :commands racer-mode
    :diminish racer-mode
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)))

(use-package nim-mode
  :disabled t
  :mode ("\\.nim$" . nim-mode))
;; Seems that I have to install nimsuggest first
; (custom-set-variables
;  '(nim-nimsuggest-path "~/.nimble/bin/nimsuggest"))
; (add-to-list 'company-backends 'company-nim)

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :config
  (setq lua-indent-level 4))

(use-package scala-mode2
  :disabled t
  :mode ("\\.scala$" . scala-mode))

(use-package ensime
  :disabled t
  :config
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package rainbow-delimiters
  :commands (rainbow-delimiters-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground 'unspecified
                      :background "purple"
                      :inherit 'error)
  (custom-set-faces
   '(rainbow-delimiters-depth-1-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-depth-2-face ((t (:foreground "deep pink"))))
   '(rainbow-delimiters-depth-3-face ((t (:foreground "chartreuse"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
   '(rainbow-delimiters-depth-5-face ((t (:foreground "yellow"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "orchid"))))
   '(rainbow-delimiters-depth-7-face ((t (:foreground "spring green"))))
   '(rainbow-delimiters-depth-8-face ((t (:foreground "sienna1"))))))

(use-package term-run
  :commands (term-run term-run-shell-command))

;; Used by org-mode config
(use-package f
  :functions (f-files f-ext?))

(use-package org
  :ensure org-plus-contrib
  :config
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c b") 'org-iswitchb)

  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode 1)
                             (whitespace-mode -1)))

  (setq org-directory "~/org"
        org-default-notes-file "~/org/refile.org")

  ;; org-drill
  (require 'org-drill)
  (defun rasen/org-drill-files ()
    (f-files "~/org/drill"
             (lambda (file) (f-ext? file "org"))
             t))
  (setq org-drill-scope (rasen/org-drill-files))
  (add-to-list 'org-modules 'org-drill)

  ;; ox-confluence
  (add-to-list 'org-modules 'ox-confluence)
  ;; ox-confluence has an issue with verbatim---it doesn't redefine
  ;; verbatim translation, so `org-ascii-verbatim' is used. The
  ;; following line makes `org-ascii-verbatim' produce proper
  ;; confluence fixed-width block.
  (setq org-ascii-verbatim-format "\{\{%s\}\}")

  ;; org-capture
  (setq org-capture-templates
        `(("u"
           "Task: Read this URL"
           entry
           (file+headline "tasks.org" "Articles To Read")
           ,(concat "* TODO Read article: '%:description'\nURL: %c\n\n")
           :empty-lines 1
           :immediate-finish t)

          ("w"
           "Capture web snippet"
           entry
           (file+headline "my-facts.org" "Inbox")
           ,(concat "* Fact: '%:description'       :"
                    (format "%s" org-drill-question-tag)
                    ":\n:PROPERTIES:\n:DATE_ADDED: %u\n:SOURCE_URL: %c\n:END:\n%i\n%?\n")
           :empty-lines 1
           :immediate-finish t)

          ("f"
           "Capture normal snippet"
           entry
           (file+headline "my-facts.org" "Inbox")
           ,(concat "* Fact: '%f'       :"
                    (format "%s" org-drill-question-tag)
                    ":\n:PROPERTIES:\n:DATE_ADDED: %u\n:SOURCE_URL: [[%l][%f]]\n:END:\n%i\n%?\n")
           :empty-lines 1
           :immediate-finish t)

          ("t" "todo" entry (file "~/org/refile.org")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

          ("n" "note" entry (file "~/org/refile.org")
           "* %? :NOTE:\n\n%a\n" :clock-in t :clock-resume t)))

  ;; %l in org-capture fails with multiline context, so use only the
  ;; first line as a context
  (setq org-context-in-file-links 1)

  ;; org-protocol
  (require 'org-protocol)
  (load "server")
  (unless (server-running-p)
    (server-start))

  ;; org-refile
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (rasen/org-drill-files :level . 0)))

  ;; org-babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (sh . t)
                                 (ditaa . t)
                                 (plantuml . t)))
  (setq org-ditaa-jar-path "~/.nix-profile/lib/ditaa.jar")
  (setq org-plantuml-jar-path "~/.nix-profile/lib/plantuml.jar"))

(use-package htmlize
  :defer t)

(use-package smart-tabs-mode
  :config
  (defadvice align (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))

  (defadvice align-regexp (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))

  (defadvice indent-relative (around smart-tabs activate)
    (let ((indent-tabs-mode nil)) ad-do-it))

  (defadvice indent-according-to-mode (around smart-tabs activate)
    (let ((indent-tabs-mode indent-tabs-mode))
      (if (memq indent-line-function
                '(indent-relative
                  indent-relative-maybe))
          (setq indent-tabs-mode nil))
      ad-do-it))

  (defmacro smart-tabs-advice (function offset)
    `(progn
       (defvaralias ',offset 'tab-width)
       (defadvice ,function (around smart-tabs activate)
         (cond
          (indent-tabs-mode
           (save-excursion
             (beginning-of-line)
             (while (looking-at "\t*\\( +\\)\t+")
               (replace-match "" nil nil nil 1)))
           (setq tab-width tab-width)
           (let ((tab-width fill-column)
                 (,offset fill-column)
                 (wstart (window-start)))
             (unwind-protect
                 (progn ad-do-it)
               (set-window-start (selected-window) wstart))))
          (t
           ad-do-it)))))

  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset))

(use-package restclient
  :disabled t)

(use-package nix-mode
  :mode "\\.nix$")

(use-package clojure-mode
  :disabled t
  :mode "\\.clj$"
  :config
  (use-package cider))

(use-package cedet
  :disabled t
  :config
  (require 'semantic/ia)
  (require 'semantic/bovine/gcc)
  (setq semantic-default-submodes
        '(global-semantic-idle-scheduler-mode
          global-semanticdb-minor-mode
          global-semantic-show-unmatched-syntax-mode
          global-semantic-show-parser-state-mode
          global-semantic-highlight-edits-mode))
  ;(when (cedet-gnu-global-version-check t)
  ;  (semanticdb-enable-gnu-global-databases 'c-mode)
  ;  (semanticdb-enable-gnu-global-databases 'c++-mode))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (interactive)
              (local-set-key "." 'semantic-complete-self-insert)
              (local-set-key ">" 'semantic-complete-self-insert)))
  (semantic-mode 1)
  ;(global-ede-mode t)
  )

(use-package hippie-exp
  :config
  (imap "C-/" 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-line
          try-expand-list)))

(setq c-doc-comment-style '((java-mode . javadoc)
                            (pike-mode . autodoc)
                            (c-mode . doxygen)
                            (c++-mode . doxygen)))

;; This const is taken from doxymacs and is subject to GPLv2.  I've
;; copied it my dotfiles as I don't need all doxymacs features and
;; setup is non-trivial. (It requires compilation, there is no melpa
;; package.)
(defconst doxymacs-doxygen-keywords
  (list
   (list
    ;; One shot keywords that take no arguments
    (concat "\\([@\\\\]\\(brief\\|li\\|\\(end\\)?code\\|sa"
            "\\|note\\|\\(end\\)?verbatim\\|return\\|arg\\|fn"
            "\\|hideinitializer\\|showinitializer"
            "\\|parblock\\|endparblock"
            ;; FIXME
            ;; How do I get & # < > % to work?
            ;;"\\|\\\\&\\|\\$\\|\\#\\|<\\|>\\|\\%"
            "\\|internal\\|nosubgrouping\\|author\\|date\\|endif"
            "\\|invariant\\|post\\|pre\\|remarks\\|since\\|test\\|version"
            "\\|\\(end\\)?htmlonly\\|\\(end\\)?latexonly\\|f\\$\\|file"
            "\\|\\(end\\)?xmlonly\\|\\(end\\)?manonly\\|property"
            "\\|mainpage\\|name\\|overload\\|typedef\\|deprecated\\|par"
            "\\|addindex\\|line\\|skip\\|skipline\\|until\\|see"
            "\\|endlink\\|callgraph\\|endcond\\|else\\)\\)\\>")
    '(0 font-lock-keyword-face prepend))
   ;; attention, warning, etc. given a different font
   (list
    "\\([@\\\\]\\(attention\\|warning\\|todo\\|bug\\)\\)\\>"
    '(0 font-lock-warning-face prepend))
   ;; keywords that take a variable name as an argument
   (list
    (concat "\\([@\\\\]\\(param\\(?:\\s-*\\[\\(?:in\\|out\\|in,out\\)\\]\\)?"
            "\\|a\\|namespace\\|relates\\(also\\)?"
            "\\|var\\|def\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-variable-name-face prepend))
   ;; keywords that take a type name as an argument
   (list
    (concat "\\([@\\\\]\\(class\\|struct\\|union\\|exception\\|enum"
            "\\|throw\\|interface\\|protocol\\)\\)\\s-+\\(\\(\\sw\\|:\\)+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-type-face prepend))
   ;; keywords that take a function name as an argument
   (list
    "\\([@\\\\]retval\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-function-name-face prepend))
   ;; bold
   (list
    "\\([@\\\\]b\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 (quote bold) prepend))
   ;; code
   (list
    "\\([@\\\\][cp]\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(2 (quote underline) prepend))
   ;; italics/emphasised
   (list
    "\\([@\\\\]e\\(m\\)?\\)\\s-+\\([^ \t\n]+\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 (quote italic) prepend))
   ;; keywords that take a list
   (list
    "\\([@\\\\]ingroup\\)\\s-+\\(\\(\\sw+\\s-*\\)+\\)\\s-*$"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend))
   ;; one argument that can contain arbitrary non-whitespace stuff
   (list
    (concat "\\([@\\\\]\\(link\\|copydoc\\|xrefitem"
            "\\|if\\(not\\)?\\|elseif\\)\\)"
            "\\s-+\\([^ \t\n]+\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-string-face prepend))
   ;; one optional argument that can contain arbitrary non-whitespace stuff
   (list
    "\\([@\\\\]\\(cond\\|dir\\)\\(\\s-+[^ \t\n]+\\)?\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; one optional argument with no space between
   (list
    "\\([@\\\\]\\(~\\)\\([^ \t\n]+\\)?\\)"
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; one argument that has to be a filename
   (list
    (concat "\\([@\\\\]\\(example\\|\\(dont\\)?include\\|includelineno"
            "\\|htmlinclude\\|verbinclude\\)\\)\\s-+"
            "\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)")
    '(1 font-lock-keyword-face prepend)
    '(4 font-lock-string-face prepend))
   ;; dotfile <file> ["caption"]
   (list
    (concat "\\([@\\\\]dotfile\\)\\s-+"
            "\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?")
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend)
    '(3 font-lock-string-face prepend t))
   ;; image <format> <file> ["caption"] [<sizeindication>=<size>]
   (list
    "\\([@\\\\]image\\)\\s-+\\(html\\|latex\\)\\s-+\\(\"?[~:\\/a-zA-Z0-9_. ]+\"?\\)\\(\\s-+\"[^\"]+\"\\)?\\(\\s-+\\sw+=[0-9]+\\sw+\\)?"
    '(1 font-lock-keyword-face prepend)
    '(2 font-lock-string-face prepend)
    '(3 font-lock-string-face prepend)
    '(4 font-lock-string-face prepend t)
    '(5 font-lock-string-face prepend t))
   ;; one argument that has to be a word
   (list
    (concat "\\([@\\\\]\\(addtogroup\\|defgroup\\|weakgroup"
            "\\|page\\|anchor\\|ref\\|section\\|subsection"
            "\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend))))

(defconst doxygen-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\(\\*[\\*!]\\|/[/!]\\)<?" limit
          doxymacs-doxygen-keywords)))))

(use-package irfc
  :config
  (setq irfc-directory "~/tmp"
        irfc-assoc-mode t))

;; Debugging
(use-package gud
  :config
  (setq gdb-many-windows t))

(use-package tramp
  :config
  (setq tramp-default-method "ssh")
  (setq tramp-default-host "ashmalko.local")

  ;; This is needed for tramp to respect remote PATH variable
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (defun sudo ()
    "Use TRAMP to `sudo' the current buffer"
    (interactive)
    (when buffer-file-name
      (find-alternate-file
       (concat "/sudo:root@localhost:"
               buffer-file-name)))))

(use-package ido
  :config
  (ido-mode 1)
  ; I still use `helm-projectile-find-file', but that should be faster
  ; for searching direct files. Time will show.
  (nmap "SPC f" 'ido-find-file)
  (nmap "SPC b" 'ido-switch-buffer))

;; I want to log all my key presses to analyze them later
;;
;; Well... it doesn't log all key presses (only command
;; invocations). That means, 'ciw' shows only 'c' in the log. (I may
;; need to change the logger to log all key presses.)
(use-package command-log-mode
  ;; I've patches command-log-mode to not merge command repetitions
  ;; into the single line.
  ;; PR is here: https://github.com/lewang/command-log-mode/pull/12
  :load-path "site-lisp/command-log-mode"

  ;; Well, I've gathered enough logs for now.
  :disabled t

  :diminish command-log-mode

  :init
  ;; Don't bind C-c o to open `clm/toggle-command-log-buffer'
  (setq command-log-mode-key-binding-open-log nil)

  :config
  ;; log all commands
  (setq clm/log-command-exceptions* '(digit-argument))
  ;; don't merge repetitions
  (setq clm/log-repeat t)

  (setq clm/logging-dir "~/log/")

  ;; Create a buffer for logging, so logging starts automatically
  (setq clm/command-log-buffer (get-buffer-create " *command-log*"))

  ;; Save logs on idle
  (run-with-idle-timer 60 t 'clm/save-command-log)
  ;; ... and on exit
  (add-hook 'kill-emacs-hook 'clm/save-command-log)

  (global-command-log-mode))

;; Shamelessly stealed from from https://github.com/purcell/emacs.d
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (when (file-exists-p filename)
          (rename-file filename new-name 1))
        (rename-buffer new-name)
        (set-visited-file-name new-name)))))

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; http://stackoverflow.com/a/13408008/2538771
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(c-add-style "rasen"
             '("k&r"
               (c-basic-offset . 4)
               (fill-column . 70)
               (whitespace-line-column . 100)
               (c-block-comment-prefix . "* ")
               (c-label-minimum-indentation . 0)
               (c-offsets-alist . ((case-label . +)
                                   (arglist-intro . ++)
                                   (arglist-cont-nonempty . ++)
                                   (inextern-lang . 0)))))
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "rasen")))

(defun add-to-path (str)
  (setenv "PATH" (concat str ":" (getenv "PATH"))))
