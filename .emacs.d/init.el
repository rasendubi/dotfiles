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

(setq use-package-always-ensure t)
; (setq use-package-verbose t)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(setq-default frame-title-format
              '("[%m] " (:eval (projectile-project-name))))

(defconst android-p
  (string-equal system-configuration "arm-unknown-linux-androideabi"))

;; Make '_' a part of words, so commands like
;; `evil-forward-word-begin' work properly.
(add-hook 'prog-mode-hook
          (lambda () (modify-syntax-entry ?_ "w")))

(tool-bar-mode -1)
(menu-bar-mode -1)
(unless android-p
  (scroll-bar-mode -1))
(column-number-mode 1)
(show-paren-mode 1)

;; Force redraw as hiding menu bar does not take effect immediately.
;; This is a workaround, but I don't have time to debug the issue.
; (redraw-display)

;; It doesn't work well on android, but is enabled by default there
(if android-p
    (xterm-mouse-mode -1)
  (xterm-mouse-mode 1))

;; Highlight current line
(global-hl-line-mode)

(setq-default indent-tabs-mode nil)

;; `scroll-conservatively' should be greater than 100 to never
;; recenter point. Value 1 helps, but eventually recenters cursor if
;; you scroll too fast
(setq scroll-margin 3
      scroll-conservatively 101)

;; Don't clutter the current directory with backups. Save them in a
;; separate directory.
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))

;; Open config (this file)
(global-set-key (kbd "<f12>") (lambda () (interactive) (find-file user-init-file)))

(defun set-tab-width (width)
  "Set tab width to WIDTH and generate tab stops."
  (setq-default tab-width width)
  (setq-default tab-stop-list (number-sequence width 120 width)))

(set-tab-width 2)

(fset 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs-backups")))

(defun rasen/font-exists-p (font)
  "Check if the FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(cond ((rasen/font-exists-p "Terminess Powerline-12")
       (set-face-attribute 'default nil :font "Terminess Powerline-12"))
      ((rasen/font-exists-p "Terminus-12")
       (set-face-attribute 'default nil :font "Terminus-12")))

(setq inhibit-startup-screen t)

(defvar dotfiles-directory
  (file-name-as-directory (expand-file-name ".." (file-name-directory (file-truename user-init-file))))
  "The path to the dotfiles directory.")

(use-package smartrep
  :commands (smartrep-read-event-loop))

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

  ; (global-set-key (kbd "<left>")  (rasen/hard-way "h"))
  ; (global-set-key (kbd "<up>")    (rasen/hard-way "j"))
  ; (global-set-key (kbd "<down>")  (rasen/hard-way "k"))
  ; (global-set-key (kbd "<right>") (rasen/hard-way "l"))
  (nmap "<left>"  (rasen/hard-way "h"))
  (nmap "<up>"    (rasen/hard-way "j"))
  (nmap "<down>"  (rasen/hard-way "k"))
  (nmap "<right>" (rasen/hard-way "l"))
  (mmap "<left>"  (rasen/hard-way "h"))
  (mmap "<up>"    (rasen/hard-way "j"))
  (mmap "<down>"  (rasen/hard-way "k"))
  (mmap "<right>" (rasen/hard-way "l"))

  (defun rasen/smart-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  (nmap "k"       'evil-next-visual-line)
  (nmap "j"       'evil-previous-visual-line)
  (nmap "gk"      'evil-next-line)
  (nmap "gj"      'evil-previous-line)
  (mmap "k"       'evil-next-line)
  (mmap "j"       'evil-previous-line)

  (nmap "C-h"     'windmove-left)
  (nmap "C-k"     'windmove-down)
  (nmap "C-j"     'windmove-up)
  (nmap "C-l"     'windmove-right)
  (mmap "C-h"     'windmove-left)
  (mmap "C-k"     'windmove-down)
  (mmap "C-j"     'windmove-up)
  (mmap "C-l"     'windmove-right)

  (nmap "SPC"     nil)
  (vmap "SPC"     nil)
  (mmap "SPC"     nil)
  (nmap "SPC SPC" 'save-buffer)
  (nmap "H"       'rasen/smart-move-beginning-of-line)
  (vmap "H"       'rasen/smart-move-beginning-of-line)
  (mmap "H"       'rasen/smart-move-beginning-of-line)
  (nmap "L"       'evil-end-of-line)
  (vmap "L"       'evil-end-of-line)
  (mmap "L"       'evil-end-of-line)

  ;; 2017-02-18 unused
  (nmap "-"       'hs-toggle-hiding)

  (nmap "SPC ,"   'previous-error)
  (nmap "SPC ."   'next-error)
  (nmap "M-,"     'previous-error)
  (nmap "M-."     'next-error)

  (nmap "SPC ;"   (lookup-key (current-global-map) (kbd "M-:")))

  (nmap "\\ \\"   (lookup-key (current-global-map) (kbd "C-x")))
  (vmap "\\ \\"   (lookup-key (current-global-map) (kbd "C-x")))

  (nmap "SPC q"   'rasen/quit-other)

  ;; Swap . and ;
  (nmap "."       'evil-repeat-find-char)
  (nmap ";"       'evil-repeat)
  (nmap "C-;"     'evil-repeat-pop)
  (nmap "g."      'goto-last-change)

  ;; 2017-02-18 unused
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
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit))

;; Some highlighting for f, F, t, T commands
(use-package evil-quickscope
  :bind (:map evil-normal-state-map
         ("f" . evil-quickscope-find-char-to)
         ("F" . evil-quickscope-find-char-backward-to)
         ("t" . evil-quickscope-find-char)
         ("T" . evil-quickscope-find-char-backward)
         :map evil-motion-state-map
         ("f" . evil-quickscope-find-char-to)
         ("F" . evil-quickscope-find-char-backward-to)
         ("t" . evil-quickscope-find-char)
         ("T" . evil-quickscope-find-char-backward)))

(use-package evil-numbers
  :bind (:map evil-normal-state-map
         ("C-a" . evil-numbers/inc-at-pt)
         ("C-x" . evil-numbers/dec-at-pt)))

(use-package smartrep
  :commands (smartrep-read-event-loop))

(use-package key-chord
  :disabled t
  :if (not android-p)
  :after evil
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
  (vmap "<escape>" (rasen/hard-way "jk"))

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
  (vmap "<escape>" (rasen/hard-way "jk")))

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

  ; Original face overrides foreground, so you don't see syntax
  ; highlight. Use underlines to show characters past limit.
  (set-face-attribute 'whitespace-line nil
                      :foreground nil
                      :background nil
                      :underline (list :color "yellow4" :style 'wave))

  (global-whitespace-mode t))

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :config
  (whitespace-cleanup-mode 1))

(set-face-attribute 'font-lock-comment-face nil
                    :overline nil
                    :foreground (face-attribute 'shadow :foreground)
                    :background (face-attribute 'shadow :background))

(use-package clean-aindent-mode
  :config
  (setq clean-aindent-is-simple-indent t)
  (clean-aindent-mode t))

(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'libnotify))

(use-package browse-url
  :commands (browse-url-at-point
             browse-url-at-mouse
             browse-url-of-file)
  :config
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "google-chrome-stable"))

(use-package compile
  :config
  (setq compilation-scroll-output t)
  (evil-add-hjkl-bindings compilation-mode-map 'motion
    (kbd "SPC x") (lookup-key evil-motion-state-map (kbd "SPC x"))
    (kbd "g")     nil
    (kbd "g g")   'evil-goto-first-line
    (kbd "g r")   'recompile))

(use-package ivy
  :diminish ivy-mode
  :config
  ;; Don't start input with ^
  (setq-default ivy-initial-inputs-alist nil)
  (setq-default ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (define-key ivy-minibuffer-map (kbd "C-e") 'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-M-e") 'ivy-immediate-done)
  (ivy-mode 1))

(use-package ivy-hydra)

(use-package smex)

(use-package counsel
  :demand
  :diminish counsel-mode
  :bind (:map evil-normal-state-map
         ("SPC x" . counsel-M-x)
         ("SPC f" . counsel-find-file)
         ("g /"   . counsel-git-grep)
         :map evil-visual-state-map
         ("SPC x" . counsel-M-x)
         :map evil-motion-state-map
         ("SPC x" . counsel-M-x)
         :map read-expression-map
         ("C-r" . counsel-expression-history))
  :config
  (counsel-mode 1))

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
  :bind (:map evil-normal-state-map
              ("g m" . magit-status))
  :diminish auto-revert-mode
  :defer 6
  :init
  (global-set-key (kbd "C-c m") (rasen/hard-way "g m"))
  :config
  ;; Don't put files into trash can. Delete them for real.
  (setq-default magit-delete-by-moving-to-trash nil)

  (setq-default magit-completing-read-function 'ivy-completing-read)

  (defun rasen/magit-push-head (target args)
    "Push HEAD to a branch read in the minibuffer."
    (interactive
     (list (magit-read-remote-branch "Push HEAD to"
                                     nil nil nil 'confirm)
           (magit-push-arguments)))
    (magit-git-push "HEAD" target args))
  (magit-define-popup-action 'magit-push-popup
    ?h "HEAD" 'rasen/magit-push-head))

(use-package evil-magit
  :after magit
  :config
  (setq evil-magit-use-y-for-yank t)

  (dolist (state (list evil-magit-state 'visual))
    (evil-define-key state magit-mode-map (kbd "j")   'evil-previous-visual-line)
    (evil-define-key state magit-mode-map (kbd "k")   'evil-next-visual-line)
    (evil-define-key state magit-mode-map (kbd "C-j") 'magit-section-backward)
    (evil-define-key state magit-mode-map (kbd "C-k") 'magit-section-forward)
    (evil-define-key state magit-mode-map (kbd "gj")  'magit-section-backward-sibling)
    (evil-define-key state magit-mode-map (kbd "gk")  'magit-section-forward-sibling))

  (defun rasen/magit-fco-master ()
    "Fetch origin/master and checkout it."
    (interactive)
    (magit-fetch "origin" "master")
    (magit-checkout "origin/master"))
  (evil-magit-define-key evil-magit-state 'magit-mode-map
                         "g m" 'rasen/magit-fco-master))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

(use-package evil-magit
  :after magit
  :config
  (setq evil-magit-use-y-for-yank t))

(use-package diff-hl
  ; :disabled t
  :after magit
  :config
  ; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (unless (display-graphic-p)
    (diff-hl-margin-mode t))
  ; (diff-hl-flydiff-mode t)
  (global-diff-hl-mode t)
  )

(use-package gitconfig-mode
  :mode "^\\.gitconfig$")

(use-package yasnippet
  :defer 5
  :diminish yas-minor-mode
  :config
  (make-directory "~/.emacs.d/snippets" t)
  (yas-global-mode 1)
  (yas-load-directory "~/.emacs.d/snippets")
  (add-hook 'term-mode-hook (lambda ()
                              (setq yas-dont-activate-functions t))))

(use-package projectile
  :bind (:map evil-normal-state-map
         ("SPC p p" . projectile-switch-project)
         ("SPC p &" . projectile-run-async-shell-command-in-root)
         ("SPC p !" . projectile-run-shell-command-in-root)
         ;; That works much better than the default
         ("g f"     . projectile-find-file-dwim)
         ("U"       . projectile-find-file)
         ("<f3>"    . projectile-test-project)
         ("<f4>"    . projectile-compile-project)
         ("<f5>"    . projectile-run-project))
  :commands (projectile-project-name)
  :init
  ;; Save default just in case
  (nmap "g F" 'find-file-at-point)
  ;; g F was bound to `evil-find-file-at-point-with-line'
  ;; I've never used it though
  :diminish projectile-mode
  :config
  ;; Use the prefix arg if you want to change the compilation command
  (setq-default compilation-read-command nil)

  (setq-default projectile-use-git-grep t)

  (setq-default projectile-completion-system 'ivy)
  (projectile-mode))

(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-on))

(use-package powerline)

(use-package airline-themes
  :config
  (require 'cl)
  (load-theme 'airline-molokai t)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (load-theme 'molokai t)

  ; (setq powerline-utf-8-separator-left        #xe0b0
  ;       powerline-utf-8-separator-right       #xe0b2
  ;       airline-utf-glyph-separator-left      #xe0b0
  ;       airline-utf-glyph-separator-right     #xe0b2
  ;       airline-utf-glyph-subseparator-left   #xe0b1
  ;       airline-utf-glyph-subseparator-right  #xe0b3
  ;       airline-utf-glyph-branch              #xe0a0
  ;       airline-utf-glyph-readonly            #xe0a2
  ;       airline-utf-glyph-linenumber          #xe0a1)
  )

(use-package company
  :defer 2
  :bind (:map evil-insert-state-map
         ("C-n" . company-complete-common-or-cycle)
         ("C-p" . company-select-previous))
  :diminish company-mode
  :config
  (setq-default company-dabbrev-downcase nil)
  (global-company-mode))

(use-package ycmd
  :commands (ycmd-hook ycmd-mode)
  :config
  (set-variable 'ycmd-server-command (list "ycmd"))
  (set-variable 'ycmd-global-config
                (concat dotfiles-directory ".nvim/.ycm_extra_conf.py")))

(use-package flycheck
  :defer 2
  :diminish flycheck-mode
  :config
  (global-flycheck-mode))

(use-package flycheck-ycmd
  :after flycheck
  :config
  (flycheck-ycmd-setup))

(use-package company-ycmd
  :after company
  :commands (company-ycmd)
  :init
  (defun rasen/ycmd-c-c++-hook ()
    (push 'company-ycmd company-backends)
    (ycmd-mode))

  (add-hook 'c-mode-hook 'rasen/ycmd-c-c++-hook)
  (add-hook 'c++-mode-hook 'rasen/ycmd-c-c++-hook))

(use-package haskell-mode
  :mode "\\.hs$"
  :init
  (setq company-ghc-show-info t)
  (setq flycheck-ghc-stack-use-nix t)
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'haskell-decl-scan-mode)

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
                   (modes . '(haskell-mode literate-haskell-mode))))))

(use-package company-ghc
  :after haskell-mode
  :commands (company-ghc)
  :config
  (add-to-list 'company-backends 'company-ghc))

(use-package ghc
  :after haskell-mode
  :init
  (add-hook 'haskell-mode-hook 'ghc-init)
  :config
  ; (setq ghc-ghc-options '("-fdefer-type-errors" "-XNamedWildCards"))
  )

(use-package hideshow
  :commands (hs-minor-mode)
  :diminish hs-minor-mode
  :init
  (add-hook 'c-mode-hook 'hs-minor-mode))

(use-package which-key
  :defer 2
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
  :diminish (undo-tree-mode global-undo-tree-mode))

(use-package cmake-mode
  :mode ("^CMakeLists\\.txt$" . cmake-mode)
  :config
  (setq cmake-tab-width 4))

(use-package cmake-font-lock
  :after cmake-mode)
(use-package company-cmake
  :after cmake-mode)

(use-package eldoc
  :commands (eldoc-mode)
  :diminish eldoc-mode)

(use-package rust-mode
  :mode ("\\.rs$" . rust-mode))

(use-package racer
  :after rust-mode
  :commands racer-mode
  :diminish racer-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode))

(use-package lua-mode
  :mode ("\\.lua$" . lua-mode)
  :config
  (setq lua-indent-level 4))

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
  :commands (f-files f-ext?))

(use-package org
  :mode ("\\.org$" . org-mode)
  :defer 5
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)
         :map evil-normal-state-map
         ("SPC o" . org-clock-out)
         ("SPC l" . org-clock-in-last)
         ("SPC c" . org-capture)
         ("SPC a" . org-agenda))
  :ensure org-plus-contrib
  :init
  (nmap "C-c c" (rasen/hard-way "SPC c"))
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode 1)
                             (whitespace-mode -1)))

  (defun rasen/org-files-in-dir (dir)
    (f-files dir
             (lambda (file) (f-ext? file "org"))
             t))

  ;; save CLOSED timestamp when task is done
  (setq org-log-done t)

  ;; (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")

  (setq org-directory "~/org"
        org-default-notes-file "~/org/refile.org"
        org-agenda-files '("~/org/notes.org" "~/org/heutagogy.org"))

  (setq-default org-todo-keywords
                '((sequence "SOMEDAY" "TODO" "|" "DONE" "CANCELED")))
  (setq-default org-todo-repeat-to-state "TODO")

  ;; org-drill
  (require 'org-drill)
  (setq org-drill-scope (rasen/org-files-in-dir "~/org/drill"))
  (add-to-list 'org-modules 'org-drill)

  ;; ox-confluence
  (add-to-list 'org-modules 'ox-confluence)
  ;; ox-confluence has an issue with verbatim---it doesn't redefine
  ;; verbatim translation, so `org-ascii-verbatim' is used. The
  ;; following line makes `org-ascii-verbatim' produce proper
  ;; confluence fixed-width block.
  (setq org-ascii-verbatim-format "\{\{%s\}\}")

  ;; remove clocks with 0 duration
  (setq-default org-clock-out-remove-zero-time-clocks t)
  ;; Save more last clocks
  (setq-default org-clock-history-length 10)

  ;; `org-update-dblock' narrows to the subtree. I hate this.
  (defun rasen/advice-org-update-dblock (f &rest r)
    (save-restriction (apply f r)))
  (advice-add 'org-update-dblock :around #'rasen/advice-org-update-dblock)

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

  ;; org-refile
  (defun rasen/org-refile-files ()
    (rasen/org-files-in-dir "~/org"))
  ;; non-nil values work bad with ivy
  (setq-default org-refile-use-outline-path nil)
  (setq org-refile-targets
        '(;(nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 2)
          (rasen/org-refile-files :level . 1)))

  ;; org-babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t)
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
  :mode "\\.http$")

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
  :bind (:map evil-insert-state-map
         ("C-/" . hippie-expand))
  :config
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
            "\\|page\\|anchor\\|ref\\|section\\|subsection\\|subsubsection\\|paragraph"
            "\\)\\)\\s-+\\(\\sw+\\)")
    '(1 font-lock-keyword-face prepend)
    '(3 font-lock-string-face prepend))))

(defconst doxygen-font-lock-keywords
  `((,(lambda (limit)
        (c-font-lock-doc-comments "/\\(\\*[\\*!]\\|/[/!]\\)<?" limit
          doxymacs-doxygen-keywords)))))

(use-package irfc
  :commands (irfc-visit irfc-mode)
  :config
  (setq irfc-directory "~/tmp"
        irfc-assoc-mode t))

;; Debugging
(use-package gud
  :config
  (setq gdb-many-windows t))

(use-package tramp
  :defer 3
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

;; Gather stats on command usage, so I can better design my bindings.
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

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

(defun delete-this-file-and-buffer ()
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
  "Interpret color escape codes in the buffer."
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(use-package avy
  :bind (:map evil-normal-state-map
         ("K" . avy-goto-char)
         :map evil-motion-state-map
         ("K" . avy-goto-char))
  :config
  (setq-default avy-keys (list ?a ?s ?h ?t ?n ?e ?o ?i)))

(use-package string-inflection
  :bind (:map evil-normal-state-map
         ("SPC s i" . string-inflection-ruby-style-cycle)))

(use-package virtualenvwrapper
  :commands (venv-workon
             venv-deactivate
             venv-mkvirtualenv
             venv-rmvirtualenv
             venv-lsvirtualenv
             venv-cdvirtualenv
             venv-cpvirtualenv))

(use-package visual-fill-column
  :commands (visual-fill-column-mode)
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local fill-column 100)
              (visual-fill-column-mode)))
  :config
  (setq-default visual-fill-column-center-text t
                visual-fill-column-fringes-outside-margins nil))

(use-package json-mode
  :mode "\\.json$")

(use-package pip-requirements
  :mode "^requirements.txt$")

(use-package web-mode
  :commands (web-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  :config
  (setq-default web-mode-content-types-alist
                '(("jsx" . ".*\\.js[x]?\\'")))
  (setq-default js-indent 2
                js-switch-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-attr-indent-offset 2
                ))

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

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
                                   (innamespace . 0)
                                   (inline-open . 0)
                                   (inextern-lang . 0)))))
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "rasen")))

(use-package gnus
  :config
  (setq user-mail-address "rasen.dubi@gmail.com"
        user-full-name "Alexey Shmalko")

  ;; (setq gnus-select-method '(nnnil ""))
  ;; (setq gnus-secondary-select-methods
  ;;       '((nnmaildir "Personal"
  ;;                    (directory "~/Mail/Personal")
  ;;                    (directory-files nnheader-directory-files-safe)
  ;;                    (get-new-mail nil))
  ;;         (nnmaildir "Work"
  ;;                    (directory "~/Mail/Work")
  ;;                    (directory-files nnheader-directory-files-safe)
  ;;                    (get-new-mail nil))))

  (setq gnus-select-method
        '(nnimap "Mail"
                 (nnimap-address "127.0.0.1")
                 (nnimap-stream network)
                 (nnimap-authenticator login)))
  (setq gnus-secondary-select-methods nil)

  (setq gnus-parameters
        '(("Work/?.*"
           (display . all)
           (posting-style
            (name "Alexey Shmalko")
            (address "ashmalko@cybervisiontech.com")
            ("X-Message-SMTP-Method" "smtp mail.cybervisiontech.com 587")))
          ("Personal/?.*"
           (display . all)
           (posting-style
            (name "Alexey Shmalko")
            (address "rasen.dubi@gmail.com"))
            ("X-Message-SMTP-Method" "smtp smtp.gmail.com 587"))))

  (setq gnus-fetch-old-headers 'some)
  (setq smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
        gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

  (setq mm-verify-option 'always)

  (setq gnus-check-new-newsgroups nil ;; NOTE: don't check for new groups
        gnus-save-newsrc-file nil ;; NOTE: don't write `.newsrc' file
        gnus-read-newsrc-file nil ;; NOTE: don't read it, either
        gnus-interactive-exit nil
        gnus-save-killed-list nil)

  (require 'gnus-article-treat-patch)
  (setq ft/gnus-article-patch-conditions
      '( "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" )))

(use-package mbsync
  :commands (mbsync)
  :init
  (define-key gnus-group-mode-map (kbd "f") 'mbsync)
  :config
  (add-hook 'mbsync-exit-hook 'gnus-group-get-new-news))

(use-package notmuch
  :config
  (setq notmuch-search-oldest-first nil))

;; For use with "Edit with Emacs" chrome plugin.
;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh?hl=en
(use-package edit-server
  :demand ; no defer
  :bind (:map edit-server-edit-mode-map
         ("C-c C-k" . edit-server-abort))
  :config
  (edit-server-start))

(defun add-to-path (str)
  "Add an STR to the PATH environment variable."
  (setenv "PATH" (concat str ":" (getenv "PATH"))))
(put 'narrow-to-region 'disabled nil)

(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-readable-p private-file)
    (load-file private-file)))

;; Save custom configuration in the "~/.emacs.d/custom.el" file
;; instead of this one.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(load "server")
(unless (server-running-p)
  (server-start))
