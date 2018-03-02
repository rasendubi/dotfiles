;; This file is a backup of my not-yet-transfered tricks while
;; transitioning to emacs.org.

(defconst android-p
  (string-equal system-configuration "arm-unknown-linux-androideabi"))

;; It doesn't work well on android, but is enabled by default there
(if android-p
    (xterm-mouse-mode -1)
  (xterm-mouse-mode 1))

(defun set-tab-width (width)
  "Set tab width to WIDTH and generate tab stops."
  (setq-default tab-width width)
  (setq-default tab-stop-list (number-sequence width 120 width)))

(set-tab-width 2)

; (set-face-attribute 'variable-pitch nil :family "Helvetica Neue" :height 160)
; (set-face-attribute 'default nil :family "Helvetica Neue" :height 160)


(use-package smartrep
  :commands (smartrep-read-event-loop))

(use-package evil
  :config

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
(use-package evil-quickscope)

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

(set-face-attribute 'font-lock-comment-face nil
                    :overline nil
                    :foreground (face-attribute 'shadow :foreground)
                    :background (face-attribute 'shadow :background))

(use-package alert
  :commands (alert)
  :config
  (setq alert-default-style 'libnotify))

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

(use-package powerline)

(use-package ycmd
  :commands (ycmd-hook ycmd-mode)
  :config
  (set-variable 'ycmd-server-command (list "ycmd"))
  (set-variable 'ycmd-global-config
                (concat rasen/dotfiles-directory ".nvim/.ycm_extra_conf.py")))

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
                                    :compile "stack build --nix"
                                    :test "stack build --nix --test")

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

(use-package color-identifiers-mode
  :defer 3
  :diminish (color-identifiers-mode
             global-color-identifiers-mode)
  :config
  (global-color-identifiers-mode))

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

(use-package org
  :mode ("\\.org$" . org-mode)
  :defer 5
  :bind (("C-c a" . org-agenda)
         ("C-c l" . org-store-link)
         ("C-c b" . org-iswitchb)
         :map evil-normal-state-map
         ("SPC o" . org-clock-out)
         ("SPC l" . org-clock-in-last)
         ("SPC j" . org-clock-goto)
         ("SPC c" . org-capture)
         ("SPC a" . org-agenda))
  :ensure org-plus-contrib
  :config

  ;; (setcdr (assoc "\\.pdf\\'" org-file-apps) "zathura %s")

  (require 'org-feed)
  (setq org-feed-default-template "* TODO %h\n:PROPERTIES:\n:CREATED: %U\n:END:\n%a")
  (setq org-feed-alist
        '(("Lambda the Ultimate"
           "http://lambda-the-ultimate.org/rss.xml"
           "~/org/feeds.org" "LtU")
          ("Reddit > Programming"
           "https://www.reddit.com/r/programming/top/.rss"
           "~/org/feeds.org" "/r/programming"
           :parse-feed org-feed-parse-atom-feed
           :parse-entry org-feed-parse-atom-entry)
          ("Reddit > Rust"
           "https://www.reddit.com/r/rust/top/.rss"
           "~/org/feeds.org" "/r/rust"
           :parse-feed org-feed-parse-atom-feed
           :parse-entry org-feed-parse-atom-entry)
          ("Reddit > Haskell"
           "https://www.reddit.com/r/haskell/top/.rss"
           "~/org/feeds.org" "/r/haskell"
           :parse-feed org-feed-parse-atom-feed
           :parse-entry org-feed-parse-atom-entry)
          ))

  ;; `org-update-dblock' narrows to the subtree. I hate this.
  (defun rasen/advice-org-update-dblock (f &rest r)
    (save-restriction (apply f r)))
  (advice-add 'org-update-dblock :around #'rasen/advice-org-update-dblock)

  (defun rasen/strip-url-from-title (title)
    (message "stripping: %s" title)
    (replace-regexp-in-string " \\[[^]]*\\]\\[[^]]*\\]$" "" title))

  ;; org-protocol
  (require 'org-protocol)

  ;; org-babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (dot . t)
                                 (shell . t)
                                 (ditaa . t)
                                 (plantuml . t)))
  (setq org-ditaa-jar-path "~/.nix-profile/lib/ditaa.jar")
  (setq org-plantuml-jar-path "~/.nix-profile/lib/plantuml.jar")

  (org-add-link-type "irfc" 'irfc-visit)

  ;; variable-pitch fonts
  (defun rasen/adjoin-to-list-or-symbol (element list-or-symbol)
    (let ((list (if (not (listp list-or-symbol))
                    (list list-or-symbol)
                  list-or-symbol)))
      (require 'cl-lib)
      (cl-adjoin element list)))

  (dolist (face '(org-code
                  org-block
                  org-block-begin-line
                  org-table
                  org-special-keyword
                  org-tag
                  ; org-block-background
                  ))
    (set-face-attribute
     face nil
     :inherit
     (rasen/adjoin-to-list-or-symbol
      'fixed-pitch
      (face-attribute face :inherit))))

  )

(use-package ob-async
  :after org)

(use-package worf
  :config
  )

(use-package org-download
  ;; TODO: bind screenshot command to <print> in org-mode
  :config
  (setq-default org-download-screenshot-method "scrot -s %s"))

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

(use-package irfc
  :commands (irfc-visit irfc-mode)
  :init
  (setq-default irfc-assoc-mode t)
  :config
  (setq irfc-directory "~/tmp"))

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

(use-package go-mode
  :mode "\\.go$")

;; (use-package tex-mode
;;   :ensure nil ; built-in
;;   :config
;;   (add-hook 'tex-mode-hook
;;             (lambda ()
;;               (visual-line-mode nil)
;;               )))

(use-package octave
  :commands (octave-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.m$" . octave-mode)))

(use-package web-mode
  :commands (web-mode)
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (setq-default js-indent 2
                js-indent-level 2
                js-switch-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-attr-indent-offset 2)
  :config
  (setq-default web-mode-content-types-alist
                '(("jsx" . ".*\\.js[x]?\\'")))
  )

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


(use-package notmuch
  :config
  (require 'org-notmuch)
  (evil-set-initial-state 'notmuch-search-mode 'motion)
  (evil-define-key 'motion notmuch-search-mode-map
    (kbd "q") 'notmuch-bury-or-kill-this-buffer
    (kbd "g r") 'notmuch-poll-and-refresh-this-buffer
    (kbd "RET") 'notmuch-search-show-thread
    (kbd "*") 'notmuch-search-tag-all
    (kbd "+") 'notmuch-search-add-tag
    (kbd "-") 'notmuch-search-remove-tag
    (kbd "t") 'notmuch-tag-jump
    (kbd "T") 'notmuch-search-filter-by-tag)
  (setq notmuch-search-oldest-first nil))

(use-package prodigy
  :init
  (prodigy-define-tag
   :name 'email
   :ready-message "Connected to server")
  (prodigy-define-service
   :name "imapnotify-gmail"
   :command "imapnotify"
   :args (list "-c" (expand-file-name "imapnotify-gmail-config.js" rasen/dotfiles-directory))
   :tags '(email)
   :kill-signal 'sigkill)
  (prodigy-define-service
   :name "imapnotify-kaaiot"
   :command "imapnotify"
   :args (list "-c" (expand-file-name "imapnotify-kaaiot-config.js" rasen/dotfiles-directory))
   :tags '(email)
   :kill-signal 'sigkill))

(use-package graphviz-dot-mode
  :mode "\\.dot$")

;; For use with "Edit with Emacs" chrome plugin.
;; https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh?hl=en
(use-package edit-server
  :demand ; no defer
  :bind (:map edit-server-edit-mode-map
         ("C-c C-k" . edit-server-abort))
  :config
  (edit-server-start))

(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-readable-p private-file)
    (load-file private-file)))
