;;;
;;; This file is auto-generated from "README.org"
;;;

(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook (defun rasen/restore-gc-threshold ()
                                (setq gc-cons-threshold 800000)))

(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(defun rasen/font-exists-p (font)
  "Check if the FONT exists."
  (and (display-graphic-p) (not (null (x-list-fonts font)))))

(defun rasen/set-my-fonts ()
  (let ((scale (if (eq system-type 'darwin) 2 1)))
    (cond
     ((rasen/font-exists-p "Iosevka")
      (set-face-attribute 'fixed-pitch nil :family "Iosevka" :height (* 80 scale) :width 'regular)
      (set-face-attribute 'default nil :family "Iosevka" :height (* 80 scale) :width 'regular))

     ((rasen/font-exists-p "IBM Plex Mono")
      (set-face-attribute 'fixed-pitch nil :family "IBM Plex Mono" :height 140)
      (set-face-attribute 'default nil :family "IBM Plex Mono" :height 140 :width 'regular))

     ((rasen/font-exists-p "Monaspace Neon")
      (set-face-attribute 'fixed-pitch nil :family "Monaspace Neon" :height 140 :width 'regular)
      (set-face-attribute 'default nil :family "Monaspace Neon" :height 140 :width 'regular))

     ((rasen/font-exists-p "Source Code Pro")
      (set-face-attribute 'fixed-pitch nil :family "Source Code Pro" :height 140 :width 'regular)
      (set-face-attribute 'default nil :family "Source Code Pro" :height 140))

     ((rasen/font-exists-p "Roboto Mono")
      (set-face-attribute 'fixed-pitch nil :family "Roboto Mono" :height 140)
      (set-face-attribute 'default nil :family "Roboto Mono" :height 140 :width 'regular))

     ((rasen/font-exists-p "DejaVu Sans Mono")
      (set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono" :height 140)
      (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 140)))

    (cond
     ((and (eq system-type 'darwin)
           (rasen/font-exists-p "Linux Libertine O"))
      (set-face-attribute 'variable-pitch nil :family "Linux Libertine O" :height 180))

     ((rasen/font-exists-p "Linux Libertine O")
      (set-face-attribute 'variable-pitch nil :family "Linux Libertine O" :height 90))
     ((rasen/font-exists-p "Vollkorn")
      (set-face-attribute 'variable-pitch nil :family "Vollkorn" :height 80))
     ((rasen/font-exists-p "DejaVu Sans")
      (set-face-attribute 'variable-pitch nil :family "DejaVu Sans")))))

(rasen/set-my-fonts)

(defun rasen/font-hook (frame)
  (select-frame frame)
  (rasen/set-my-fonts))

(add-hook 'after-make-frame-functions #'rasen/font-hook)

(require 'modus-themes)
(require 'modus-operandi-theme)
(require 'modus-vivendi-theme)

(setq modus-themes-italic-constructs t)
(setq modus-themes-bold-constructs t)

;; Use proportional fonts only when I explicitly configure them.
(setq modus-themes-no-mixed-fonts t)

(setq modus-themes-links '(faint background no-underline))
(setq modus-themes-org-agenda
      '((header-date . (workaholic bold-all))
        (scheduled . uniform)))

(load-theme 'modus-operandi t)

(modus-themes-with-colors
  (let ((custom--inhibit-theme-enable nil))
    (custom-theme-set-faces
     'modus-operandi

     `(rasen/org-project-face ((t :weight bold :background ,bg-dim)))

     `(rasen/agenda-date-header ((t :weight bold :foreground ,fg-dim)))

     `(fringe ((,c (:background ,bg-main :foreground ,fg-main))))

     ;; custom colors for vterm
     `(vterm-color-default ((,c (:background ,bg-main :foreground ,fg-main))))
     `(vterm-color-black ((,c (:background "#000" :foreground "#000"))))
     `(vterm-color-white ((,c (:background "#fff" :foreground "#fff"))))
     `(vterm-color-red ((,c (:background ,bg-red-intense :foreground ,red))))
     `(vterm-color-green ((,c (:background ,bg-green-intense :foreground ,green))))
     `(vterm-color-yellow ((,c (:background ,bg-yellow-intense :foreground ,yellow))))
     `(vterm-color-blue ((,c (:background ,bg-blue-intense :foreground ,blue))))
     `(vterm-color-magenta ((,c (:background ,bg-magenta-intense :foreground ,magenta))))
     `(vterm-color-cyan ((,c (:background ,bg-cyan-intense :foreground ,cyan))))

     ;; do not bold matches (this makes text jiggle with variable-pitch fonts)
     `(isearch ((,c (:inherit modus-themes-search-success))))
     `(query-replace ((,c :inherit modus-theme-intense-yellow)))

     ;; `(org-level-1 ((,c :inherit modus-themes-heading-1 :height 1.2 :background ,bg-cyan-subtle)))

     ;; Make TODOs bold
     `(org-todo ((,c :foreground ,magenta-warmer
                     :weight bold)))
     ;; `(org-table ((,c (:foreground ,fg-special-cold :inherit nil))))

     ;; Make tags stand out
     ;; `(org-tag ((,c :weight bold :foreground nil :background nil)))

     ;; dim done items
     `(org-done ((,c :foreground ,fg-alt)))
     `(org-headline-done ((,c :foreground ,fg-alt)))
     `(org-agenda-calendar-event ((,c))) ;; plain black is fine
     `(org-agenda-done ((,c :foreground ,fg-alt)))
     ;; remove special background from archived items
     `(org-archived ((,c (:foreground ,fg-alt))))

     `(org-link ((,c :foreground ,blue-warmer :background ,bg-dim :underline nil :inherit link)))
     `(rasen/org-id-link ((,c :foreground ,green-warmer :inherit org-link)))
     ))

  (setq org-todo-keyword-faces
        `(("TODO"     . (:foreground ,blue-intense :inherit fixed-pitch))
          ("NEXT"     . (:foreground ,red-intense  :inherit fixed-pitch))
          ("BUILD"    . (:foreground ,red-intense  :inherit fixed-pitch))
          ("WAIT"     . (:foreground ,magenta-warmer  :inherit fixed-pitch))
          ("DONE"     . (:foreground ,fg-dim       :inherit fixed-pitch))
          ("CANCELED" . (:foreground ,fg-dim       :inherit fixed-pitch)))))
