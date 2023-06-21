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
  (cond
   ((and (eq system-type 'darwin)
         (rasen/font-exists-p "Input"))
    (set-face-attribute 'fixed-pitch nil :family "Input" :height 140)
    (set-face-attribute 'default nil :family "Input" :height 140))

   ((rasen/font-exists-p "Input") ; check for custom four-family font first
    (set-face-attribute 'fixed-pitch nil :family "Input" :height 70)
    (set-face-attribute 'default nil :family "Input" :height 70))
   ((rasen/font-exists-p "Input Mono")
    (set-face-attribute 'fixed-pitch nil :family "Input Mono" :height 65)
    (set-face-attribute 'default nil :family "Input Mono" :height 65))
   ((rasen/font-exists-p "Fira Code Retina")
    (set-face-attribute 'fixed-pitch nil :family "Fira Code Retina" :height 65)
    (set-face-attribute 'default nil :family "Fira Code Retina" :height 65))
   ((rasen/font-exists-p "Terminess Powerline")
    (set-face-attribute 'fixed-pitch nil :family "Terminess Powerline" :height 160)
    (set-face-attribute 'default nil :family "Terminess Powerline" :height 160))
   ((rasen/font-exists-p "Terminus")
    (set-face-attribute 'fixed-pitch nil :family "Terminus" :height 160)
    (set-face-attribute 'default nil :family "Terminus" :height 160)))

  (cond
   ((and (eq system-type 'darwin)
         (rasen/font-exists-p "Linux Libertine O"))
    (set-face-attribute 'variable-pitch nil :family "Linux Libertine O" :height 180))

   ((rasen/font-exists-p "Linux Libertine O")
    (set-face-attribute 'variable-pitch nil :family "Linux Libertine O" :height 90))
   ((rasen/font-exists-p "Vollkorn")
    (set-face-attribute 'variable-pitch nil :family "Vollkorn" :height 80))
   ((rasen/font-exists-p "DejaVu Sans")
    (set-face-attribute 'variable-pitch nil :family "DejaVu Sans"))))

(rasen/set-my-fonts)

(defun rasen/font-hook (frame)
  (select-frame frame)
  (rasen/set-my-fonts))

(add-hook 'after-make-frame-functions #'rasen/font-hook)

(require 'modus-themes)
(require 'modus-operandi-theme)

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

     ;; Make TODOs bold
     `(org-todo ((,c :foreground ,magenta-warmer
                     :weight bold)))
     ;; `(org-table ((,c (:foreground ,fg-special-cold :inherit nil))))

     ;; Make tags stand out
     `(org-tag ((,c :foreground ,fg-main :background ,bg-yellow-intense)))

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
