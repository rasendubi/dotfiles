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
   ((rasen/font-exists-p "Input") ; check for custom four-family font first
    (set-face-attribute 'fixed-pitch nil :family "Input" :height 65)
    (set-face-attribute 'default nil :family "Input" :height 65))
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

(require 'modus-operandi-theme)

(setq modus-operandi-theme-slanted-constructs t)
(setq modus-operandi-theme-bold-constructs t)

;; Use proportional fonts only when I explicitly configure them.
(setq modus-operandi-theme-proportional-fonts nil)

(load-theme 'modus-operandi t)

(modus-operandi-theme-with-color-variables
  (let ((custom--inhibit-theme-enable nil))
    (custom-theme-set-faces
     'modus-operandi

     `(vterm-color-default ((,class (:background ,bg-main :foreground ,fg-main))))
     `(vterm-color-black ((,class (:background "#000" :foreground "#000"))))
     `(vterm-color-white ((,class (:background "#fff" :foreground "#fff"))))
     `(vterm-color-red ((,class (:background ,red-intense-bg :foreground ,red))))
     `(vterm-color-green ((,class (:background ,green-intense-bg :foreground ,green))))
     `(vterm-color-yellow ((,class (:background ,yellow-intense-bg :foreground ,yellow))))
     `(vterm-color-blue ((,class (:background ,blue-intense-bg :foreground ,blue))))
     `(vterm-color-magenta ((,class (:background ,magenta-intense-bg :foreground ,magenta))))
     `(vterm-color-cyan ((,class (:background ,cyan-intense-bg :foreground ,cyan))))

     ;; do not bold matches (this makes text jiggle with variable-pitch fonts
     `(isearch ((,class (:inherit modus-theme-intense-green))))
     `(query-replace ((,class :inherit modus-theme-intense-yellow)))
     ;; `(show-paren-match ((,class (:background ,bg-paren-match :foreground ,fg-main))))

     ;; do not change background/foreground — just add wiggly line
     `(flyspell-duplicate
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,yellow)))
        (,class (:underline ,yellow))))
     `(flyspell-incorrect
       ((,(append '((supports :underline (:style wave))) class)
         (:underline (:style wave :color ,red)))
        (,class (:underline ,red))))

     ;; do not change foreground for italics
     `(italic ((,class :slant italic)))

     ;; Make TODOs bold
     `(org-todo ((,class :foreground ,magenta-alt-other
                         :weight bold)))
     `(org-archived ((,class (:foreground ,fg-alt))))
     `(org-done ((,class :foreground ,fg-alt)))
     `(org-headline-done ((,class :foreground ,fg-alt)))
     `(org-agenda-done ((,class :foreground ,fg-alt)))
     `(org-date ((,class :foreground ,cyan-alt :underline nil)))

     `(org-link ((,class :foreground ,blue-alt :background ,bg-alt :underline nil :inherit link)))
     `(org-roam-link ((,class :foreground ,green-alt :inherit org-link)))))

  (setq org-todo-keyword-faces
        `(("TODO"     . (:foreground ,blue-intense :inherit fixed-pitch))
          ("NEXT"     . (:foreground ,red-intense  :inherit fixed-pitch))
          ("BUILD"    . (:foreground ,red-intense  :inherit fixed-pitch))
          ("WAIT"     . (:foreground ,magenta-alt  :inherit fixed-pitch))
          ("DONE"     . (:foreground ,fg-alt       :inherit fixed-pitch))
          ("CANCELED" . (:foreground ,fg-alt       :inherit fixed-pitch)))))
