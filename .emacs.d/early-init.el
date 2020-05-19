;; [[file:~/dotfiles/emacs.org::*GC hacks][GC hacks:1]]
;;;
;;; This file is auto-generated from "emacs.org"
;;;
(setq gc-cons-threshold most-positive-fixnum)
;; GC hacks:1 ends here
;; [[file:~/dotfiles/emacs.org::*package][package:1]]
(require 'package)
(setq package-archives nil)
(setq package-enable-at-startup nil)
;; package:1 ends here
;; [[file:~/dotfiles/emacs.org::*Remove the clutter][Remove the clutter:1]]
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; Remove the clutter:1 ends here
;; [[file:~/dotfiles/emacs.org::*Color theme][Color theme:1]]
(require 'modus-operandi-theme)

(setq modus-operandi-theme-slanted-constructs t)
(setq modus-operandi-theme-bold-constructs t)

;; Use proportional fonts only when I explicitly configure them.
(setq modus-operandi-theme-proportional-fonts nil)

(load-theme 'modus-operandi t)

(let ((class '((class color) (min-colors 89)))
      (fg-main "#000000") (bg-main "#ffffff")
      (fg-alt "#505050") (bg-alt "#f3f1f3")
      (fg-dim "#282828") (bg-dim "#f8f8f8")
      ;; specifically for on/off states (e.g. `mode-line')
      ;;
      ;; must be combined with themselves
      (fg-active "#191919") (bg-active "#e0e0e0")
      (fg-inactive "#424242") (bg-inactive "#efedef")
      ;; special base values, used only for cases where the above
      ;; fg-* or bg-* cannot or should not be used (to avoid confusion)
      ;; must be combined with: {fg,bg}-{main,alt,dim}
      (fg-special-cold "#093060") (bg-special-cold "#dde3f4")
      (fg-special-mild "#184034") (bg-special-mild "#c4ede0")
      (fg-special-warm "#5d3026") (bg-special-warm "#f0e0d4")
      (fg-special-calm "#61284f") (bg-special-calm "#f8ddea")
      ;; styles for the main constructs
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      (red "#a80000") (green "#005200")
      (yellow "#8b3800") (blue "#0030a6")
      (magenta "#721045") (cyan "#005589")
      ;; styles for common, but still specialised constructs
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      (red-alt "#880000") (green-alt "#4a5700")
      (yellow-alt "#714900") (blue-alt "#223fbf")
      (magenta-alt "#8f0075") (cyan-alt "#185870")
      ;; same purpose as above, just slight differences
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      (red-alt-other "#9d2020") (green-alt-other "#145a00")
      (yellow-alt-other "#804000") (blue-alt-other "#0000bb")
      (magenta-alt-other "#5317ac") (cyan-alt-other "#005a68")
      ;; styles for elements that should be very subtle
      ;;
      ;; must be combined with: `bg-main', `bg-alt', `bg-dim'
      (red-nuanced "#4d0006") (green-nuanced "#003000")
      (yellow-nuanced "#3a2a00") (blue-nuanced "#001170")
      (magenta-nuanced "#381050") (cyan-nuanced "#003434")
      ;; styles for elements that should draw attention to themselves
      ;;
      ;; must be combined with: `bg-main'
      (red-intense "#b60000") (green-intense "#006800")
      (yellow-intense "#904200") (blue-intense "#1111ee")
      (magenta-intense "#7000e0") (cyan-intense "#205b93")
      ;; styles for background elements that should be visible yet
      ;; subtle
      ;;
      ;; must be combined with: `fg-dim'
      (red-subtle-bg "#f2b0a2") (green-subtle-bg "#aecf90")
      (yellow-subtle-bg "#e4c340") (blue-subtle-bg "#b5d0ff")
      (magenta-subtle-bg "#f0d3ff") (cyan-subtle-bg "#c0efff")
      ;; styles for background elements that should be visible and
      ;; distinguishable
      ;;
      ;; must be combined with: `fg-main'
      (red-intense-bg "#ff8892") (green-intense-bg "#5ada88")
      (yellow-intense-bg "#f5df23") (blue-intense-bg "#6aaeff")
      (magenta-intense-bg "#d5baff") (cyan-intense-bg "#42cbd4")
      ;; styles for refined git diffs and other contexts where both the
      ;; foreground and the background need to have the same/similar hue
      ;;
      ;; must be combined with themselves OR the foregrounds can be
      ;; combined with any of the base backgrounds
      (red-refine-bg "#ffcccc") (green-refine-bg "#aceaac")
      (yellow-refine-bg "#fff29a") (blue-refine-bg "#8ac7ff")
      (magenta-refine-bg "#ffccff") (cyan-refine-bg "#8eecf4")
      (red-refine-fg "#780000") (green-refine-fg "#004c00")
      (yellow-refine-fg "#604000") (blue-refine-fg "#002288")
      (magenta-refine-fg "#770077") (cyan-refine-fg "#004850")
      ;; styles that are meant exclusively for the mode line
      ;;
      ;; must be combined with: `bg-active', `bg-inactive'
      (red-active "#930000") (green-active "#005300")
      (yellow-active "#703700") (blue-active "#0033c0")
      (magenta-active "#6320a0") (cyan-active "#004882")

      ;; styles reserved for specific faces
      ;;
      ;; `bg-hl-line' is between `bg-dim' and `bg-alt', so it should
      ;; work with all accents that cover those two, plus `bg-main'
      ;;
      ;; `bg-header' is between `bg-active' and `bg-inactive', so it
      ;; can be combined with any of the "active" values, plus the
      ;; "special" and base foreground colours
      ;;
      ;; `bg-region' must be combined with `fg-main'
      ;;
      ;; the window divider colours apply to faces with just an fg value
      ;;
      ;; all other pairs are combinable with themselves
      (bg-hl-line "#f1f2f6")
      (bg-region "#bcbcbc")
      (fg-window-divider-inner "#888888")
      (fg-window-divider-outer "#585858")
      (fg-header "#2a2a2a") (bg-header "#e5e5e5")
      (fg-whitespace "#645060") (bg-whitespace "#fff8fc")
      (fg-paren-match "#222222") (bg-paren-match "#deb8af"))

  (custom-theme-set-faces
   'modus-operandi

   ;; do not bold matches (this makes text jiggle with variable-pitch fonts
   `(isearch ((,class (:inherit modus-theme-intense-green))))
   `(query-replace ((,class :inherit modus-theme-intense-yellow)))
   `(show-paren-match ((,class (:background ,bg-paren-match :foreground ,fg-paren-match))))

   `(flyspell-duplicate
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,yellow)))
      (,class (:underline ,yellow))))
   `(flyspell-incorrect
     ((,(append '((supports :underline (:style wave))) class)
       (:underline (:style wave :color ,red)))
      (,class (:underline ,red))))

   `(italic ((,class :slant italic)))

   `(org-todo ((,class :foreground ,magenta-alt-other
                       :weight bold)))
   `(org-archived ((,class (:foreground ,fg-alt))))
   `(org-done ((,class :foreground ,fg-alt)))
   `(org-headline-done ((,class :foreground ,fg-alt)))
   `(org-agenda-done ((,class :foreground ,fg-alt)))
   `(org-link ((,class :background ,bg-alt :underline nil :inherit link)))
   `(org-roam-link ((,class :foreground ,green-alt-other
                            :inherit org-link)))
   ;; remove special styling from backlinks
   `(org-roam-backlink ((,class))))

  (setq org-todo-keyword-faces
        `(("TODO"     . (:foreground ,blue-intense :inherit fixed-pitch))
          ("NEXT"     . (:foreground ,red-intense  :inherit fixed-pitch))
          ("BUILD"    . (:foreground ,red-intense  :inherit fixed-pitch))
          ("WAIT"     . (:foreground ,magenta-alt  :inherit fixed-pitch))
          ("DONE"     . (:foreground ,fg-alt       :inherit fixed-pitch))
          ("CANCELED" . (:foreground ,fg-alt       :inherit fixed-pitch)))))
;; Color theme:1 ends here
;; [[file:~/dotfiles/emacs.org::*Fonts][Fonts:1]]
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
;; Fonts:1 ends here
;; [[file:~/dotfiles/emacs.org::*Fonts][Fonts:2]]
(defun rasen/font-hook (frame)
  (select-frame frame)
  (rasen/set-my-fonts))

(add-hook 'after-make-frame-functions 'rasen/font-hook)
;; Fonts:2 ends here
