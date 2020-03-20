;;; org-variable-pitch.el --- Minor mode for variable pitch text in org mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018, 2019  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: faces
;; Version: 1.1.0
;; URL: https://github.com/cadadr/elisp/blob/master/org-variable-pitch.el
;; Package-Requires: ((emacs "25"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Variable-pitch support for org-mode.  This minor mode enables
;; ‘variable-pitch-mode’ in the current Org-mode buffer, and sets some
;; particular faces up so that they are are rendered in fixed-width
;; font.  Also, indentation, list bullets and checkboxes are displayed
;; in monospace, in order to keep the shape of the outline.

;;; Installation:

;; Have this file somewhere in the load path, then:

;;   (require 'org-variable-pitch)
;;   (add-hook 'org-mode-hook 'org-variable-pitch-minor-mode)

;;; Configurables:

;;   - ‘org-variable-pitch-fixed-font’: The font used for parts of the
;;     buffer to be kept in fixed-width font.

;;   - ‘org-variable-pitch-fixed-faces’: List of org-mode faces to
;;     keep monospace.


;;; Code:

(require 'org)
(require 'rx)

(defgroup org-variable-pitch nil
  "Customisations for ‘org-variable-pitch-minor-mode’."
  :group 'org
  :prefix "org-variable-pitch-")

(defcustom org-variable-pitch-fixed-font "Monospace"
  "Monospace font to use with ‘org-variable-pitch-minor-mode’."
  :group 'org-variable-pitch
  :type 'string
  :risky t)

(defcustom org-variable-pitch-fixed-faces
  `(org-block
    org-block-begin-line
    org-block-end-line
    org-checkbox
    org-code
    org-date
    org-document-info-keyword
    org-done
    org-drawer
    org-formula
    ;; org-indent
    org-meta-line
    org-property-value
    org-special-keyword
    org-table
    org-tag
    org-todo
    org-verbatim
    ,@org-level-faces)
  "Faces to keep fixed-width when using ‘org-variable-pitch-minor-mode’."
  :group 'org-variable-pitch
  :type '(repeat symbol))

(defcustom org-variable-pitch-fontify-headline-prefix nil
  "Fontify the headline prefix.
When non-nil, headline prefix will use the monospace face.
Otherwise the headline will use the default `org-level-*' face.

Note that this will drop all `org-level-*' face styles and only
apply the monospace face to the headline prefix."
  :group 'org-variable-pitch
  :type 'boolean)

(defface org-variable-pitch-face
  `((t . (:family ,org-variable-pitch-fixed-font)))
  "Face for initial space and list item bullets.
This face is used to keep them in monospace when using
‘org-variable-pitch-minor-mode’."
  :group 'org-variable-pitch)

(defvar org-variable-pitch-font-lock-keywords)
(defvar org-variable-pitch-headline-font-lock-keywords)
(let ((code '(0 (put-text-property
                 (match-beginning 0)
                 (match-end 0)
                 'face 'org-variable-pitch-face))))
  (setq
   org-variable-pitch-font-lock-keywords
   `((,(rx bol (1+ blank))
      ,code)
     (,(rx bol (0+ blank)
           (or (: (or (+ digit) letter) (in ".)"))
               (: (or (in "-+") (1+ blank "\*"))
                (opt blank "[" (in "-X ") "]")))
           blank)
      ,code))
   org-variable-pitch-headline-font-lock-keywords
   `((,(rx bol (1+ "\*") blank)
      ,code))))


(defvar org-variable-pitch--cookies nil
  "Face remappings to restore when the minor mode is deactivated")

;;;###autoload
(define-minor-mode org-variable-pitch-minor-mode
  "Set up the buffer to be partially in variable pitch.
Keeps some elements in fixed pitch in order to keep layout."
  nil " OVP" nil
  (set-face-attribute 'org-variable-pitch-face nil :family org-variable-pitch-fixed-font)
  (if org-variable-pitch-minor-mode
      (progn
        (variable-pitch-mode 1)
        (dolist (face org-variable-pitch-fixed-faces)
          (if (facep face)
              (push (face-remap-add-relative face 'org-variable-pitch-face)
                    org-variable-pitch--cookies)
            (message "‘%s’ is not a valid face, thus OVP skipped it"
                     (symbol-name face))))
        (font-lock-add-keywords nil org-variable-pitch-font-lock-keywords)
        (when org-variable-pitch-fontify-headline-prefix
          (font-lock-add-keywords nil org-variable-pitch-headline-font-lock-keywords)))
    (variable-pitch-mode -1)
    (mapc #'face-remap-remove-relative org-variable-pitch--cookies)
    (setq org-variable-pitch--cookies nil)
    (font-lock-remove-keywords nil org-variable-pitch-font-lock-keywords)
    (font-lock-remove-keywords nil org-variable-pitch-headline-font-lock-keywords))
  (font-lock-ensure))

;;; Footer:

(provide 'org-variable-pitch)
;;; org-variable-pitch.el ends here
