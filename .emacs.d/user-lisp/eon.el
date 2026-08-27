;;; eon.el --- Brief description -*- lexical-binding: t; -*-

;; Author: Oleksii Shmalko <rasen.dubi@gmail.com>
;; Maintainer: Oleksii Shmalko <rasen.dubi@gmail.com>
;; URL: https://github.com/rasendubi/eon
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, tools

;;; Commentary:

;; A brief description of what this package does and how to use it.
;; Optionally include usage examples and important notes.

;;; Code:

(defvar eon/states '(eon-motion-state
                     eon-normal-state
                     eon-insert-state)
  "List of eon states.")

(defvar eon-normal-state-entry-hook nil
  "Hook run when entering normal state.")

(defvar eon-insert-state-entry-hook nil
  "Hook run when entering insert state.")

(defvar-local eon-motion-state nil)
(defvar-local eon-normal-state nil)
(defvar-local eon-insert-state nil)

(defvar eon-motion-state-map
  (let ((keymap (make-sparse-keymap)))
    keymap)
  "Keymap for Eon motion state.")

(defvar eon-normal-state-map
  (let ((keymap (make-sparse-keymap)))
    (set-keymap-parent keymap eon-motion-state-map)
    (suppress-keymap keymap)
    keymap)
  "Keymap for Eon normal state.")

(defvar eon-insert-state-map
  (let ((keymap (make-sparse-keymap)))
    keymap))

(defvar eon-leader-map
  (let ((keymap (make-sparse-keymap)))
    keymap))

;;;###autoload
(define-minor-mode eon-mode
  "Eon minor mode."
  :lighter nil
  (if eon-mode
      (eon--enable)
    (eon--disable)))

;;;###autoload
(define-global-minor-mode eon-global-mode eon-mode
  (lambda ()
    (eon-mode 1))
  :group 'eon
  (if eon-global-mode
      (eon--global-enable)
    (eon--global-disable)))

(defun eon--enable ()
  "Enable Eon."
  (if (minibufferp)
      (eon-insert-state)
    (eon-normal-state)))

(defun eon--disable ()
  (dolist (state eon/states)
    (set state nil)))

(defun eon--global-enable ()
  (add-hook 'org-capture-mode-hook #'eon-insert-state))

(defun eon--global-disable ()
  (remove-hook 'org-capture-mode-hook #'eon-insert-state))

(defun eon-normal-state ()
  (interactive)
  (run-hooks 'eon-normal-state-entry-hook)
  (dolist (state eon/states)
    (set state nil))
  (setq-local eon-normal-state t)
  (setq-local cursor-type 'box))

(defun eon-insert-state ()
  (interactive)
  (dolist (state eon/states)
    (set state nil))
  (setq eon-insert-state t)
  (setq-local cursor-type 'bar)
  (run-hooks 'eon-insert-state-entry-hook))

(defun eon-insert ()
  "Switch to insert state. Deletes region if active."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (eon-insert-state))

(defun eon-kill (arg)
  (interactive "*p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (delete-char arg t)))

(define-key eon-normal-state-map (kbd "t") #'eon-insert)
(define-key eon-insert-state-map (kbd "<escape>") #'eon-normal-state)
(define-key eon-insert-state-map (kbd "ESC") nil)
(define-key eon-normal-state-map (kbd "h") #'eon-kill)


(defun eon-back-to-indentation ()
  "Same as `back-to-indentation' but with a repeat-map."
  (interactive "^")
  (back-to-indentation))
(put 'eon-back-to-indentation 'repeat-map
     (define-keymap "n" 'beginning-of-line))

(defun eon-mark ()
  (interactive)
  (if (region-active-p)
      (if (and (consp transient-mark-mode) (eq 'only (car transient-mark-mode)))
          (progn
            (setq transient-mark-mode (or (default-value 'transient-mark-mode) 'lambda))
            (when (called-interactively-p 'interactive)
              (message "Permanent mark")))
        (exchange-point-and-mark))
    (push-mark nil nil t)))

;; The reason we need `eon--execute-kbd-macro' is because binding keys
;; to macros directly does not properly set `this-command', so some
;; commands behave differently (e.g., next/prev line don't remember
;; target column).
(defun eon--execute-kbd-macro (kbd-macro &rest args)
  "Execute KBD-MACRO."
  (when-let* ((ret (if (stringp kbd-macro)
                       ;; Disable eon states during lookup, so eon keybindings don't interfere with lookup.
                       (let ((eon-normal-state nil)
                             (eon-motion-state nil)
                             (eon-insert-state nil))
                         (key-binding (read-kbd-macro kbd-macro)))
                     kbd-macro)))
    (cond
     ((commandp ret t)
      (setq this-original-command ret) ; god-mode sets it. Do I need it?
      (setq this-command ret)
      (setq real-this-command ret)
      (when (plist-get args :shift-translate)
        (setq this-command-keys-shift-translated t))
      (call-interactively ret))

     ((keymapp ret)
      (set-transient-map ret nil nil))
     ((and (symbolp ret) (keymapp (symbol-value ret)))
      (set-transient-map (symbol-value ret) nil nil))

     (t (execute-kbd-macro ret)))))

(repeat-mode 1)

(defmacro eon--wrap-kbd (kbd-macro &rest args)
  `(defun ,(make-symbol (concat "eon--wrapped/" (if (symbolp kbd-macro) (symbol-name kbd-macro) kbd-macro))) ()
     (interactive)
     (eon--execute-kbd-macro ',kbd-macro ,@args)))

(defun eon-yank ()
  "Similar to `yank' but replaces the current selection."
  (interactive "*")
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (call-interactively #'yank))
(defun eon-yank-pop ()
  "Similar to `yank-pop' but replaces the current selection."
  (interactive "*")
  (when (region-active-p)
    (delete-region (region-beginning) (region-end)))
  (call-interactively #'yank-pop))

(defun eon-quit ()
  (interactive)
  (cond ((region-active-p) (setq deactivate-mark t))
        ((minibufferp) (abort-minibuffers))
        (t (keyboard-quit))))

(pcase-dolist (`(,from . ,target) `(("<escape>" eon-quit)

                                    ("u" "C-p")
                                    ("U" "C-p" :shift-translate t)
                                    ("e" "C-n")
                                    ("E" "C-n" :shift-translate t)
                                    ("n" "C-b")
                                    ("N" "C-b" :shift-translate t)
                                    ("o" "C-f")
                                    ("O" "C-f" :shift-translate t)
                                    ("f" "M-b")
                                    ("F" "M-b" :shift-translate t)
                                    ("p" "M-f")
                                    ("P" "M-f" :shift-translate t)
                                    ("y" "C-<up>")
                                    ("Y" "C-<up>" :shift-translate t)
                                    ("i" "C-<down>")
                                    ("I" "C-<down>" :shift-translate t)

                                    ("a" execute-extended-command)
                                    ("x" "C-x")
                                    ("m" kill-ring-save)
                                    ("c" nil)

                                    ("SPC n" eon-back-to-indentation)
                                    ("SPC o" "C-e")
                                    ("SPC O" "C-e" :shift-translate t)
                                    ("SPC u" "M-<")
                                    ("SPC U" "M-<" :shift-translate t)
                                    ("SPC e" "M->")
                                    ("SPC E" "M->" :shift-translate t)

                                    ("SPC p" project-prefix-map)
                                    ("SPC f" find-file)

                                    ;; registers and bookmarks
                                    ("r" "C-x r")
                                    ;; r SPC — point to register
                                    ;; r w — window configuration to register
                                    ;; r j — jump to register
                                    ;;
                                    ;; r m — set bookmark
                                    ;; r b — jump to bookmark
                                    ;;
                                    ;; r s — copy to register
                                    ;; r i — insert register

                                    ("\\ \\" org-roam-node-find)
                                    ("\\ <" org-roam-dailies-goto-previous-note)
                                    ("\\ >" org-roam-dailies-goto-next-note)
                                    ;; ("\\ ," org-roam-dailies-capture-yesterday)
                                    ;; ("\\ ." rasen/org-daily-dispatch)
                                    ("<f2>" rasen/org-daily-dispatch)
                                    ("\\ n" rasen/org-roam-new-node)
                                    ("\\ c" org-capture)
                                    ("\\ a" org-agenda)
                                    ("\\ o" rasen/org-clock-out-or-last)

                                    ;; TODO:
                                    ;; - find place for kill-whole-line
                                    ;; - C-c
                                    ("s a" eglot-code-actions)
                                    ("s r" eglot-rename)
                                    ;; - save mark, save window config (?)
                                    ;; - save buffer / some buffer
                                    ("k" avy-goto-char)
                                    ("K" avy-goto-char-2)

                                    ("'" eon-mark)))
  (define-key eon-motion-state-map (kbd from) (eval `(eon--wrap-kbd ,@target))))

(defun rasen/org-clock-out-or-last ()
  (interactive)
  (if (org-clocking-p)
      (org-clock-out)
    (org-clock-in-last '(4))))

(define-key eon-normal-state-map (kbd "c") #'eon-yank)
(define-key eon-normal-state-map (kbd "C") #'eon-yank-pop)

(add-to-ordered-list 'emulation-mode-map-alists `((eon-normal-state . ,eon-normal-state-map)
                                                  (eon-motion-state . ,eon-motion-state-map)
                                                  (eon-insert-state . ,eon-insert-state-map))
                     0)

(with-eval-after-load 'magit
  (define-key magit-mode-map (kbd "C-<down>") #'magit-section-forward-sibling)
  (define-key magit-mode-map (kbd "C-<up>") #'magit-section-backward-sibling))

;; yanking to replace region.
;; (delete-selection-mode 1)

(provide 'eon)

;;; eon.el ends here

(global-set-key (kbd "s-v") #'split-window-right)
(global-set-key (kbd "s-s") #'split-window-below)


(defun local/project-try-explicit (dir)
  "Find a super-directory of DIR containing a root file."
  (locate-dominating-file dir ".root"))

(cl-defmethod project-root ((project string))
  project)

(with-eval-after-load 'project
  (add-hook 'project-find-functions #'local/project-try-explicit)
  (add-hook 'project-find-functions #'project-try-vc))

(defun rasen/reset-project (dir)
  (interactive "D")
  (message "resetting %s" dir)
  (when dir
    (vc-file-setprop dir 'project-vc nil)
    (rasen/reset-project (file-name-parent-directory dir))))

;;; Input Method Management

(defvar-local eon--saved-input-method nil
  "Saved input method for restoration when entering insert state.")

(defun eon--save-input-method ()
  "Save the current input method when leaving insert state."
  (when eon-insert-state
    (setq-local eon--saved-input-method current-input-method)
    (when current-input-method
      (deactivate-input-method))))

(defun eon--restore-input-method ()
  "Restore the saved input method when entering insert state."
  (when eon--saved-input-method
    (activate-input-method eon--saved-input-method)))

(add-hook 'eon-normal-state-entry-hook #'eon--save-input-method)
(add-hook 'eon-insert-state-entry-hook #'eon--restore-input-method)
