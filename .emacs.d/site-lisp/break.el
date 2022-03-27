;;; break.el --- A mode to remind you to take breaks regularly.

;; Author: Alexey Shmalko <rasen.dubi@gmail.com>
;; URL:
;; Created: December 10, 2021
;; Version: 1.0.0
;; Package-Requires: ()

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

;;;; Options
(defgroup break nil
  "Customization option for break-mode"
  :group 'environment
  :prefix "break-")

(defcustom break-work-length 30
  "The length of work interval in minutes."
  :group 'break
  :type 'integer)

(defcustom break-rest-length 5
  "The minimum length of the rest in minutes."
  :group 'break
  :type 'integer)

(define-minor-mode break-mode
  "A mode to remind you about taking breaks regularly."
  :init-value nil
  :global t
  (if break-mode
      (break-mode--mode-setup)
    (break-mode--mode-teardown)))

(defun break-mode--mode-setup ()
  (break--start-work-timer)
  (break--mode-line-setup))
(defun break-mode--mode-teardown ()
  (break--stop-work-timer)
  (break--stop-rest-timer)
  (break--mode-line-teardown))

(defmacro break--deftimer (name default-interval)
  (let (($timer (intern (concat "break--" (symbol-name name) "-timer")))
        ($start (intern (concat "break--start-" (symbol-name name) "-timer")))
        ($stop  (intern (concat "break--stop-" (symbol-name name) "-timer")))
        ($on    (intern (concat "break--on-" (symbol-name name) "-timer"))))
    `(progn
       (defvar ,$timer nil)
       (defun ,$start (&optional interval)
         (let ((interval (or interval (* ,default-interval 60))))
           (,$stop)
           (setq ,$timer (run-at-time interval nil #',$on))))
       (defun ,$stop ()
         (when ,$timer
           (cancel-timer ,$timer)
           (setq ,$timer nil))))))

(break--deftimer work break-work-length)
(break--deftimer rest break-rest-length)

(defun break--on-work-timer ()
  (break--stop-work-timer)
  (break--start-rest))

(defun break--on-rest-timer ()
  (break--stop-rest-timer)
  (break--stop-rest))

(defvar break--saved-global-map nil)
(defvar break--saved-frame-configuration nil)
(defun break--save-state ()
  (unless (eq (current-global-map) break-rest-mode-map)
    (setq break--saved-global-map (current-global-map)))
  (setq break--saved-frame-configuration (current-frame-configuration)))
(defun break--restore-state ()
  ;; `when'’s are here for a safety check
  (when break--saved-global-map
    (use-global-map break--saved-global-map)
    (setq break--saved-global-map nil))
  (when break--saved-frame-configuration
    ;; BUG: with EXWM, the restored configuration seems to be slightly
    ;; off. For me, the selected frame is slightly higher and goes off
    ;; screen.
    (set-frame-configuration break--saved-frame-configuration)))

(defun break--start-rest ()
  (interactive)
  (break--save-state)
  (break--start-rest-timer)
  (break--show-rest-buffer)
  ;; This should be new `make-sparse-keymap', but setting it to
  ;; `break-rest-mode-map' is safer---at least it has an escape hatch.
  (use-global-map break-rest-mode-map))
(defun break--stop-rest ()
  (break--show-rest-end-buffer))

(defvar break--frame nil)
(defun break--frame ()
  (when (not (and break--frame (frame-live-p break--frame)))
    (setq break--frame (make-frame '((name . "Take a break")
                                     (minibuffer . nil)
                                     (fullscreen . fullboth)
                                     ;; disable tab-bar on this frame
                                     (tab-bar-lines . 0)
                                     (internal-border-width . 320)
                                     (vertical-scroll-bars . nil)
                                     (horizontal-scroll-bars . nil)
                                     (tool-bar-lines . 0)))))
  break--frame)

(defun break-rest-quit ()
  "Quit from the current break."
  (interactive)

  ;; restore state first thing, so if anything below fails, we still
  ;; have restored the global keymap.
  (break--restore-state)

  (message "break-rest-quit")
  (kill-buffer "*break-rest*")
  (delete-frame break--frame 'force)
  (break--stop-rest-timer)
  (break--start-work-timer))

(defvar break-rest-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'break-rest-quit)
    map))
(define-derived-mode break-rest-mode special-mode "break-rest"
  "Major mode for the rest screen of `break-mode'."
  (set-keymap-parent break-rest-mode-map nil))
(defun break--show-rest-buffer ()
  (let ((buffer (get-buffer-create "*break-rest*")))
    (with-current-buffer buffer
      (break-rest-mode))
    (break--build-rest-buffer buffer)
    (break--maximize-buffer buffer)))
(defun break--show-rest-end-buffer ()
  (with-current-buffer (get-buffer-create "*break-rest*")
    (break-rest-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Your break has ended\n")
      (insert "\n")
      (insert "You can return to the work."))
    (break--maximize-buffer (current-buffer))))

(defun break--rest-buffer-update ()
  (when-let ((buffer (get-buffer "*break-rest*")))
    (if break--rest-timer
        ;; rest is still in progress
        (break--build-rest-buffer buffer)
      ;; rest-end buffer doesn't need updates
      ;; (break--build-rest-end-buffer buffer)
      )))
(defun break--build-rest-buffer (buffer)
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "It's time to take a break\n")
      (insert "\n")
      (insert "Break ends in ") (insert (break--timer-to-string break--rest-timer)) (insert "\n")
      (insert "\n")
      (insert "`q' to quit"))))

(defun break--maximize-buffer (buffer)
  (let* ((frame (break--frame))
         (window (frame-selected-window frame)))
    (delete-other-windows window)
    (set-window-buffer window buffer)
    (set-window-dedicated-p window t)
    (select-frame-set-input-focus frame)
    ;; hide cursor
    (internal-show-cursor window nil)

    (with-current-buffer buffer
      (when (fboundp 'visual-fill-column-mode)
        (visual-fill-column-mode))
      ;; hide mode line
      (setq-local mode-line-format nil)
      (when (fboundp 'turn-off-evil-mode)
        ;; turn off evil mode in rest buffer
        (turn-off-evil-mode)))))

(defvar break--saved-work-timer nil)
(defun break-pause ()
  (interactive)
  (message "break-pause")
  (when break--work-timer
    (setq break--saved-work-timer
          (- (timer-until break--work-timer (current-time))))
    (break--stop-work-timer)))
(defun break-resume ()
  (interactive)
  (message "break-resume")
  (unless break--work-timer
    (break--start-work-timer break--saved-work-timer)
    (setq break--saved-work-timer nil)))
;; TODO: make this break-away / break-here.
;;
;; Reset work timer if away for more than rest duration, otherwise
;; resume work timer.
;;
;; Potentially, stop rest.

;; TODO: add a command for reset

;;;; Mode-line
(defvar break--mode-line-string "")
(put 'break--mode-line-string 'risky-local-variable t)

(defvar break--rest-timeout-string "")

(defvar break--mode-line-timer nil)

(defun break--timer-to-string (timer)
  (let* ((time (- (timer-until timer (current-time))))
         (minutes (/ (floor time) 60))
         (seconds (% (floor time) 60)))
    (format "%d:%02d" minutes seconds)))

(defun break--mode-line-setup ()
  (break--mode-line-teardown)

  (if (listp global-mode-string)
      (unless (memq 'break--mode-line-string global-mode-string)
        (push 'break--mode-line-string global-mode-string)
        (push "" global-mode-string))

    (setq global-mode-string
          (list "" 'break--mode-line-string global-mode-string)))

  (setq break--mode-line-timer (run-at-time 0 1 #'break--mode-line-update))
  (break--mode-line-update))

(defun break--mode-line-teardown ()
  (when break--mode-line-timer
    (cancel-timer break--mode-line-timer)
    (setq break--mode-line-timer nil))
  (setq break--mode-line-string ""))

(defun break--mode-line-update ()
  (if break--work-timer
      (setq break--mode-line-string
            (format " [%s] " (break--timer-to-string break--work-timer)))
    (setq break--mode-line-string ""))
  (break--rest-buffer-update)
  (force-mode-line-update))

;; (break--mode-line-update)
;; (break--mode-line-setup)


(provide 'break)
;;; break.el ends here

;; (break-rest-quit)
;; (break--start-rest)
;; (break--show-rest-buffer)
;; (break--start-work-timer)
;; (break--stop-work-timer)
;; (general-def 'local "<f8>" #'eval-buffer)

(message "break.el reloaded")

;; (exwm-workspace--update-workareas)
;; exwm-workspace--list
;; (setq exwm-workspace--list (remove-if-not #'frame-live-p exwm-workspace--list))
