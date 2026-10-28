;;; teleprompter.el --- Turn any buffer into a teleprompter -*- lexical-binding: t; -*-

;; Copyright (C) 2026  yottanami

;; Author: yottanami <yottanami@gnu.org>
;; URL: https://github.com/yottanami/teleprompter.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multimedia, convenience

;; This file is not part of GNU Emacs.

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

;; `teleprompter-mode' is a buffer-local minor mode that turns any text
;; buffer into a teleprompter: large text on a black background with
;; timer-driven auto-scrolling and play/pause control.
;;
;; Usage: open your script, M-x teleprompter-mode, then:
;;
;;   SPC          play / pause
;;   <up> / f     scroll faster
;;   <down> / s   scroll slower
;;   + / -        text bigger / smaller
;;   <home>       jump back to the top
;;   q            quit teleprompter mode
;;
;; The buffer is read-only while the mode is active so the plain-letter
;; keys never insert text; turning the mode off restores the buffer
;; exactly as it was.  Scrolling pauses by itself when the end of the
;; script scrolls into view.
;;
;; Hardware teleprompters show the screen through beamsplitter glass
;; that mirrors the image horizontally.  This package does not flip the
;; text; mirror the output at the display-server level instead, e.g. on
;; X11: xrandr --output <OUTPUT> --reflect x

;;; Code:

(require 'face-remap)

(defgroup teleprompter nil
  "Turn any buffer into a teleprompter."
  :group 'multimedia
  :prefix "teleprompter-")

(defcustom teleprompter-text-scale 4
  "Steps passed to `text-scale-set' when the mode starts.
Each step scales the text by `text-scale-mode-step' (1.2 by default)."
  :type 'integer)

(defcustom teleprompter-scroll-interval 0.6
  "Initial seconds between scroll ticks; one line scrolls per tick.
Adjustable at runtime with \\[teleprompter-faster] and
\\[teleprompter-slower]."
  :type 'number)

(defcustom teleprompter-background "black"
  "Background color while the mode is active."
  :type 'color)

(defcustom teleprompter-foreground "white"
  "Text color while the mode is active."
  :type 'color)

(defcustom teleprompter-line-spacing 0.3
  "Additional spacing between lines while the mode is active.
See `line-spacing' for the accepted values."
  :type '(choice number (const nil)))

(defvar-local teleprompter--timer nil)
(defvar-local teleprompter--interval nil)
(defvar-local teleprompter--face-cookies nil)
(defvar-local teleprompter--saved-read-only nil)
(defvar-local teleprompter--saved-visual-line nil)

;;; Scrolling

(defun teleprompter--tick (buffer timer)
  "Scroll BUFFER's window one line; pause at the end.
Cancels TIMER itself when BUFFER is gone or the mode was turned
off, so a stray timer can never outlive its buffer."
  (if (not (and (buffer-live-p buffer)
                (buffer-local-value 'teleprompter-mode buffer)))
      (cancel-timer timer)
    (let ((win (get-buffer-window buffer t)))
      ;; No window on any frame: keep the timer, skip the tick.
      (when win
        (with-selected-window win
          (if (pos-visible-in-window-p (point-max) win)
              (progn
                (teleprompter--stop)
                (message "Teleprompter: end of script"))
            (scroll-up-line 1)))))))

(defun teleprompter--start ()
  "Start (or restart) the scroll timer for the current buffer."
  (teleprompter--stop)
  (let ((buffer (current-buffer))
        (timer nil))
    (setq timer (run-at-time teleprompter--interval teleprompter--interval
                             (lambda () (teleprompter--tick buffer timer))))
    (setq teleprompter--timer timer)))

(defun teleprompter--stop ()
  "Cancel the current buffer's scroll timer, if any."
  (when teleprompter--timer
    (cancel-timer teleprompter--timer)
    (setq teleprompter--timer nil)))

;;; Commands

(defun teleprompter-play-pause ()
  "Toggle auto-scrolling in the current teleprompter buffer."
  (interactive)
  (if teleprompter--timer
      (progn
        (teleprompter--stop)
        (message "Teleprompter: paused"))
    (teleprompter--start)
    (message "Teleprompter: playing (%.2fs per line)" teleprompter--interval)))

(defun teleprompter--set-interval (interval)
  "Set the scroll INTERVAL, restarting the timer when playing."
  (setq teleprompter--interval (max 0.05 (min 5.0 interval)))
  (when teleprompter--timer
    (teleprompter--start))
  (message "Teleprompter: %.2fs per line" teleprompter--interval))

(defun teleprompter-faster ()
  "Scroll faster."
  (interactive)
  (teleprompter--set-interval (* teleprompter--interval 0.8)))

(defun teleprompter-slower ()
  "Scroll slower."
  (interactive)
  (teleprompter--set-interval (/ teleprompter--interval 0.8)))

(defun teleprompter-bigger ()
  "Increase the text size."
  (interactive)
  (text-scale-increase 1))

(defun teleprompter-smaller ()
  "Decrease the text size."
  (interactive)
  (text-scale-decrease 1))

(defun teleprompter-rewind ()
  "Pause and jump back to the top of the script."
  (interactive)
  (teleprompter--stop)
  (goto-char (point-min))
  (recenter 0)
  (message "Teleprompter: rewound"))

(defun teleprompter-quit ()
  "Turn off `teleprompter-mode'."
  (interactive)
  (teleprompter-mode -1))

;;; Mode

(defvar teleprompter-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC") #'teleprompter-play-pause)
    (define-key map (kbd "<up>") #'teleprompter-faster)
    (define-key map (kbd "f") #'teleprompter-faster)
    (define-key map (kbd "<down>") #'teleprompter-slower)
    (define-key map (kbd "s") #'teleprompter-slower)
    (define-key map (kbd "+") #'teleprompter-bigger)
    (define-key map (kbd "=") #'teleprompter-bigger)
    (define-key map (kbd "-") #'teleprompter-smaller)
    (define-key map (kbd "<home>") #'teleprompter-rewind)
    (define-key map (kbd "q") #'teleprompter-quit)
    map)
  "Keymap for `teleprompter-mode'.")

;;;###autoload
(define-minor-mode teleprompter-mode
  "Show the current buffer as a teleprompter.
Large text on a black background with timer-driven auto-scrolling;
see `teleprompter-mode-map' for the playback keys."
  :lighter " Prompter"
  :keymap teleprompter-mode-map
  (if teleprompter-mode
      (teleprompter--enable)
    (teleprompter--disable)))

;;;###autoload
(defalias 'teleprompter #'teleprompter-mode)

(defun teleprompter--enable ()
  "Apply the teleprompter look and prepare playback state."
  (setq teleprompter--interval teleprompter-scroll-interval)
  (setq teleprompter--saved-read-only buffer-read-only)
  (setq buffer-read-only t)
  (setq teleprompter--saved-visual-line visual-line-mode)
  (visual-line-mode 1)
  (setq teleprompter--face-cookies
        (list (face-remap-add-relative 'default
                                       :background teleprompter-background
                                       :foreground teleprompter-foreground)
              (face-remap-add-relative 'fringe
                                       :background teleprompter-background)))
  (text-scale-set teleprompter-text-scale)
  (setq-local line-spacing teleprompter-line-spacing)
  (setq-local mode-line-format nil)
  (setq-local cursor-type nil)
  (add-hook 'kill-buffer-hook #'teleprompter--stop nil t)
  (message (substitute-command-keys
            "Teleprompter: \\[teleprompter-play-pause] play/pause, \
\\[teleprompter-faster]/\\[teleprompter-slower] speed, \
\\[teleprompter-bigger]/\\[teleprompter-smaller] size, \
\\[teleprompter-quit] quit")))

(defun teleprompter--disable ()
  "Undo everything `teleprompter--enable' changed."
  (teleprompter--stop)
  (mapc #'face-remap-remove-relative teleprompter--face-cookies)
  (setq teleprompter--face-cookies nil)
  (text-scale-set 0)
  (kill-local-variable 'line-spacing)
  (kill-local-variable 'mode-line-format)
  (kill-local-variable 'cursor-type)
  (setq buffer-read-only teleprompter--saved-read-only)
  (unless teleprompter--saved-visual-line
    (visual-line-mode -1))
  (remove-hook 'kill-buffer-hook #'teleprompter--stop t))

(provide 'teleprompter)
;;; teleprompter.el ends here
