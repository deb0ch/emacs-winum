;;; winum-api.el --- winum non-interactive user-facing functions
;;
;; Copyright (c) 2016-2019 Thomas Chauvot de Beauchêne
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;


;;; Commentary:

;; Winum public API: the non-interactive functions to use winum programmatically.


;;; Code:


(require 'winum-config)
(require 'winum-core)


;;;###autoload
(defun winum-set-keymap-prefix (prefix)
  "Set key bindings prefix for `winum-keymap' based on `winum-base-map'.
This function overrides the value of `winum-keymap', so you
should call it before customization of `winum-keymap' and/or
after customization of `winum-base-map'.
PREFIX must be a key sequence, like the ones returned by `kbd'."
  (setq winum-keymap (when prefix (let ((map (make-sparse-keymap)))
                                    (define-key map prefix winum-base-map)
                                    map)))
  (setcdr (assoc 'winum-mode minor-mode-map-alist)
          winum-keymap))


;;;###autoload
(defun winum-get-window-by-number (n)
  "Return window numbered N if exists, nil otherwise."
  (let ((window-vector (winum--get-window-vector)))
    (when (and (>= n 0) (< n (length window-vector)))
      (aref window-vector n))))


;;;###autoload
(defun winum-get-number-string (&optional window)
  "Get the current or specified window's current number as a propertized string.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned."
  (let* ((n (winum-get-number window))
         (s (if (numberp n)
                (int-to-string n)
              "")))
    (propertize s 'face 'winum-face)))


;;;###autoload
(defun winum-get-number (&optional window)
  "Get the current or specified window's current number.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned."
  (let ((w (or window (selected-window))))
    (gethash w (winum--get-numbers-table))))


;;;###autoload
(defun winum-switch-to-window (window)
  "Switch to the window WINDOW and switch input focus if on a different frame."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (if (window-live-p window)
        (select-window window)
      (error "Got a dead window %S" window))))


(defun winum--move-buffer-to-window (windownum follow-focus-p)
  "Move a buffer to a WINDOWNUM.
FOLLOW-FOCUS-P controls whether focus moves to new window (with
buffer), or stays on current"
  (interactive)
  (let ((b (current-buffer))
        (w1 (selected-window))
        (w2 (winum-get-window-by-number windownum)))
    (unless (eq w1 w2)
      (set-window-buffer w2 b)
      (switch-to-prev-buffer)
      (unrecord-window-buffer w1 b)))
  (when follow-focus-p (select-window (winum-get-window-by-number windownum))))


(defun winum--swap-buffers-to-window (windownum follow-focus-p)
  "Swap visible buffers between active window and WINDOWNUM.
FOLLOW-FOCUS-P controls whether focus moves to new window (with
buffer), or stays on current."
  (interactive)
  (let* ((b1 (current-buffer))
         (w1 (selected-window))
         (w2 (winum-get-window-by-number windownum))
         (b2 (window-buffer w2)))
    (unless (eq w1 w2)
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (unrecord-window-buffer w1 b1)
      (unrecord-window-buffer w2 b2)))
  (when follow-focus-p (winum-select-window-by-number windownum)))


(provide 'winum-api)

;;; winum-api.el ends here
