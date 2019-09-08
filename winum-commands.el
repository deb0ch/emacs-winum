;;; winum-commands.el --- winum interactive commands
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

;; User facing interactive functions.
;; Although most of the definitions are repetitive and the functions could be
;; generated, they are volontarily not to work better with the Emacs debugger /
;; stacktrace and (mainly) to allow autoloading.


;;; Code:


(require 'winum-api)


;;;###autoload
(defun winum-select-window-by-number (&optional arg)
  "Select or delete window which number is specified by ARG.
If the number is negative, delete the window instead of selecting it.
There are several ways to provide the number:
- if called from elisp with an argument, use it.
- if called interactively with a numeric prefix argument, use it.
- if prefix argument is the negative argument, delete window 0.
- if prefix argument is the default prefix argument, delete current window.
- if called interactively and no valid argument is provided, read from
  minibuffer."
  (interactive "P")
  (let* ((n (cond
             ((integerp arg) arg)
             ((eq arg '-) 0) ; the negative argument
             (arg (winum-get-number))
             ((called-interactively-p 'any)
              (let ((user-input-str (read-from-minibuffer "Window: ")))
                (if (not (string-match-p "[+-]?[0-9]+\.*" user-input-str))
                    (winum-get-number)
                  (string-to-number user-input-str))))
             (t (winum-get-number))))
         (w (winum-get-window-by-number (abs n)))
         (delete (and arg
                      (or (not (integerp arg))
                          (> 0 n)))))
    (if w
        (if delete
            (delete-window w)
          (winum-switch-to-window w))
      (error "No window numbered %d" n))))


;;;###autoload
(defun winum-select-window-0-or-10 (&optional arg)
  "Jump to window 0 if assigned or 10 if exists.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (let ((n (if (winum-get-window-by-number 0)
               (if arg '- 0)
             (if arg -10 10))))
    (winum-select-window-by-number n)))


;; Select window


;;;###autoload
(defun winum-select-window-0 (&optional arg)
  "Jump to window 0.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg '- 0)))


;;;###autoload
(defun winum-select-window-1 (&optional arg)
  "Jump to window 1.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -1 1)))


;;;###autoload
(defun winum-select-window-2 (&optional arg)
  "Jump to window 2.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -2 2)))


;;;###autoload
(defun winum-select-window-3 (&optional arg)
  "Jump to window 3.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -3 3)))


;;;###autoload
(defun winum-select-window-4 (&optional arg)
  "Jump to window 4.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -4 4)))


;;;###autoload
(defun winum-select-window-5 (&optional arg)
  "Jump to window 5.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -5 5)))


;;;###autoload
(defun winum-select-window-6 (&optional arg)
  "Jump to window 6.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -6 6)))


;;;###autoload
(defun winum-select-window-7 (&optional arg)
  "Jump to window 7.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -7 7)))


;;;###autoload
(defun winum-select-window-8 (&optional arg)
  "Jump to window 8.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -8 8)))


;;;###autoload
(defun winum-select-window-9 (&optional arg)
  "Jump to window 9.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (winum-select-window-by-number (if arg -9 9)))


;; Move buffer to window


;;;###autoload
(defun winum-move-buffer-to-window-0 (&optional arg)
  "Move buffer to the window numbered 0.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 0 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-1 (&optional arg)
  "Move buffer to the window numbered 1.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 1 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-2 (&optional arg)
  "Move buffer to the window numbered 2.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 2 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-3 (&optional arg)
  "Move buffer to the window numbered 3.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 3 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-4 (&optional arg)
  "Move buffer to the window numbered 4.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 4 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-5 (&optional arg)
  "Move buffer to the window numbered 5.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 5 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-6 (&optional arg)
  "Move buffer to the window numbered 6.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 6 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-7 (&optional arg)
  "Move buffer to the window numbered 7.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 7 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-8 (&optional arg)
  "Move buffer to the window numbered 8.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 8 t arg))


;;;###autoload
(defun winum-move-buffer-to-window-9 (&optional arg)
  "Move buffer to the window numbered 9.
If prefix ARG is provided, swap buffers instead. See
`winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 9 t arg))


;; Move buffer to window no follow


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-0 (&optional arg)
  "Move buffer to the window numbered 0.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 0 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-1 (&optional arg)
  "Move buffer to the window numbered 1.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 1 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-2 (&optional arg)
  "Move buffer to the window numbered 2.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 2 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-3 (&optional arg)
  "Move buffer to the window numbered 3.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 3 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-4 (&optional arg)
  "Move buffer to the window numbered 4.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 4 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-5 (&optional arg)
  "Move buffer to the window numbered 5.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 5 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-6 (&optional arg)
  "Move buffer to the window numbered 6.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 6 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-7 (&optional arg)
  "Move buffer to the window numbered 7.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 7 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-8 (&optional arg)
  "Move buffer to the window numbered 8.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 8 nil arg))


;;;###autoload
(defun winum-move-buffer-to-window-no-follow-9 (&optional arg)
  "Move buffer to the window numbered 9.
The input focus will remain in the current window. If prefix ARG
is provided, swap buffers instead. See `winum-swap-buffers-with-window'."
  (interactive "P")
  (winum-move-buffer-to-window 9 nil arg))


;; Swap buffers with window


;;;###autoload
(defun winum-swap-buffers-with-window-0 (&optional arg)
  "Swap buffers between the active window and the window numbered 0.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 0 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-1 (&optional arg)
  "Swap buffers between the active window and the window numbered 1.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 1 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-2 (&optional arg)
  "Swap buffers between the active window and the window numbered 2.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 2 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-3 (&optional arg)
  "Swap buffers between the active window and the window numbered 3.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 3 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-4 (&optional arg)
  "Swap buffers between the active window and the window numbered 4.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 4 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-5 (&optional arg)
  "Swap buffers between the active window and the window numbered 5.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 5 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-6 (&optional arg)
  "Swap buffers between the active window and the window numbered 6.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 6 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-7 (&optional arg)
  "Swap buffers between the active window and the window numbered 7.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 7 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-8 (&optional arg)
  "Swap buffers between the active window and the window numbered 8.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 8 (not arg)))


;;;###autoload
(defun winum-swap-buffers-with-window-9 (&optional arg)
  "Swap buffers between the active window and the window numbered 9.
If prefix ARG is provided, do not move input focus to the new window."
  (interactive "P")
  (winum-swap-buffers-with-window 9 (not arg)))


;; Swap buffers with window no follow


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-0 (&optional arg)
  "Swap buffers between the active window and the window numbered 0.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 0 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-1 (&optional arg)
  "Swap buffers between the active window and the window numbered 1.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 1 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-2 (&optional arg)
  "Swap buffers between the active window and the window numbered 2.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 2 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-3 (&optional arg)
  "Swap buffers between the active window and the window numbered 3.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 3 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-4 (&optional arg)
  "Swap buffers between the active window and the window numbered 4.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 4 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-5 (&optional arg)
  "Swap buffers between the active window and the window numbered 5.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 5 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-6 (&optional arg)
  "Swap buffers between the active window and the window numbered 6.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 6 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-7 (&optional arg)
  "Swap buffers between the active window and the window numbered 7.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 7 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-8 (&optional arg)
  "Swap buffers between the active window and the window numbered 8.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 8 arg))


;;;###autoload
(defun winum-swap-buffers-with-window-no-follow-9 (&optional arg)
  "Swap buffers between the active window and the window numbered 9.
If prefix ARG is provided, the input focus moves to the new
window with the buffer."
  (interactive "P")
  (winum-swap-buffers-with-window 9 arg))


(provide 'winum-commands)

;;; winum-commands.el ends here
