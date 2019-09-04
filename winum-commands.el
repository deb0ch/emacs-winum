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
(defun winum-select-window-0-or-10 (&optional arg)
  "Jump to window 0 if assigned or 10 if exists.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (let ((n (if (winum-get-window-by-number 0)
               (if arg '- 0)
             (if arg -10 10))))
    (winum-select-window-by-number n)))


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


(dotimes (i 9)
  (let ((n (+ i 1)))

    (eval `(defun ,(intern (format "winum-buffer-to-window-%s" n)) (&optional arg)
              ,(format "Move buffer to the window with number %i." n)
              (interactive "P")
              (if arg
                  (winum--swap-buffers-to-window ,n t)
                (winum--move-buffer-to-window ,n t))))

    (eval `(defun ,(intern (format "winum-move-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (winum--move-buffer-to-window ,n nil)))

    (eval `(defun ,(intern (format "winum-swap-buffer-window-no-follow-%s" n)) ()
             (interactive)
             (winum--swap-buffers-to-window ,n nil)))
    ))


(provide 'winum-commands)

;;; winum-commands.el ends here
