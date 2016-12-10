;;; winum.el --- Window numbers: navigate windows and frames using numbers.
;;
;; Copyright (c) 2006-2015 Nikolaj Schumacher
;; Copyright (c) 2016 Thomas Chauvot de Beauchêne
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
;; Author: Thomas de Beauchêne <thomas.de.beauchene@gmail.com>
;; Version: 1.0
;; Keywords: convenience, frames, windows, multi-screen
;; URL: http://github.com/deb0ch/winum.el
;; Created: 2016
;; Compatibility: GNU Emacs 24.x
;; Package-requires: ((cl-lib "0.5"))
;;
;; This file is NOT part of GNU Emacs.
;;
;;; Commentary:
;;
;; Window numbers for Emacs: Navigate your windows and frames using numbers.
;;
;; This package is an extended and actively maintained version of the
;; https://github.com/nschum/window-numbering.el package by Nikolaj Schumacher,
;; with some ideas and code taken from https://github.com/abo-abo/ace-window.
;;
;;; Code:

;; FIXME: Error during redisplay: (eval (winum-get-number-string)) signaled
;;        (wrong-type-argument numberp nil) when opening a helm buffer.

(eval-when-compile (require 'cl-lib))

(defgroup winum nil
  "Navigate and manage windows using numbers."
  :group 'convenience)

;; FIXME: when changed from frame-local to non-local in customize, need to
;;        force update or `winum-get-number' fails and messes the
;;        modeline until next update.
(defcustom winum-scope 'global
  "Frames affected by a number set."
  :group 'winum
  :type  '(choice
           (const :tag "frame local" frame-local)
           (const :tag "visible frames" visible)
           (const :tag "global" global)))

(defcustom winum-reverse-frame-list nil
  "If t, order frames by reverse order of creation.
Has effect only when `winum-scope' is not 'frame-local."
  :group 'winum
  :type  'boolean)

(defcustom winum-auto-assign-0-to-minibuffer t
  "If non-nil, `winum-mode' assigns 0 to the minibuffer when active."
  :group 'winum
  :type  'boolean)

(defcustom winum-assign-func nil
  "Function called for each window by `winum-mode'.
This is called before automatic assignment begins.  The function should
return a number to have it assigned to the current-window, nil otherwise.
This function along with `winum-auto-assign-0-to-minibuffer' are the only
ways to have 0 assigned to a window."
  :group 'winum
  :type  'function)

(defcustom winum-auto-setup-mode-line t
  "When nil, `winum-mode' will not display window numbers in the mode-line.
You might want this to be nil if you use a package that already manages window
numbers in the mode-line.")

(defcustom winum-mode-line-position 1
  "The position in the mode-line `winum-mode' displays the number."
  :group 'winum
  :type  'integer)

(defcustom winum-ignored-buffers '(" *which-key*")
  "List of buffers to ignore when selecting window."
  :type '(repeat string))

(defface winum-face '()
  "Face used for the number in the mode-line."
  :group 'winum)

(defvar winum-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-`") 'select-window-by-number)
    (define-key map (kbd "M-0") 'select-window-0-or-10)
    (define-key map (kbd "M-1") 'select-window-1)
    (define-key map (kbd "M-2") 'select-window-2)
    (define-key map (kbd "M-3") 'select-window-3)
    (define-key map (kbd "M-4") 'select-window-4)
    (define-key map (kbd "M-5") 'select-window-5)
    (define-key map (kbd "M-6") 'select-window-6)
    (define-key map (kbd "M-7") 'select-window-7)
    (define-key map (kbd "M-8") 'select-window-8)
    (define-key map (kbd "M-9") 'select-window-9)
    map)
  "Keymap used in by `winum-mode'.")

;;;###autoload
(define-minor-mode winum-mode
  "A minor mode that allows for managing windows based on window numbers."
  nil
  nil
  winum-keymap
  :global t
  (if winum-mode
      (winum--init)
    (winum--deinit)))

(defun select-window-0-or-10 (&optional arg)
  "Jump to window 0 if assigned or 10 if exists.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (let ((n (if (winum-get-window-by-number 0)
               (if arg '- 0)
             (if arg -10 10))))
    (select-window-by-number n)))

(defun select-window-0 (&optional arg)
  "Jump to window 0.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg '- 0)))

(defun select-window-1 (&optional arg)
  "Jump to window 1.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -1 1)))

(defun select-window-2 (&optional arg)
  "Jump to window 2.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -2 2)))

(defun select-window-3 (&optional arg)
  "Jump to window 3.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -3 3)))

(defun select-window-4 (&optional arg)
  "Jump to window 4.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -4 4)))

(defun select-window-5 (&optional arg)
  "Jump to window 5.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -5 5)))

(defun select-window-6 (&optional arg)
  "Jump to window 6.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -6 6)))

(defun select-window-7 (&optional arg)
  "Jump to window 7.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -7 7)))

(defun select-window-8 (&optional arg)
  "Jump to window 8.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -8 8)))

(defun select-window-9 (&optional arg)
  "Jump to window 9.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (select-window-by-number (if arg -9 9)))

(defun select-window-by-number (&optional arg)
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
             ((eq arg '-) 0) ; negative-argument
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
          (winum--switch-to-window w))
      (error "No window numbered %d" n))))

;;;###autoload
(defun winum-get-window-by-number (n)
  "Return window numbered N if exists, nil otherwise."
  (let ((windows (if (eq winum-scope 'frame-local)
                     (car (gethash (selected-frame) winum--frames-table))
                   winum--window-vector))
        window)
    (if (and (>= n 0) (< n (1+ winum--window-count))
             (setq window (aref windows n)))
        window
      nil)))

;;;###autoload
(defun winum-get-number-string (&optional window)
  "Get the current or specified window's current number as a propertized string.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned."
  (let ((s (int-to-string (winum-get-number window))))
    (propertize s 'face 'winum-face)))

;;;###autoload
(defun winum-get-number (&optional window)
  "Get the current or specified window's current number.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned."
  (let ((w (or window (selected-window))))
    (if (eq winum-scope 'frame-local)
        (gethash w (cdr (gethash (selected-frame) winum--frames-table)))
      (gethash w winum--numbers-table))))

;; Implementation

(defvar winum--max-frames 16
  "Maximum number of frames that can be numbered.")

(defvar winum--window-count nil
  "Current count of windows to be numbered.")

(defvar winum--remaining nil
  "A list of window numbers to assign.")

(defvar winum--window-vector nil
  "Vector of windows indexed by their number.
Used internally by winum to get a window provided a number.")

(defvar winum--numbers-table nil
  "Hash table of numbers indexed by their window.
Used internally by winum to get a number provided a window.")

(defvar winum--frames-table nil
  "Table linking windows to numbers and numbers to windows for each frame.

Used only when `winum-scope' is 'frame-local to keep track of
separate window numbers sets in every frame.

It is a hash table using Emacs frames as keys and cons of the form
\(`winum--window-vector' . `winum--numbers-table')
as values.

To get a window given a number, use the `car' of a value.
To get a number given a window, use the `cdr' of a value.

Such a structure allows for per-frame bidirectional fast access.")

(defun winum--init ()
  "Initialize winum-mode."
  (setq winum--window-count (length (winum--window-list)))
  (if (eq winum-scope 'frame-local)
      (setq winum--frames-table (make-hash-table :size winum--max-frames))
    (setq winum--numbers-table (make-hash-table :size winum--window-count)))
  (when winum-auto-setup-mode-line
    (winum--install-mode-line))
  (add-hook 'minibuffer-setup-hook 'winum--update)
  (add-hook 'window-configuration-change-hook 'winum--update)
  (dolist (frame (frame-list))
    (select-frame frame)
    (winum--update)))

(defun winum--deinit ()
  "Actions performed when turning off winum-mode."
  (when winum-auto-setup-mode-line
    (winum--clear-mode-line))
  (remove-hook 'minibuffer-setup-hook 'winum--update)
  (remove-hook 'window-configuration-change-hook 'winum--update)
  (setq winum--frames-table nil))

(defun winum--install-mode-line (&optional position)
  "Install the window number from `winum-mode' to the mode-line.
POSITION: position in the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (dotimes (i (min (or position winum-mode-line-position 1)
                     (length mode-line)))
      (push (pop mode-line) res))
    (push '(:eval (winum-get-number-string)) res)
    (while mode-line
      (push (pop mode-line) res))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun winum--clear-mode-line ()
  "Remove the window number of `winum-mode' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (equal item '(:eval (winum-get-number-string)))
          (push item res)))
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun winum--update ()
  "Update window numbers."
  (let ((windows (winum--window-list)))
    (setq winum--window-count (length windows)
          winum--remaining (winum--available-numbers))
    (if (eq winum-scope 'frame-local)
        (puthash (selected-frame)
                 (cons (make-vector (1+ winum--window-count) nil)
                       (make-hash-table :size winum--window-count))
                 winum--frames-table)
      (setq winum--window-vector (make-vector (1+ winum--window-count) nil))
      (clrhash winum--numbers-table))
    (when winum-assign-func
      (mapc (lambda (w)
              (with-selected-window w
                (with-current-buffer (window-buffer w)
                  (let ((num (funcall winum-assign-func)))
                    (when num
                      (winum--assign w num))))))
            windows))
    (when (and winum-auto-assign-0-to-minibuffer
               (active-minibuffer-window)
               (not (winum-get-window-by-number 0)))
      (winum--assign (active-minibuffer-window) 0))
    (dolist (w windows)
      (winum--assign w))))

(defun winum--assign (window &optional number)
  "Assign to window WINDOW the number NUMBER.
If NUMBER is not specified, determine it first based on
`winum--remaining'.
Returns the assigned number, or nil on error."
  (if number
      (if (aref (winum--get-window-vector) number)
          (progn (message "Number %s already assigned to %s, can't assign to %s"
                          number (aref winum--window-vector number) window)
                 nil)
        (setf (aref (winum--get-window-vector) number) window)
        (puthash window number (winum--get-numbers-table))
        (setq winum--remaining (delq number winum--remaining))
        number)
    ;; else determine number and assign
    (when winum--remaining
      (unless (gethash window (winum--get-numbers-table))
        (let ((number (car winum--remaining)))
          (winum--assign window number))))))

(defun winum--window-list ()
  "Return a list of interesting windows."
  (cl-remove-if
   (lambda (w)
     (let ((f (window-frame w)))
       (or (not (and (frame-live-p f)
                     (frame-visible-p f)))
           (string= "initial_terminal" (terminal-name f))
           (member (buffer-name (window-buffer w)) winum-ignored-buffers))))
   (cl-case winum-scope
     (global
      (cl-mapcan 'winum--list-windows-in-frame
                 (if winum-reverse-frame-list
                     (frame-list)
                   (nreverse (frame-list)))))
     (visible
      (cl-mapcan 'winum--list-windows-in-frame
                 (if winum-reverse-frame-list
                     (visible-frame-list)
                   (nreverse (visible-frame-list)))))
     (frame-local
      (winum--list-windows-in-frame))
     (t
      (error "Invalid `winum-scope': %S" winum-scope)))))

(defun winum--list-windows-in-frame (&optional f)
  "List windows in frame F using natural Emacs ordering."
  (window-list f 0 (frame-first-window f)))

(defun winum--get-window-vector ()
  "Return the window vector used to get a window given a number.
This vector is not stored the same way depending on the value of
`winum-scope'."
  (if (eq winum-scope 'frame-local)
      (car (gethash (selected-frame) winum--frames-table))
    winum--window-vector))

(defun winum--get-numbers-table ()
  "Return the numbers hashtable used to get a number given a window.
This hashtable is not stored the same way depending on the value of
`winum-scope'"
  (if (eq winum-scope 'frame-local)
      (cdr (gethash (selected-frame) winum--frames-table))
    winum--numbers-table))

(defun winum--available-numbers ()
  "Return a list of numbers from 1 to `winum--window-count'.
0 is is not part of the list as its assignment is either manual
using the `winum-assign-func', or using `winum-auto-assign-0-to-minibuffer'."
  (let ((numbers))
    (dotimes (i winum--window-count)
      (push (1+ i) numbers))
    (nreverse numbers)))

(defun winum--switch-to-window (window)
  "Switch to the window WINDOW and switch input focus if on a different frame."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (if (window-live-p window)
        (select-window window)
      (error "Got a dead window %S" window))))

(push "^No window numbered .$"     debug-ignored-errors)
(push "^Got a dead window .$"      debug-ignored-errors)
(push "^Invalid `winum-scope': .$" debug-ignored-errors)

(provide 'winum)

;;; winum.el ends here
