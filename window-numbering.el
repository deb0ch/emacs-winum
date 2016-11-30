;;; winum.el --- Window numbers for Emacs. An extended version of window-numbering.el, ideal for multi-screen.
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
;; Keywords: windows, window management, numbers
;; URL: http://github.com/deb0ch/winum.el
;; Created: 2016
;; Compatibility: GNU Emacs 24.x
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

(eval-when-compile (require 'cl))

;; TODO rename to 'winum.el' and keep backward compatibility

(defgroup window-numbering nil
  "Navigate and manage windows using numbers."
  :group 'convenience)

;; TODO bug: when changed from frame-local to non-local in customize, need to
;;      force update or `window-numbering-get-number' fails and messes the
;;      modeline until next update.
(defcustom window-numbering-scope 'global
  "Frames affected by a number set."
  :group 'window-numbering
  :type  '(choice
           (const :tag "frame local" frame-local)
           (const :tag "visible frames" visible)
           (const :tag "global" global)))

(defcustom window-numbering-reverse-frame-list nil
  "If t, order frames by reverse order of creation.
Has effect only when `window-numbering-scope' is not 'frame-local."
  :group 'window-numbering
  :type  'boolean)

(defcustom window-numbering-auto-assign-0-to-minibuffer t
  "If non-nil, `window-numbering-mode' assigns 0 to the minibuffer if active."
  :group 'window-numbering
  :type  'boolean)

;; TODO see if useful
(defcustom window-numbering-before-hook nil
  "Hook called before `window-numbering-mode' starts assigning numbers.
The list of windows to be numbered is passed as a parameter.
Use `window-numbering--assign' to manually assign some of them a number.
If you want to assign a number to just one buffer, use
`window-numbering-assign-func' instead."
  :group 'window-numbering
  :type  'hook)

(defcustom window-numbering-assign-func nil
  "Function called for each window by `window-numbering-mode'.
This is called before automatic assignment begins.  The function should
return a number to have it assigned to the current-window, nil otherwise."
  :group 'window-numbering
  :type  'function)

(defcustom window-numbering-mode-line-position 1
  "The position in the mode-line `window-numbering-mode' displays the number."
  :group 'window-numbering
  :type  'integer)

(defcustom window-numbering-window-number-max 10
  "Max number of windows that can be numbered."
  :group 'window-numbering
  :type  'integer)

(defcustom window-numbering-ignored-buffers '(" *which-key*")
  "List of buffers to ignore when selecting window."
  :type '(repeat string))

(defface window-numbering-face '()
  "Face used for the number in the mode-line."
  :group 'window-numbering)

(defvar window-numbering-keymap (let ((map (make-sparse-keymap)))
                                  (define-key map "\M-0" 'select-window-0)
                                  (define-key map "\M-1" 'select-window-1)
                                  (define-key map "\M-2" 'select-window-2)
                                  (define-key map "\M-3" 'select-window-3)
                                  (define-key map "\M-4" 'select-window-4)
                                  (define-key map "\M-5" 'select-window-5)
                                  (define-key map "\M-6" 'select-window-6)
                                  (define-key map "\M-7" 'select-window-7)
                                  (define-key map "\M-8" 'select-window-8)
                                  (define-key map "\M-9" 'select-window-9)
                                  map)
  "Keymap used in by `window-numbering-mode'.")

;;;###autoload
(define-minor-mode window-numbering-mode
  "A minor mode that allows for managing windows based on window numbers."
  nil
  nil
  window-numbering-keymap
  :global t
  (if window-numbering-mode
      (window-numbering--init)
    (window-numbering--deinit)))

;; define interactive functions window-numbering-select-window-[0..n]
(dotimes (i (max 10 window-numbering-window-number-max))
  (eval `(defun ,(intern (format "select-window-%s" i)) (&optional arg)
           ,(format "Jump to window %d.\nIf prefix ARG is given, delete the\
 window instead of selecting it." i)
           (interactive "P")
           (select-window-by-number ,i arg))))

;;;###autoload
(defun select-window-by-number (i &optional arg)
  "Select window given number I by `window-numbering-mode'.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (let ((w (window-numbering-get-window-by-number i)))
    (if arg
        (delete-window w)
      (window-numbering--switch-to-window w))))

;;;###autoload
(defun window-numbering-get-window-by-number (i)
  "Return window numbered I if exists."
  (let ((windows (if (eq window-numbering-scope 'frame-local)
                     (car (gethash (selected-frame)
                                   window-numbering--frames-table))
                   window-numbering--window-vector))
        window)
    (if (and (>= i 0) (< i window-numbering-window-number-max)
             (setq window (aref windows i)))
        window
      (error "No window numbered %s" i))))

;; TODO function to select window of unlimited input number:
;;      - prefix argument
;;      - read-from-minibuffer

;;;###autoload
(defun window-numbering-get-number-string (&optional window)
  "Get the current or specified window's current number as a propertized string.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned."
  (let ((s (int-to-string (window-numbering-get-number window))))
    (propertize s 'face 'window-numbering-face)))

;;;###autoload
(defun window-numbering-get-number (&optional window)
  "Get the current or specified window's current number.
WINDOW: if specified, the window of which we want to know the number.
        If not specified, the number of the currently selected window is
        returned."
  (let ((w (or window (selected-window))))
    (if (eq window-numbering-scope 'frame-local)
        (gethash w (cdr (gethash (selected-frame)
                                 window-numbering--frames-table)))
      (gethash w window-numbering--numbers-table))))

;; Internal variables

(defvar window-numbering--max-frames 16
  "Maximum number of frames that can be numbered.")

(defvar window-numbering--window-vector nil
  "Vector of windows indexed by their number.
Used internally by window-numbering to get a window provided a number.")

(defvar window-numbering--numbers-table nil
  "Hash table of numbers indexed by their window.
Used internally by window-numbering to get a number provided a window.")

(defvar window-numbering--frames-table nil
  "Table linking windows to numbers and numbers to windows for each frame.

Used only when `window-numbering-scope' is 'frame-local to keep track of
separate window numbers sets in every frame.

It is a hash table using Emacs frames as keys and cons of the form
\(`window-numbering--window-vector' . `window-numbering--numbers-table')
as values.

To get a window given a number, use the `car' of a value.
To get a number given a window, use the `cdr' of a value.

Such a structure allows for per-frame bidirectional fast access.")

(defvar window-numbering--remaining nil
  "A list of available window numbers.")

(defun window-numbering--init ()
  "Initialize window-numbering-mode."
  (if (eq window-numbering-scope 'frame-local)
      (setq window-numbering--frames-table (make-hash-table :size window-numbering--max-frames))
    (setq window-numbering--numbers-table (make-hash-table :size window-numbering-window-number-max)))
  (window-numbering--install-mode-line)
  (add-hook 'minibuffer-setup-hook 'window-numbering--update)
  (add-hook 'window-configuration-change-hook 'window-numbering--update)
  (dolist (frame (frame-list))
    (select-frame frame)
    (window-numbering--update)))

(defun window-numbering--deinit ()
  "Actions performed when turning off window-numbering-mode."
  (window-numbering--clear-mode-line)
  (remove-hook 'minibuffer-setup-hook 'window-numbering--update)
  (remove-hook 'window-configuration-change-hook 'window-numbering--update)
  (setq window-numbering--frames-table nil))

(defun window-numbering--install-mode-line (&optional position)
  "Install the window number from `window-numbering-mode' to the mode-line.
POSITION: position in the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (dotimes (i (min (or position window-numbering-mode-line-position 1)
                     (length mode-line)))
      (push (car mode-line) res)
      (pop mode-line))
    (push '(:eval (window-numbering-get-number-string)) res)
    (while mode-line
      (push (car mode-line) res)
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun window-numbering--clear-mode-line ()
  "Remove the window number of `window-numbering-mode' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        (res))
    (while mode-line
      (let ((item (car mode-line)))
        (unless (equal item '(:eval (window-numbering-get-number-string)))
          (push item res)))
      (pop mode-line))
    (setq-default mode-line-format (nreverse res)))
  (force-mode-line-update t))

(defun window-numbering--update ()
  "Update window numbers."
  (setq window-numbering--remaining (window-numbering--available-numbers))
  (if (eq window-numbering-scope 'frame-local)
      (puthash (selected-frame)
               (cons (make-vector window-numbering-window-number-max nil)
                     (make-hash-table :size window-numbering-window-number-max))
               window-numbering--frames-table)
    (setq window-numbering--window-vector (make-vector window-numbering-window-number-max nil))
    (clrhash window-numbering--numbers-table))
  (when (and window-numbering-auto-assign-0-to-minibuffer
             (active-minibuffer-window))
    (window-numbering--assign (active-minibuffer-window) 0))
  (let ((windows (window-numbering--window-list)))
    (run-hook-with-args 'window-numbering-before-hook windows)
    (when window-numbering-assign-func
      (mapc (lambda (w)
              (with-selected-window w
                (with-current-buffer (window-buffer w)
                  (let ((num (funcall window-numbering-assign-func)))
                    (when num
                      (window-numbering--assign w num))))))
            windows))
    (dolist (w windows)
      (window-numbering--assign w))))

(defun window-numbering--assign (window &optional number)
  "Assign to window WINDOW the number NUMBER.
If NUMBER is not specified, determine it first based on
`window-numbering--remaining'.
Returns the assigned number, or nil on error."
  (if number
      (if (aref (window-numbering--get-window-vector) number)
          (progn (message "Number %s assigned to two buffers (%s and %s)"
                          number window (aref window-numbering--window-vector number))
                 nil)
        (setf (aref (window-numbering--get-window-vector) number) window)
        (puthash window number (window-numbering--get-numbers-table))
        (setq window-numbering--remaining (delq number window-numbering--remaining))
        number)
    ;; else determine number and assign
    (when window-numbering--remaining
      (unless (gethash window (window-numbering--get-numbers-table))
        (let ((number (car window-numbering--remaining)))
          (window-numbering--assign window number))))))

(defun window-numbering--window-list ()
  "Return a list of interesting windows."
  (cl-remove-if
   (lambda (w)
     (let ((f (window-frame w)))
       (or (not (and (frame-live-p f)
                     (frame-visible-p f)))
           (string= "initial_terminal" (terminal-name f))
           (member (buffer-name (window-buffer w)) window-numbering-ignored-buffers))))
   (cl-case window-numbering-scope
     (global
      (cl-mapcan 'window-numbering--list-windows-in-frame
                 (if window-numbering-reverse-frame-list
                     (frame-list)
                   (nreverse (frame-list)))))
     (visible
      (cl-mapcan 'window-numbering--list-windows-in-frame
                 (if window-numbering-reverse-frame-list
                     (visible-frame-list)
                   (nreverse (visible-frame-list)))))
     (frame-local
      (window-numbering--list-windows-in-frame))
     (t
      (error "Invalid `window-numbering-scope': %S" window-numbering-scope)))))

(defun window-numbering--list-windows-in-frame (&optional f)
  "List windows in frame F using natural Emacs ordering."
  (window-list f 0 (frame-first-window f)))

(defun window-numbering--get-window-vector ()
  "Return the window vector used to get a window given a number.
This vector is not stored the same way depending on the value of
`window-numbering-scope'."
  (if (eq window-numbering-scope 'frame-local)
      (car (gethash (selected-frame)
                    window-numbering--frames-table))
    window-numbering--window-vector))

(defun window-numbering--get-numbers-table ()
  "Return the numbers hashtable used to get a number given a window.
This hashtable is not stored the same way depending on the value of
`window-numbering-scope'"
  (if (eq window-numbering-scope 'frame-local)
      (cdr (gethash (selected-frame)
                    window-numbering--frames-table))
    window-numbering--numbers-table))

(defun window-numbering--available-numbers ()
  "Return a list of numbers from 1 to `window-numbering-window-number-max'.
0 is the last element of the list."
  (let ((numbers))
    (dotimes (i window-numbering-window-number-max)
      (push (% (1+ i) window-numbering-window-number-max) numbers))
    (nreverse numbers)))

(defun window-numbering--switch-to-window (window)
  "Switch to the window WINDOW and switch input focus if on a different frame."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (if (window-live-p window)
        (select-window window)
      (error "Got a dead window %S" window))))

(push "^No window numbered .$"                debug-ignored-errors)
(push "^Got a dead window .$"                 debug-ignored-errors)
(push "^Invalid `window-numbering-scope': .$" debug-ignored-errors)

(provide 'window-numbering)

;;; window-numbering.el ends here
