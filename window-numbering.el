;;; winum.el --- Window numbers
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
;; This project is an extended and actively maintained version of
;; window-numbering.el
;;
;; Bla bla bla disclaimer, links & credits to original project, ace-window
;; etc...
;;
;;; Code:

(eval-when-compile (require 'cl))

(defgroup window-numbering nil
  "Navigate and manage windows using numbers."
  :group 'convenience)

(defcustom window-numbering-auto-assign-0-to-minibuffer t
  "If non-nil, `window-numbering-mode' assigns 0 to the minibuffer if active."
  :group 'window-numbering
  :type 'boolean)

(defcustom window-numbering-before-hook nil
  "Hook called before `window-numbering-mode' starts assigning numbers.
The list of windows to be numbered is passed as a parameter.
Use `window-numbering-assign' to manually assign some of them a number.
If you want to assign a number to just one buffer, use
`window-numbering-assign-func' instead."
  :group 'window-numbering
  :type 'hook)

(defcustom window-numbering-assign-func nil
  "Function called for each window by `window-numbering-mode'.
This is called before automatic assignment begins.  The function should
return a number to have it assigned to the current-window, nil otherwise."
  :group 'window-numbering
  :type 'function)

(defcustom window-numbering-mode-line-position 1
  "The position in the mode-line `window-numbering-mode' displays the number."
  :group 'window-numbering
  :type 'integer)

;; TODO when changed from frame-local to non-local in customize, need to force
;;      update, or `window-numbering-get-number' fails and crashes the modeline
;;      until next update.
(defcustom window-numbering-frame-scope 'global
  "The scope of number sets."
  :group 'window-numbering
  :type '(choice
          (const :tag "frame local" frame-local)
          (const :tag "visible frames" visible)
          (const :tag "global" global)))

(defcustom window-numbering-reverse-frame-list nil
  "If t, order frames by reverse order of creation.
Has effect only when `window-numbering-frame-scope' is not 'frame-local."
  :group 'window-numbering
  :type 'boolean)

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

(defvar window-numbering--windows nil
  "Vector of windows indexed by their number.
Used internally by window-numbering to get a window provided a number.")

(defvar window-numbering--numbers nil
  "Hash table of numbers indexed by their window.
Used internally by window-numbering to get a number provided a window.")

(defvar window-numbering--frames-table nil
  "Bidirectional table between windows and window numbers.

Used when `window-numbering-frame-scope' is 'frame-local to keep
track of separate window numbers sets in every frame.

It is a hash table using emacs frames as keys and cons of the form
\(`window-numbering--windows' . `window-numbering--numbers') as values.

To get a window given a number, use the `car' of a value.
To get a number given a window, use the `cdr' of a value.

Such a structure allows for per-frame bidirectional fast access.")

(defvar window-numbering--left nil
  "A list of unused window numbers.")

(defun window-numbering-calculate-left (windows)
  "Return a list of numbers currently available for assignment.
WINDOWS: a vector of currently assigned windows."
  (let ((i 9)
        left)
    (while (>= i 0)
      (let ((window (aref windows i)))
        (unless window
          (push (% (1+ i) 10) left)))
      (decf i))
    left))

(defun window-numbering-list-windows-in-frame (&optional f)
  "List windows in frame F using natural Emacs ordering."
  (window-list f 0 (frame-first-window f)))

(defun window-numbering-window-list ()
  "Return a list of interesting windows."
  (cl-remove-if
   (lambda (w)
     (let ((f (window-frame w)))
       (or (not (and (frame-live-p f)
                     (frame-visible-p f)))
           (string= "initial_terminal" (terminal-name f))
           ;; (window-numbering-ignored-p w) ;; TODO implement
           )))
   (cl-case window-numbering-frame-scope
     (global
      (cl-mapcan 'window-numbering-list-windows-in-frame
                 (if window-numbering-reverse-frame-list
                     (frame-list)
                   (nreverse (frame-list)))))
     (visible
      (cl-mapcan 'window-numbering-list-windows-in-frame
                 (if window-numbering-reverse-frame-list
                     (visible-frame-list)
                   (nreverse (visible-frame-list)))))
     (frame-local
      (window-numbering-list-windows-in-frame))
     (t
      (error "Invalid `window-numbering-frame-scope': %S"
             window-numbering-frame-scope)))))

(defun window-numbering-assign (window &optional number)
  "Assign to window WINDOW the number NUMBER.
If NUMBER is not specified, determine it first based on
`window-numbering--left'.
Returns the assigned number, or nil on error."
  (if number
      (if (aref window-numbering--windows number)
          (progn (message "Number %s assigned to two buffers (%s and %s)"
                          number window (aref window-numbering--windows number))
                 nil)
        (setf (aref window-numbering--windows number) window)
        (puthash window number window-numbering--numbers)
        (setq window-numbering--left (delq number window-numbering--left))
        number)
    ;; else determine number and assign
    (when window-numbering--left
      (unless (gethash window window-numbering--numbers)
        (let ((number (car window-numbering--left)))
          (window-numbering-assign window number))))))

;; TODO update mode-line in all frames
(defun window-numbering-update ()
  "Update window numbers."
  (setq window-numbering--windows (make-vector 10 nil)
        window-numbering--numbers (make-hash-table :size 10)
        window-numbering--left (window-numbering-calculate-left
                                window-numbering--windows))
  (when (eq window-numbering-frame-scope 'frame-local)
    (puthash (selected-frame)
             (cons window-numbering--windows window-numbering--numbers)
             window-numbering--frames-table))
  (when (and window-numbering-auto-assign-0-to-minibuffer
             (active-minibuffer-window))
    (window-numbering-assign (active-minibuffer-window) 0))
  (let ((windows (window-numbering-window-list)))
    (run-hook-with-args 'window-numbering-before-hook windows)
    (when window-numbering-assign-func
      (mapc (lambda (window)
              (with-selected-window window
                (with-current-buffer (window-buffer window)
                  (let ((num (funcall window-numbering-assign-func)))
                    (when num
                      (window-numbering-assign window num))))))
            windows))
    (dolist (window windows)
      (window-numbering-assign window))))

(defun window-numbering-switch-to-window (window)
  "Switch to the window WINDOW and switch input focus if on a different frame."
  (let ((frame (window-frame window)))
    (when (and (frame-live-p frame)
               (not (eq frame (selected-frame))))
      (select-frame-set-input-focus frame))
    (if (window-live-p window)
        (select-window window)
      (error "Got a dead window %S" window))))

;;;###autoload
(defun window-numbering-install-mode-line (&optional position)
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

;;;###autoload
(defun window-numbering-clear-mode-line ()
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
    (if (eq window-numbering-frame-scope 'frame-local)
        (gethash w (cdr (gethash (selected-frame)
                                 window-numbering--frames-table)))
      (gethash w window-numbering--numbers))))

;;;###autoload
(defun get-window-by-number (i)
  "Return window numbered I if exists."
  (let ((windows (if (eq window-numbering-frame-scope 'frame-local)
                     (car (gethash (selected-frame)
                                   window-numbering--frames-table))
                   window-numbering--windows))
        window)
    (if (and (>= i 0) (< i 10)
             (setq window (aref windows i)))
        window
      (error "No window numbered %s" i))))

;;;###autoload
(defun select-window-by-number (i &optional arg)
  "Select window given number I by `window-numbering-mode'.
If prefix ARG is given, delete the window instead of selecting it."
  (interactive "P")
  (let ((w (get-window-by-number i)))
    (if arg
        (delete-window w)
      (window-numbering-switch-to-window w))))

;; TODO select window of unlimited input number:
;;      - prefix argument
;;      - read-from-minibuffer

;;;###autoload
(define-minor-mode window-numbering-mode
  "A minor mode that allows for managing windows based on window numbers."
  nil
  nil
  window-numbering-keymap
  :global t
  (if window-numbering-mode
      (unless window-numbering--frames-table
        (save-excursion
          (setq window-numbering--frames-table (make-hash-table :size 16))
          (window-numbering-install-mode-line)
          (add-hook 'minibuffer-setup-hook 'window-numbering-update)
          (add-hook 'window-configuration-change-hook
                    'window-numbering-update)
          (dolist (frame (frame-list))
            (select-frame frame)
            (window-numbering-update))))
    (window-numbering-clear-mode-line)
    (remove-hook 'minibuffer-setup-hook 'window-numbering-update)
    (remove-hook 'window-configuration-change-hook
                 'window-numbering-update)
    (setq window-numbering--frames-table nil)))

;; (defun window-numbering-select-window-[0-9] ())
(dotimes (i 10)
  (eval `(defun ,(intern (format "select-window-%s" i)) (&optional arg)
           ,(format "Select the window with number %i." i)
           (interactive "P")
           (select-window-by-number ,i arg))))

(push "^No window numbered .$" debug-ignored-errors)

(provide 'window-numbering)

;;; window-numbering.el ends here
