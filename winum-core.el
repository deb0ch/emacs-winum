;;; winum-core.el --- winum core mechanics
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

;; Winum core mechanics: internal variables, init and deinit, update hook,
;; managing the mode line and the internal data structures.


;;; Code:


(eval-when-compile (require 'cl-lib))
(require 'dash)

(require 'winum-config)


(defvar winum-base-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "`") 'winum-select-window-by-number)
    (define-key map (kbd "²") 'winum-select-window-by-number)
    (define-key map (kbd "0") 'winum-select-window-0-or-10)
    (define-key map (kbd "1") 'winum-select-window-1)
    (define-key map (kbd "2") 'winum-select-window-2)
    (define-key map (kbd "3") 'winum-select-window-3)
    (define-key map (kbd "4") 'winum-select-window-4)
    (define-key map (kbd "5") 'winum-select-window-5)
    (define-key map (kbd "6") 'winum-select-window-6)
    (define-key map (kbd "7") 'winum-select-window-7)
    (define-key map (kbd "8") 'winum-select-window-8)
    (define-key map (kbd "9") 'winum-select-window-9)
    map)
  "Keymap to be used under the prefix provided by `winum-keymap-prefix'.")


(defvar winum-keymap (let ((map (make-sparse-keymap)))
                       (define-key map (kbd "C-x w") winum-base-map)
                       map)
  "Keymap used for `winum-mode'.")


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


(defvar winum--mode-line-segment
  '(:eval (format winum-format (winum-get-number-string)))
  "What is pushed into `mode-line-format' when setting it up automatically.")


(defvar winum--last-used-scope winum-scope
  "Tracks the last used `winum-scope'.
Needed to detect scope changes at runtime.")


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
        res)
    (dotimes (i (min (or position winum-mode-line-position 1)
                     (length mode-line)))
      (push (pop mode-line) res))
    (unless (equal (car mode-line) winum--mode-line-segment)
      (push winum--mode-line-segment res))
    (while mode-line
      (push (pop mode-line) res))
    (let ((nres (nreverse res)))
      (setq mode-line-format nres)
      (setq-default mode-line-format nres)))
  (force-mode-line-update t))


(defun winum--clear-mode-line ()
  "Remove the window number of `winum-mode' from the mode-line."
  (let ((mode-line (default-value 'mode-line-format))
        res)
    (while mode-line
      (let ((item (pop mode-line)))
        (unless (equal item winum--mode-line-segment)
        (push item res))))
    (let ((nres (nreverse res)))
      (setq mode-line-format nres)
      (setq-default mode-line-format nres)))
  (force-mode-line-update t))


(defun winum--update ()
  "Update window numbers."
  (let ((windows (winum--window-list)))
    (setq winum--window-count (length windows)
          winum--remaining (winum--available-numbers))
    (winum--set-window-vector (make-vector (1+ winum--window-count) nil))
    (clrhash (winum--get-numbers-table))
    (when winum-assign-functions
      (-each windows #'winum--try-to-find-custom-number))
    (when (and winum-auto-assign-0-to-minibuffer
               (active-minibuffer-window)
               (not (winum-get-window-by-number 0)))
      (winum--assign (active-minibuffer-window) 0))
    (dolist (w windows)
      (winum--assign w))))


(defun winum--try-to-find-custom-number (window)
  "Try to find and assign a custom number for WINDOW.
Do so by trying every function in `winum-assign-functions' and assign the
*first* non nil integer.
When multiple functions assign a number to a window log a warning and use the
first number anyway."
  (with-selected-window window
    (with-current-buffer (window-buffer window)
      (let* ((nums (->> winum-assign-functions
                        (--map (cons it (funcall it)))
                        (--remove (null (cdr it)))))
             (num (-> nums (cl-first) (cdr))))
        (when (> (length nums) 1)
          (message "Winum conflict - window %s was assigned a number by multiple custom assign functions: '%s'"
                   window (--map (format "%s -> %s" (car it) (cdr it)) nums)))
        (when (integerp num) (winum--assign window num))))))


(defun winum--assign (window &optional number)
  "Assign to window WINDOW the number NUMBER.
If NUMBER is not specified, determine it first based on `winum--remaining'.
Returns the assigned number, or nil on error."
  (if number
      (progn
        (winum--maybe-expand-window-vector number)
        (if (aref (winum--get-window-vector) number)
            (progn
              (message "Number %s already assigned to %s, can't assign to %s"
                       number (aref (winum--get-window-vector) number) window)
              nil)
          (setf (aref (winum--get-window-vector) number) window)
          (puthash window number (winum--get-numbers-table))
          (setq winum--remaining (delq number winum--remaining))
          number))
    ;; else determine number and assign
    (when winum--remaining
      (unless (gethash window (winum--get-numbers-table))
        (let ((number (car winum--remaining)))
          (winum--assign window number))))))


(defun winum--maybe-expand-window-vector (number)
  "Expand `winum--window-vector' if NUMBER is bigger than its size.
The size of `winum--window-vector' is normally based on the number of live
windows, however a higher number can be reserved by the user-defined
`winum-assign-func'."
  (let* ((window-vector (winum--get-window-vector))
         (window-vector-length (length window-vector)))
    (when (> number window-vector-length)
      (winum--set-window-vector
       (vconcat window-vector
                (make-vector (1+ (- number window-vector-length)) nil))))))


(defun winum--window-list ()
  "Return a list of interesting windows."
  (cl-remove-if
   #'winum--ignore-window-p
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


(defun winum--ignore-window-p (window)
  "Non-nil if WINDOW should be ignored for numbering."
  (let ((f (window-frame window)))
    (or (not (and (frame-live-p f)
                  (frame-visible-p f)))
        (string= "initial_terminal" (terminal-name f))
        (member (buffer-name (window-buffer window)) winum-ignored-buffers)
        (cl-some
         (lambda (regex) (string-match regex (buffer-name (window-buffer window))))
         winum-ignored-buffers-regexp))))


(defun winum--list-windows-in-frame (&optional f)
  "List windows in frame F using natural Emacs ordering."
  (window-list f 0 (frame-first-window f)))


(defun winum--set-window-vector (window-vector)
  "Set WINDOW-VECTOR according to the current `winum-scope'."
  (winum--check-for-scope-change)
  (if (eq winum-scope 'frame-local)
      (puthash (selected-frame)
               (cons window-vector
                     (make-hash-table :size winum--window-count))
               winum--frames-table)
    (setq winum--window-vector window-vector)))


(defun winum--get-window-vector ()
  "Return the window vector used to get a window given a number.
This vector is not stored the same way depending on the value of `winum-scope'."
  (winum--check-for-scope-change)
  (if (eq winum-scope 'frame-local)
      (car (gethash (selected-frame) winum--frames-table))
    winum--window-vector))


(defun winum--get-numbers-table ()
  "Return the numbers hashtable used to get a number given a window.
This hashtable is not stored the same way depending on the value of
`winum-scope'"
  (winum--check-for-scope-change)
  (winum--check-frames-table)
  (if (eq winum-scope 'frame-local)
      (cdr (gethash (selected-frame) winum--frames-table))
    winum--numbers-table))


(defun winum--check-frames-table ()
  "Make sure `winum--frames-table' exists and is correctly equipped.
Verifies 2 things (when `winum-scope' is frame local):
 * When `winum-scope' is frame-local for the first time it may be necessary to
   instantiate `winum--frames-table'.
 * A table entry for the current frame must be made when the frame has just
   been created."
  (when (eq winum-scope 'frame-local)
    (unless winum--frames-table
      (setq winum--frames-table (make-hash-table :size winum--max-frames)))
    (unless (gethash (selected-frame) winum--frames-table)
      (winum--update))))


(defun winum--available-numbers ()
  "Return a list of numbers from 1 to `winum--window-count'.
0 is is not part of the list as its assignment is either manual
using the `winum-assign-func', or using `winum-auto-assign-0-to-minibuffer'."
  (let ((numbers))
    (dotimes (i winum--window-count)
      (push (1+ i) numbers))
    (nreverse numbers)))


(defun winum--check-for-scope-change ()
  "Check whether the `winum-scope' has been changed.
If a change is detected run `winum--init' to reinitialize all
internal data structures according to the new scope."
  (unless (eq winum-scope winum--last-used-scope)
    (setq winum--last-used-scope winum-scope)
    (winum--init)))


(defun winum--remove-deleted-frame-from-frames-table (frame)
  "Remove FRAME from `winum--frames-table' after it was deleted."
  (when winum--frames-table
    (remhash frame winum--frames-table)))


(provide 'winum-core)

;;; winum-core.el ends here
