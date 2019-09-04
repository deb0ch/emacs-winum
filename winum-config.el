;;; winum-config.el --- winum configuration options
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

;; Options to configure Winum from Customize or user config.


;;; Code:


(defgroup winum nil
  "Navigate and manage windows using numbers."
  :group 'convenience)


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
ways to have 0 assigned to a window.

Example: always assign *Calculator* the number 9 and *NeoTree* the number 0:

  (defun my-winum-assign-func ()
    (cond
     ((equal (buffer-name) \"*Calculator*\")
      9)
     ((string-match-p (buffer-name) \".*\\*NeoTree\\*.*\")
      0)
     (t
      nil)))

  (setq winum-assign-func 'my-winum-assign-func)"
  :group 'winum
  :type  'function)


(make-obsolete-variable 'winum-assign-func 'winum-assign-functions "2.0.0")


(defcustom winum-assign-functions nil
  "List of functions called for each window by `winum-mode'.

These functions allow for deterministic assignment of numbers to windows. Each
function is called for every window. A function should return the number to be
assigned to a window or nil. The *first* function to output a number for
a given window will determine this window's number.

If the list is empty or if every functions returns nil for a given window winum
will proceed to automatic number assignment.

Since this list is meant to allow custom window assignment for *mutiple*
packages at once it should never be directly set, only added to and removed
from.

These functions, along with `winum-auto-assign-0-to-minibuffer', are the only
way to have 0 assigned to a window.

Example: always assign *Calculator* the number 9, *Flycheck-errors* the number 8
and *NeoTree* the number 0:

  (defun winum-assign-9-to-calculator-8-to-flycheck-errors ()
    (cond
     ((equal (buffer-name) \"*Calculator*\") 9)
     ((equal (buffer-name) \"*Flycheck errors*\") 8)))

  (defun winum-assign-0-to-neotree ()
    (when (string-match-p (buffer-name) \".*\\*NeoTree\\*.*\") 10))

  (add-to-list
    'winum-assign-functions #'winum-assign-9-to-calculator-8-to-flycheck-errors)
  (add-to-list
    'winum-assign-functions #'winum-assign-0-to-neotree)"
  :group 'winum
  :type  'list)


(defcustom winum-auto-setup-mode-line t
  "When nil, `winum-mode' will not display window numbers in the mode-line.
You might want this to be nil if you use a package that already manages window
numbers in the mode-line."
  :group 'winum
  :type  'boolean)


(defcustom winum-mode-line-position 1
  "The position in the mode-line `winum-mode' displays the number."
  :group 'winum
  :type  'integer)


(defcustom winum-format " %s "
  "Format string defining how the window number looks like in the mode-line.
This string is passed to the `format' function along with the
result of `winum-get-number-string'."
  :group 'winum
  :type  'string)


(defcustom winum-ignored-buffers '(" *which-key*")
  "List of buffers to ignore when assigning numbers."
  :group 'winum
  :type  '(repeat string))


(defcustom winum-ignored-buffers-regexp '()
  "List of regexps for buffer names to ignore when assigning numbers.
See Info node `(emacs) Regexps' or Info node `(elisp) Regular Expressions'"
  :group 'winum
  :type '(repeat string)
  :risky t)


(defface winum-face '()
  "Face used for the number in the mode-line."
  :group 'winum)


(provide 'winum-config)

;;; winum-config.el ends here
