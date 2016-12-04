;;; window-numbering.el --- Compatibility code for window-numbering configurations
;;
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
;;; Commentary:
;;
;; Ensure compatibility with existing window-numbering configurations.
;;
;; This file exists for transitional purposes and will be deleted in the next
;; updates.
;;
;;; Code:

(require 'winum)

(defalias 'select-window-0 'select-window-0-or-10)
(defalias 'window-numbering-get-number-string 'winum-get-number-string)
(defalias 'window-numbering-get-window-by-number 'winum-get-window-by-number)
(defalias 'window-numbering-get-number 'winum-get-number)
(defalias 'window-numbering-mode 'winum-mode)

(defvaralias 'window-numbering-mode 'winum-mode)
(defvaralias 'window-numbering-keymap 'winum-keymap)
(defvaralias 'window-numbering-ignored-buffers 'winum-ignored-buffers)
(defvaralias 'window-numbering-mode-line-position 'winum-mode-line-position)
(defvaralias 'window-numbering-before-hook 'winum-before-hook)
(defvaralias 'window-numbering-scope 'winum-scope)
(defvaralias 'window-numbering-auto-assign-0-to-minibuffer 'winum-auto-assign-0-to-minibuffer)
(defvaralias 'window-numbering-assign-func 'winum-assign-func)
(defvaralias 'window-numbering-window-number-max 'winum-window-number-max)
(defvaralias 'window-numbering-reverse-frame-list 'winum-reverse-frame-list)

(provide 'window-numbering)
