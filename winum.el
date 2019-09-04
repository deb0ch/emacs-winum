;;; winum.el --- Navigate windows and frames using numbers.
;;
;; Copyright (c) 2006-2015 Nikolaj Schumacher
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


;; Author: Thomas de Beauchêne <thomas.de.beauchene@gmail.com>
;; Version: 2.2.0
;; Keywords: convenience, frames, windows, multi-screen
;; URL: http://github.com/deb0ch/winum.el
;; Created: 2016
;; Compatibility: GNU Emacs 24.x
;; Package-requires: ((cl-lib "0.5") (dash "2.13.0"))
;;
;; This file is NOT part of GNU Emacs.


;;; Commentary:

;; Window numbers for Emacs: Navigate your windows and frames using numbers.
;;
;; This package is an extended and actively maintained version of the
;; https://github.com/nschum/window-numbering.el package by Nikolaj Schumacher,
;; with some ideas and code taken from https://github.com/abo-abo/ace-window.
;;
;; This version brings, among other things, support for number sets across multiple
;; frames, giving the user a smoother experience of multi-screen Emacs.
;;
;; FIXME: The mode-line's window number is not always up to date in all frames.


;;; Code:


(require 'winum-config)
(require 'winum-core)
(require 'winum-api)
(require 'winum-commands)


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


(add-hook 'delete-frame-functions #'winum--remove-deleted-frame-from-frames-table)


(push "^No window numbered .$"     debug-ignored-errors)
(push "^Got a dead window .$"      debug-ignored-errors)
(push "^Invalid `winum-scope': .$" debug-ignored-errors)


(provide 'winum)

;;; winum.el ends here
