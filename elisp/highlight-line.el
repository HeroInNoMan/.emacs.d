;;; highlight-line.el --- Highlight lines in Dired, package menu and many other list-like buffers -*- lexical-binding: t; -*-
;;
;; Copyright © 2015 Mark Karpov <markkarpov@openmailbox.org>
;;
;; Author: Mark Karpov <markkarpov@openmailbox.org>
;; URL: https://github.com/mrkkrp/highlight-line
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, readability, Dired
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs has that pretty minor mode for highlighting current line:
;; `hl-line-mode', not many people use it all the time, but it's worth
;; enabling in many types of buffers that deal with lists of items, one item
;; per line. This packages allows to enable it everywhere for readability.

;;; Code:

(defvar highlight-line-target-modes
  '(Buffer-menu-mode
    bookmark-bmenu-mode
    dired-mode
    dired-by-name-mode
    gnus-group-mode
    gnus-summary-mode
    ibuffer-mode
    package-menu-mode)
  "List of modes that are affected by `highlight-line-mode'.")

;;;###autoload
(define-minor-mode highlight-line-mode
  "Toggle highlight-line-mode minor mode.
With a prefix argument ARG, enable highlight-line if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.
This minor mode is global. When it's active the following major
modes activate `hl-line-mode' automatically:
* buffer menu mode
* bookmark menu mode
* Dired mode
* GNUS group mode
* GNUS summary mode
* IBuffer mode
* package menu mode"
  :global t
  (dolist (mode highlight-line-target-modes)
    (funcall
     (if highlight-line-mode
         #'add-hook
       #'remove-hook)
     (intern (concat (symbol-name mode) "-hook"))
     #'hl-line-mode)))

(provide 'highlight-line)

;;; highlight-line.el ends here
