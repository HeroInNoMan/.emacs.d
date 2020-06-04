;;; font-size.el --- Change frame font size dynamically.

;; Copyright (C) 2016 by nabeix (Yusuke Watanabe)

;; Author: nabeix (Yusuke Watanabe) <yusukew052@gmail.com>
;; URL: https://github.com/nabeix/emacs-font-size
;; Version: 0.0.2

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Provide function `font-size-change' to change frame font size dynamically.

;;; Code:
(defgroup font-size nil
  "Change font size.")

(defcustom font-size--default-size 16
  "Default font size (default 16)."
  :type 'number
  :group 'font-size)

(defcustom font-size--current-size nil
  "Current font size."
  :type 'number
  :group 'font-size)

(defun font-size--adjust-current (size)
  "Adjust current font size."
  (custom-set-variables '(font-size--current-size size))
  (set-face-attribute 'default nil :height (* size 10)))

(defun font-size-decrease ()
  "Decrease font size."
  (interactive)
  (font-size--adjust-current (- font-size--current-size 1))
  (message "%dpt" font-size--current-size)
  )

(defun font-size-increase ()
  "Increase font size."
  (interactive)
  (font-size--adjust-current (+ font-size--current-size 1))
  (message "%dpt" font-size--current-size)
  )

(defun font-size-default ()
  "Adjust default font size."
  (interactive)
  (font-size--adjust-current font-size--default-size)
  (message "%dpt" font-size--current-size)
  )

(defun font-size-change ()
  "Change font size interactive."
  (interactive)
  (catch 'end-flag
    (while t
      (setq action
            (read-key-sequence-vector (format "Change font size[%dpt] (+,-,0) "
                                              font-size--current-size)))
      (setq c (aref action 0))
      (cond ((= c ?+)
             (setq size (+ font-size--current-size 1)))
            ((= c ?-)
             (setq size (- font-size--current-size 1)))
            ((= c ?0)
             (setq size font-size--default-size))
            (t
             (message "Quit")
             (throw 'end-flag t)))
      (font-size--adjust-current size)
      )))

(defun font-size-init (default-size)
  "Initialize font-size module."
  (custom-set-variables '(font-size--default-size default-size))
  (font-size--adjust-current default-size))

(provide 'font-size)

;;; font-size.el ends here
