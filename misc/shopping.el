;;; shopping.el --- summary -*- lexical-binding: t -*-

;; Author: Arthur Léothaud
;; Maintainer: Arthur Léothaud
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; Set of fuctions helpful to manage a shopping list in org-mode

;;; Code:

;; espace de test ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
* headline numéro 1
- [ ] tomate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun uncheck-or-increment-ingredient (ingredient)
  "Uncheck the checkbox or increment the value of INGREDIENT."
  (interactive)
  ;;(org-element-property )
  (org-element-contents
   ;; (org-element-map (org-element-parse-buffer) 'item
   ;;   (lambda (item) (eq (org-element-property :checkbox item) 'on))
   ;;   nil t)
   ;; )
   (org-element-map (org-element-parse-buffer) 'item
     (lambda (item) (eq (org-element-property :checkbox item) 'on))
     nil t)
   )

  (message "uncheck-or-increment-ingredient finished!"))

(global-set-key (kbd "<f6>") '(lambda () (interactive) (uncheck-or-increment-ingredient "tomate")))

(provide 'shopping)

;;; shopping.el ends here
