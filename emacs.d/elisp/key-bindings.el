;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "\C-c\C-r") 'reload-file)
(global-set-key (kbd "C-M-z") 'undo)
(global-set-key (kbd "C-c u") 'simplified-beginning-of-buffer)
(global-set-key (kbd "C-c d") 'simplified-end-of-buffer)
(global-set-key (kbd "C-c h") 'replace-string)
(global-set-key (kbd "C-c j") 'replace-regexp)
(global-set-key (kbd "C-c o") 'bury-buffer)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
;; (global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c i") 'iwb)
(global-set-key (kbd "C-S-l") 'my-horizontal-recenter)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
;; (global-set-key (kbd "<up>") 'up-arrow)
;; (global-set-key (kbd "<down>") 'down-arrow)
;; (global-set-key (kbd "<up>") 'previous-line)
;; (global-set-key (kbd "<down>") 'next-line)
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key (kbd "<C-M-down>") 'duplicate-current-line)

(define-key global-map (read-kbd-macro "C--") 'font-zoom-decrease-font-size)
(define-key global-map (read-kbd-macro "C-+") 'font-zoom-increase-font-size)
(define-key global-map (read-kbd-macro "C-=") 'font-zoom-reset-font-size)

(global-set-key (kbd "M-n") 'smart-symbol-go-forward)
(global-set-key (kbd "M-p") 'smart-symbol-go-backward)

(global-set-key (kbd "C-c e") 'er/expand-region)

;; Multiple cursors keybindings
(global-set-key (kbd "M-é") 'mc/edit-lines)
(global-set-key (kbd "M-è") 'mc/mark-all-like-this)
(global-set-key (kbd "M-È") 'mc/mark-next-like-this)
(global-set-key (kbd "M-É") 'mc/mark-previous-like-this)

