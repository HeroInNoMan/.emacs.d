;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key-chords (experimental key pairs)
(key-chord-define-global "«»"     'er/expand-region)
(key-chord-define-global "bf"     'ido-switch-buffer)
(key-chord-define-global "qg"     'magit-status) ;; git status

;; function keys
(global-set-key (kbd "<f5>") 'reload-file) ;; re-read file from disk
(global-set-key (kbd "<f7>") 'recentf-open-files) ;; set F7 to open a list of recently opened file
(global-set-key (kbd "<f8>") 'minimap-toggle) ;; toggle minimap
(global-set-key (kbd "<f9>") 'ispell-word) ;; check spelling of word at point or words in region
(global-set-key (kbd "C-<f9>") 'flyspell-mode) ;; check spelling on the fly

;; custom shortcuts
(global-set-key (kbd "M-à") 'ace-jump-mode)
(global-set-key "\C-x\C-b" 'electric-buffer-list) ;; Electric buffer by default
(global-set-key (kbd "\C-c\C-r") 'reload-file)
(global-set-key (kbd "C-M-z") 'undo) ;; usefull when C-/ does not work (windows/putty)
(global-set-key (kbd "C-c u") 'simplified-beginning-of-buffer) ;; usefull when C-< does not work (windows/putty)
(global-set-key (kbd "C-c d") 'simplified-end-of-buffer) ;; usefull when C-> does not work (windows/putty)
(global-set-key (kbd "M-«") 'simplified-beginning-of-buffer) ;; usefull when C-< does not work (windows/putty)
(global-set-key (kbd "M-»") 'simplified-end-of-buffer) ;; usefull when C-> does not work (windows/putty)

(global-set-key (kbd "C-c h") 'replace-string)
(global-set-key (kbd "C-c j") 'replace-regexp)
(global-set-key (kbd "C-c o") 'bury-buffer) ;; put buffer at bottom of buffer list
(global-set-key (kbd "C-c k") 'kill-this-buffer) ;; kill buffer without confirmation
;; (global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c i") 'iwb) ;; indent whole buffer
(global-set-key (kbd "C-S-l") 'my-horizontal-recenter)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
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

;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

