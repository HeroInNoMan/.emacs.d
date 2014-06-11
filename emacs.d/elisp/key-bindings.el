;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom key-bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Key unbinding
(global-unset-key (kbd "C-x C-c")) ;; too easy to hit by accident, use “M-x kill-emacs” instead

;; key-chords
(key-chord-define-global (kbd "éè") 'rgrep) ;; call rgrep
(key-chord-define-global (kbd "«»") 'ibuffer) ;; call ibuffer
(key-chord-define-global (kbd "bf") 'ido-switch-buffer) ;; quickly switch buffer
(key-chord-define-global (kbd "qg") 'magit-status) ;; run git status for current buffer

;; function keys
(global-set-key (kbd "<f5>") 'reload-file) ;; re-read file from disk
(global-set-key (kbd "C-<f5>") 'copy-current-file-path) ;; copy current file path
(global-set-key (kbd "M-<f5>") 'show-file-name) ;; show the file name in minibuffer
(global-set-key (kbd "<f7>") 'recentf-open-files) ;; open a list of recently opened files
(global-set-key (kbd "<f12>") 'minimap-toggle) ;; toggle minimap
(global-set-key (kbd "<f8>") 'sublimity-mode) ;; toggle sublimity
(global-set-key (kbd "<f9>") 'ispell-word) ;; check spelling of word at point or words in region
(global-set-key (kbd "C-<f9>") 'flyspell-mode) ;; check spelling on the fly
(global-set-key (kbd "<f10>") 'dirtree) ;; call a visual directory tree to browse

;; custom shortcuts
(global-set-key (kbd "C-x r q") 'kill-emacs) ;; really quit emacs
(global-set-key (kbd "M-à") 'ace-jump-mode) ;; quickly jump to word by pressing its first letter
(global-set-key (kbd "C-x C-b") 'electric-buffer-list) ;; electric buffer by default
(global-set-key (kbd "C-M-z") 'undo) ;; useful when C-/ does not work (windows/putty)
(global-set-key (kbd "M-«") 'simplified-beginning-of-buffer) ;; useful when C-< does not work (windows/putty)
(global-set-key (kbd "M-»") 'simplified-end-of-buffer) ;; useful when C-> does not work (windows/putty)
(global-set-key (kbd "M-o") 'other-window) ;; quickly switch to other window

(global-set-key (kbd "C-c h") 'replace-string)
(global-set-key (kbd "C-c j") 'replace-regexp)
(global-set-key (kbd "C-c o") 'bury-buffer) ;; put buffer at bottom of buffer list
(global-set-key (kbd "C-c k") 'kill-this-buffer) ;; kill buffer without confirmation
;; (global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c i") 'iwb) ;; indent whole buffer
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
;; (global-set-key (kbd "<up>") 'previous-line)
;; (global-set-key (kbd "<down>") 'next-line)

;; eclipse-like shortcuts
(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key (kbd "<C-M-down>") 'duplicate-current-line)

(global-set-key (kbd "M-n") 'smart-symbol-go-forward) ;; find next occurence of word at point
(global-set-key (kbd "M-p") 'smart-symbol-go-backward) ;; find previous occurence of word at point

(global-set-key (kbd "C-c e") 'er/expand-region) ;; expand region by syntaxic units

;; Multiple cursors keybindings
(global-set-key (kbd "M-é") 'mc/edit-lines) ;; new cursor on each line of region
(global-set-key (kbd "M-è") 'mc/mark-all-like-this) ;; new cursor on each occurence of current region
(global-set-key (kbd "M-È") 'mc/mark-next-like-this) ;; new cursor on next occurence of current region
(global-set-key (kbd "M-É") 'mc/mark-previous-like-this) ;; new cursor on previous occurence of current region

;; Org-mode
(global-set-key (kbd "\C-c l") 'org-store-link)
(global-set-key (kbd "\C-c a") 'org-agenda)
(global-set-key (kbd "\C-c b") 'org-iswitchb)
(global-set-key (kbd "C-M-r") 'remember)

;; Remember
(global-set-key (kbd "\C-cr") 'org-remember)
(global-set-key (kbd "C-M-r") 'org-remember)

;; Groovy-Mode
(global-set-key (kbd "<f6>") 'groovy-eval-execute-buffer)

;; multi-scratch
(global-set-key (kbd "C-x \"") 'multi-scratch-new) ;; create new scratch buffer named “multi-scratch<#>”
(global-set-key (kbd "M-\"") 'multi-scratch-new) ;; create new scratch buffer named “multi-scratch<#>”
(global-set-key (kbd "C-x «") 'multi-scratch-prev) ;; jump to previous scratch buffer
(global-set-key (kbd "C-x »") 'multi-scratch-next) ;; jump to next scratch buffer

;; lisp manipulation
(global-set-key (kbd "C-c x") 'eval-and-replace) ;; eval sexp and replace it by its value 
