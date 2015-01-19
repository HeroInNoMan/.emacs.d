;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired
(setq global-auto-revert-non-file-buffers t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show active region
(setq-default transient-mark-mode t)

;; Don't move files to trash when deleting
(setq delete-by-moving-to-trash nil)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; <tab> inserts spaces, not tabs and spaces
;; (setq-default indent-tabs-mode nil)

;; No splash screen
(setq inhibit-startup-screen t)

;; No *scratch* message
(setq initial-scratch-message nil)

;; Answer questions with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Sentences end with a single space
(setq sentence-end-double-space nil)

;; UTF-8 everywhere
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Remove text in active region if inserting text
(pending-delete-mode t)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 70
(setq-default fill-column 80)

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Allow scrolling with mouse wheel
(mouse-wheel-mode t)

;; Don't soft-break lines for me, please
(setq-default truncate-lines t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Change how buffer names are made unique
(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Normal tab completion in Eshell
(setq eshell-cmpl-cycle-completions nil)

(provide 'sane-defaults)
