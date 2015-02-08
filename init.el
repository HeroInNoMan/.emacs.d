;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    emacs config file                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALLATION & LOADING

;; package-style dependencies
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

;; packages to be installed and loaded
(setq package-list '(ace-jump-mode
					 ace-window
					 better-defaults
					 browse-kill-ring
					 color-theme
					 company
					 diminish
					 dirtree
					 expand-region
					 flx-ido
					 gitconfig-mode
					 guide-key
					 guide-key-tip
					 helm
					 helm-projectile
					 idle-highlight-mode
					 ido-ubiquitous
					 key-chord
					 magit
					 markdown-mode
					 move-text
					 multiple-cursors
					 smartscan
					 tree-mode
					 undo-tree
					 web-mode
					 w3m))

;; fetch the list of packages available if no elpa dir present
(or (file-exists-p package-user-dir) (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
	(package-install package)))

;; add .emacs.d/elisp/ and .emacs.d/elisp/groovy-mode/ to load-path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/groovy-mode" user-emacs-directory))

;; custom conf files
(require 'my-functions) ;; custom functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS

;; No splash screen
(setq inhibit-startup-screen t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Do not auto refresh dired
(setq global-auto-revert-non-file-buffers nil)

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
(setq line-number-mode t
	  column-number-mode t)

;; Lines should be 80 characters wide, not 70
(setq-default fill-column 80)

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 500) ;; just 20 is too recent

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Allow scrolling with mouse wheel
(when (display-graphic-p) (mouse-wheel-mode t))

;; Don't soft-break lines for me, please
(setq-default truncate-lines t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Change how buffer names are made unique
(setq uniquify-buffer-name-style 'post-forward
	  uniquify-separator ":")

;; A saner ediff
(setq ediff-diff-options "-w"
	  ediff-split-window-function 'split-window-horizontally
	  ediff-window-setup-function 'ediff-setup-windows-plain)

;; Normal tab completion in Eshell
(setq eshell-cmpl-cycle-completions nil)

;; No flashing!
(setq visible-bell nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIOUS SETTINGS

;; Killing emacs
(global-unset-key (kbd "C-x C-c")) ;; too easy to hit by accident, use “M-x kill-emacs” instead
(global-set-key (kbd "C-x r q") 'kill-emacs) ;; really quit emacs

;; C-z is for tmux
(global-unset-key (kbd "C-z")) ;; used for tmux (C-z z for suspend-frame)
(global-set-key (kbd "C-z z") 'suspend-frame) ;; C-z is saved for tmux

;; case-insensitive policy
(setq read-file-name-completion-ignore-case t ;; case-insensitive completion
	  read-buffer-completion-ignore-case t) ;; case-insensitive completion

;; colors, appearance
(require 'iso-transl) ;; some environments don’t handle dead keys
(setq font-lock-maximum-decoration t) ;; all possible colours
(blink-cursor-mode -1) ;; no blinking cursor
(global-hl-line-mode -1) ;; don’t highlight current line
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c i") 'iwb) ;; indent whole buffer

;; some visual modes toggling with function keys
(global-set-key (kbd "<f6>") 'whitespace-mode)
(global-set-key (kbd "C-<f6>") 'toggle-show-trailing-whitespace)
(global-set-key (kbd "<f7>") 'linum-mode)
(global-set-key (kbd "<f8>") 'idle-highlight-mode)

;; activate key-chords
(require 'key-chord)
(key-chord-mode 1)

;; buffer & file handling
(key-chord-define-global (kbd "«»") 'ibuffer) ;; call ibuffer
(key-chord-define-global (kbd "bf") 'ido-switch-buffer) ;; quickly switch buffer
(global-set-key (kbd "C-x C-b") 'electric-buffer-list) ;; electric buffer by default
(global-set-key (kbd "C-c f") 'recentf-open-files) ;; open a list of recently opened files
(global-set-key (kbd "C-c o") 'bury-buffer) ;; put buffer at bottom of buffer list
(global-set-key (kbd "C-c k") 'kill-this-buffer) ;; kill buffer without confirmation
(global-set-key (kbd "M-o") 'ace-window) ;; quickly switch to other window
(global-set-key (kbd "<f5>") 'reload-file) ;; re-read file from disk
(global-set-key (kbd "C-<f5>") 'copy-current-file-path) ;; copy current file path
(global-set-key (kbd "M-<f5>") 'show-file-name) ;; show the file name in minibuffer
(global-set-key (kbd "C-x C-r") 'sudo-edit) ;; sudo open file

;; undo
(undo-tree-mode t) ;; powerfull undo/redo mode
(diminish 'undo-tree-mode)
(global-set-key (kbd "C-M-z") 'undo) ;; useful when C-/ does not work (windows/putty)

;; smartscan
(require 'smartscan)
(global-set-key (kbd "M-n") 'smartscan-symbol-go-forward) ;; find next occurence of word at point
(global-set-key (kbd "M-p") 'smartscan-symbol-go-backward) ;; find previous occurence of word at point
(global-set-key (kbd "M-'") 'smartscan-symbol-replace) ;; replace all occurences of word at point

;; line handling features
(move-text-default-bindings) ;; M-up / M-down to move line or region
(global-set-key (kbd "<C-M-down>") 'duplicate-current-line)
;; (global-set-key (kbd "<up>") 'previous-line)
;; (global-set-key (kbd "<down>") 'next-line)

;; activate additional features
(helm-mode t) ;; helm-mode in all other places
(diminish 'helm-mode)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x) ;; superior to M-x
(setq helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
	  helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(global-set-key (kbd "C-c h") 'helm-mini) ;; call helm completion
(global-set-key (kbd "M-ç") 'helm-mini) ;; call helm for current buffers and recent files
(global-set-key (kbd "C-ç") 'helm-find-files) ;; call helm for new files

;; dirtree
(require 'dirtree)
(global-set-key (kbd "<f10>") 'dirtree) ;; call a visual directory tree to browse

;; minimap (disabled because it causes org-mode to bug)
;; (require 'minimap)
;; (global-set-key (kbd "<f12>") 'minimap-toggle) ;; toggle minimap

;; cursor movement and features
(global-set-key (kbd "C-c e") 'er/expand-region) ;; expand region by syntaxic units
(global-set-key (kbd "M-à") 'ace-jump-mode) ;; quickly jump to word by pressing its first letter
(global-set-key (kbd "C-à") 'ace-jump-char-mode) ;; quickly jump to any char in word
(global-set-key (kbd "M-«") 'simplified-beginning-of-buffer) ;; useful when C-< does not work (windows/putty)
(global-set-key (kbd "M-»") 'simplified-end-of-buffer) ;; useful when C-> does not work (windows/putty)


;; Multiple cursors keybindings
(global-set-key (kbd "M-é") 'mc/edit-lines) ;; new cursor on each line of region
(global-set-key (kbd "M-è") 'mc/mark-all-like-this) ;; new cursor on each occurence of current region
(global-set-key (kbd "M-È") 'mc/mark-next-like-this) ;; new cursor on next occurence of current region
(global-set-key (kbd "M-É") 'mc/mark-previous-like-this) ;; new cursor on previous occurence of current region

;; auto-completion with company-mode
(global-company-mode) ;; enable company in all buffers
(diminish 'company-mode)

;; ido
(ido-ubiquitous-mode t)
(ido-everywhere t)
(flx-ido-mode 1)

;; projectile-mode
(projectile-global-mode) ;; activate projectile-mode everywhere
(setq projectile-enable-caching t) ;; enable caching for projectile-mode

;; spell-check
(require 'ispell)
(setq ispell-dictionary "francais") ;; french dictionary for auto-correct
(setq-default ispell-program-name "aspell") ;; aspell by default
(global-set-key (kbd "<f9>") 'flyspell-mode) ;; check spelling on the fly

;; guide key
(require 'guide-key)
(require 'guide-key-tip)
(guide-key-mode 1)
(diminish 'guide-key-mode)
(setq guide-key-tip/enabled t
	  guide-key/recursive-key-sequence-flag t
	  guide-key/popup-window-position 'right
	  guide-key/guide-key-sequence '("<f1>"
									 "<f2>"
									 "C-c"
									 "C-h"
									 "C-x <RET>"
									 "C-x +"
									 "C-x n"
									 "C-x r"
									 "C-x v"
									 "C-x 4"
									 "C-x 5"
									 "C-x 6"
									 "C-x 8"
									 "C-x C-k"
									 "C-x C-v"
									 "M-s"))

;; regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string) ;; syntax used in the re-buidler

;; dired
(setq dired-listing-switches "-AlhGF") ;; dired human readable size format, hide group

;; scratch
(require 'multi-scratch)
(setq multi-scratch-buffer-name "new")
(global-set-key (kbd "C-x \"") 'multi-scratch-new) ;; create new scratch buffer named “new<#>”
(global-set-key (kbd "M-\"") 'multi-scratch-new) ;; create new scratch buffer named “new<#>”
(global-set-key (kbd "C-x «") 'multi-scratch-prev) ;; jump to previous scratch buffer
(global-set-key (kbd "C-x »") 'multi-scratch-next) ;; jump to next scratch buffer

;; date, time, calendar
(setq display-time-day-and-date t ;; display date and time
	  display-time-24hr-format t ;; 24h time format
	  european-calendar-style t ;; day/month/year format for calendar
	  calendar-week-start-day 1) ;; start week on Monday
(display-time) ;; display time

;;Indentation
(setq-default tab-width 4
			  c-auto-newline t
			  c-basic-offset 4
			  c-block-comment-prefix ""
			  c-default-style "k&r"
			  indent-tabs-mode t)

;; enable commands disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)

;; set default browser to firefox
(setq gnus-button-url 'browse-url-generic
	  browse-url-generic-program "firefox"
	  browse-url-browser-function gnus-button-url)

;; kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; session saving, backup management
(setq vc-make-backup-files t) ;; make backups of files, even when they're in version control
(setq desktop-base-lock-name      "lock"
	  desktop-save                t
	  desktop-dirname             user-emacs-directory
	  desktop-path                (list desktop-dirname)
	  ;; desktop-files-not-to-save   "^$" ;reload tramp paths
	  desktop-load-locked-desktop nil)
(desktop-save-mode 1)
(savehist-mode 1)
(desktop-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAJOR MODE SPECIFIC CONFIGURATION

;; text
(setq default-major-mode 'text-mode) ;; text-mode by default
(add-hook 'text-mode-hook 'visual-line-mode) ;; auto-wrapping (soft wrap) in text-mode
(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; no auto-fill since I use visual-line-mode
(remove-hook 'text-mode-hook 'flyspell-mode) ;; auto-correct disabled by default

;; mail-mode
(remove-hook 'html-helper-mode-hook 'flyspell-mode) ;; auto-correct disabled by default
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping in mail-mode

;; git
;; (remove-hook 'git-commit-mode-hook 'flyspell-mode) ;; auto-correct disabled in git-commit buffers
(autoload 'gitconfig-mode "gitconfig-mode" "Major mode for editing gitconfig files." t)
(add-to-list 'auto-mode-alist '(".gitconfig$" . gitconfig-mode))
(key-chord-define-global (kbd "qg") 'magit-status) ;; run git status for current buffer

;; SH
(add-hook 'sh-mode-hook (lambda () (setq tab-width 4 sh-basic-offset 4 indent-tabs-mode t)))
;;(autoload 'sh-mode "sh-mode" "Major mode for editing shell scripts." t)
(add-to-list 'auto-mode-alist '(".*rc$" . sh-mode))
(add-to-list 'auto-mode-alist '(".*bash.*$" . sh-mode))

;; SQL
(add-to-list 'auto-mode-alist '(".sql$" . sql-mode))
(add-to-list 'auto-mode-alist '(".pks$" . sql-mode))
(add-to-list 'auto-mode-alist '(".pkb$" . sql-mode))
(add-to-list 'auto-mode-alist '(".mvw$" . sql-mode))
(add-to-list 'auto-mode-alist '(".con$" . sql-mode))
(add-to-list 'auto-mode-alist '(".ind$" . sql-mode))
(add-to-list 'auto-mode-alist '(".sqs$" . sql-mode))
(add-to-list 'auto-mode-alist '(".tab$" . sql-mode))
(add-to-list 'auto-mode-alist '(".trg$" . sql-mode))
(add-to-list 'auto-mode-alist '(".vw$" . sql-mode))
(add-to-list 'auto-mode-alist '(".prc$" . sql-mode))
(add-to-list 'auto-mode-alist '(".pk$" . sql-mode))
;;; sql-oracle connection without a tnsnames.ora
;; (description=(address_list=(address=(protocol=TCP)(host=myhost.example.com)(port=1521)))(connect_data=(SERVICE_NAME=myservicename)))
;; GÉO : (description=(address_list=(address=(protocol=TCP)(host=DEV-GEO-BACK)(port=1521)))(connect_data=(SID=GEODEV1)
(add-hook 'sql-mode (setq truncate-lines nil))
(add-hook 'sql-mode (setq linesize 9999))

(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
(add-hook 'sql-interactive-mode-hook
		  (function (lambda ()
					  (setq comint-output-filter-functions 'comint-truncate-buffer
							comint-buffer-maximum-size 5000
							comint-scroll-show-maximum-output t
							comint-input-ring-size 500))))

;; GROOVY
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
		  '(lambda ()
			 (require 'groovy-electric)
			 (groovy-electric-mode)))
(autoload 'groovy-eval "groovy-eval" "Groovy Evaluation" t)
(add-hook 'groovy-mode-hook 'groovy-eval)

;; RUBY
;; Loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(add-to-list 'auto-mode-alist '(".rb$" . ruby-mode))

;; HTML, XML, JSP (using web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))

(setq web-mode-engines-alist '(("php" . "\\.phtml\\'")
							   ("blade" . "\\.blade\\.")))

;; Java
;; (global-set-key (kbd "<f12>") 'ctags-create-or-update-tags-table) ;; exuberant ctags update

;; JAVASCRIPT (to be tested)
(autoload 'json-pretty-print "json-pretty-print" "json-pretty-print" t)
(add-hook 'json-mode-hook 'json-pretty-print)

;; LISP
(global-set-key (kbd "C-c x") 'eval-and-replace) ;; eval sexp and replace it by its value
;; (global-set-key (kbd "C-c c") 'compile)

;; PYTHON
;; (add-hook 'python-mode-hook 'jedi:setup) ;; fire up jedi in python env
;; (setq jedi:complete-on-dot t) ;; optional

;; ORG-MODE
;; notes in
(setq org-default-notes-file
	  `(("." . ,(expand-file-name
				 (concat user-emacs-directory ".notes")))))
(setq org-completion-use-ido t)
;; font and faces customization
(setq org-todo-keyword-faces
	  '(("INPR" . (:foreground "yellow" :weight bold))
		("STARTED" . (:foreground "yellow" :weight bold))
		("WAIT" . (:foreground "yellow" :weight bold))
		("INPROGRESS" . (:foreground "yellow" :weight bold))))
(global-set-key (kbd "\C-c l") 'org-store-link)
(global-set-key (kbd "\C-c a") 'org-agenda)
(global-set-key (kbd "\C-c b") 'org-iswitchb)
(global-set-key (kbd "C-M-r") 'remember)
;; Remember
(global-set-key (kbd "\C-cr") 'org-remember)
(global-set-key (kbd "C-M-r") 'org-remember)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specific configuration

;; Load system-specific library and setup system-specific things that
;; must be setup before main setup
(defun load-windows-specific-conf ()
  "Loads all windows-nt specific conf"
  (set-clipboard-coding-system 'utf-16le-dos) ;; MS Windows clipboard is UTF-16LE
  ;; cygwin conf
  (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
  (setq exec-path (cons "c:/cygwin/bin/" exec-path))
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  (require 'dos) ;; batch scripts
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict/")
  (setq python-shell-interpreter "c:/cygwin/bin/python3.2m.exe")

  ;; Prevent issues with the Windows null device (NUL)
  ;; when using cygwin find with rgrep.
  (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
	"Use cygwin's /dev/null as the null-device."
	(let ((null-device "/dev/null"))
	  ad-do-it))
  (ad-activate 'grep-compute-defaults)
  )
(defun load-linux-specific-conf ()
  "Loads all GNU/Linux specific conf"
  )
(cond ((eq system-type 'windows-nt) (load-windows-specific-conf))
	  ((eq system-type 'gnu/linux) (load-linux-specific-conf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIOUS KEY BINDINGS
(key-chord-define-global (kbd "éè") 'rgrep) ;; call rgrep

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EPILOGUE

(find-file (expand-file-name "init.el" user-emacs-directory))

;; set up a nice dark theme
(color-theme-initialize)
(color-theme-dark-laptop)

;; server mode
(server-start)
(global-set-key (kbd "M-#") 'server-edit) ;; send back to server, quicker than C-x #

;; init.el ends here.
