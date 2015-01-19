;;;;;;;;;;;;;;;;;;;;;;;
;; emacs config file ;;
;;;;;;;;;;;;;;;;;;;;;;;

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
					 dirtree
					 expand-region
					 flx-ido
					 gitconfig-mode
					 guide-key
					 guide-key-tip
					 helm
					 helm-projectile
					 ido-ubiquitous
					 ido-vertical-mode
					 key-chord
					 magit
					 markdown-mode
					 move-text
					 multiple-cursors
					 smartscan
					 tree-mode
					 undo-tree))

;; fetch the list of packages available if no elpa dir present
(or (file-exists-p package-user-dir) (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; initialize files and directories as variables
(setq elisp-dir (expand-file-name "elisp" user-emacs-directory)) ;; .emacs.d/elisp/
(add-to-list 'load-path elisp-dir) ;; load everything in .emacs.d/elisp/

;; custom conf files
(require 'sane-defaults) ;; sane defaults from danjacka
(require 'my-functions) ;; custom functions
(require 'my-key-bindings) ;; custom keybindings
(require 'my-macros) ;; custom macros

;; load packages
(require 'browse-kill-ring)
(require 'dirtree)
(require 'guide-key)
(require 'guide-key-tip)
(require 'iso-transl)
(require 'ispell)
(require 'key-chord)
(require 'multi-scratch)
(require 're-builder)
(require 'smartscan)
;; (require 'minimap) ;; bugs with org-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VARIOUS SETTINGS

;; case-insensitive policy
(setq read-file-name-completion-ignore-case t) ;; case-insensitive completion
(setq read-buffer-completion-ignore-case t) ;; case-insensitive completion

;; colors, appearance
(setq font-lock-maximum-decoration t) ;; all possible colours
(blink-cursor-mode -1) ;; no blinking cursor
(global-hl-line-mode -1) ;; don’t highlight current line
(setq-default show-trailing-whitespace nil) ;; don’t display trailing whitespaces

;; activate additional features
(undo-tree-mode t) ;; powerfull undo/redo mode
(ido-vertical-mode 1)
(flx-ido-mode 1)
(helm-mode 0) ;; helm-mode in all other places
(key-chord-mode 1)
(move-text-default-bindings) ;; M-up / M-down to move line or region
(add-hook 'after-init-hook 'global-company-mode) ;; enable company in all buffers
(setq ido-everywhere t) ;; ido goodness everywhere

;; guide key
(guide-key-mode 1)
(setq guide-key-tip/enabled t)
(setq guide-key/guide-key-sequence '("C-x r" "C-x <RET>" "C-x v" "C-x 4" "C-x 5" "C-x 6" "C-x 8" "C-c" "C-x C-k" "C-x n" "C-x +" "C-h" "<f1>" "<f2>" ))
(setq guide-key/recursive-key-sequence-flag t)
(setq guide-key/popup-window-position 'bottom)

;; regexp-builder
(setq reb-re-syntax 'string) ;; syntax used in the re-buidler

;; dired
(setq dired-listing-switches "-AlhGF") ;; dired human readable size format, hide group

;; scratch
(setq multi-scratch-buffer-name "new")

;; date, time, calendar
(setq display-time-day-and-date t ;; display date and time
      display-time-24hr-format t) ;; 24h time format
(setq european-calendar-style t) ;; day/month/year format for calendar
(setq calendar-week-start-day 1) ;; start week on Monday
(display-time) ;; display time

;; text-mode, mail-mode, spellchecking
(setq default-major-mode 'text-mode) ;; text-mode by default
(add-hook 'text-mode-hook 'visual-line-mode) ;; auto-wrapping (soft wrap) in text-mode
(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; no auto-fill since I use visual-line-mode
(setq ispell-dictionary "francais") ;; french dictionary for auto-correct
(setq-default ispell-program-name "aspell") ;; aspell by default
(remove-hook 'text-mode-hook 'flyspell-mode) ;; auto-correct disabled by default
(remove-hook 'html-helper-mode-hook 'flyspell-mode) ;; auto-correct disabled by default
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping in mail-mode

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

;; set default browser to firefox
(setq gnus-button-url 'browse-url-generic
      browse-url-generic-program "firefox"
      browse-url-browser-function gnus-button-url)

;; kill-ring
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; session saving, backup management
(setq vc-make-backup-files t) ;; make backups of files, even when they're in version control
(setq desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-save                t
	  desktop-dirname             user-emacs-directory
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode)
(desktop-read)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE-SPECIFIC CONFIGURATION

;; git
(remove-hook 'git-commit-mode-hook 'flyspell-mode) ;; auto-correct disabled in git-commit buffers
(autoload 'gitconfig-mode "gitconfig-mode" "Major mode for editing gitconfig files." t)
(setq auto-mode-alist  (cons '(".gitconfig$" . gitconfig-mode) auto-mode-alist))

;; SH
;; (add-hook 'sh-mode-hook (lambda () (setq tab-width 4 sh-basic-offset 4 indent-tabs-mode t)))
;; (autoload 'sh-mode "sh-mode" "Major mode for editing shell scripts." t)
(add-to-list 'auto-mode-alist '("rc$" . sh-mode))
(add-to-list 'auto-mode-alist '("bash" . sh-mode))

;; SQL
(setq auto-mode-alist  (cons '(".sql$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".pks$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".pkb$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".mvw$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".con$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".ind$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".sqs$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".tab$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".trg$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".vw$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".prc$" . sql-mode) auto-mode-alist)
	  auto-mode-alist  (cons '(".pk$" . sql-mode) auto-mode-alist))
;;; sql-oracle connection without a tnsnames.ora
;; (description=(address_list=(address=(protocol=TCP)(host=myhost.example.com)(port=1521)))(connect_data=(SERVICE_NAME=myservicename)))
;; GÉO : (description=(address_list=(address=(protocol=TCP)(host=DEV-GEO-BACK)(port=1521)))(connect_data=(SID=GEODEV1)
(add-hook 'sql-mode (setq truncate-lines nil))
(add-hook 'sql-mode (setq linesize 9999))

(add-hook 'sql-interactive-mode-hook
          (function (lambda ()
                      (setq comint-output-filter-functions 'comint-truncate-buffer
                            comint-buffer-maximum-size 5000
                            comint-scroll-show-maximum-output t
                            comint-input-ring-size 500))))

;; GROOVY
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(add-to-list 'load-path (expand-file-name "groovy-mode" elisp-dir)) ;; manual load-path
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
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))

;; HTML
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))
(setq html-helper-use-expert-menu t) ;; use expert menu
(add-hook 'html-helper-load-hook 'my-html-helper-load-hook) ;; automatically indent html

;; JAVASCRIPT (to be tested)
(autoload 'json-pretty-print "json-pretty-print" "json-pretty-print" t)
(add-hook 'json-mode-hook 'json-pretty-print)

;; WIKI
;; confluence mode + it’s-all-text
(add-to-list 'auto-mode-alist '("\.wiki\.vsct\.fr.*\.txt$" . confluence-edit-mode))

;; PYTHON
;; (add-hook 'python-mode-hook 'jedi:setup) ;; fire up jedi in python env
;; (setq jedi:complete-on-dot t) ;; optional

;; ORG-MODE
;; notes in
(setq org-default-notes-file
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory ".notes")))))

;; font and faces customization
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("STARTED" . (:foreground "yellow" :weight bold))
        ("WAIT" . (:foreground "yellow" :weight bold))
        ("INPROGRESS" . (:foreground "yellow" :weight bold))
        ))

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
;; EPILOGUE

(find-file (expand-file-name "init.el" user-emacs-directory))

(color-theme-initialize)
(color-theme-dark-laptop)

(server-start)

;; init.el ends here.
