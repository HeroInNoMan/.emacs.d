;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el config file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tool-bar-mode -1) ;; no tool-bar (only in non-shell emacs)
(menu-bar-mode -1) ;; no menu-bar
(setq inhibit-startup-message t) ;; no message at startup

;; package-style dependencies
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  )

;; initialize directories as variables
(setq conf-dir (file-name-directory load-file-name)) ;; emacs-conf/
(setq elisp-dir (expand-file-name "elisp" conf-dir)) ;; emacs-conf/elisp/
(setq elpa-dir (expand-file-name "elpa" conf-dir)) ;; emacs-conf/elpa/
(setq backup-dir (expand-file-name "backups" conf-dir)) ;; emacs-conf/backups/
(setq desktop-dir (expand-file-name "desktop" conf-dir)) ;; emacs-conf/desktop/
(setq places-file (expand-file-name ".places" conf-dir)) ;; emacs-conf/.places

(add-to-list 'load-path elisp-dir) ;; manual load-path
(add-to-list 'load-path elpa-dir) ;; package-managed load-path

(require 'misc-functions) ;; custom functions
(require 'key-bindings) ;; custom keybindings
(require 'dirtree) ;; tree view for directories

(require 'multi-scratch)
(setq multi-scratch-buffer-name "untitled")

(require 're-builder)
(setq reb-re-syntax 'string) ;; syntax used in the re-buidler

(recentf-mode 1) ;; keep a list of recently opened files
(setq-default transient-mark-mode t) ;; colour between mark and point
(setq default-major-mode 'text-mode) ;; text-mode by default
(add-hook 'text-mode-hook 'visual-line-mode) ;; auto-wrapping (soft wrap) in text-mode
(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; no auto-fill in text-mode since I use visual-line-mode
(global-font-lock-mode t) ;; syntax highlight
(setq font-lock-maximum-decoration t) ;; all possible colours
(setq initial-scratch-message nil) ;; empty *scratch*
(setq read-file-name-completion-ignore-case t) ;; case-insensitive completion
(setq read-buffer-completion-ignore-case t) ;; case-insensitive completion
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping in mail-mode
(setq column-number-mode t) ;; display column-number
(setq-default show-trailing-whitespace nil) ;; don’t display trailing whitespace
(setq display-time-day-and-date t ;; display date and time
      display-time-24hr-format t) ;; 24h time format
(display-time) ;; display time
(fset 'yes-or-no-p 'y-or-n-p) ;; short answer mode
(setq european-calendar-style t) ;; day/month/year format for calendar
(setq calendar-week-start-day 1) ;; start week on Monday
(setq sentence-end-double-space nil) ;; sentences end with a single space
(setq c-auto-newline t) ;; automatically indent - no need to tab
(setq-default tab-width 4) ;; eclipse-like
(setq-default indent-tabs-mode t) ;; indentation uses tabs instead of spaces
(global-hl-line-mode -1) ;; don’t highlight current line
(auto-compression-mode 1) ;; parse, open, modify and save compressed archives
(blink-cursor-mode -1) ;; no blinking cursor
(ido-mode 1) ;; better prompt for buffer search / switch
(ido-vertical-mode 1)
(flx-ido-mode 1)
(helm-mode 0) ;; helm-mode in all other places

(show-paren-mode t) ;; hl parenthesis couples
(setq show-paren-delay 0)           ;; how long to wait before displaying parenthesis couple
(setq show-paren-style 'parenthesis) ;; alternatives are 'parenthesis' and 'mixed'

;; dired customization
(setq dired-listing-switches "-AlhGF") ;; human readable size format, hide group

(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x <RET>" "C-x v" "C-x 4" "C-x 5" "C-x 6" "C-c" "C-x C-k" "C-x n" "C-h" "<f1>" "<f2>" ))
(guide-key-mode 1) ; Enable guide-key-mode

(require 'guide-key-tip) ;; guide key as tip
(setq guide-key-tip/enabled t)

;; Spellchecking
(require 'ispell)
(setq ispell-dictionary "francais") ;; french dictionary for auto-correct
(setq-default ispell-program-name "aspell") ;; aspell by default
;; (add-hook 'text-mode-hook 'flyspell-mode) ;; auto-correct in text mode
;; (add-hook 'html-helper-mode-hook 'flyspell-mode) ;; auto-correct in html mode

;;Indentation
(setq tab-width 4
      c-default-style "k&r"
      c-block-comment-prefix ""
      c-basic-offset 4)
(setq truncate-lines t)

;; Encodage
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)) ;; from Emacs wiki

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; set default browser to firefox
(setq gnus-button-url 'browse-url-generic
      browse-url-generic-program "firefox"
      browse-url-browser-function gnus-button-url)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,backup-dir)))

(setq vc-make-backup-files t) ;; make backups of files, even when they're in version control

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file places-file)

;; Automatically save and restore sessions
(setq desktop-dirname             desktop-dir
      desktop-base-file-name      "emacs.desktop"
      desktop-base-lock-name      "lock"
      desktop-path                (list desktop-dirname)
      desktop-save                t
      desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop nil)
(desktop-save-mode 0)
(desktop-read)

;; minimap
;; (require 'minimap)

;; Sublimity Mode
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)

;; ERC conf
(require 'erc-hl-nicks)
(setq erc-input-line-position -2) ;; so the prompt is always at the bottom

(require 'key-chord)
(key-chord-mode 1)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)

(undo-tree-mode t) ;; powerfull undo/redo mode

(server-start) ;; start in server mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-specific configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SH
(add-hook 'sh-mode-hook (lambda () (setq tab-width 4 sh-basic-offset 4 indent-tabs-mode t)))

;; SQL
(setq auto-mode-alist  (cons '(".sql$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".pks$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".pkb$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".mvw$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".con$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".ind$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".sqs$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".tab$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".trg$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".vw$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".prc$" . sql-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".pk$" . sql-mode) auto-mode-alist))
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
;; basic html configuration
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))
(setq html-helper-use-expert-menu t) ;; use expert menu
(add-hook 'html-helper-load-hook 'my-html-helper-load-hook) ;; automatically indent html
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
                 (concat conf-dir ".notes")))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

  (require 'tramp)
  (set-default 'tramp-auto-save-directory "c:/cygwin/home/arthur_leothaud/tramp_tmp/")
  (set-default 'tramp-default-method "plink")
  ;; tramp setup (to be tested)
  ;; (setq shell-file-name "bash")
  ;; (setq explicit-shell-file-name shell-file-name)
  ;; (cond  ((eq window-system 'w32)
  ;;         (setq tramp-default-method "scpx"))
  ;;        (t
  ;;         (setq tramp-default-method "scpc")))
  ;; Aspell Windows (http://www.emacswiki.org/emacs/AspellWindows)
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

;; init.el ends here.
