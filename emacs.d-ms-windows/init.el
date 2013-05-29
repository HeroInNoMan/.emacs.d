;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el config file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun files-in-below-directory (directory)
  "List the .el files in DIRECTORY and in its sub-directories."
  ;; Although the function will be used non-interactively,
  ;; it will be easier to test if we make it interactive.
  ;; The directory will have a name such as
  ;;  "/usr/local/share/emacs/22.1.1/lisp/"
  (interactive "DDirectory name: ")
  (let (el-files-list
        (current-directory-list
         (directory-files-and-attributes directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (cond
       ;; check to see whether filename ends in `.el'
       ;; and if so, append its name to a list.
       ((equal ".el" (substring (car (car current-directory-list)) -3))
        (setq el-files-list
              (cons (car (car current-directory-list)) el-files-list)))
       ;; check whether filename is that of a directory
       ((eq t (car (cdr (car current-directory-list))))
        ;; decide whether to skip or recurse
        (if
            (equal "."
                   (substring (car (car current-directory-list)) -1))
            ;; then do nothing since filename is that of
            ;;   current directory or parent, "." or ".."
            ()
          ;; else descend into the directory and repeat the process
          (setq el-files-list
                (append
                 (files-in-below-directory
                  (car (car current-directory-list)))
                 el-files-list)))))
      ;; move to the next filename in the list; this also
      ;; shortens the list so the while loop eventually comes to an end
      (setq current-directory-list (cdr current-directory-list)))
    ;; return the filenames
    el-files-list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path user-emacs-directory) ;; supposedly "~~/.emacs.d"
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/"))
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/expand-region.el"))
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/multiple-cursors.el"))

;; customization in separate file
;; (setq custom-file (expand-file-name "~~/.emacs.d/elisp/custom.el"))
;; (load custom-file 'noerror)

;; Functions (load all files in "~~/.emacs.d/elisp")
(setq elisp-dir "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/")
(dolist (file (files-in-below-directory elisp-dir))
  (when (file-regular-p file)
    (load file)))

;; (setq load-dir-recursive '("~~/.emacs.d/elisp")) ;; or Customize it
;; (require 'load-dir) ;; this will add `load-dirs' to your `after-init-hook'

;; (load-dirs "~~/.emacs.d/elisp/")
;; (load-dirs-reload)
(require 'expand-region)
(require 'multiple-cursors)

;; Browse kill ring TODO : install browse-kill-ring
;; (require 'browse-kill-ring)
;; (setq browse-kill-ring-quit-action 'save-and-restore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Electric buffer by default
(global-set-key "\C-x\C-b" 'electric-buffer-list)
;; Coloration entre marque et curseur
(setq-default transient-mark-mode t)
;; text-mode is default
(setq default-major-mode 'text-mode)
;; Auto-wrapping in text-mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'html-helper-mode-hook 'flyspell-mode)
;; (add-hook 'text-mode-hook 'flyspell-mode)
(global-font-lock-mode t)
;; All available colours
(setq font-lock-maximum-decoration t)
;; No startup message
(setq inhibit-startup-message t)
;; Background colour
;; (set-background-color "gray85")
;; case-insensitive completion
(setq read-file-name-completion-ignore-case t)
;; case-insensitive completion
(setq read-buffer-completion-ignore-case t)
;; mutt configuration
(setq auto-mode-alist (cons '("mutt-realpeche" . text-mode) auto-mode-alist))
;; Display column number
(setq column-number-mode t)
;; Display trailing spaces
(setq-default show-trailing-whitespace t)
;; 24h-format
(setq display-time-day-and-date t
      display-time-24hr-format t)
(display-time)
;; Remove the toolbars on startup (only in non-shell emacs)
(tool-bar-mode -1)
(menu-bar-mode -1)
;; short yes-no answer
(fset 'yes-or-no-p 'y-or-n-p)
;; french ispell dictionary
(setq ispell-dictionary "francais")
;; format jour/mois/an pour le calendrier (M-x calendar)
(setq european-calendar-style t)
;; use expert-menu
(setq html-helper-use-expert-menu t)
;; auto-indent html
(add-hook 'html-helper-load-hook 'my-html-helper-load-hook)
;; auto-indent on RET
(setq c-auto-newline t)
;;
(setq-default indent-tabs-mode nil)
;;Indentation
(setq tab-width 4
      c-default-style "k&r"
      c-block-comment-prefix ""
      c-basic-offset 4)
(setq truncate-lines t)
;; encodage
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
;; From Emacs wiki
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
;; MS Windows clipboard is UTF-16LE
(set-clipboard-coding-system 'utf-16le-dos)
;; Bold line
;; (global-hl-line-mode t)

;; used re-buidler syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; key-chords
;; (key-chord-define-global "fg" 'iy-go-to-char)
;; (key-chord-define-global "fd" 'iy-go-to-char-backward)

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; start in server mode
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
