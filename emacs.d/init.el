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
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )

;; init.el is called by .emacs, conf-dir is the dir containing init.el
(defvar conf-dir (file-name-directory load-file-name))

(setq elisp-dir (expand-file-name "elisp" conf-dir)) ;; elisp/ is on same lvl as init.el

;; add all elisp dir to load-path recursively
(let ((default-directory elisp-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; load all files in emacs.d/elisp
(dolist (file (files-in-below-directory elisp-dir))
  (when (file-regular-p file)
    (load file)))

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

(require 'dirtree) ;; Tree view for directories

(require 're-builder)
(setq reb-re-syntax 'string) ;; Syntaxe utilisée dans le re-buidler

(recentf-mode 1) ;; keep a list of recently opened files
(global-set-key (kbd "<f7>") 'recentf-open-files) ;; set F7 to open a list of recently opened file
(setq tramp-default-method "ssh") ;; Tramp mode; does not seem to work so far
(global-set-key "\C-x\C-b" 'electric-buffer-list) ;; Electric buffer by default
(setq-default transient-mark-mode t) ;; Coloration entre marque et curseur
(setq default-major-mode 'text-mode) ;; Mode texte par défaut
(add-hook 'text-mode-hook 'visual-line-mode) ;; Auto-wrapping (soft wrap) en mode texte
(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; No auto-fill in text-mode since I use visual-line-mode
(global-font-lock-mode t) ;; coloration syntaxique
(setq font-lock-maximum-decoration t) ;; Toutes les couleurs possibles
(setq inhibit-startup-message t) ;; Pas de message au lancement
(setq initial-scratch-message nil) ;; empty *scratch*
;; (set-background-color "gray85") ;; background color
(setq read-file-name-completion-ignore-case t) ;; completion case-insensitive
(setq read-buffer-completion-ignore-case t) ;; completion case-insensitive
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping en mode mail
(setq column-number-mode t) ;; Affiche le numéro de colonne
(setq-default show-trailing-whitespace t) ;; Affiche les espaces en fin de ligne
(setq display-time-day-and-date t ;; Affiche date et heure
      display-time-24hr-format t) ;; format de l’heure 24h
(display-time) ;; affiche la date et l’heure
(tool-bar-mode -1) ;; no tool-bar on startup (only in non-shell emacs)
(menu-bar-mode -1) ;; no menu-bar
(fset 'yes-or-no-p 'y-or-n-p) ;; Pour ne pas avoir à taper en entier la réponse yes/no
(setq ispell-dictionary "francais") ;; dictionnaire francais pour la correction orthographique ispell
(setq european-calendar-style t) ;; format jour/mois/an pour le calendrier (M-x calendar)
(setq html-helper-use-expert-menu t) ;; use expert menu
(add-hook 'html-helper-load-hook 'my-html-helper-load-hook) ;; automatically indent html
(setq c-auto-newline t) ;; automatically indent - no need to tab
(setq-default indent-tabs-mode nil) ;; ???
;; (global-hl-line-mode t) ;; highlight current line

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
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)) ;; From Emacs wiki

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat conf-dir "backups")))))

(setq vc-make-backup-files t) ;; Make backups of files, even when they're in version control

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" conf-dir))

;; Org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(server-start) ;; start in server mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; windows / linux specific conf ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load system-specific library and setup system-specific things that
;; must be setup before main setup
(defun load-windows-specific-conf ()
  "Loads all windows-nt specific conf"
  (set-clipboard-coding-system 'utf-16le-dos) ;; MS Windows clipboard is UTF-16LE
  (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
  (setq exec-path (cons "c:/cygwin/bin/" exec-path))
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  )

(defun load-linux-specific-conf ()
  "Loads all GNU/Linux specific conf"
  )

(cond ((eq system-type 'windows-nt) (load-windows-specific-conf))
      ((eq system-type 'gnu/linux) (load-linux-specific-conf)))

;; Macros

;; transforms code into concatenated strings to be inserted in java code (as a string). "s are escaped so java doesn’t misinterprets them.
(fset 'stringify-code-for-java
      [?\M-x ?t ?e ?x ?t ?- ?m ?o ?d ?e return ?\C-c ?i ?\C-c ?h ?$ backspace ?\" return ?\\ ?\" return ?\M-< ?\C-c ?j ?^ return ?\" return ?\M-< ?\C-c ?j ?$ return ?\" ?  ?+ ?  ?/ ?/ return backspace backspace backspace backspace backspace])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not ready yet / TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; key-chords
;; (key-chord-define-global "fg" 'iy-go-to-char)
;; (key-chord-define-global "fd" 'iy-go-to-char-backward)

;; configuration pour mutt
;; (setq auto-mode-alist (cons '("mutt-realpeche" . text-mode) auto-mode-alist))

;; Browse kill ring ← TODO : install browse-kill-ring
;; (require 'browse-kill-ring)
;; (setq browse-kill-ring-quit-action 'save-and-restore)

;; customization in separate file
;; (setq custom-file (expand-file-name "~/.emacs.d/elisp/custom.el"))
;; (load custom-file 'noerror)

;; (add-hook 'html-helper-mode-hook 'flyspell-mode)
;; (add-hook 'text-mode-hook 'flyspell-mode)

;; init.el ends here.
