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

(package-initialize) ;; init the packages before init.el loads
;; Functions (load all files in "~/.emacs.d/elisp")
(setq elisp-dir (expand-file-name "elisp" conf-dir)) ;; convert-standard-filename si besoin

(let ((default-directory elisp-dir))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(dolist (file (files-in-below-directory elisp-dir))
  (when (file-regular-p file)
    (load file)))

;; customization in separate file
;; (setq custom-file (expand-file-name "~/.emacs.d/elisp/custom.el"))
;; (load custom-file 'noerror)

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

(require 'expand-region)
(require 'multiple-cursors)

;; Browse kill ring ← TODO : install browse-kill-ring
;; (require 'browse-kill-ring)
;; (setq browse-kill-ring-quit-action 'save-and-restore)

;; Tree view for directories
(require 'dirtree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(recentf-mode 1) ;; keep a list of recently opened files

(global-set-key (kbd "<f7>") 'recentf-open-files) ;; set F7 to open a list of recently opened file

;; Tramp mode
(setq tramp-default-method "ssh") ;; does not seem to work so far
(global-set-key "\C-x\C-b" 'electric-buffer-list) ;; Electric buffer by default

(setq-default transient-mark-mode t) ;; Coloration entre marque et curseur

(setq default-major-mode 'text-mode) ;; Mode texte par défaut

(add-hook 'text-mode-hook 'visual-line-mode) ;; Auto-wrapping (soft wrap) en mode texte

(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; No auto-fill in text-mode since I use visual-line-mode

;; (add-hook 'html-helper-mode-hook 'flyspell-mode)
;; (add-hook 'text-mode-hook 'flyspell-mode)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t) ;; Toutes les couleurs possibles

(setq inhibit-startup-message t) ;; Pas de message au lancement

;; Couleur de fond
;; (set-background-color "gray85")
(setq read-file-name-completion-ignore-case t) ;; completion case-insensitive

(setq read-buffer-completion-ignore-case t) ;; completion case-insensitive

;; configuration pour mutt
;; (setq auto-mode-alist (cons '("mutt-realpeche" . text-mode) auto-mode-alist))
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping en mode mail

(setq column-number-mode t) ;; Affiche le numéro de colonne

(setq-default show-trailing-whitespace t) ;; Affiche les espaces en fin de ligne

(setq display-time-day-and-date t ;; Affiche l'heure en mode 24h et la date

      display-time-24hr-format t)
(display-time)
(tool-bar-mode -1) ;; Remove the toolbars on startup (only in non-shell emacs)

(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p) ;; Pour ne pas avoir à taper en entier la réponse yes/no

(setq ispell-dictionary "francais") ;; dictionnaire francais pour la correction orthographique ispell

(setq european-calendar-style t) ;; format jour/mois/an pour le calendrier (M-x calendar)

(setq html-helper-use-expert-menu t) ;; Utiliser le menu expert

(add-hook 'html-helper-load-hook 'my-html-helper-load-hook) ;; Indenter automatiquement lorsque l'on appuie sur entrée pour le html

(setq c-auto-newline t) ;; pour que l'on n'ait pas à taper sur TAB pour indenter

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
;; (set-clipboard-coding-system 'utf-16le-dos)
;; Bold line
;; (global-hl-line-mode t)

;; Syntaxe utilisée dans le re-buidler
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
                 (concat conf-dir "backups")))))

(setq vc-make-backup-files t) ;; Make backups of files, even when they're in version control


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" conf-dir))

;; key-chords
;; (key-chord-define-global "fg" 'iy-go-to-char)
;; (key-chord-define-global "fd" 'iy-go-to-char-backward)

;; The following lines are always needed.  Choose your own keys.
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(server-start) ;; start in server mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
