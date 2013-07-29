;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el config file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package-style dependencies
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  )

(defun files-in-below-directory (directory) ;; TODO externalize this function
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

(require 'dirtree) ;; Tree view for directories

(require 're-builder)
(setq reb-re-syntax 'string) ;; Syntaxe utilisée dans le re-buidler

(recentf-mode 1) ;; keep a list of recently opened files
(setq-default transient-mark-mode t) ;; Coloration entre marque et curseur
(setq default-major-mode 'text-mode) ;; Mode texte par défaut
(add-hook 'text-mode-hook 'visual-line-mode) ;; Auto-wrapping (soft wrap) en mode texte
(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; No auto-fill in text-mode since I use visual-line-mode
(global-font-lock-mode t) ;; coloration syntaxique
(setq font-lock-maximum-decoration t) ;; Toutes les couleurs possibles
(setq inhibit-startup-message t) ;; Pas de message au lancement
(setq initial-scratch-message nil) ;; empty *scratch*
(setq read-file-name-completion-ignore-case t) ;; completion case-insensitive
(setq read-buffer-completion-ignore-case t) ;; completion case-insensitive
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping en mode mail
(setq column-number-mode t) ;; Affiche le numéro de colonne
(setq-default show-trailing-whitespace nil) ;; don’t display thoses — rather annoying
(setq display-time-day-and-date t ;; Affiche date et heure
      display-time-24hr-format t) ;; format de l’heure 24h
(display-time) ;; affiche la date et l’heure
(tool-bar-mode -1) ;; no tool-bar on startup (only in non-shell emacs)
(menu-bar-mode -1) ;; no menu-bar
(fset 'yes-or-no-p 'y-or-n-p) ;; Pour ne pas avoir à taper en entier la réponse yes/no
(setq european-calendar-style t) ;; format jour/mois/an pour le calendrier (M-x calendar)
(setq c-auto-newline t) ;; automatically indent - no need to tab
(setq-default tab-width 4) ;; eclipse-like
(setq-default indent-tabs-mode nil) ;; ???
(global-hl-line-mode -1) ;; don’t highlight current line

;; Spellchecking
(require 'ispell)
(setq ispell-dictionary "francais") ;; french dictionary for auto-correct
(setq-default ispell-program-name "aspell") ;; aspell by default
(add-hook 'text-mode-hook 'flyspell-mode) ;; auto-correct in text mode
(add-hook 'html-helper-mode-hook 'flyspell-mode) ;; auto-correct in html mode

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

;; MINIMAP
(defun minimap-toggle-retain-size ()
  "Toggle minimap"
  (interactive)
  (if (or (not (boundp 'minimap-exists))
          (not minimap-exists))
      (progn (minimap-create)
             (setf minimap-exists t)
             (set-frame-width (selected-frame) 100))
    (progn (minimap-kill)
           (setf minimap-exists nil)
           (set-frame-width (selected-frame) 80))))

;;;###autoload
(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (if (null minimap-bufname)
      (minimap-create)
    (minimap-kill)))

;; CUSTOMIZATION
(custom-set-variables
 ;; '(show-paren-mode t nil (paren))
 ;; '(show-paren-style (quote expression))
 (custom-set-faces
  '(show-paren-match ((((class color)) (:weight bold))))
  ))

;; ERC conf
(require 'erc-hl-nicks)
(setq erc-input-line-position -2) ;; so the prompt is always at the bottom

(require 'key-chord)
(key-chord-mode 1)

(server-start) ;; start in server mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-specific configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; Loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

;; basic html configuration
(setq html-helper-use-expert-menu t) ;; use expert menu
(add-hook 'html-helper-load-hook 'my-html-helper-load-hook) ;; automatically indent html

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
  )

(defun load-linux-specific-conf ()
  "Loads all GNU/Linux specific conf"
  )

(cond ((eq system-type 'windows-nt) (load-windows-specific-conf))
      ((eq system-type 'gnu/linux) (load-linux-specific-conf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; transforms code into concatenated strings to be inserted in java
;; code (as a string). "s are escaped so java doesn’t misinterpret
;; them.
(fset 'stringify-code-for-java
      [?\M-x ?t ?e ?x ?t ?- ?m ?o ?d ?e return ?\C-c ?i ?\C-c ?h ?$ backspace ?\" return ?\\ ?\" return ?\M-< ?\C-c ?j ?^ return ?\" return ?\M-< ?\C-c ?j ?$ return ?\" ?  ?+ ?  ?/ ?/ return backspace backspace backspace backspace backspace])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not ready yet / TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIL
;; configuration pour mutt -> gnus (?)
;; (setq auto-mode-alist (cons '("mutt-realpeche" . text-mode) auto-mode-alist))

;; BROWSE KILL RING ← TODO : install browse-kill-ring
;; (require 'browse-kill-ring)
;; (setq browse-kill-ring-quit-action 'save-and-restore)

;; CUSTOMIZATION in separate file
;; (setq custom-file (expand-file-name "~/.emacs.d/elisp/custom.el"))
;; (load custom-file 'noerror)

;; TRAMP
;; setup TRAMP for both cygwin and GNU/Linux
;; (setq tramp-default-method "ssh") ;; Tramp mode; does not seem to work so far

;; init.el ends here.
