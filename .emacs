;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This .emacs file is supposed to be placed where emacs looks it by
;; default, so the whole config is loaded everytime emacs is started
;; started. It’s contents should be replaced by the actual location of the
;; emacs-conf folder (conf-dir), wherever it is.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DEFINE CONF DIRECTORY HERE!
(defvar conf-dir "/media/Sauvegarder/Arthur/emacs-conf/emacs.d/")
;; (defvar conf-dir "C:/cygwin/home/arthur_leothaud/.emacs.d/")

;; MS Windows clipboard is UTF-16LE (windows specific)
;; (set-clipboard-coding-system 'utf-16le-dos)


;; load actual init file
(load-file (concat conf-dir (convert-standard-filename "/init.el")))

;; EOF
