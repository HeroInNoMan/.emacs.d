;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; init.el config file for windows 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path user-emacs-directory) ;; supposedly "~~/.emacs.d"
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/"))
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/expand-region.el"))
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/multiple-cursors.el"))
(add-to-list 'load-path (expand-file-name "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/groovy-mode")) ;; problème au chargement du mode (pas au chargement de la conf)

;; Functions (load all files in "~~/.emacs.d/elisp")
(setq elisp-dir "C:/cygwin/home/arthur_leothaud/.emacs.d/elisp/")
(dolist (file (files-in-below-directory elisp-dir))
  (when (file-regular-p file)
    (load file)))

;; MS Windows clipboard is UTF-16LE
(set-clipboard-coding-system 'utf-16le-dos)

;; EOF
