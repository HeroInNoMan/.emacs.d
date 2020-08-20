;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(setq comp-deferred-compilation t)

;; (debug-watch 'mode-local-init-hook)

(setq base-emacs-directory user-emacs-directory)
(setq config-file (expand-file-name "emacs.el" base-emacs-directory))

(if (file-exists-p config-file)
    (load-file config-file)
  (org-babel-load-file (expand-file-name "emacs.org" base-emacs-directory)))
