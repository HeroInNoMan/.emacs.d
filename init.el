
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)
(setq base-emacs-directory user-emacs-directory)
;; (setq base-emacs-directory "~/Terminalcity/.emacs.d/")
;; (add-to-list 'load-path base-emacs-directory)
(add-to-list 'load-path (expand-file-name "elisp" base-emacs-directory))
(org-babel-load-file (expand-file-name "emacs.org" base-emacs-directory))
