;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(defvar base-emacs-directory user-emacs-directory
  "Root directory of my EMACS config.")
(defvar config-file (expand-file-name "emacs.el" base-emacs-directory)
  "Main configuration file. Result of tangling `emacs.org'.")

(if (file-exists-p config-file)
    (load-file config-file)
  (org-babel-load-file (expand-file-name "emacs.org" base-emacs-directory)))
