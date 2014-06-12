;;; load-dir.el --- load all Emacs Lisp files in given directories

;; Copyright (C) 2011 Free Software Foundation, Inc

;; Authors: Teodor Zlatanov <tzz@lifelogs.com>,
;;          Ben Key <bkey76@gmail.com>
;; With-Help-From: Evans Winner <ego111@gmail.com>, PJ Weisberg <pj@irregularexpressions.net>
;; Version: 0.0.1
;; Keywords: lisp, files, convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library will load all Lisp files found in the `load-dirs' variable.
;; (you may also want to set `load-dir-debug', `load-dir-recursive',
;;  and `load-dir-ignore-errors')

;; Normal usage in .emacs:

;; (setq load-dirs '("~/mystuff")) ;; or Customize it
;; (require 'load-dir) ;; this will add `load-dirs' to your `after-init-hook'

;; Then after startup:

;; Explicitly load new files only...
;; M-x load-dirs

;; Or reload all your files...
;; M-x load-dirs-reload

;;; Code:

(eval-when-compile (require 'cl))

(defgroup load-dir nil
  "Automatically load all Emacs Lisp files in given directories."
  :group 'initialization)

(defcustom load-dir-debug t
  "Debugging messages toggle, default to t."
  :group 'load-dir
  :require 'load-dir
  :type 'boolean)

(defcustom load-dir-recursive nil
  "List of directories to load."
  :group 'load-dir
  :require 'load-dir
  :type 'boolean)

(defcustom load-dir-ignore-errors nil
  "Whether errors in the loaded files should be ignored."
  :group 'load-dir
  :require 'load-dir
  :type 'boolean)

(defcustom load-dirs nil
  "This variable allows you to define which directories should be loaded in one
of several ways.

If load-dirs is nil, no directories are loaded.  This is the default behavior.
If load-dirs is t, only files in the default directory, which is ~/.emacs.d/load.d,
will be loaded.
If load-dirs is a single directory, only files in that directory will be loaded.
If load-dirs is a list of directories, all files found in all the directories in
the list will be loaded."
  :group 'load-dir
  :require 'load-dir
  :type '(choice (const t) (const nil) directory (repeat directory)))

(defun load-dirs ()
  "Load all Emacs Lisp files in `load-dirs'.
Will not load a file twice (use `load-dir-reload' for that).
Recurses into subdirectories if `load-dir-recursive' is t."
  (interactive)
  ;; avoid the case where users inadvertently set `load-dirs' to a string
  (mapc 'load-dir-one (if (eq load-dirs t)
						  (list (expand-file-name "~/.emacs.d/load.d"))
						(if (stringp load-dirs)
							(list load-dirs)
						  load-dirs))))

;;;###autoload
(defun load-dirs-reload ()
  "Load all Emacs Lisp files in `load-dirs'.
Clears the list of loaded files and just calls `load-dir-load'."
  (interactive)
  (setq load-dir-loaded nil)
  (load-dirs))

(defvar load-dir-loaded nil
  "List of already loaded files.")

(defun load-dir-one (dir)
  "Load all Emacs Lisp files in DIR.
Recurses into subdirectories if `load-dir-recursive' is t."
  (load-dir-debug "Loading Emacs Lisp code from %s" dir)
  (let ((suffixes (get-load-suffixes)))
    (dolist (f (and (file-exists-p dir)
                    (file-directory-p dir)
                    (directory-files dir t)))
      (when (and (not (file-directory-p f))
                 (member (file-name-extension f t) suffixes))
        (setq f (file-name-sans-extension f))
        (if (member f load-dir-loaded)
            (load-dir-debug "Skipping %s, it's already loaded." f)
          (if load-dir-ignore-errors
              (with-demoted-errors (load f))
            (load f))
          (add-to-list 'load-dir-loaded f))))

    (when load-dir-recursive
      (dolist (f (directory-files dir t))
        (when (file-directory-p f)
          (load-dir-one f))))))

(defun load-dir-debug (&rest args)
  "Print a debug message like `message' if `load-dir-debug' is set."
  (when load-dir-debug
    (apply 'message args)))

;;;###autoload
(add-hook 'after-init-hook 'load-dirs)

(provide 'load-dir)
;;; load-dir.el ends here
