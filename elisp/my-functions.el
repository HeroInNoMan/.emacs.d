;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Custom lisp functions                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-file ()
  "Reload file"
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))

(defun my-html-helper-load-hook ()
  (define-key html-mode-map (kbd "RET") 'newline-and-indent))

(defun iwb ()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  ;;(untabify (point-min) (point-max))
  (message "File indented"))

(defun simplified-beginning-of-buffer ()
  "Move point to the beginning of the buffer;
     leave mark at previous position."
  (interactive)
  (push-mark)
  (goto-char (point-min)))

(defun simplified-end-of-buffer ()
  "Move point to the end of the buffer;
     leave mark at previous position."
  (interactive)
  (push-mark)
  (goto-char (point-max)))

(defun up-arrow ()
  "Move cursor up one line and buffer down one"
  (interactive)
  (previous-line)
  (scroll-down 1))

(defun down-arrow ()
  "Move cursor down one line and buffer up one"
  (interactive)
  (next-line)
  (scroll-up 1))

(defun copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new filename)))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun duplicate-current-line (&optional n)
  "Duplicate current line, make more than 1 copy given a numeric argument"
  (interactive "p")
  (save-excursion
    (let ((nb (or n 1))
          (current-line (thing-at-point 'line)))
      ;; when on last line, insert a newline first
      (when (or (= 1 (forward-line 1)) (eq (point) (point-max)))
        (insert "\n"))

      ;; now insert as many time as requested
      (while (> n 0)
        (insert current-line)
        (decf n)))))

;; sql related functions
(defvar sql-last-prompt-pos 1
  "Position of last prompt when added recording started")
(make-variable-buffer-local 'sql-last-prompt-pos)
(put 'sql-last-prompt-pos 'permanent-local t)

(defun sql-add-newline-first (output)
  "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'
    This fixes up the display of queries sent to the inferior buffer
    programatically."
  (let ((begin-of-prompt
         (or (and comint-last-prompt-overlay
                  ;; sometimes this overlay is not on prompt
                  (save-excursion
                    (goto-char (overlay-start comint-last-prompt-overlay))
                    (looking-at-p comint-prompt-regexp)
                    (point)))
             1)))
    (if (> begin-of-prompt sql-last-prompt-pos)
        (progn
          (setq sql-last-prompt-pos begin-of-prompt)
          (concat "\n" output))
      output)))

(defun sqli-add-hooks ()
  "Add hooks to `sql-interactive-mode-hook'."
  (add-hook 'comint-preoutput-filter-functions
            'sql-add-newline-first))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun sudo-edit (&optional arg)
  "edit file as root"
  (interactive "p")
  (if (or arg (not buffer-file-name))
	  (ido-find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
	(find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun toggle-show-trailing-whitespace ()
  "toggle display trailing whitespaces"
  (interactive)
  (if show-trailing-whitespace
	  (progn
		(setq show-trailing-whitespace nil)
		(message "trailing whitespaces disabled"))
	(progn
	  (setq show-trailing-whitespace t)
	  (message "trailing whitespaces enabled"))))

(provide 'my-functions)
;; misc-functions.el ends here.
