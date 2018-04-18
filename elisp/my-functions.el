;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                  Custom lisp functions                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ale-revert-buffer-no-confirm ()
  ;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

(defun ale-indent-region-or-buffer ()
  "Indent region or whole buffer"
  (interactive)
  (cond ((region-active-p)
         (let ((start (region-beginning))
               (end (region-end)))
           (indent-region start end nil)
           (delete-trailing-whitespace start end)
           (untabify start end)
           (message "Region indented.")))
        (t
         (indent-region (point-min) (point-max) nil)
         (delete-trailing-whitespace (point-min) (point-max))
         (untabify (point-min) (point-max))
         (message "Buffer indented."))))

(unless (fboundp 'prefix-region)
  (defun prefix-region (prefix)
    "Add a prefix string to each line between mark and point."
    (interactive "sPrefix string: ")
    (if prefix
        (let ((count (count-lines (mark) (point))))
          (goto-char (min (mark) (point)))
          (while (> count 0)
            (setq count (1- count))
            (beginning-of-line 1)
            (insert prefix)
            (end-of-line 1)
            (forward-char 1))))))

(defun up-arrow ()
  "Move cursor up one line and buffer down one"
  (interactive)
  (forward-line -1)
  (scroll-down 1))

(defun down-arrow ()
  "Move cursor down one line and buffer up one"
  (interactive)
  (forward-line)
  (scroll-up 1))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun ale-copy-and-show-current-file-path ()
  "Add current file path to kill ring. Limits the filename to
   project root if possible. Show the full path file name in the
   minibuffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new filename))
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

(defun ale-toggle-show-trailing-whitespace ()
  "toggle display trailing whitespaces"
  (interactive)
  (if show-trailing-whitespace
      (progn
        (setq show-trailing-whitespace nil)
        (message "trailing whitespaces disabled"))
    (progn
      (setq show-trailing-whitespace t)
      (message "trailing whitespaces enabled"))))

(defun ale-jirify ()
  "creates an org link with a ticket ID using the URL in my-private-work-bugtracker-url."
  (interactive)
  (if (and
       (boundp 'my-private-work-bugtracker-url)
       (not (null my-private-work-bugtracker-url)))
      (let* ((id (thing-at-point 'symbol))
             (bounds (bounds-of-thing-at-point 'symbol))
             (org-link (concat "[[" my-private-work-bugtracker-url id "][" id "]]")))
        (delete-region (car bounds) (cdr bounds))
        (insert org-link))
    (message "Var my-private-work-bugtracker-url is nil or undefined. You must define a bugtracker URL first.")))

(defun ale-reload-config ()
  "reload init.el"
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "Reloaded init.el file"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("x" . "XML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))

(defun ale-load-windows-specific-conf ()
  "Loads all windows-nt specific conf"
  (set-clipboard-coding-system 'utf-16le-dos) ;; MS Windows clipboard is UTF-16LE
  ;; cygwin conf
  (setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
  (setq exec-path (cons "c:/cygwin/bin/" exec-path))
  (require 'cygwin-mount)
  (cygwin-mount-activate)
  (require 'dos) ;; batch scripts
  (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/")
  (setq ispell-personal-dictionary "C:/Program Files (x86)/Aspell/dict/")
  (setq python-shell-interpreter "c:/cygwin/bin/python3.2m.exe")

  ;; Prevent issues with the Windows null device (NUL)
  ;; when using cygwin find with rgrep.
  (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
    "Use cygwin's /dev/null as the null-device."
    (let ((null-device "/dev/null"))
      ad-do-it))
  (ad-activate 'grep-compute-defaults)
  )

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun ale-add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'ale-add-d-to-ediff-mode-map)

(defun ale-swap-buffers ()
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))

(defun ale-toggle-window-split ()
  "Switch between vertical and horizontal split of windows. Swap buffers in the process"
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (funcall splitter)
        (ale-swap-buffers)
        (let ((first-win (selected-window)))
          (set-window-buffer (selected-window) this-win-buffer)
          (if this-win-2nd (other-window 1))
          (select-window first-win)
          (set-window-buffer (next-window) next-win-buffer)
          (if this-win-2nd (other-window 1))))))

(defun goto-line-with-feedback ()
  "Show line numbers and whitespaces temporarily, while prompting
for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (whitespace-mode 1)
        (goto-line (read-number "Goto line: ")))
    (progn
      (linum-mode -1)
      (git-gutter-mode +1)
      (whitespace-mode -1))))

;; from http://emacswiki.org/emacs/InsertingTodaysDate
(defun insert-todays-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%d-%m-%Y")
            (format-time-string "%Y-%m-%d"))))

(defun ale-insert-ticket-prefix ()
  "Inserts a prefix containing the number of the Jira ticket"
  (let* ((result  (re-search-forward "\\(?:US\\|RV\\|FEEDBACK\\|TASK\\|BUG\\)-\\([A-Z]+\\)-?\\([0-9]+\\).*$" nil t))
         (s (concat (match-string 1) "-" (match-string 2))))
    (goto-char (point-min))
    (if (and result
             (not (string-match (concat "\\[" s "\\]") (buffer-string))))
        (insert (concat "[" (upcase s) "] "))
      (unless (string-match (concat "\\[.*\\]") (buffer-string))
        (insert (concat "[TECH] "))))))

(defun ale-toggle-camel-snake-kebab-case ()
  "Toggle between camelcase and underscore notation for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t)))
           (currently-using-dashes-p (progn (goto-char start)
                                            (re-search-forward "-" end t))))
      (cond (currently-using-underscores-p ;; snake-case → camel-case
             (progn
               (upcase-initials-region start end)
               (replace-string "_" "" nil start end)
               (downcase-region start (1+ start))))
            (currently-using-dashes-p ;; kebab-case → snake-case
             (replace-string "-" "_" nil start end))
            (t ;; camel-case → kebab-case
             (progn
               (replace-regexp "\\([A-Z]\\)" "-\\1" nil (1+ start) end)
               (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))))

(defun ale-find-rest-client-file ()
  "Find rest-client file"
  (interactive)
  (find-file "~/projets/restclient/restclient-buffer"))

(defun ale-find-init-file ()
  "Find init file"
  (interactive)
  (find-file (expand-file-name "emacs.org" user-emacs-directory)))

(defun ale-find-diary-file ()
  "Find work diary file"
  (interactive)
  (find-file (expand-file-name my-private-work-diary-org-file)))

(defun ale-find-remote-diary-file ()
  "Find remote diary file"
  (interactive)
  (find-file (expand-file-name my-private-remote-diary-org-file my-private-remote-home-dir)))

(defun ale-toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (cond
    (column (if selective-display nil column))
    ((not selective-display) 1)
    ((equal selective-display 1) 2)
    ((equal selective-display 2) 3)
    ((equal selective-display 3) 4)
    ((equal selective-display 4) nil))))

(defun ale-switch-to-en-dict ()
  "Switch to English dictionary"
  (interactive)
  (ispell-change-dictionary "en_GB"))

(defun ale-open-project (args)
  "open project magit logs and status as split windows"
  (interactive "D")
  (progn
    (find-file args)
    (magit-log-all magit-log-arguments)
    (delete-other-windows)
    (magit-status-internal args)
    (other-window 1)
    (beginning-of-buffer)))

(provide 'my-functions)
;; misc-functions.el ends here.
