;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Définition de fonctions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reload-file ()
  "reload file"
  (interactive)
  (let ((curr-scroll (window-vscroll)))
    (find-file (buffer-name))
    (set-window-vscroll nil curr-scroll)
    (message "Reloaded file")))
(defun my-html-helper-load-hook ()
  (define-key html-mode-map (kbd "RET") 'newline-and-indent))
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max))
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
(defun my-horizontal-recenter ()
  "make the point horizontally centered in the window"
  (interactive)
  (let ((mid (/ (window-width) 2))
        (line-len (save-excursion (end-of-line) (current-column)))
        (cur (current-column)))
    (if (< mid cur)
        (set-window-hscroll (selected-window)
                            (- cur mid)))))
(defun up-arrow ()
  "move cursor up one line and buffer down one"
  (interactive)
  (previous-line)
  (scroll-down 1))
(defun down-arrow ()
  "move cursor down one line and buffer up one"
  (interactive)
  (next-line)
  (scroll-up 1))
(defun copy-current-file-path ()
  "Add current file path to kill ring. Limits the filename to project root if possible."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new filename)))
