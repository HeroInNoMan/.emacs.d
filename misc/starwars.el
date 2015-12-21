(defvar sw/substitution-list
  '(("." . 0)
    (" " . 0)
    ("[.,;:!?] " . 1)
    ("\\B[aeiou]\\B" . 0)
    ("\\B[bcdfghjklmnpqrstvwxyz]\\B" . 0)
    ("\\w\\b" . 0)
    ("[.,;:!?]" . 0)
    ("[^.,;:!?] " . 1)
    ("\\b\\w" . 0)
    (".$" . 0)
    ("^." . 0))
  "A list of dotted pairs with car equal to the regex matching
the character we want to delete and cdr equal to how many
characters we want to move the point forward before actually
deleting a character (useful in the case of space after a
punctuation).  We begin with the substitutions we want to perform
first.  If more than one regex matches, the last one is valid, so
it is probably a good idea to begin with \".\".")

(defun center-line-no-tabs ()
  "A simplified version of center-line, using no tabs (and not
taking into account leading/trailing whitespace."
  (save-excursion
    (let ((length (progn (end-of-line)
                         (current-column))))
      (beginning-of-line)
      (insert (make-string (max 0 (/ (- fill-column length) 2)) ?\s)))))

(defun sw/scroll-prepare-marker-list ()
  "Prepare (and return) a list of markers pointing at characters
to delete from the current line, in the \"right\" order."
  (save-excursion
    (let ((limit (progn
                   (center-line-no-tabs) ; this may use tabs!
                   (back-to-indentation)
                   (point)))
          (subst-list sw/substitution-list)
          (marker-list nil))
      (while subst-list
        (end-of-line)
        (while (re-search-backward (caar subst-list) limit t)
          (forward-char (cdar subst-list))
          (push (point-marker) marker-list))
        (setq subst-list (cdr subst-list)))
      (delete-dups marker-list)
      (setq marker-list (nreverse marker-list)))))

(defvar sw/untouched-lines 3
  "Number of lines at the bottom of the window which should not
  be touched by character deletion.")

(defvar sw/delay .5
  "Delay (in seconds) between frames of animation.")

(defun sw/scroll-current-buffer ()
  "Actually do SW-like scroll in the current buffer."
  (let (marker-list-list)
    (beginning-of-buffer)
    (open-line (window-height))
    (goto-char (point-max))
    (move-beginning-of-line 0)
    (while (progn
             (push (sw/scroll-prepare-marker-list) marker-list-list)
             (> (point) (+ (point-min) (window-height))))
      (previous-line))
    (while (< (point-min) (point-max)) ; here the actual scroll begins
      (goto-char (point-min))
      (kill-line 1)
      (redisplay t)
      (sleep-for sw/delay)
      (let ((walker marker-list-list))
        (while (progn ;
                 (goto-char (or (caar walker) (point-min)))
                 (and walker (< (line-number-at-pos) (- (window-height) sw/untouched-lines))))
          (when (car walker)
            (goto-char (caar walker))
            (delete-char 1)
            (setf (car walker) (cdar walker)))
          (when (car walker)
            (goto-char (caar walker))
            (delete-char 1)
            (setf (car walker) (cdar walker))
            (beginning-of-line)
            (insert " "))
          (setq walker (cdr walker)))))))

(defun star-wars-scroll ()
  "Do Star-Wars-like scroll of the region, or the whole buffer if
  no region is active, in a temporary buffer, and delete it
  afterwards.  Special care is taken to make the lines more or
  less legible as long as possible, for example spaces after
  punctuation are deleted before vowels, vowels are deleted
  before consonants etc."
  (interactive)
  (save-excursion
    (let ((begin (point-min)) (end (point-max)))
      (when (region-active-p)
        (setq begin (region-beginning))
        (setq end (region-end)))
      (copy-region-as-kill begin end)
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (rename-buffer "*Star Wars Scroll*")
		(untabify (point-min) (point-max))
        (save-window-excursion
          (delete-other-windows)
          (yank)
          (sw/scroll-current-buffer))))))
