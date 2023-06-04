;;; my-functions.el --- summary -*- lexical-binding: t -*-

;; Author: Arthur Léothaud
;; Maintainer: Arthur Léothaud
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This project is libre, and licenced under the terms of the
;; DO WHAT THE FUCK YOU WANT TO PUBLIC LICENCE, version 3.1,
;; as published by dtf on July 2019. See the COPYING file or
;; https://ph.dtf.wtf/w/wtfpl/#version-3-1 for more details.

;;; Commentary:

;; my custom lips functions

;;; Code:

(defun ale/revert-buffer-no-confirm ()
  ;; Source: http://www.emacswiki.org/emacs-en/download/misc-cmds.el
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

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
  "Move cursor up one line and buffer down one."
  (interactive)
  (forward-line -1)
  (scroll-down 1))

(defun down-arrow ()
  "Move cursor down one line and buffer up one."
  (interactive)
  (forward-line)
  (scroll-up 1))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun ale/copy-and-show-current-file-path ()
  "Add current file path to kill ring.
Limits the filename to project root if possible.
Show the full path file name in the minibuffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (kill-new filename))
  (message (buffer-file-name)))

(defun ale/jirify ()
  "Create an org link with a ticket ID using the URL in my-private-primary-work-bugtracker-url."
  (interactive)
  (if (and
       (boundp 'my-private-primary-work-bugtracker-url)
       (not (null my-private-primary-work-bugtracker-url)))
      (let* ((id (upcase (thing-at-point 'symbol)))
             (bounds (bounds-of-thing-at-point 'symbol))
             (org-link (concat "[[" my-private-primary-work-bugtracker-url id "][#" id "]]")))
        (delete-region (car bounds) (cdr bounds))
        (ale/delete-hash-at-point)
        (insert org-link))
    (message "Var my-private-primary-work-bugtracker-url is nil or undefined. You must define a bugtracker URL first.")))

(defun ale/rtify ()
  "Create an org link with a ticket ID using the URL in my-private-secondary-work-bugtracker-url."
  (interactive)
  (if (and
       (boundp 'my-private-secondary-work-bugtracker-url)
       (not (null my-private-secondary-work-bugtracker-url)))
      (let* ((id (upcase (thing-at-point 'symbol)))
             (bounds (bounds-of-thing-at-point 'symbol))
             (org-link (concat "[[" my-private-secondary-work-bugtracker-url id "][RT-" id "]]")))
        (delete-region (car bounds) (cdr bounds))
        (ale/delete-hash-at-point)
        (insert org-link))
    (message "Var my-private-secondary-work-bugtracker-url is nil or undefined. You must define a bugtracker URL first.")))

(defun ale/delete-hash-at-point ()
  "Delete # symbol if present under point."
  (save-excursion
    (backward-char)
    (when (looking-at "#")
      (delete-char 1))))

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

(defun ediff-copy-both-to-C ()
  "In ediff session, copy both versions to merge buffer."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

(defun ale/add-d-to-ediff-mode-map ()
  "Add an option for keeping both sides."
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'ale/add-d-to-ediff-mode-map)

(defun ale/kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun ale/swap-buffers ()
  "Swap the contents of windows in the frame."
  (interactive)
  (cond ((one-window-p) (display-buffer (other-buffer)))
        ((let* ((buffer-a (current-buffer))
                (window-b (cadr (window-list)))
                (buffer-b (window-buffer window-b)))
           (set-window-buffer window-b buffer-a)
           (switch-to-buffer buffer-b)
           (other-window 1)))))

(defun ale/toggle-window-split ()
  "Switch between vertical and horizontal split of windows.
Swap buffers in the process"
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
        (ale/swap-buffers)
        (let ((first-win (selected-window)))
          (set-window-buffer (selected-window) this-win-buffer)
          (if this-win-2nd (other-window 1))
          (select-window first-win)
          (set-window-buffer (next-window) next-win-buffer)
          (if this-win-2nd (other-window 1))))))

(defun goto-line-with-feedback ()
  "Show line numbers and whitespaces temporarily, while prompting for the line number input."
  (interactive)
  (let ((linum display-line-numbers-mode))
    (unwind-protect
        (progn
          (when (not linum)
            (display-line-numbers-mode 1))
          (goto-line (read-number "Goto line: ")))
      (progn
        (when (not linum)
          (display-line-numbers-mode -1))
        (git-gutter-mode +1)))))

(defun ale/insert-ticket-prefix ()
  "Insert a prefix containing the ticket ID."
  (interactive)
  (let* ((ticket-types (if (boundp 'my-private-ticket-types)
                           my-private-ticket-types
                         '("hot" "bg" "tk" "ft")))
         (branch-name (magit-get-current-branch))
         (ticket-type (car (split-string branch-name "-")))
         (ticket-id (car (cdr (split-string branch-name "-"))))
         (comment-char (magit-get "core.commentChar"))
         (repo-url (magit-get "remote.origin.url"))
         (project-prefix (when (string-match "sinapse-scripts" repo-url) "enercoop/Sinapse"))
         (prefix-char (concat project-prefix "#"))
         (prefix (if (or (not comment-char)
                         (string= comment-char prefix-char))
                     (concat "[" ticket-id "]")
                   (concat prefix-char ticket-id))))
    (when (and (member ticket-type ticket-types)
               (string-match "^[0-9]+$" ticket-id)
               (not (search-forward prefix nil t)))
      (goto-char (point-min))
      (insert (concat prefix " "))
      (save-buffer))))

(defun ale/toggle-camel-snake-kebab-case ()
  "Cycle between camelCase, snake_case and kebab-case for the symbol at point."
  (interactive)
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t)))
           (currently-using-dashes-p (progn (goto-char start)
                                            (re-search-forward "-" end t))))
      (cond (currently-using-underscores-p ;; snake_case → camelCase
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

(defun ale/json-reformat-region-or-buffer ()
  "Call json-reformat-region on region (if active) or on whole buffer."
  (interactive)
  (let ((first (if (region-active-p) (region-beginning) (point-min)))
        (last (if (region-active-p) (region-end) (point-max))))
    (json-reformat-region first last)))

(defun ale/format-all-buffer-or-region ()
  "Call `format-all-region' if region is active, `format-all-buffer' otherwise."
  (interactive)
  (if (region-active-p)
      (format-all-region (region-beginning) (region-end))
    (format-all-buffer)))

(defun ale/new-scratch-or-reuse ()
  "Create a new scratch buffer or re-use existing empty scratch."
  (interactive)
  (let ((scratches nil))
    (mapc
     (lambda (buff)
       (when (and (= (buffer-size buff) 0)
                  (string-match "^\\*scratch\\*\\(<[0-9]+>\\)?" (buffer-name buff)))
         (setq scratches (append scratches (list buff)))))
     (buffer-list))
    (if (> (length scratches) 0)
        (switch-to-buffer (buffer-name (car scratches)))
      (crux-create-scratch-buffer))))

(defun dash-or-scratch ()
  "Switch to dashboard if exists and if there is only one frame.
Also make frame fullscreen. Otherwise, open a new scratch
 buffer."
  (interactive)
  (if (< (length (frame-list)) 3) ;; only one frame
      (progn
        (unless (cdr (assoc 'fullscreen (frame-parameters)))
          (toggle-frame-fullscreen))
        (when (get-buffer "*dashboard*")
          (switch-to-buffer "*dashboard*")))
    (ale/new-scratch-or-reuse)))

(defun ale/find-rest-client-file ()
  "Find rest-client file."
  (interactive)
  (find-file "~/projets/restclient/restclient-buffer"))

(defun ale/find-init-file ()
  "Find init file."
  (interactive)
  (find-file (expand-file-name "emacs.org" user-emacs-directory)))

(defun ale/find-todo-file ()
  "Find TODO file."
  (interactive)
  (find-file (expand-file-name my-private-local-todo-org-file)))

(defun ale/find-diary-file ()
  "Find diary file."
  (interactive)
  (find-file (expand-file-name my-private-local-diary-file)))

(defun ale/find-work-file ()
  "Find work diary file."
  (interactive)
  (find-file (expand-file-name my-private-work-diary-org-file)))

(defun ale/vimtutor ()
  "Open vimtutor in EMACS."
  (interactive)
  (find-file (expand-file-name "misc/vimtutor.txt" user-emacs-directory)))

(defun ale/toggle-selective-display (column)
  "Hide lines indented as far as COLUMN."
  (interactive "P")
  (set-selective-display
   (cond
    (column (if selective-display nil column))
    ((not selective-display) 1)
    ((equal selective-display 1) 3)
    ((equal selective-display 3) 5)
    ((equal selective-display 5) 10)
    ((equal selective-display 10) nil))))

(defun ale/switch-to-all-dict ()
  "Switch to all possible dictionaries."
  (interactive)
  (flyspell-mode)
  (ispell-change-dictionary "fr_FR,en_GB,en_US,de_DE"))

(defun ale/switch-to-fr-dict ()
  "Switch to French dictionary."
  (interactive)
  (flyspell-mode)
  (ispell-change-dictionary "fr_FR"))

(defun ale/switch-to-de-dict ()
  "Switch to German dictionary."
  (interactive)
  (flyspell-mode)
  (ispell-change-dictionary "de_DE"))

(defun ale/switch-to-en-dict ()
  "Switch to British English dictionary."
  (interactive)
  (flyspell-mode)
  (ispell-change-dictionary "en_GB"))

(defun ale/switch-to-us-dict ()
  "Switch to American English dictionary."
  (interactive)
  (flyspell-mode)
  (ispell-change-dictionary "en_US"))

(defun ale/open-project (&optional dir)
  "Open DIR project magit logs and status as split windows."
  (when dir (find-file dir))
  (magit-log-all (car (magit-log-arguments)))
  (delete-other-windows)
  (magit-status)
  (other-window 1)
  (magit-fetch-all (magit-fetch-arguments))
  (goto-char (point-min)))

(defun set-fira-font-if-possible ()
  "Set the font to “Fira Code” in it is available."
  (interactive)
  (when (member "Fira Code" (font-family-list))
    (set-frame-font "Fira Code" t t)))

(defun ajust-font-size-to-monitor ()
  "Ajust the font size according to the monitor resolution."
  (interactive)
  (let* ((small-display-p (string= "eDP-1" (alist-get 'name (car (display-monitor-attributes-list)))))
         (new-font-size (if small-display-p 130 100)))
    (set-face-attribute 'default nil :height new-font-size)
    (set-fira-font-if-possible)))

(defun cycle-font-size ()
  "Cycle between small, big, and very big font size."
  (interactive)
  (let* ((small-font 100)
         (normal-font 130)
         (big-font 150)
         (current-size (face-attribute 'default :height))
         (new-size (cond ((<= current-size small-font) normal-font)
                         ((<= current-size normal-font) big-font)
                         (t small-font))))
    (set-face-attribute 'default nil :height new-size)
    (set-fira-font-if-possible)))

;; functions stolen from angrybacon
(defun ab/date-iso ()
  "Insert the current date, ISO format, eg. 2016-12-09."
  (interactive)
  (insert (format-time-string "%F")))

(defun ab/date-iso-with-day ()
  "Insert the current date, ISO format, with day, eg. 2016-12-09 lundi."
  (interactive)
  (insert (format-time-string "%F %A")))

(defun ab/date-iso-with-time ()
  "Insert the current date, ISO format with time, eg. 2016-12-09T14:34:54+0100."
  (interactive)
  (insert (format-time-string "%FT%T%z")))

(defun ab/date-long ()
  "Insert the current date, long format, eg. 09 December 2016."
  (interactive)
  (insert (format-time-string "%d %B %Y")))

(defun ab/date-long-with-day ()
  "Insert the current date, long format, with day, eg. lundi 09 December 2016."
  (interactive)
  (insert (format-time-string "%A %d %B %Y")))

(defun ab/date-long-with-time ()
  "Insert the current date, long format, eg. lundi 09 December 2016 - 14:34."
  (interactive)
  (insert (format-time-string "%A %d %B %Y - %H:%M")))

(defun ab/date-short ()
  "Insert the current date, short format, eg. 09/12/2016."
  (interactive)
  (insert (format-time-string "%d/%m/%Y")))

(defun ab/date-short-with-time ()
  "Insert the current date, short format with time, eg. 2016/12/09 14:34."
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M")))

(defun ab/date-short-only-time ()
  "Insert the time, eg. 13:12."
  (interactive)
  (insert (format-time-string "%H:%M")))

(defun ale/org-date ()
  "Insert the current date, org timestamp format eg. <2019-07-29 lun.>."
  (interactive)
  (insert (format-time-string "<%F %a>")))

(defun ale/org-date-time ()
  "Insert the current date and time, org timestamp format eg. <2019-07-29 lun. 14:09>."
  (interactive)
  (insert (format-time-string "<%F %a %H:%M>")))

(defun cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.

If a is before b, return -1. If a is after b, return 1. If they
are equal return nil."
  (lexical-let ((prop prop))
    #'(lambda (a b)

        (let* ((a-pos (get-text-property 0 'org-marker a))
               (b-pos (get-text-property 0 'org-marker b))
               (a-date (or (org-entry-get a-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (b-date (or (org-entry-get b-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (cmp (compare-strings a-date nil nil b-date nil nil))
               )
          (if (eq cmp t) nil (signum cmp))))))

(defun ale/apply-local-theme-modern ()
  "Apply locally-defined modern theme."
  (when (featurep 'color-theme-modern)
    (if (boundp 'my-private-theme)
        (progn
          (load-theme (intern my-private-theme) t t)
          (enable-theme (intern my-private-theme)))
      (message "my-private-theme is not set. No theme will be loaded"))))

(defun ale/get-color-for-level (level)
  "Map a color on LEVEL."
  (cond ((eq level (intern "error")) "red")
        ((eq level (intern "warning")) "orange")
        ((eq level (intern "info")) "blue")
        (t "white")))

(defun ale/flycheck-format-level (errors level)
  "Return the mode-line string with ERRORS for this LEVEL."
  (let* ((count (alist-get level errors)))
    (when count
      (propertize (concat "✘" (number-to-string count)) 'face `(:foreground ,(ale/get-color-for-level level))))))

(defun ale/flycheck-mode-line ()
  "Build the mode-line string for flycheck-mode."
  (when (flycheck-has-current-errors-p)
    (let* ((all-errors (flycheck-count-errors flycheck-current-errors)))
      (string-join (cl-remove-if #'null
                                 (mapcar (apply-partially #'ale/flycheck-format-level all-errors) '(error warning info))) " "))))

(defun ale/diff-last-2-yanks ()
  "Run ediff on latest two entries in `kill-ring'."
  (interactive)
  ;; Implementation depends on `lexical-binding' being t, otherwise #'clean-up
  ;; will not be saved as closure to `ediff-cleanup-hook' and thus will lose
  ;; reference to itself.
  (setq-local lexical-binding t)
  (let ((a (generate-new-buffer "*diff-yank*"))
        (b (generate-new-buffer "*diff-yank*")))
    (cl-labels ((clean-up ()
                  (kill-buffer a)
                  (kill-buffer b)
                  (remove-hook 'ediff-cleanup-hook #'clean-up)
                  (winner-undo)))
      (add-hook 'ediff-cleanup-hook #'clean-up)
      (with-current-buffer a
        (insert (elt kill-ring 0)))
      (with-current-buffer b
        (insert (elt kill-ring 1)))
      (ediff-buffers a b))))

(defun ale/narrow-to-region-indirect (start end)
  "Restrict editing in this buffer to the current region (START, END), indirectly."
  (interactive "r")
  (deactivate-mark)
  (let ((buf (clone-indirect-buffer nil nil)))
    (with-current-buffer buf
      (narrow-to-region start end))
    (switch-to-buffer buf)))

(defun ale/ediff-current-buffers ()
  "If frame is split in two windows, call ‘ediff-buffers’ on the two buffers."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((this-win-buffer (buffer-name))
           (next-win-buffer (buffer-name (window-buffer (next-window)))))
      (ediff-buffers this-win-buffer next-win-buffer))))

(defun next-user-buffer (&optional arg)
  "Switch to next user buffer. If ARG is non-nil, simply switch to next buffer."
  (interactive "P")
  (let ((curr-buff (current-buffer))
        (found-p))
    (unless arg
      (next-buffer)
      (while (and (not found-p)
                  (not (eq curr-buff (current-buffer))))
        (if (user-buffer-p)
            (setq found-p t)
          (next-buffer))))))

(defun previous-user-buffer (&optional arg)
  "Switch to previous user buffer. If ARG is non-nil, simply switch to previous buffer."
  (interactive "P")
  (let ((curr-buff (current-buffer))
        (found-p))
    (unless arg
      (previous-buffer)
      (while (and (not found-p)
                  (not (eq curr-buff (current-buffer))))
        (if (user-buffer-p)
            (setq found-p t)
          (previous-buffer))))))

(defun user-buffer-p ()
  "Return t if current buffer is a user buffer, else nil."
  (interactive)
  (cond
   ((string-equal major-mode "dired-mode") nil)
   ((string-equal major-mode "erc-mode") nil)
   ((string-prefix-p "*scratch*" (buffer-name)) t)
   ((string-prefix-p "*Org Src" (buffer-name)) t)
   ((string-prefix-p "*eww" (buffer-name)) t)
   ((string-prefix-p "*" (buffer-name)) nil)
   ((string-prefix-p "magit" (buffer-name)) nil)
   (t t)))

(defun crontab-e ()
  "Run `crontab -e' in an Emacs buffer."
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(defun ale/org-diary-goto-today ()
  "Move point to current date in an org file."
  (interactive)
  (let ((date (format-time-string "%F")))
    (unless (search-forward date nil t nil)
      (search-backward date))))

(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by ARG."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun decrement-number-decimal (&optional arg)
  "Decrement the number forward from point by ARG."
  (interactive "p*")
  (increment-number-decimal (if arg (- arg) -1)))

(defun play-youtube-video (url)
  (interactive "sURL: ")
  (shell-command
   (concat "youtube-dl  -o - " url " | vlc -")))

(defun w3m-play-youtube-video ()
  (interactive)
  (play-youtube-video
   (w3m-print-this-url (point))))

(defun init-server-alist ()
  "Init the list of SQL connections based on `sql-connection-alist'."
  (defvar ale-sql-servers-list '()
    "Alist of server name and the function to connect.")
  (cl-loop for db in sql-connection-alist
           collect
           (add-to-list
            'ale-sql-servers-list ; string/function pairs
            (list
             (prin1-to-string (car db) t)
             `(lambda ()
                (setq sql-product ,(car (cdr (assoc 'sql-product db))))
                ;;TODO: check if connection already exists

                (sql-connect (quote ,(car db))))))))

(defun ale/cleanup-buffer-and-save ()
  "Indent, untabify, clean-up and save buffer."
  (interactive)
  (crux-cleanup-buffer-or-region)
  (save-buffer))

(defun togfun (buffer-name buffer-create-fn &optional switch-cont)
  "Make a toggle-function to have raise-or-create behaviour.

Creates a toggle-function that executes BUFFER-CREATE-FN if a
buffer named BUFFER-NAME doesn't exist, switches to the buffer
named BUFFER-NAME if it exists, and switches to the previous
buffer if we are currently visiting buffer BUFFER-NAME.

The SWITCH-CONT argument is a function which, if given, is called
after the buffer has been created or switched to.  This allows
running further actions that setup the state of the buffer or
modify it.

Found on reddit.com/r/emacs
Thanks to Ashjkaell!"

  (lambda ()
    (interactive)
    (let ((target-buf (get-buffer buffer-name)))
      (if target-buf
          (if (eq (current-buffer) target-buf)
              (winner-undo)
            (progn
              (switch-to-buffer buffer-name)
              (when switch-cont (funcall switch-cont))))
        (funcall buffer-create-fn)
        (when switch-cont (funcall switch-cont))))))

(defun xah-cycle-hyphen-underscore-space ( &optional @begin @end )
  "Cycle {underscore, space, hyphen} chars in selection or inside quote/bracket or
 line.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters, in that order.

The region to work on is by this order:
 ① if there's active region (text selection), use that.
 ② If cursor is string quote or any type of bracket, and is within current line, work on that region.
 ③ else, work on current line.

URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2019-02-12"
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of $charArray.
  (let ($p1 $p2)
    (if (and @begin @end)
        (progn (setq $p1 @begin $p2 @end))
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (if (nth 3 (syntax-ppss))
            (save-excursion
              (skip-chars-backward "^\"")
              (setq $p1 (point))
              (skip-chars-forward "^\"")
              (setq $p2 (point)))
          (let (
                ($skipChars
                 (if (boundp 'xah-brackets)
                     (concat "^\"" xah-brackets)
                   "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）")))
            (skip-chars-backward $skipChars (line-beginning-position))
            (setq $p1 (point))
            (skip-chars-forward $skipChars (line-end-position))
            (setq $p2 (point))
            (set-mark $p1)))))
    (let* (
           ($charArray ["_" "-" " "])
           ($length (length $charArray))
           ($regionWasActive-p (region-active-p))
           ($nowState
            (if (eq last-command this-command)
                (get 'xah-cycle-hyphen-underscore-space 'state)
              0 ))
           ($changeTo (elt $charArray $nowState)))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (goto-char (point-min))
          (while
              (re-search-forward
               (elt $charArray (% (+ $nowState 2) $length))
               ;; (concat
               ;;  (elt $charArray (% (+ $nowState 1) $length))
               ;;  "\\|"
               ;;  (elt $charArray (% (+ $nowState 2) $length)))
               (point-max)
               "move")
            (replace-match $changeTo "FIXEDCASE" "LITERAL"))))
      (when (or (string-equal $changeTo " ") $regionWasActive-p)
        (goto-char $p2)
        (set-mark $p1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-underscore-space 'state (% (+ $nowState 1) $length)))))
(defun ale/insert-date-as-word (&optional date)
  "Insert the word produced by `ale/convert-date-to-word'."
  (interactive "P")
  (insert (ale/convert-date-to-word date)))

(defun ale/convert-date-to-word (&optional date)
  "Convert a DATE ( current if no arg ) to a four-letter word.
The DATE format must include hours & minutes.
Example: 2020-09-22T13:12:17+0200 → ACAB"
  (interactive)
  (let ((date (if date
                  date
                (format-time-string "%H:%M"))))
    (string-match "[0-9]\\{2\\}:[0-9]\\{2\\}" date)
    (let ((time (match-string 0 date)))
      (message
       (concat
        (ale/convert-digit-to-letter (substring time 0 1))
        (ale/convert-digit-to-letter (substring time 1 2))
        (ale/convert-digit-to-letter (substring time 3 4))
        (ale/convert-digit-to-letter (substring time 4 5)))))))

(defun ale/convert-digit-to-letter (digit)
  "Convert a DIGIT to the corresponding letter.
 A=1, B=2, …, I=9. By choice, O=0."
  (cond ((string= digit "0") "𝑶")
        ((string= digit "1") "𝑨")
        ((string= digit "2") "𝑩")
        ((string= digit "3") "𝑪")
        ((string= digit "4") "𝑫")
        ((string= digit "5") "𝑬")
        ((string= digit "6") "𝑭")
        ((string= digit "7") "𝑮")
        ((string= digit "8") "𝑯")
        ((string= digit "9") "𝑰")))

(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end))
  (message "Formatting done."))

(provide 'my-functions)
;;; my-functions.el ends here
