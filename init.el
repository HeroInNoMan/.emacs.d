;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    emacs config file                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALLATION & LOADING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package-style dependencies
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

;; packages to be installed and loaded
(setq package-list '(2048-game
                     ace-window
                     avy
                     better-defaults
                     browse-kill-ring
                     char-menu
                     color-theme
                     company
                     define-word
                     diminish
                     dired-details
                     dired-narrow
                     engine-mode
                     epresent
                     expand-region
                     flycheck
                     gitconfig-mode
                     god-mode
                     helm
					 helm-descbinds
                     helm-projectile
                     hydra
                     idle-highlight-mode
                     imenu-anywhere
                     key-chord
                     keyfreq
                     magit
                     markdown-mode
                     move-text
                     multiple-cursors
                     org-bullets
					 rainbow-delimiters
                     rainbow-mode
                     restclient
                     s ;; ???
                     shrink-whitespace
                     smart-mode-line
                     smartscan
                     speed-type
                     sublimity
                     swiper
                     swiper-helm
                     undo-tree
                     w3m
                     web-mode
                     which-key
                     yasnippet))

;; fetch the list of packages available if no elpa dir present
(or (file-exists-p package-user-dir) (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; add .emacs.d/elisp/ and .emacs.d/elisp/groovy-mode/ to load-path
(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "elisp/groovy-mode" user-emacs-directory))

;; custom conf files
(require 'my-functions) ;; custom functions
(require 'flycheck-java) ;; flycheck minor mode for java
(require 'highlight-line) ;; highlight line in list buffers

;;;;;;;;;;;;;;
;; DEFAULTS ;;
;;;;;;;;;;;;;;

;; No splash screen
(setq inhibit-startup-screen t)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Do not auto refresh dired
(setq global-auto-revert-non-file-buffers nil)
;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show active region
(setq-default transient-mark-mode t)

;; Don't move files to trash when deleting
(setq delete-by-moving-to-trash nil)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; <tab> inserts spaces, not tabs and spaces
;; (setq-default indent-tabs-mode nil)

;; No *scratch* message
(setq initial-scratch-message nil)

;; Answer questions with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Sentences end with a single space
(setq sentence-end-double-space nil)

;; UTF-8 everywhere
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Remove text in active region if inserting text
(pending-delete-mode t)

;; Always display line and column numbers
(setq line-number-mode t
      column-number-mode t)

;; Lines should be 80 characters wide, not 70
(setq-default fill-column 80)

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 500) ;; just 20 is too recent

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Allow scrolling with mouse wheel
(when (display-graphic-p) (mouse-wheel-mode t))

;; Don't soft-break lines for me, please
(setq-default truncate-lines t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; Change how buffer names are made unique
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; A saner ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Normal tab completion in Eshell
(setq eshell-cmpl-cycle-completions nil)

;; No flashing!
(setq visible-bell nil)

;; use count-words instead of count-words-region as it works on buffer
;; if no region is selected
(global-set-key (kbd "M-=") 'count-words)

;; use tab to auto-comlete if indentation is right
(setq tab-always-indent 'complete)

;;;;;;;;;;;;;;;;;;;;;;
;; VARIOUS SETTINGS ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Killing emacs
(global-unset-key (kbd "C-x C-c")) ;; too easy to hit by accident, use “M-x kill-emacs” instead
(global-set-key (kbd "C-x r q") 'kill-emacs) ;; really quit emacs

;; C-z is for tmux
(global-unset-key (kbd "C-z")) ;; used for tmux (C-z z for suspend-frame)
(global-set-key (kbd "C-z z") 'suspend-frame) ;; C-z is saved for tmux

;; case-insensitive policy
(setq read-file-name-completion-ignore-case t ;; case-insensitive completion
      read-buffer-completion-ignore-case t) ;; case-insensitive completion

;; colors, appearance
(require 'iso-transl) ;; some environments don’t handle dead keys
(setq font-lock-maximum-decoration t) ;; all possible colours
(blink-cursor-mode -1) ;; no blinking cursor
(global-hl-line-mode -1) ;; don’t highlight current line
(highlight-line-mode 1) ;; except in “list” modes
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)
(global-set-key (kbd "C-c i") 'iwb) ;; indent whole buffer

;; activate key-chords
(require 'key-chord)
(key-chord-mode 1)

;; buffer & file handling
(key-chord-define-global (kbd "«»") 'ibuffer) ;; call ibuffer
(global-set-key (kbd "C-x C-b") 'electric-buffer-list) ;; electric buffer by default
(global-set-key (kbd "C-c o") 'bury-buffer) ;; put buffer at bottom of buffer list
(global-set-key (kbd "C-c k") 'kill-this-buffer) ;; kill buffer without confirmation
(key-chord-define-global (kbd "+-") 'kill-this-buffer) ;; kill buffer without confirmation
(global-set-key (kbd "M-o") 'ace-window) ;; quickly switch to other window
(global-set-key (kbd "<f5>") 'reload-file) ;; re-read file from disk
(global-set-key (kbd "C-<f5>") 'copy-current-file-path) ;; copy current file path
(global-set-key (kbd "M-<f5>") 'show-file-name) ;; show the file name in minibuffer
(global-set-key (kbd "C-x C-r") 'sudo-edit) ;; sudo open file

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 30 30 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

;; undo
(undo-tree-mode t) ;; powerfull undo/redo mode
(diminish 'undo-tree-mode)
(global-set-key (kbd "C-M-z") 'undo) ;; useful when C-/ does not work (windows/putty)

;; smartscan
(require 'smartscan)
(global-set-key (kbd "M-n") 'smartscan-symbol-go-forward) ;; find next occurence of word at point
(global-set-key (kbd "M-p") 'smartscan-symbol-go-backward) ;; find previous occurence of word at point
(global-set-key (kbd "M-'") 'smartscan-symbol-replace) ;; replace all occurences of word at point

;; line handling features
(move-text-default-bindings) ;; M-up / M-down to move line or region
(global-set-key (kbd "<C-M-down>") 'duplicate-current-line)
;; (global-set-key (kbd "<up>") 'previous-line)
;; (global-set-key (kbd "<down>") 'next-line)

;; activate additional features
(helm-mode 0) ;; helm-mode only on demand
(diminish 'helm-mode)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x) ;; superior to M-x
(setq helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
;; helm-for-file looks everywhere, no need for anything else
(key-chord-define-global (kbd "bf") 'helm-for-files)

(global-set-key (kbd "C-h v") 'helm-apropos)
(global-set-key (kbd "C-h f") 'helm-apropos)
(global-set-key (kbd "C-h a") 'helm-apropos)
(global-set-key (kbd "C-h b") 'helm-descbinds)
(global-set-key (kbd "M-ç") 'helm-for-files)
(global-set-key (kbd "C-ç") 'helm-for-files)
(global-set-key (kbd "C-x w") 'helm-wikipedia-suggest) ;; quick wp lookup
;; more commands with helm
(global-set-key (kbd "C-c h p") 'helm-list-elisp-packages-no-fetch)
(global-set-key (kbd "C-c h P") 'helm-apt)
(global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)

;; rgrep
(key-chord-define-global (kbd "éè") 'rgrep)

;; sublimity (minimap, distraction-free mode)
(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)

;; cursor movement and features
(global-set-key (kbd "C-c e") 'er/expand-region) ;; expand region by syntaxic units
(global-set-key (kbd "M-à") 'avy-goto-word-1) ;; quickly jump to word by pressing its first letter
(global-set-key (kbd "C-à") 'avy-goto-char-timer) ;; quickly jump to any char in word
(global-set-key (kbd "M-«") 'simplified-beginning-of-buffer) ;; useful when C-< does not work (windows/putty)
(global-set-key (kbd "M-»") 'simplified-end-of-buffer) ;; useful when C-> does not work (windows/putty)

;; god-mode
(require 'god-mode)
(global-set-key (kbd "C-c g") 'god-mode-all)
(global-set-key (kbd "<f8>") 'god-mode-all)
(defun my-update-cursor ()
  (if god-local-mode
      (set-cursor-color "red")
    (set-cursor-color "yellow")))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(add-hook 'window-configuration-change-hook 'my-update-cursor)
(define-key god-local-mode-map (kbd "z") 'repeat)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "i") 'god-mode-all)

(defun c/god-mode-update-cursor ()
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-mode (progn
                      (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                      (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))
(define-key god-local-mode-map (kbd "i") 'god-local-mode)
(define-key god-local-mode-map (kbd ".") 'repeat)
(add-to-list 'god-exempt-major-modes 'ibuffer-mode)

;; better access to window manipulation commands
(global-set-key (kbd "C-\"") 'delete-other-windows)
(global-set-key (kbd "C-«") 'split-window-below)
(global-set-key (kbd "C-»") 'split-window-right)
(global-set-key (kbd "C-*") 'delete-window)

;; Multiple cursors keybindings
(global-set-key (kbd "M-é") 'mc/edit-lines) ;; new cursor on each line of region
(global-set-key (kbd "M-è") 'mc/mark-all-like-this) ;; new cursor on each occurence of current region
(global-set-key (kbd "M-È") 'mc/mark-next-like-this) ;; new cursor on next occurence of current region
(global-set-key (kbd "M-É") 'mc/mark-previous-like-this) ;; new cursor on previous occurence of current region
(global-set-key (kbd "C-M-é") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-M-è") 'mc/unmark-previous-like-this)

(define-prefix-command 'endless/mc-map)
;; C-x m is usually `compose-mail'. Bind it to something else if you use this command.
(define-key ctl-x-map "m" 'endless/mc-map)
(define-key endless/mc-map "i" 'mc/insert-numbers)
(define-key endless/mc-map "h" 'mc-hide-unmatched-lines-mode)
(define-key endless/mc-map "a" 'mc/mark-all-like-this)
(define-key endless/mc-map "d" 'mc/mark-all-symbols-like-this-in-defun)
(define-key endless/mc-map "D" 'mc/mark-all-dwim)
(define-key endless/mc-map "r" 'mc/reverse-regions)
(define-key endless/mc-map "s" 'mc/sort-regions)
(define-key endless/mc-map "l" 'mc/edit-lines)
(define-key endless/mc-map "\C-a" 'mc/edit-beginnings-of-lines)
(define-key endless/mc-map "\C-e" 'mc/edit-ends-of-lines)

;; auto-completion with company-mode
(global-company-mode) ;; enable company in all buffers
(diminish 'company-mode)
(setq company-show-numbers t)

(add-hook 'markdown-mode-hook 'company-mode)
(add-hook 'text-mode-hook 'company-mode)

;; projectile-mode
(projectile-global-mode) ;; activate projectile-mode everywhere
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t) ;; enable caching for projectile-mode
(setq projectile-switch-project-action 'projectile-vc) ;; magit-status or svn
(diminish 'projectile-mode)


;; spell-check
(require 'ispell)
(setq ispell-dictionary "francais") ;; french dictionary for auto-correct
(setq-default ispell-program-name "aspell") ;; aspell by default

;; Tell ispell.el that ’ can be part of a word.
(setq ispell-local-dictionary-alist
      `((nil "[[:alpha:]]" "[^[:alpha:]]"
             "['\x2019]" nil ("-B") nil utf-8)))
;; Don't send ’ to the subprocess.
(defun endless/replace-apostrophe (args)
  (cons (replace-regexp-in-string
         "’" "'" (car args))
        (cdr args)))
(advice-add #'ispell-send-string :filter-args
            #'endless/replace-apostrophe)
;; Convert ' back to ’ from the subprocess.
(defun endless/replace-quote (args)
  (if (not (derived-mode-p 'org-mode))
      args
    (cons (replace-regexp-in-string
           "'" "’" (car args))
          (cdr args))))
(advice-add #'ispell-parse-output :filter-args
            #'endless/replace-quote)

;; auto-correct
(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table global-abbrev-table)
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

(setq save-abbrevs 'silently)
(setq-default abbrev-mode t)

;; more auto-correct: remove accidental double capitals
(defun dcaps-to-scaps ()
  "Convert word in DOuble CApitals to Single Capitals."
  (interactive)
  (and (= ?w (char-syntax (char-before)))
       (save-excursion
         (and (if (called-interactively-p)
                  (skip-syntax-backward "w")
                (= -3 (skip-syntax-backward "w")))
              (let (case-fold-search)
                (looking-at "\\b[[:upper:]]\\{2\\}[[:lower:]]"))
              (capitalize-word 1)))))

(add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
(define-minor-mode dubcaps-mode
  "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
  :init-value nil
  :lighter (" DC")
  (if dubcaps-mode
      (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
    (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local)))

;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))
(global-set-key (kbd "C-,") 'flyspell-goto-previous-error)

(define-key flycheck-command-map "d"
  #'endless/flycheck-dir)
(defun endless/flycheck-dir (dir)
  "Run flycheck for each file in current directory.
Results are reported in a compilation buffer."
  (interactive "DDirectory: ")
  (displaying-byte-compile-warnings
   (let ((p nil))
     (with-current-buffer (get-buffer-create
                           byte-compile-log-buffer)
       (setq default-directory dir)
       (unless (eq major-mode 'compilation-mode)
         (compilation-mode))
       (goto-char (point-max))
       (let ((inhibit-read-only t))
         (insert "\n\xc\n\n"))
       (setq p (point)))
     (dolist (file (directory-files "./" nil
                                    "\\`[^\\.].*\\'"))
       (endless/-flycheck-file file))
     (with-selected-window (display-buffer
                            byte-compile-log-buffer)
       (goto-char p)
       (recenter 1)))))

(defun endless/-report-error (fmt &rest args)
  "Print an error on `byte-compile-log-buffer'."
  (let ((inhibit-read-only t)
        (fill-prefix "    "))
    (with-current-buffer byte-compile-log-buffer
      (let ((l (point)))
        (insert "\n" (apply #'format fmt args))
        (fill-region (1+ l) (point))))))

(defun endless/-flycheck-file (file)
  "Check FILE and report to `byte-compile-log-buffer'."
  (let ((was-visited (find-buffer-visiting file)))
    (with-current-buffer (or was-visited
                             (progn (find-file file)
                                    (current-buffer)))
      (when (ignore-errors (flycheck-buffer))
        (while (flycheck-running-p)
          (accept-process-output nil 0.1))
        (pcase flycheck-last-status-change
          ((or `errored `suspicious)
           (endless/-report-error
            "%s: Something wrong here!"
            (file-name-nondirectory (buffer-file-name))))
          (`finished
           (dolist (e flycheck-current-errors)
             (endless/-report-error
              "%s:%s:%s:%s: %s"
              (file-name-nondirectory (buffer-file-name))
              (flycheck-error-line e)
              (flycheck-error-column e)
              (flycheck-error-level e)
              (flycheck-error-message e))))))
      (if was-visited
          (bury-buffer was-visited)
        (kill-buffer (current-buffer))))))

;; define word at point
(global-set-key (kbd "<f12>") 'define-word-at-point)

;; shrink-whitespace
(require 'shrink-whitespace)
(global-set-key (kbd "C-x C-o") 'shrink-whitespace)

;; which-key (replacement for guide-key)
(require 'which-key)
(which-key-mode)

;; regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string) ;; syntax used in the re-buidler

;; swipe instead of regular search
(global-set-key (kbd "C-S-s") 'swiper-helm)

;; scratch
(require 'multi-scratch)
(setq multi-scratch-buffer-name "new")
(global-set-key (kbd "C-x \"") 'multi-scratch-new) ;; create new scratch buffer named “new<#>”
(global-set-key (kbd "M-\"") 'multi-scratch-new) ;; create new scratch buffer named “new<#>”
(global-set-key (kbd "C-x «") 'multi-scratch-prev) ;; jump to previous scratch buffer
(global-set-key (kbd "C-x »") 'multi-scratch-next) ;; jump to next scratch buffer

;; date, time, calendar
(setq display-time-day-and-date t ;; display date and time
      display-time-24hr-format t ;; 24h time format
      display-time-string-forms '((propertize
                                   (format-time-string
                                    (or display-time-format
                                        (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                                    now)
                                   'help-echo
                                   (format-time-string "%a %e %b %Y" now))
                                  (if
                                      (and
                                       (not display-time-format)
                                       display-time-day-and-date)
                                      (format-time-string ", %a %e %b %Y" now)
                                    ""))
      european-calendar-style t ;; day/month/year format for calendar
      calendar-week-start-day 1) ;; start week on Monday
(display-time) ;; display time

;;Indentation
(setq-default tab-width 4
              c-auto-newline t
              c-basic-offset 4
              c-block-comment-prefix ""
              c-default-style "k&r"
              indent-tabs-mode t)

;; enable commands disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)

;; rainbow-delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; set default browser to firefox
(setq gnus-button-url 'browse-url-generic
      browse-url-generic-program "firefox"
      browse-url-browser-function gnus-button-url)

;; kill-ring
(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(setq browse-kill-ring-quit-action 'save-and-restore)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; snippets
(require 'yasnippet)
(yas-global-mode 1)
;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
            (not (= (point) (point-min) ))
            (not
             (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
           (word (buffer-substring init-word original-point))
           (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
                  (lambda (s) (string-match (concat "^" word) s)) list)))
        (if (= (length key) 1)
            (setq key (pop key))
          (setq key (ido-completing-read "key: " list nil nil word)))
        (delete-char (- init-word original-point))
        (insert key)
        (yas-expand)))))
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-ido-expand)

;; engine-mode
(require 'engine-mode)
(engine-mode t) ;; prefix C-c /
(defengine duckduckgo "https://duckduckgo.com/?q=%s" :keybinding "d")
(defengine github "https://github.com/search?ref=simplesearch&q=%s" :keybinding "h")
(defengine google "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s" :keybinding "g")
(defengine google-images "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s" :keybinding "i")
(defengine leo "http://dict.leo.org/frde/index_de.html#/search=%s&searchLoc=0&resultOrder=basic&multiwordShowSingle=on" :keybinding "l")
(defengine google-maps "http://maps.google.com/maps?q=%s" :keybinding "m")
(defengine stack-overflow "https://stackoverflow.com/search?q=%s" :keybinding "o")
(defengine wikipedia "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s" :keybinding "w")
(defengine wiktionary "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s" :keybinding "t")
(defengine youtube "http://www.youtube.com/results?aq=f&oq=&search_query=%s" :keybinding "y")
(defengine torrentz "https://torrentz.eu/search?f=%s" :keybinding "z")
(defengine confluence "http://confluence.sfrdev.fr/dosearchsite.action?queryString=%s" :keybinding "c")
(defengine wordreference "www.wordreference.com/enfr/%s" :keybinding "r")
(defengine wordreference "www.wordreference.com/fren/%s" :keybinding "R")
(engine/set-keymap-prefix (kbd "C-c s"))

;;revert windows on ediff exit - needs winner mode
(winner-mode)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;; hydra mode
(require 'hydra)
(defvar whitespace-mode nil)
(defvar idle-highlight-mode nil)
(defvar linum-mode nil)
(defhydra hydra-toggle-mode (:color blue)
  "
^Major mode^    ^Run^             ^Minor mode^
^^^^^^^^------------------------------------------------
_mg_: groovy    _b_: tabify       _a_: abbrev     %`abbrev-mode
_mj_: java      _B_: untabify     _c_: dubcaps    %`dubcaps-mode
_mJ_: js        _d_: dirtree      _f_: flyspell   %`flyspell-mode
_ml_: lisp      _D_: change dict. _g_: god        %`god-local-mode
_mm_: markdown  _F_: flycheck     _h_: highlight  %`idle-highlight-mode
_mo_: org       _i_: indent       _l_: linum      %`linum-mode
_mp_: python    _r_: reload conf  _s_: sublimity  %`sublimity-mode
_mr_: ruby      _T_: rm. trail.   _t_: trailing   %`show-trailing-whitespace
_ms_: shell     _u_: undo-tree    _w_: whitespace %`whitespace-mode
_mS_: sql
_mt_: text
_mw_: web
_mx_: xml
"
  ("a" abbrev-mode "abbrev")
  ("b" tabify "tabify")
  ("B" untabify "untabify")
  ("c" dubcaps-mode "dubcaps")
  ("d" dirtree "dirtree")
  ("D" ispell-change-dictionary "change dict.")
  ;; ("d" toggle-debug-on-error "debug")
  ("f" flyspell-mode "flyspell")
  ("F" flyspell-buffer "flycheck")
  ("g" god-mode "GOD")
  ("h" idle-highlight-mode "highlight")
  ("i" iwb "indent")
  ("l" linum-mode "linum")
  ("r" reload-config "reload conf")
  ("s" sublimity-mode "sublimity")
  ("t" toggle-show-trailing-whitespace "trailing")
  ("T" delete-trailing-whitespace "rm trail.")
  ("u" undo-tree-visualize "undo-tree")
  ("w" whitespace-mode "whitespace")
  ("mg" groovy-mode "groovy")
  ("mj" java-mode "java")
  ("mJ" javascript-mode "javascript")
  ("ml" lisp-mode "lisp")
  ("mm" markdown-mode "markdown")
  ("mo" org-mode "org")
  ("mp" python-mode "python")
  ("mr" ruby-mode "ruby")
  ("ms" sh-mode "shell")
  ("mS" sql-mode "sql")
  ("mt" text-mode "text")
  ("mw" web-mode "web")
  ("mx" xml-mode "xml")
  ("q" nil "cancel"))
(global-set-key (kbd "C-c m") 'hydra-toggle-mode/body)
(global-set-key (kbd "C-=") 'hydra-toggle-mode/body)

(defhydra hydra-move-around (:color pink)
  "move around"
  ("«" simplified-beginning-of-buffer "top")
  ("»" simplified-end-of-buffer "bottom")
  ("a" move-beginning-of-line "home")
  ("e" move-end-of-line "end")
  ("n" next-line "down")
  ("j" next-line "down")
  ("p" previous-line "up")
  ("k" previous-line "up")
  ("f" forward-char "right")
  ("b" backward-char "left")
  ("h" backward-char "left")
  ("<SPC>" scroll-up-command "pg down")
  ("d" scroll-up-command "pg down")
  ("v" scroll-up-command "pg down")
  ("u" scroll-down-command "pg up")
  ("l" recenter-top-bottom "recenter")
  ("g" goto-line "goto")
  ("q" nil "cancel" :color blue))
(global-set-key (kbd "C-c c") 'hydra-move-around/body)

(defhydra hydra-spell (:color blue)
  "spelling"
  ("t" endless/ispell-word-then-abbrev "corr. & add")
  ("f" flyspell-mode "flyspell")
  ("c" flyspell-buffer "flycheck buffer")
  ("F" flyspell-buffer "flycheck buffer")
  ("d" ispell-change-dictionary "change dictionary")
  ("q" nil "cancel"))
(global-set-key (kbd "<f9>") 'hydra-spell/body)

(defhydra hydra-arabic (:color pink)
  "type in arabic"
  ("a" (insert-char 1575) "ا") ;; ARABIC LETTER ALEF
  ("b" (insert-char 1576) "ب") ;; ARABIC LETTER BEH
  ("t" (insert-char 1578) "ت") ;; ARABIC LETTER TEH
  ("þ" (insert-char 1579) "ث") ;; ARABIC LETTER THEH
  ("j" (insert-char 1580) "ج") ;; ARABIC LETTER JEEM
  ("H" (insert-char 1581) "ح") ;; ARABIC LETTER HAH
  ("†" (insert-char 1582) "خ") ;; ARABIC LETTER KHAH
  ("d" (insert-char 1583) "د") ;; ARABIC LETTER DAL
  ("ð" (insert-char 1584) "ذ") ;; ARABIC LETTER THAL
  ("r" (insert-char 1585) "ر") ;; ARABIC LETTER REH
  ("z" (insert-char 1586) "ز") ;; ARABIC LETTER ZAIN
  ("s" (insert-char 1587) "س") ;; ARABIC LETTER SEEN
  ("ß" (insert-char 1588) "ش") ;; ARABIC LETTER SHEEN
  ("S" (insert-char 1589) "ص") ;; ARABIC LETTER SAD
  ("D" (insert-char 1590) "ض") ;; ARABIC LETTER DAD
  ("T" (insert-char 1591) "ط") ;; ARABIC LETTER TAH
  ("Z" (insert-char 1592) "ظ") ;; ARABIC LETTER ZAH
  ("g" (insert-char 1593) "ع") ;; ARABIC LETTER AIN
  ("®" (insert-char 1594) "غ") ;; ARABIC LETTER GHAIN
  ("f" (insert-char 1601) "ف") ;; ARABIC LETTER FEH
  ("Q" (insert-char 1602) "ق") ;; ARABIC LETTER QAF
  ("k" (insert-char 1603) "ك") ;; ARABIC LETTER KAF
  ("l" (insert-char 1604) "ل") ;; ARABIC LETTER LAM
  ("m" (insert-char 1605) "م") ;; ARABIC LETTER MEEM
  ("n" (insert-char 1606) "ن") ;; ARABIC LETTER NOON
  ("h" (insert-char 1607) "ه") ;; ARABIC LETTER HEH
  ("w" (insert-char 1608) "و") ;; ARABIC LETTER WAW
  ("y" (insert-char 1610) "ي") ;; ARABIC LETTER YEH
  ("'" (insert-char 1569) "ء") ;; ARABIC LETTER HAMZA
  ("q" nil "cancel" :color blue))

(global-set-key (kbd "<f6>") 'hydra-arabic/body)

;; char-menu
(require 'char-menu)
(global-set-key (kbd "<f7>") 'char-menu)
(setq char-menu '(("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                  ("Math"       "≈" "≡" "∞" "√" "∀" "∃")
                  ("cyrillic"       "а" "б" "в" "г" "д" "е" "ж" "з" "и" "й" "к" "л" "м" "н" "о" "п" "р" "с")
                  ("Smileys"       "☺" "☹")
                  ("Arrows"     "←" "→" "↑" "↓" "↔" "↕" "⇔" "⇐" "⇒")))


;; statistics on key use frequency
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

(global-set-key (kbd "C-.") 'helm-imenu-anywhere)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MAJOR MODE SPECIFIC CONFIGURATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; text
(setq default-major-mode 'text-mode) ;; text-mode by default
(add-hook 'text-mode-hook 'flyspell-mode) ;; flyspell by default
(add-hook 'text-mode-hook 'visual-line-mode) ;; auto-wrapping (soft wrap) in text-mode
(add-hook 'text-mode-hook 'dubcaps-mode) ;; auto-correct double capitals
(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; visual-line-mode instead of auto-fill

;; mail-mode
;; (remove-hook 'html-helper-mode-hook 'flyspell-mode) ;; auto-correct disabled by default
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping in mail-mode

;; dired
(require 'dired-details)
(dired-details-install)
(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-AlhGF") ;; dired human readable size format, hide group
(require 'dired-narrow)
(define-key dired-mode-map (kbd "/") 'dired-narrow)

;; git
(require 'magit)
(autoload 'gitconfig-mode "gitconfig-mode" "Major mode for editing gitconfig files." t)
(add-to-list 'auto-mode-alist '(".gitconfig$" . gitconfig-mode))
(key-chord-define-global (kbd "qg") 'magit-status) ;; run git status for current buffer
(setq magit-last-seen-setup-instructions "1.4.0")
(magit-define-popup-switch 'magit-log-popup ?w "date-order" "--date-order")

;; SH
(add-hook 'sh-mode-hook (lambda () (setq tab-width 4 sh-basic-offset 4 indent-tabs-mode t)))
;;(autoload 'sh-mode "sh-mode" "Major mode for editing shell scripts." t)
(add-to-list 'auto-mode-alist '(".*rc$" . sh-mode))
(add-to-list 'auto-mode-alist '(".*bash.*$" . sh-mode))

;; SQL
(add-to-list 'auto-mode-alist '(".sql$" . sql-mode))
(add-to-list 'auto-mode-alist '(".pks$" . sql-mode))
(add-to-list 'auto-mode-alist '(".pkb$" . sql-mode))
(add-to-list 'auto-mode-alist '(".mvw$" . sql-mode))
(add-to-list 'auto-mode-alist '(".con$" . sql-mode))
(add-to-list 'auto-mode-alist '(".ind$" . sql-mode))
(add-to-list 'auto-mode-alist '(".sqs$" . sql-mode))
(add-to-list 'auto-mode-alist '(".tab$" . sql-mode))
(add-to-list 'auto-mode-alist '(".trg$" . sql-mode))
(add-to-list 'auto-mode-alist '(".vw$" . sql-mode))
(add-to-list 'auto-mode-alist '(".prc$" . sql-mode))
(add-to-list 'auto-mode-alist '(".pk$" . sql-mode))
;;; sql-oracle connection without a tnsnames.ora
;; (description=(address_list=(address=(protocol=TCP)(host=myhost.example.com)(port=1521)))(connect_data=(SERVICE_NAME=myservicename)))
;; GÉO : (description=(address_list=(address=(protocol=TCP)(host=DEV-GEO-BACK)(port=1521)))(connect_data=(SID=GEODEV1)
(add-hook 'sql-mode (setq truncate-lines nil))
(add-hook 'sql-mode (setq linesize 9999))

(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
(add-hook 'sql-interactive-mode-hook
          (function (lambda ()
                      (setq comint-output-filter-functions 'comint-truncate-buffer
                            comint-buffer-maximum-size 5000
                            comint-scroll-show-maximum-output t
                            comint-input-ring-size 500))))

;; GROOVY
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))
;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))
(autoload 'groovy-eval "groovy-eval" "Groovy Evaluation" t)
(add-hook 'groovy-mode-hook 'groovy-eval)

;; RUBY
;; Loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(add-to-list 'auto-mode-alist '(".rb$" . ruby-mode))

;; HTML, XML, JSP (using web-mode)
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tag\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.wsdl\\'" . web-mode))

(setq web-mode-engines-alist '(("php" . "\\.phtml\\'")
                               ("blade" . "\\.blade\\.")))
(add-hook 'css-mode-hook 'rainbow-mode)

;; JAVA
(add-hook 'java-mode-hook (lambda () (setq flycheck-java-ecj-jar-path "/home/arthur/outils/java/ecj-4.5.jar")))

;; JAVASCRIPT (to be tested)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq js2-basic-offset 2)
;; (setq js2-use-font-lock-faces t)


(autoload 'json-pretty-print "json-pretty-print" "json-pretty-print" t)
(add-hook 'json-mode-hook 'json-pretty-print)

(require 'flycheck)
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))

;; LISP
(define-key lisp-mode-map (kbd "C-c x") 'eval-and-replace) ;; eval sexp and replace it by its value
;; (global-set-key (kbd "C-c c") 'compile)

;; PYTHON
;; (add-hook 'python-mode-hook 'jedi:setup) ;; fire up jedi in python env
;; (setq jedi:complete-on-dot t) ;; optional

;; ORG-MODE
(require 'org)
(global-set-key (kbd "\C-c l") 'org-store-link)
(global-set-key (kbd "\C-c a") 'org-agenda)
(global-set-key (kbd "\C-c b") 'org-iswitchb)
(global-set-key (kbd "\C-c j") 'jirify)
(define-key org-mode-map (kbd "\C-c t") 'org-begin-template)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ORG-CAPTURE
(setq org-default-notes-file (concat user-emacs-directory "notes.org"))
(setq terminalcity-dir "~/Terminalcity/")
(setq sfr-journal-file (concat terminalcity-dir "SFR.org"))
(setq polopeche-home-dir "/sshx:polopeche:/home/duncan/")

(key-chord-define-global (kbd "gx") 'org-capture)
(setq org-export-coding-system 'utf-8)

;; org-capture-templates
(setq org-capture-templates
      '(
        ;; local
        ("d" "local - Diary entry" entry (file+datetree (concat terminalcity-dir "Textes/diary.org")) "* %<%Hh%M>\n\t%i%?")
        ("n" "local - Note" entry (file+datetree org-default-notes-file) "* %<%Hh%M>\n\t%i%?")
        ("t" "local - TODO" entry (file+headline (concat terminalcity-dir "Todo/arthur.org") "VRAC") "* TODO %?\n\t%i")
        ("y" "local - Code snippet" plain (file (concat user-emacs-directory "code-snippets.txt")) "\n%i%?")
        ;; remote
        ("D" "polopeche - Diary entry" entry (file+datetree (concat polopeche-home-dir "Terminalcity/Textes/diary.org")) "* %<%Hh%M>\n\t%i%?")
        ("T" "polopeche - TODO" entry (file+headline (concat polopeche-home-dir "Terminalcity/Todo/arthur.org") "VRAC") "* TODO %?\n\t%i")
        ))

(setq org-completion-use-ido t)

;; font and faces customization
(setq org-todo-keyword-faces
      '(("INPR" . (:foreground "yellow" :weight bold))
        ("STARTED" . (:foreground "yellow" :weight bold))
        ("WAIT" . (:foreground "yellow" :weight bold))
        ("INPROGRESS" . (:foreground "yellow" :weight bold))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS-specific configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load system-specific library and setup system-specific things that
;; must be setup before main setup
(defun load-windows-specific-conf ()
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
(defun load-linux-specific-conf ()
  "Loads all GNU/Linux specific conf"
  )
(cond ((eq system-type 'windows-nt) (load-windows-specific-conf))
      ((eq system-type 'gnu/linux) (load-linux-specific-conf)))

;;;;;;;;;;;;;;;;;;;
;; CUSTOMISATION ;;
;;;;;;;;;;;;;;;;;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;;;;;;;;;;;;;
;; EPILOGUE ;;
;;;;;;;;;;;;;;

;; mode line (smart-mode-line)
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))
(setq sml/theme 'dark)
(sml/setup)

;; set up a dark theme
(color-theme-initialize)
(color-theme-dark-laptop)

;; server mode
(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))
(global-set-key (kbd "M-#") 'server-edit) ;; send back to server, quicker than C-x #

;; session saving, backup management
(setq vc-make-backup-files t) ;; make backups of files, even when they're in version control
(setq desktop-base-lock-name      "lock"
      desktop-save                t
      desktop-dirname             user-emacs-directory
      desktop-path                (list desktop-dirname)
      ;; desktop-files-not-to-save   "^$" ;reload tramp paths
      desktop-load-locked-desktop t)
(desktop-save-mode 1)
(savehist-mode 1)
(desktop-read)

;; always open init file!
(find-file (expand-file-name "init.el" user-emacs-directory))

;;; init.el ends here
