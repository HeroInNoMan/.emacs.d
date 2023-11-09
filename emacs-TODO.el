(use-package emojify
  :hook ((org-mode erc-mode) . (lambda () (emojify-mode t))))

(use-package saveplace
  :straight nil
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name "places" base-emacs-directory)
        vc-make-backup-files t ;; make backups of files, even when they're in version control
        backup-directory-alist `(("." . ,(expand-file-name "backups"
                                                           base-emacs-directory)))))

;; some more config
(setq recentf-max-saved-items 100  ;; just 20 is too recent
      delete-by-moving-to-trash t) ;; move files to trash when deleting
(recentf-mode 1)

(use-package savehist
  :init (savehist-mode)
  (add-to-list 'savehist-additional-variables 'register-alist)
  (add-to-list 'savehist-additional-variables 'search-ring)
  (add-to-list 'savehist-additional-variables 'regexp-search-ring)
  (add-to-list 'savehist-additional-variables 'log-edit-comment-ring))

(setq-default transient-mark-mode t ;; Show active region
              truncate-lines t ;; Don't soft-break lines for me, please
              truncate-string-ellipsis "…")

(setq font-lock-maximum-decoration t ;; all possible colours
      inhibit-startup-screen t ;; No splash screen
      echo-keystrokes 0.1 ;; Show keystrokes in progress
      initial-scratch-message nil ;; No *scratch* message
      initial-major-mode 'text-mode
      visible-bell nil) ;; No flashing!

(use-package default-text-scale
  :bind
  ("C-+" . default-text-scale-increase)
  ("C-=" . default-text-scale-decrease)
  :config
  (setq default-text-scale-amount 5)
  (defadvice default-text-scale-increase (after fix-font activate) (set-fira-font-if-possible))
  (defadvice default-text-scale-decrease (after fix-font activate) (set-fira-font-if-possible))
  (defadvice default-text-scale-reset (after fix-font activate) (set-fira-font-if-possible)))

(use-package font-size
  :straight '(font-size :host github :repo "nabeix/emacs-font-size")
  :bind
  ("M-+" . font-size-increase)
  ("M-=" . font-size-decrease)
  ;; ("M-0" . font-size-default)
  :config (font-size-init 10)
  (defadvice font-size-increase (after fix-font activate) (set-fira-font-if-possible))
  (defadvice font-size-decrease (after fix-font activate) (set-fira-font-if-possible))
  (defadvice font-size-default (after fix-font activate) (set-fira-font-if-possible)))


(use-package dimmer
  :config (dimmer-mode)
  (setq dimmer-fraction 0.25))

(use-package font-lock+ :defer t)

(use-package kurecolor :defer t)

(use-package dired
  :straight nil
  :bind ("C-x C-j". dired-jump)
  :config
  (unbind-key "M-b" dired-mode-map)
  (put 'dired-find-alternate-file 'disabled nil)
  (setq dired-dwim-target t
        ;; dired human readable size format
        dired-listing-switches "-AlhF"
        auto-revert-verbose nil
        ;; always delete and copy recursively
        dired-recursive-deletes 'always
        dired-recursive-copies 'always))

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   my-private-dirvish-quick-access-entries)
  :config
  (setq dirvish-peek-categories '(project-file library))
  (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash nil)
  (setq dirvish-time-format-string "%Y-%m-%d %R")
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("/"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("h"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("i" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(use-package ctrlxo
  :bind ("C-x o" . ctrlxo))

(use-package fill-page :defer t)

(use-package dumb-jump
  :bind (:map prog-mode-map
              ("C-." . xref-find-definitions)
              ("C-," . xref-pop-marker-stack)
              ("C-;" . dumb-jump-quick-look))
  :config (setq dumb-jump-default-project
                (if (boundp 'my-default-project-root)
                    my-default-project-root
                  "~")))

;; Allow scrolling with mouse wheel
(when (display-graphic-p)
  (progn (mouse-wheel-mode t)
         (mouse-avoidance-mode 'none)
         (setq mouse-autoselect-window nil)))

;;Indentation
(setq-default tab-width 2
              c-auto-newline t
              c-basic-offset 2
              c-block-comment-prefix ""
              c-default-style "k&r"
              indent-tabs-mode nil ;; <tab> inserts spaces, not tabs and spaces
              sentence-end-double-space nil) ;; Sentences end with a single space

;; use tab to auto-comlete if indentation is right
(setq tab-always-indent 'complete)



(set-face-attribute 'nobreak-space nil
                    :inherit 'escape-glyph
                    :foreground "black"
                    :underline t)

(use-package origami
  :hook (prog-mode . origami-mode)
  :config (setq origami-fold-replacement "..."))

(pretty-hydra-define hydra-fold
  (:color pink :quit-key "q")
  ("built-in"
   (("s" ale/toggle-selective-display "selective display")
    ("n" narrow-to-defun "narrow"))
   "vimish fold"
   (("f" vimish-fold "fold")
    ("k" vimish-fold-delete "kill")
    ("K" vimish-fold-delete-all "kill all")
    ("p" vimish-fold-previous-fold "go previous")
    ("n" vimish-fold-next-fold "go next")
    ("<tab>" vimish-fold-toggle "toggle current")
    ("a" vimish-fold-toggle-all "toggle all"))
   "origami"
   (("x" origami-toggle-all-nodes "toggle all")
    ("y" origami-show-only-node "show only node")
    ("c" origami-recursively-toggle-node "cycle"))
   ))
(global-set-key (kbd "C-c <tab>") 'hydra-fold/body)

(use-package isearch-dabbrev
  :bind (:map isearch-mode-map
              ("<tab>" . isearch-dabbrev-expand)
              ("M-/" . isearch-dabbrev-expand)))

(use-package smartscan
  :bind
  ("M-n". smartscan-symbol-go-forward) ;; find next occurence of word at point
  ("M-p". smartscan-symbol-go-backward) ;; find previous occurence of word at point
  ("M-'". smartscan-symbol-replace)) ;; replace all occurences of word at point

;; regexp-builder
(use-package re-builder
  :defer t
  :config (setq reb-re-syntax 'string)) ;; syntax used in the re-buidler

(use-package visual-regexp-steroids
  :bind
  (("M-s r" . vr/replace)
   ("M-s q" . vr/query-replace)
   ("C-M-%" . vr/query-replace)
   ("M-s m" . vr/mc-mark) ;; useful with multiple-cursors
   ("M-s C-M-s" . vr/isearch-forward)
   ("M-s C-M-r" . vr/isearch-backward)
   ("M-s S" . isearch-forward)
   ("M-s R" . isearch-backward))
  :config (require 'visual-regexp)) ;; TODO check if really necessary

(use-package ctrlf
  :commands ctrlf-yank-word-or-char

  :bind (("C-s" . ctrlf-forward-literal)
         ("C-r" . ctrlf-backward-literal)
         ("C-M-s" . ctrlf-forward-regexp)
         ("C-M-r" . ctrlf-backward-regexp)
         :map minibuffer-local-map
         ("C-s" . ctrlf-forward-literal)
         ("C-r" . ctrlf-backward-literal)
         ("C-w" . ctrlf-yank-word-or-char))
  :config
  (ctrlf-mode)
  (defun ctrlf-yank-word-or-char ()
    (interactive)
    (let ((input (field-string (point-max))) yank)
      (when (or ctrlf--match-bounds (= (length input) 0))
        (with-current-buffer (window-buffer (minibuffer-selected-window))
          (setq yank (buffer-substring-no-properties
                      (or (and ctrlf--match-bounds
                               (cdr ctrlf--match-bounds))
                          ctrlf--current-starting-point)
                      (progn (forward-word) (point)))))
        (goto-char (field-end (point-max)))
        (insert yank))))
  :custom-face (ctrlf-highlight-active ((t (:inherit isearch :background "dark orange")))))

(use-package phi-search
  :bind
  ("M-s C-s" . phi-search)
  ("M-s C-r" . phi-search-backward)
  :custom-face (phi-search-selection-face ((t (:inherit isearch :background "dark orange")))))

(setq completion-ignore-case t
      pcomplete-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-quit-action 'save-and-restore))
(use-package deadgrep
  :bind
  (:map deadgrep-mode-map
        ("f" . next-error-follow-minor-mode) ;; follow
        ("v" . next-error-follow-minor-mode))) ;; view

(use-package dabbrev

  ;; Swap M-/ and C-M-/
  ;; :bind (("M-/" . dabbrev-completion)
  ;;        ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

(use-package hippie-exp
  :straight nil
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-line
          try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-from-kill
          try-expand-dabbrev-all-buffers
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

(use-package company
  :hook (prog-mode . global-company-mode)
  :config
  (global-company-mode 1) ;; enable company in all buffers
  (setq company-tooltip-limit 30
        company-idle-delay 0.5 ;; almost no delay before showing candidates
        company-minimum-prefix-length 2 ;; completion rigth away!
        company-show-numbers 'on
        company-dabbrev-downcase nil))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode)
  :config
  (company-prescient-mode)
  (prescient-persist-mode))

(use-package yasnippet
  :commands yas-new-snippet yas-insert-snippet
  :chords ("yq" . hydra-yasnippet/body)
  :hook
  (snippet-mode . (lambda () (aggressive-indent-mode -1)))
  (text-mode . (lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
  (python-mode . (lambda () (set (make-local-variable 'yas-indent-line) 'fixed)))
  :bind
  ("<backtab>" . yas-complete-expand)
  ("C-h y" . yas-describe-tables)
  ("C-c y" . hydra-yasnippet/body)
  :config
  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs
        `(,(expand-file-name "etc/yasnippet/snippets" base-emacs-directory)
          ,(car (file-expand-wildcards
                 (expand-file-name "straight/repos/yasnippet-snippets/snippets" base-emacs-directory)))))
  ;; Completing point by some yasnippet key

  (yas-global-mode 1))

(use-package yasnippet-snippets :after yasnippet)

(use-package auto-yasnippet
  :after yasnippet
  :config
  (setq aya-persist-snippets-dir
        (expand-file-name "etc/yasnippet/snippets" base-emacs-directory)))


(use-package consult-yasnippet
  :after yasnippet)

(use-package embark-consult)

(use-package consult-git-log-grep
  :custom (consult-git-log-grep-open-function #'magit-show-commit))

(use-package consult-recoll
  :if (= 0 (shell-command "hash recoll"))) ;; check if recoll is present on the system

(use-package wgrep
  :bind
  (:map grep-mode-map
        ("e" . wgrep-change-to-wgrep-mode)
        ("C-c C-e" . wgrep-change-to-wgrep-mode)))



;; Remove text in active region if inserting text
(pending-delete-mode t)

;; Allow pasting selection outside of Emacs
(setq-default select-enable-clipboard t
              x-select-enable-clipboard t)

;; easier access to transposition commands
(global-set-key (kbd "C-x M-h") 'transpose-paragraphs)
(global-set-key (kbd "C-§") 'transpose-paragraphs)
(global-set-key (kbd "C-x M-s") 'transpose-sentences)
(global-set-key (kbd "C-x M-t") 'transpose-sexps)

(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows)
  ("C-\"" . zygospore-toggle-delete-other-windows))

(use-package winner
  :bind
  ("<s-left>"  . winner-undo)
  ("<s-right>" . winner-redo)
  ([f1]        . winner-undo)
  ([f2]        . winner-redo)
  :config (winner-mode))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

(use-package autorevert
  :straight nil
  :config
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t))

(pretty-hydra-define hydra-file
  (:color blue :quit-key "q")
  ("Refresh"
   (("<f5>" ale/revert-buffer-no-confirm "revert buffer"))
   "Crux commands…"
   (("f" crux-kill-buffer-truename          "copy qualified name")
    ("k" crux-kill-other-buffers            "kill other buffers")
    ("r" crux-rename-file-and-buffer        "rename")
    ("d" crux-delete-file-and-buffer        "delete")
    ("c" crux-copy-file-preserve-attributes "copy"))))
(global-set-key (kbd "<f5>") 'hydra-file/body)

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default)
  (defun persistent-scratch-numbered-scratch-buffer-p ()
    "Return non-nil iff the current buffer's name begins with *scratch*."
    (and (> (length (buffer-name)) 8)
         (string-equal "*scratch*"  (substring (buffer-name) 0 9))))
  (setq persistent-scratch-scratch-buffer-p-function
        'persistent-scratch-numbered-scratch-buffer-p))

;; C-M-e to edit minibuffer in a full-size buffer
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

(use-package vc
  :defer t
  :config (setq vc-follow-symlinks t))

(use-package vdiff
  :disabled
  :bind (:map vdiff-mode-map
              ("C-c" . vdiff-mode-prefix-map))
  :init (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  :config
  (setq
   ;; Whether to lock scrolling by default when starting vdiff
   vdiff-lock-scrolling t
   ;; external diff program/command to use
   vdiff-diff-program "diff"
   ;; Extra arguments to pass to diff. If this is set wrong, you may break vdiff.
   vdiff-diff-program-args ""
   ;; Commands that should be executed in other vdiff buffer to keep lines in
   ;; sync. There is no need to include commands that scroll the buffer here,
   ;; because those are handled differently.
   vdiff-mirrored-commands '(next-line
                             previous-line
                             beginning-of-buffer
                             end-of-buffer)
   ;; Minimum number of lines to fold
   vdiff-fold-padding 2
   ;; Unchanged lines to leave unfolded around a fold
   vdiff-min-fold-size 4
   ;; Function that returns the string printed for a closed fold. The arguments
   ;; passed are the number of lines folded, the text on the first line, and the
   ;; width of the buffer.
   vdiff-fold-string-function 'vdiff-fold-string-default))

(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-vertically
      ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package smerge-mode
  :straight nil
  :bind (("M-s e" . hydra-smerge/body))
  :config
  (setq smerge-command-prefix "")
  ;; https://github.com/alphapapa/unpackaged.el#smerge-mode
  (pretty-hydra-define hydra-smerge
    (:color pink :quit-key "q" :post (smerge-auto-leave))
    ("Move"
     (("n" smerge-next "next")
      ("p" smerge-prev "prev"))
     "Keep"
     (("b" smerge-keep-base "base")
      ("u" smerge-keep-upper "upper")
      ("l" smerge-keep-lower "lower")
      ("a" smerge-keep-all "all")
      ("RET" smerge-keep-current "current"))
     "Diff"
     (("<" smerge-diff-base-upper "upper/base")
      ("=" smerge-diff-upper-lower "upper/lower")
      (">" smerge-diff-base-lower "base/lower")
      ("R" smerge-refine "Refine")
      ("e" smerge-ediff "e-diff" :color blue))
     "Other"
     (("c" smerge-combine-with-next "combine")
      ("r" smerge-resolve "resolve")
      ("k" smerge-kill-current "kill current")))))

(use-package git-timemachine :defer t)

(use-package git-messenger
  :bind (:map git-messenger-map
              ("d" . git-messenger:popup-diff)
              ("s" . git-messenger:)
              ("c" . git-messenger:copy-commit-id))
  :hook (git-messenger:popup-buffer-hook . magit-commit-mode)
  :config (setq git-messenger:show-detail t))

(use-package gitignore-mode
  :straight (:host github :repo "magit/git-modes"
             :files ("gitignore-mode.el"))
  :mode ".gitignore$")

(use-package gitconfig-mode
  :straight (:host github :repo "magit/git-modes"
             :files ("gitconfig-mode.el"))
  :mode ".gitconfig$")

(use-package git-gutter
  :hook prog-mode
  :bind
  ("M-N" . git-gutter:next-hunk)
  ("M-P" . git-gutter:previous-hunk)
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:ask-p nil
        git-gutter:hide-gutter t))

(use-package github-review
  :config
  (setq github-review-view-comments-in-code-lines t
        github-review-reply-inline-comments t))

(use-package magit
  :chords ("qg" . magit-status) ;; run git status for current buffer
  :bind ("C-x g" . hydra-git/body)
  :hook
  (git-commit-setup . ale/insert-ticket-prefix)
  (git-commit-setup . ale/switch-to-all-dict)
  :custom-face (magit-branch-current ((t ( :foreground "#5cEfFF"
                                                       :box ( :line-width 3
                                                                          :color "#ee0000"
                                                                          :style released-button)))))
  :config
  (put 'magit-edit-line-commit 'disabled nil)
  (setq magit-diff-refine-hunk 'all)
  (pretty-hydra-define hydra-git
    (:color blue :quit-key "q")
    ("Gutter"
     (("n" git-gutter:next-hunk "next hunk" :color pink)
      ("p" git-gutter:previous-hunk "prev hunk" :color pink)
      ("r" git-gutter:revert-hunk "revert hunk" :color pink)
      ("s" git-gutter:stage-hunk "stage hunk" :color pink)
      ("u" git-gutter-mode "gutter mode"))
     "Magit"
     (("." magit-status "status")
      ("d" magit-dispatch "dispatch…")
      ("g" magit-file-dispatch "file action…")
      ("c" magit-clone "clone…"))
     "Github"
     (("S" github-review-forge-pr-at-point "Start review")
      ("a" github-review-approve "Approve")
      ("R" (github-review-comment) "Comment"))
     "Other"
     (("t" git-timemachine "time machine")
      ("m" git-messenger:popup-message "popup message"))))

  (use-package forge)

  (use-package magit-todos
    :config (setq magit-todos-max-items 30))

  (use-package magit-org-todos
    :config
    (magit-org-todos-autoinsert)
    (when (boundp 'my-private-work-diary-org-file)
      (setq-default magit-org-todos-filename my-private-work-diary-org-file))))

(use-package projectile
  :defer t
  :config
  (require 'f)
  (when (version< (car (cdr (s-split " " (s-trim (shell-command-to-string "fdfind --version"))))) "8.3")
    (setq projectile-git-fd-args "-H -0 -E .git -tf"))
  (setq projectile-mode-line-prefix " ")
  (setq projectile-completion-system 'auto)
  (setq projectile-require-project-root nil)
  (setq projectile-enable-caching t) ;; enable caching for projectile-mode
  ;; (setq projectile-project-search-path '(my-private-repos-dir my-default-project-root))
  (setq projectile-switch-project-action #'ale/open-project)
  (projectile-mode) ;; activate projectile-mode everywhere
  (def-projectile-commander-method ?d
                                   "Open project root in dired."
                                   (projectile-dired))

  (def-projectile-commander-method ?f
                                   "Git fetch."
                                   (magit-status)
                                   (call-interactively #'magit-fetch-current))
  (ale/scan-projects))

(use-package tramp
  :straight nil
  ;; :defer t
  :config
  (setq recentf-keep '(file-remote-p file-readable-p))
  (tramp-cleanup-all-connections)
  (tramp-cleanup-all-buffers)
  (setq
   ;; remote-file-name-inhibit-cachefile-name-inhibit-cache nil ;; cache file-name forever
   remote-file-name-inhibit-cachefile-name-inhibit-cache t
   tramp-verbose 0 ;; log all
   tramp-syntax 'default
   ;; make sure vc stuff is not making tramp slower
   tramp-use-ssh-controlmaster-options nil
   vc-ignore-dir-regexp (format "%s\\|%s"
                                vc-ignore-dir-regexp
                                tramp-file-name-regexp)))

(use-package time
  :straight nil
  :config
  (setq display-time-day-and-date t ;; display date and time
        display-time-24hr-format t ;; 24h time format
        european-calendar-style t ;; day/month/year format for calendar
        calendar-week-start-day 1 ;; start week on Monday
        display-time-string-forms '((if (and (not display-time-format)
                                             display-time-day-and-date)
                                        (format-time-string "%H:%M" now))))
  (display-time))

(pretty-hydra-define hydra-dates
  (:color blue :quit-key "q")
  ("Insert date"
   (("s" ab/date-short (format-time-string "%d/%m/%Y"))
    ("d" ab/date-iso (format-time-string "%F"))
    ("D" ab/date-iso-with-day (format-time-string "%F %A"))
    ("l" ab/date-long (format-time-string "%d %B %Y"))
    ("L" ab/date-long-with-day (format-time-string "%A %d %B %Y"))
    ("o" ale/org-date (format-time-string "<%F %a>")))
   "Insert date & time"
   (("t" ab/date-short-with-time (format-time-string "%Y/%m/%d %H:%M"))
    ("T" ab/date-long-with-time (format-time-string "%A %d %B %Y - %H:%M") :width 33)
    ("i" ab/date-iso-with-time (format-time-string "%FT%T%z"))
    ("x" crux-insert-date "crux format")
    ("O" ale/org-date-time (format-time-string "<%F %a %H:%M>")))
   "Go to"
   (("g" ale/org-diary-goto-today (format-time-string "%F")))))

(global-set-key (kbd "C-c d") 'hydra-dates/body)

(use-package calfw
  :commands open-calendar
  :config
  ;; Unicode characters
  (setq cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓)
  (defun open-calendar ()
    (interactive)
    (cfw:open-calendar-buffer
     :contents-sources
     (list
      (cfw:ical-create-source
       "Perso"
       my-private-personal-ical-url
       "RoyalBlue")
      (cfw:org-create-source
       "grey50") ; orgmode source
      (cfw:ical-create-source
       "Vacances"
       "https://www.data.gouv.fr/fr/datasets/r/17254f2a-a611-4b1f-995c-df45a4570f12"
       "goldenrod") ; ICS source1
      (cfw:ical-create-source
       "Moon"
       "http://cantonbecker.com/astronomy-calendar/astrocal.ics"
       "gray20"))))

  (use-package calfw-cal)
  (use-package calfw-ical)
  (use-package calfw-org))

(use-package text-mode
  :straight nil
  :hook (text-mode . visual-line-mode) ;; auto-wrapping (soft wrap) in text-mode
  :config
  (setq default-major-mode 'text-mode) ;; text-mode by default
  (remove-hook 'text-mode-hook #'turn-on-auto-fill)) ;; visual-line-mode instead of auto-fill)

(use-package csv-mode
  :mode ("\\.csv\\'")
  :config (setq csv-separators '("," "	" ";")))

(use-package adoc-mode
  :mode
  ("\\.asciidoc\\'" . adoc-mode)
  ("\\.adoc" . adoc-mode))

(use-package verb
  :disabled ;; worth enabling only when necessary
  :after org
  :config (define-key org-mode-map (kbd "C-c C-v") verb-command-map)
  (use-package ob-verb :straight nil :after org))

(use-package ob-shell :straight nil :after org)
(use-package htmlize :after org)
(use-package swagger-to-org :after org)
(use-package ox-asciidoc :after org)
(use-package org-tree-slide :after org)
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode))

(use-package flyspell
  :bind ("C-è" . flyspell-hydra/body)
  :commands dubcaps-mode
  :hook ((text-mode . flyspell-mode) ;; flyspell by default
         (org-mode . dubcaps-mode)) ;; auto-correct double capitals
  :pretty-hydra ((:color teal :quit-key "q")
                 ("Spelling"
                  (("t" flyspell-mode       "toggle flyspell")
                   ("T" guess-language-mode "toggle guess")
                   ("g" guess-language      "guess"))
                  "Flyspell"
                  (("c" flyspell-buffer                 "check buffer")
                   ("a" endless/ispell-word-then-abbrev "correct & add abbrev")
                   ("." flyspell-auto-correct-word      "correct word"   :color pink)
                   ("n" flyspell-goto-next-error        "next error"     :color pink)
                   ("p" flyspell-goto-previous-error    "previous error" :color pink))
                  "Dictionary"
                  (("D" ispell-change-dictionary "choose" )
                   ("A" ale/switch-to-all-dict   "all"    )
                   ("f" ale/switch-to-fr-dict    "fr (FR)")
                   ("d" ale/switch-to-de-dict    "de (DE)")
                   ("e" ale/switch-to-en-dict    "en (GB)")
                   ("u" ale/switch-to-us-dict    "en (US)"))
                  "Look up"
                  (("l" define-word-at-point                "word definition")
                   ("s" powerthesaurus-lookup-word-dwim     "synonym (DWIM)")
                   ("w" powerthesaurus-lookup-word          "synonym for word")
                   ("S" powerthesaurus-lookup-word-at-point "synonym at point"))))
  :config
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  ;; (setenv "LANG" "fr_FR")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "fr_FR,en_GB,en_US,de_DE")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "fr_FR,en_GB,en_US,de_DE")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_fr_FR.
  (setq ispell-personal-dictionary "~/.hunspell_personal")
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))

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
    "Call `ispell-word', then create a local abbrev for it.
With prefix P, create global abbrev. Otherwise it will
be local.
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
              (if p global-abbrev-table local-abbrev-table)
              bef aft)
            (message "\"%s\" now expands to \"%s\" %sally"
                     bef aft (if p "glob" "loc"))
            (write-abbrev-file abbrev-file-name))
        (user-error "No typo at or before point"))))

  (define-minor-mode dubcaps-mode
    "Toggle `dubcaps-mode'.  Converts words in DOuble CApitals to
Single Capitals as you type."
    :init-value nil
    :lighter (" DC")
    ;; more auto-correct: remove accidental double capitals
    (progn
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
      (if dubcaps-mode
          (add-hook 'post-self-insert-hook #'dcaps-to-scaps nil 'local)
        (remove-hook 'post-self-insert-hook #'dcaps-to-scaps 'local))))

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
  (define-key flyspell-mode-map (kbd "C-,") 'flyspell-goto-previous-error)

  (defun endless/flyspell-word-then-abbrev (p)
    "Call `ispell-word', then create an abbrev for it.
With prefix P, create global abbrev. Otherwise it will
be local."
    (interactive "P")
    (save-excursion
      (if (flyspell-goto-previous-word (point))
          (let ((bef (downcase (or (thing-at-point 'word)
                                   "")))
                aft)
            (call-interactively 'flyspell-auto-correct-word)
            (setq aft (downcase
                       (or (thing-at-point 'word) "")))
            (unless (or (string= aft bef)
                        (string= aft "")
                        (string= bef ""))
              (define-abbrev
                (if p global-abbrev-table local-abbrev-table)
                bef aft)
              (message "\"%s\" now expands to \"%s\" %sally"
                       bef aft (if p "glob" "loc"))))
        (message "Cannot find a misspelled word"))))

  ;; (define-key ctl-x-map "\C-i"
  ;;   #'endless/flyspell-word-then-abbrev)

  (defun flyspell-goto-previous-word (position)
    "Go to the first misspelled word that occurs before POSITION (point).
But don't look beyond what's visible on the screen."
    (interactive "d")

    (let ((top (window-start))
          (bot (window-end)))
      (save-restriction
        (narrow-to-region top bot)
        (overlay-recenter (point))

        (add-hook 'pre-command-hook
                  (function flyspell-auto-correct-previous-hook) t t)

        (unless flyspell-auto-correct-previous-pos
          ;; only reset if a new overlay exists
          (setq flyspell-auto-correct-previous-pos nil)

          (let ((overlay-list (overlays-in (point-min) position))
                (new-overlay 'dummy-value))

            ;; search for previous (new) flyspell overlay
            (while (and new-overlay
                        (or (not (flyspell-overlay-p new-overlay))
                            ;; check if its face has changed
                            (not (eq (get-char-property
                                      (overlay-start new-overlay) 'face)
                                     'flyspell-incorrect))))
              (setq new-overlay (car-safe overlay-list))
              (setq overlay-list (cdr-safe overlay-list)))

            ;; if nothing new exits new-overlay should be nil
            (if new-overlay ;; the length of the word may change so go to the start
                (setq flyspell-auto-correct-previous-pos
                      (overlay-start new-overlay)))))

        (if (not flyspell-auto-correct-previous-pos)
            nil
          (goto-char flyspell-auto-correct-previous-pos)
          t)))))

(use-package define-word :defer t)

(use-package guess-language :defer t)

(use-package caps-lock :defer t)

(use-package abbrev
  :straight nil
  :hook ((org-mode erc-mode) . abbrev-mode)
  :config
  (setq save-abbrevs 'silently)
  (when (boundp 'my-private-abbrev-file)
    (setq abbrev-file-name my-private-abbrev-file)))

(use-package powerthesaurus :defer t)

(use-package flycheck-grammalecte :defer t)

(use-package tree-sitter
  :init (global-tree-sitter-mode)
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs)

(use-package prog-mode
  :straight nil
  :hook
  (prog-mode . (lambda () (setq show-trailing-whitespace t)))
  (prog-mode . prettify-symbols-mode)
  :custom-face (trailing-whitespace ((t (:background "grey12"))))
  :config
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (defconst prettify-symbols-alist
    '(("lambda"  . ?λ)
      ("!=" . ?≠)
      ("==" . ?＝)
      ("<=" . ?⩽)
      (">=" . ?⩾)
      ("->" . ?→)
      ("<-" . ?←)
      ("=>" . ?⇒)
      ("..." . ?…)
      ("[ ]" . ?☐)
      ("[X]" . ?☑)
      ("[-]" . ?❍))))

(use-package prog-fill
  :bind (:map prog-mode-map
              ("M-q" . prog-fill)))

(use-package format-all :defer t)

(use-package reformatter
  :defer t
  :config (reformatter-define xml-format
                              :program "xmllint"
                              :args '("--format" "-")
                              :mode nil))

(use-package flycheck
  :hook
  (prog-mode . flycheck-mode)
  (lisp-data-mode . (lambda () (add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)))
  :config
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
  (defun endless/-report-error (fmt &rest args)
    "Print an error on `byte-compile-log-buffer'."
    (let ((inhibit-read-only t)
          (fill-prefix "    "))
      (with-current-buffer byte-compile-log-buffer
        (let ((l (point)))
          (insert "\n" (apply #'format fmt args))
          (fill-region (1+ l) (point))))))
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
         (recenter 1))))))

(use-package lsp-mode
  :disabled
  :defer t
  :config
  (setq lsp-prefer-flymake nil)) ;; flycheck instead of flymake

(use-package company-lsp
  :disabled
  :requires lsp company
  :after lsp-mode)

(use-package lsp-ui
  :disabled
  :requires lsp-mode flycheck
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-update-mode 'point
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))

(use-package dap-mode
  :disabled
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1))

(use-package codeium
  :defer t
  :straight '(codeium :type git :host github :repo "Exafunction/codeium.el")
  :config (setq codeium/metadata/api_key my-private-codeium-api-key))

(use-package paren
  :straight nil
  :config (show-paren-mode 1)
  :custom-face (show-paren-match ((t ( :underline "white"
                                                  :weight ultra-bold
                                                  :background unspecified
                                                  :foreground unspecified)))))



(use-package wrap-region
  :hook
  ((text-mode fundamental-mode)  . wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(("“" "”")
     ("⩽" "⩾")
     ("‘" "’")
     ("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" nil org-mode)
     ("+" "+" nil org-mode)
     ("_" "_" nil org-mode)
     ("$" "$" nil org-mode))))

(use-package emacs-surround
  :straight (:host github :repo "ganmacs/emacs-surround")
  :bind ("M-s M-z" . emacs-surround)
  :config
  (add-to-list 'emacs-surround-alist '("<" . ("<" . ">")))
  (add-to-list 'emacs-surround-alist '("“" . ("“" . "”")))
  (add-to-list 'emacs-surround-alist '("‘" . ("‘" . "’")))
  (add-to-list 'emacs-surround-alist '("⩽" . ("⩽" . "⩾")))
  (add-to-list 'emacs-surround-alist '("«" . ("«" . "»")))
  (add-to-list 'emacs-surround-alist '("»" . ("« " . " »")))
  (add-to-list 'emacs-surround-alist '("`" . ("`" . "'"))))

(use-package eat)

(use-package sh-script
  :straight nil
  :hook
  (sh-mode . flycheck-mode)
  (sh-mode . (lambda () (setq tab-width 2
                              sh-basic-offset 2
                              indent-tabs-mode nil))))

(use-package shx :defer t)
;;(autoload 'sh-mode "sh-mode" "Major mode for editing shell scripts." t)
(add-to-list 'auto-mode-alist '(".*rc$" . sh-mode))
(add-to-list 'auto-mode-alist '(".*bash.*$" . sh-mode))

(use-package fish-mode
  :mode ("\\.fish$" . fish-mode)
  :hook (fish-mode . flycheck-mode)
  :config (setq tab-width 2
                sh-basic-offset 2
                fish-indent-offset 2
                indent-tabs-mode nil))

;; Normal tab completion in Eshell
(setq eshell-cmpl-cycle-completions nil)

;; another C-d in shell kills shell buffer
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
                        (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))

(use-package shell-pop
  :bind (:map shell-mode-map ("C-c C-l" . comint-input-ring))
  :config
  (setq shell-pop-default-directory nil)
  (setq shell-pop-shell-type
        (quote ("shell" "*shell*" (lambda nil (shell shell-pop-term-shell)))))
  ;; (setq shell-pop-shell-type (quote ("vterm" "*vterm*" (lambda () (vterm shell-pop-term-shell)))))
  (setq shell-pop-term-shell "/bin/bash")
  ;;(setq shell-pop-universal-key "C-t")
  (setq shell-pop-window-size 30)
  (setq shell-pop-full-span t)
  (setq shell-pop-window-position "bottom")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package ed-mode
  :commands ed
  :straight '(ed-mode :host github
              :repo "ryanprior/ed-mode"))

(use-package powershell
  :mode ("\\.ps1\\'" . powershell-mode)
  :config (setq powershell-indent 2))

(use-package ielm
  :straight nil
  :hook (ielm-mode . (lambda () (setq-local scroll-margin 0))))

(use-package elisp-slime-nav
  :after emacs-lisp-mode
  :hook emacs-lisp-mode)

(use-package litable
  :hook (lisp-interaction-mode . litable-mode))

(use-package redshank
  :hook (lisp-data-mode . redshank-mode))

(use-package sql
  :straight nil
  :bind (:map sql-mode-map
              ("C-c C-f" . hydra-format-sql/body))
  :hook
  (sql-mode . (lambda ()
                (setq-local truncate-lines nil)
                (aggressive-indent-mode -1)
                (setq-local linesize 9999)))

  (sql-interactive-mode . (lambda ()
                            (setq-local comint-output-filter-functions 'comint-truncate-buffer)
                            (setq-local comint-buffer-maximum-size 5000)
                            (setq-local comint-scroll-show-maximum-output t)
                            (setq-local comint-input-ring-size 500)))
  :mode (("\\.sql\\'" . sql-mode)
         ("\\.pks\\'" . sql-mode)
         ("\\.pkb\\'" . sql-mode)
         ("\\.mvw\\'" . sql-mode)
         ("\\.con\\'" . sql-mode)
         ("\\.ind\\'" . sql-mode)
         ("\\.sqs\\'" . sql-mode)
         ("\\.tab\\'" . sql-mode)
         ("\\.trg\\'" . sql-mode)
         ("\\.vw\\'" .  sql-mode)
         ("\\.prc\\'" . sql-mode)
         ("\\.pk\\'" .  sql-mode)))

(use-package sqlformat :after sql)

(use-package sql-indent :after sql)

(use-package sql-upcase
  :straight nil
  :load-path "elisp/"
  :hook (sql-mode . sql-upcase-mode))

(pretty-hydra-define hydra-format-sql
  (:color blue :quit-key "q")
  ("Format"
   (("f" sqlformat "paragraph")
    ("b" sqlformat-buffer "buffer")
    ("r" sqlformat-region "region")
    ("i" sql-indent-buffer "indent"))
   "Upcase"
   (("u" sql-upcase-buffer "buffer")
    ("U" sql-upcase-region "region"))))

(use-package groovy-mode
  :mode ("\\.groovy\\'" "\\.gradle\\'")
  :config (setq groovy-indent-offset 2))

(use-package ruby-mode
  :mode "\\.rb\\'"
  :interpreter "ruby")

(use-package rust-mode
  :hook
  (rust-mode . cargo-minor-mode)
  (rust-mode . racer-mode)
  (racer-mode . company-mode))

(use-package cargo
  :after rust-mode
  :config
  (add-to-list
   'exec-path
   (expand-file-name ".cargo/bin" (if (boundp 'my-home-dir) my-home-dir "~"))))

(use-package flycheck-rust
  :after rust-mode
  :config (flycheck-rust-setup))

(use-package racer
  :after rust-mode
  :config
  (setq company-tooltip-align-annotations t)
  ;; (setq racer-rust-src-path (expand-file-name
  ;; "/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib/rustlib/x86_64-unknown-linux-gnu/lib"
  ;; my-home-dir))
  (local-set-key (kbd "TAB") #'company-indent-or-complete-common))

(use-package flycheck-java ;; flycheck minor mode for java
  :disabled
  :straight '(flycheck-java
              :host github
              :repo "akorobov/flycheck-java")
  :hook java-mode)

(use-package malabar-mode
  :disabled
  :hook
  (after-init . (lambda () (message "activate-malabar-mode") (activate-malabar-mode)))
  (malabar-java-mode . flycheck-mode)
  (malabar-groovy-mode . flycheck-mode)
  (malabar-mode . (lambda () (progn
                               (add-hook 'after-save-hook 'malabar-http-compile-file-silently nil t)
                               (add-hook 'after-save-hook 'malabar-compile-file-silently nil t))))
  :config
  ;; JAVA (malabar-mode)
  ;; mimic the IDEish compile-on-save behaviour
  ;; (load-file "~/outils/cedet/cedet-devel-load.el")
  (load-file "~/projets/malabar-mode/src/main/lisp/malabar-mode.el")
  (load-file "~/projets/cedet/cedet-devel-load.el"))

(use-package eclim
  :disabled
  :hook
  (java-mode . (lambda () (setq flycheck-java-ecj-jar-path "~/outils/java/ecj-4.5.jar")))
  (java-mode . eclim-mode)
  :config
  (global-eclim-mode)
  (require 'eclimd)
  (setq eclim-eclipse-dirs "~/outils/eclipse/eclipse-mars"
        eclim-executable "~/outils/eclipse/eclipse-mars/eclim")
  ;; (require 'company)
  (require 'company-emacs-eclim)
  (global-company-mode t)
  ;; (company-emacs-eclim-ignore-case t)
  (company-emacs-eclim-setup))

(use-package ecb :disabled
  :config  (setq ecb-options-version "2.40"))

(use-package lsp-java
  :disabled
  :bind ("C-S-o" . lsp-execute-code-action)
  :after lsp
  :hook (java-mode . lsp))

(use-package dap-java
  :disabled
  :straight nil
  :after lsp-java)

(use-package lsp-java-treemacs
  :disabled
  :straight nil
  :after treemacs)

(use-package autodisass-java-bytecode
  :disabled)

(use-package google-c-style
  :disabled
  :commands google-set-c-style)

(use-package meghanada
  :disabled
  :commands
  (meghanada-mode)
  :bind
  (:map meghanada-mode-map
        ("M-m" . hydra-meghanada/body))
  :hook
  (java-mode . (lambda ()
                 ;; meghanada-mode on
                 (meghanada-mode t)
                 (flycheck-mode +1)
                 (setq c-basic-offset 4)))
  (before-save . meghanada-code-beautify-before-save)
  :config
  (use-package realgud ;; I don't even know what this package is or does
    :disabled)
  (setq indent-tabs-mode nil
        tab-width 4
        c-basic-offset 4
        meghanada-server-remote-debug t
        meghanada-javac-xlint "-Xlint:all,-processing"
        meghanada-java-path "java"
        meghanada-maven-path "mvn")
  :pretty-hydra
  ((:color teal :quit-key "q")
   ("Meghanada"
    (("M-m" meghanada-restart "restart")
     ("." meghanada-reference "reference")
     ("h" meghanada-typeinfo "type info"))
    "Compile"
    (("f" meghanada-compile-file "file")
     ("c" meghanada-compile-project "project"))
    "Edit"
    (("o" meghanada-optimize-import "organise imports")
     ("i" meghanada-import-all "import all")
     ("v" meghanada-local-variable))
    "Run & test"
    (("R" meghanada-run-task "run task")
     ("t" meghanada-run-junit-test-case "run JUnit test case")
     ("T" meghanada-run-junit-class "run JUnit class")
     ("r" meghanada-run-junit-recent "run JUnit recent")
     ("s" meghanada-switch-test-case "switch test case")))))

(use-package js2-mode
  ;; :bind (:js2-mode-map ("C-c C-c" . compile))
  :mode ("\\.js\\'\\|\\.json\\'" . js2-mode)
  :hook ((json-mode . json-pretty-print)
         (js2-mode . json-pretty-print-buffer)
         (js2-mode . aggressive-indent-mode)
         (js2-mode . js2-refactor-mode)
         (js2-mode . (lambda () (flycheck-mode t))))
  :config
  (use-package web-beautify
    :bind (:map js2-mode-map ("C-c C-f" . web-beautify-js)))
  (setq js2-basic-offset 2
        js-indent-level 2
        js2-use-font-lock-faces t)
  (autoload 'json-pretty-print "json-pretty-print" "json-pretty-print" t))

(use-package js2-refactor
  :after js2-mode
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (setq js2-skip-preprocessor-directives t))
;; à tester
(use-package js-comint
  :disabled
  :hook
  (inferior-js-mode . (lambda ()
                        (add-hook 'comint-output-filter-functions 'js-comint-process-output)))
  (js2-mode . (lambda ()
                (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
                (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
                (local-set-key (kbd "C-c b") 'js-send-buffer)
                (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
                (local-set-key (kbd "C-c l") 'js-load-file-and-go))))

(use-package json-mode
  :mode ("\\.json\\'"
         "\\.rasi\\'")
  :config
  (setq js-indent-level 2)
  (use-package json-reformat
    :bind (:map json-mode-map ("C-c C-f" . ale/json-reformat-region-or-buffer))
    :config (setq json-reformat:indent-width js-indent-level)))

(use-package web-mode ;; HTML, XML, JSP (using web-mode)
  :mode ("\\.phtml\\'"
         "\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.js\\'"
         "\\.jsx\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.rhtml\\'"
         "\\.htm\\'"
         "\\.html\\'"
         "\\.tag\\'"
         "\\.tmpl\\'"
         "\\.tsx\\'"
         "\\.vue\\'"
         "\\.xml\\'"
         "\\.xsd\\'"
         "\\.wsdl\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-quoting t
        web-mode-engines-alist '(("php" . "\\.phtml\\'")
                                 ("blade" . "\\.blade\\."))))

(defun mu-xml-format ()
  "Format an XML buffer with `xmllint'."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "xmllint -format -"
                           (current-buffer) t
                           "*Xmllint Error Buffer*" t))

(use-package emmet-mode
  :hook web-mode)

(use-package impatient-mode
  :commands impatient-mode)

(use-package simple-httpd
  :config
  (setq httpd-port 7070)
  (setq httpd-host (system-name)))

(defun my-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

(use-package tide
  :disabled
  :chords (:map tide-mode-map
                ("+-" . bury-buffer))
  :bind (:map tide-mode-map
              ("C-c k" . bury-buffer)
              ("C-." . tide-jump-to-definition)
              ("C-," . tide-jump-back)
              ("C-c C-c" . hydra-tide/body))
  :hook
  ;; formats the buffer before saving
  (before-save . tide-format-before-save)
  (typescript-mode . setup-tide-mode)
  (js2-mode . setup-tide-mode)
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    ;; flycheck-typescript-tslint-executable "tslint"
    ;; (eldoc-mode +1)
    (company-mode +1))

  (setq company-tooltip-align-annotations t  ;; aligns annotation to the right hand side
        typescript-indent-level 2
        ;; format options
        tide-format-options '(
                              :insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                              :placeOpenBraceOnNewLineForFunctions nil))

  ;; (setq tide-tsserver-process-environment
  ;; '("TSS_LOG=-level verbose -file ~/projets/tss.log"))
  :pretty-hydra
  ((:color blue :quit-key "q")
   ("Tide"
    (("s" tide-restart-server "restart server"))
    "Edit"
    (("r" tide-rename-symbol "rename")
     ("f" tide-format "format"))
    "Navigate"
    (("e" tide-project-errors "errors")
     ("g" tide-references "references")))))

(use-package typescript-mode
  :disabled
  :mode ("\\.ts\\'"
         "\\.json\\'"))

(use-package sass-mode
  :mode ("\\.sass$" . sass-mode))

(use-package python
  :bind (:map python-mode-map
              ("M-g M-p" . flycheck-previous-error)
              ("M-g M-n" . flycheck-next-error)
              ("C-x C-e" . python-shell-send-defun))
  :hook (python-mode . (lambda ()
                         (progn (aggressive-indent-mode -1)
                                ;; (elpy-enable)
                                ;; (elpy-mode)
                                (flycheck-mode))))
  :config
  (setq python-indent-offset 4
        tab-always-indent t))

(use-package jedi
  :after python
  :config (setq jedi:complete-on-dot t))

(use-package elpy
  :after python
  :config
  (delete "elpy-module-flymake" elpy-modules) ;; use flycheck instead
  (setq elpy-rpc-backend "jedi")) ;; fire up jedi in python env

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(use-package crontab-mode
  :mode ("crontab\\'" . crontab-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-open-command "grip"
              markdown-command "markdown")
  :config (setq markdown-command "pandoc -t html5"))

(use-package grip-mode :defer t)

(use-package dokuwiki-mode :defer t)
(use-package dokuwiki
  :hook dokuwiki-mode)

(use-package tex
  :mode ("\\.tex\\'")
  :straight auctex
  :config
  (use-package auctex-latexmk
    :config
    (auctex-latexmk-setup)
    (TeX-global-PDF-mode t)))

(use-package gradle-mode
  :hook groovy-mode)

(use-package docker :defer t)
(use-package dockerfile-mode
  :mode "\\DockerFile\\'")

(use-package syslog-mode :defer t)

(use-package sendmail
  :straight nil
  :hook (mail-mode . visual-line-mode)) ;; wrapping in mail-mode

(use-package mu4e
  :load-path my-private-mu4e-path
  :straight nil
  :commands mu4e
  :custom-face
  (mu4e-flagged-face ((t (:foreground "gold"))))
  (mu4e-header-highlight-face ((t (;; :weight bold
                                   :background "black"
                                   ;; :foreground "red"
                                   ;; :box ( :line-width 1
                                   ;;        :color "grey10"
                                   ;; :style released-button
                                   ))))
  :bind ( :map mu4e-view-mode-map
               ("<down>" . down-arrow)
               ("<up>" . up-arrow)
               ("{" . mu4e-view-headers-prev-unread)
               ("}" . mu4e-view-headers-next-unread)
               :map mu4e-headers-mode-map
               ("o" . mu4e-headers-change-sorting)
               ("{" . mu4e-headers-prev-unread)
               ("}" . mu4e-headers-next-unread)
               :map mu4e-main-mode-map
               ("," . mu4e-context-switch)
               ("u" . mu4e-update-mail-and-index)
               ("U" . set-update-rate)
               ("p" . previous-line)
               ("n" . next-line)
               ("Q" . mu4e-quit)
               ("q" . bury-buffer)
               ("x" . mu4e-kill-update-mail))
  :hook
  (mu4e-view-mode . visual-line-mode)
  (mu4e-compose-mode-hook . ale/switch-to-fr-dict)
  :config
  (defun set-update-rate (arg)
    "Prompt for an update rate ARG in minutes."
    (interactive "NUpdate rate in minutes: ")
    (let ((rate (prefix-numeric-value arg)))
      (setq mu4e-update-interval (* 60 rate))
      (message "Will check mail every %s minutes (after mu4e restarts)." rate)))

  (require 'org-mu4e)
  ;;store link to message if in header view, not to header query
  (setq org-mu4e-link-query-in-headers-mode nil)

  (setq mu4e-headers-unread-mark    '("u" . "📩 ")
        mu4e-headers-draft-mark     '("D" . "🚧 ")
        mu4e-headers-flagged-mark   '("F" . "🚩 ")
        mu4e-headers-new-mark       '("N" . "✨ ")
        mu4e-headers-passed-mark    '("P" . "↪ ")
        mu4e-headers-replied-mark   '("R" . "↩ ")
        mu4e-headers-seen-mark      '("S" . " ")
        mu4e-headers-trashed-mark   '("T" . "🗑️")
        mu4e-headers-attach-mark    '("a" . "📎 ")
        mu4e-headers-encrypted-mark '("x" . "🔑 ")
        mu4e-headers-signed-mark    '("s" . "🖊 "))

  (setq mu4e-maildir (expand-file-name "~/Maildir/ProtonMail")
        mu4e-attachment-dir            "~/Téléchargements"
        mu4e-drafts-folder             "/Drafts"
        mu4e-sent-folder               "/Sent"
        mu4e-trash-folder              "/Trash"
        mu4e-refile-folder             "/Archive"

        message-kill-buffer-on-exit t

        ;; Send mail
        message-send-mail-function  'smtpmail-send-it
        smtpmail-auth-credentials   my-private-auth-file
        smtpmail-smtp-server        my-private-smtp-server
        smtpmail-smtp-service       my-private-smtp-port
        mu4e-sent-messages-behavior 'sent)

  ;; signing & encrypting
  (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)
  (setq mm-sign-option nil) ;; use default key

  ;; setup some handy shortcuts
  (setq mu4e-maildir-shortcuts
        '(("/INBOX"   . ?i)
          ("/starred" . ?s)
          ("/Drafts"  . ?d)
          ("/Sent"    . ?t)
          ("/Spam"    . ?m)
          ("/Archive" . ?a)
          ("/Gmail"   . ?g))

        ;; allow for updating mail using 'U' in the main view:
        mu4e-get-mail-command             "mbsync --all --quiet"
        mu4e-change-filenames-when-moving t ;; needed for mbsync
        mu4e-update-interval              (* 5 60) ;; update every 5 minutes
        mu4e-index-cleanup                t ;; don't do a full cleanup check
        mu4e-index-lazy-check             t ;; don't consider up-to-date dirs
        mu4e-index-update-in-background   t ;; update in the background
        mu4e-use-fancy-chars              t ;; unicode characters mess up thread drawings
        mu4e-view-show-images             t
        mu4e-headers-results-limit        200
        user-mail-address                 my-private-mail-address
        user-full-name                    my-private-full-name
        message-signature                 my-private-short-name
        mu4e-compose-signature            message-signature)

  ;; Various options for email writing
  (setq mu4e-compose-dont-reply-to-self t
        mu4e-compose-in-new-frame nil
        mu4e-compose-format-flowed nil)

  ;; Use imagemagick, if available.
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Sometimes html email is just not readable in a text based client, this lets me open the
  ;; email in my browser.
  (setq mu4e-view-actions
        '(("capture message"  . mu4e-action-capture-message)
          ("view in browser"  . mu4e-action-view-in-browser)
          ("show this thread" . mu4e-action-show-thread)))

  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)

  ;; bookmarks  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq mu4e-bookmarks
        '((:name "Nouveaux"         :query (concat "date:30d.."
                                                   " AND NOT %work"
                                                   " AND %unread")                  :key ?n)
          (:name "Non lus"          :query "%unread"                                :key ?u)
          (:name "Perso"            :query (concat "%unread"
                                                   " AND NOT %work")                :key ?p)
          (:name "Enercoop"         :query (concat " date:1w.."
                                                   " AND %work")                    :key ?e)
          (:name "Importants"       :query "flag:flagged"                           :key ?i)
          (:name "Récents"          :query "date:5h.. AND NOT %work"                :key ?r)
          (:name "Aujourd’hui"      :query "date:today.. AND NOT %work"             :key ?t)
          (:name "Hier"             :query "date:1d.. AND NOT %work"                :key ?h)
          (:name "Dernière semaine" :query "date:7d.. AND NOT %work" :hide-unread t :key ?w)
          (:name "Dernier mois"     :query "date:1m.. AND NOT %work" :hide-unread t :key ?m)
          (:name "Work"             :query (concat "%work"
                                                   " AND flag:unread")              :key ?e)
          ;; (:name "Avec images"   :query "mime:image/*"                :key ?i)
          ))

  ;; optimize header column sizes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq mu4e-headers-fields
        '((:human-date     . 11)
          (:flags          . 9)
          (:mailing-list   . 20)
          (:from-or-to     . 25)
          (:thread-subject . nil))))

(use-package mu4e-marker-icons
  :disabled
  :config (mu4e-marker-icons-mode 1))

(use-package mu4e-jump-to-list :after mu4e)

(use-package mu4e-query-fragments
  :after mu4e
  :config
  (setq mu4e-query-fragments-list
        '(("%unread"  . "flag:unread")
          ("%today"    . "date:today..")
          ("%junk"    . "maildir:/Junk OR subject:SPAM")
          ("%hidden"  . "flag:trashed OR %junk")
          ("%gmail"   . "maildir:/Gmail")
          ("%autos"   . "from:easter-eggs.com OR to:enercoop.org OR list:\"Coopener.enercoop.github.com\"")
          ("%encp-fw" . "maildir:/Folders/Enercoop-forward")
          ("%encp-lb" . "maildir:/Labels/Enercoop")
          ("%work"    . "%encp-fw OR %encp-lb OR %autos"))
        mu4e-query-fragments-append "AND NOT %hidden"))

(use-package mu4e-alert
  :after mu4e
  :config
  (setq mu4e-alert-email-notification-types '(subjects)
        mu4e-alert-interesting-mail-query (concat "flag:unread" " AND maildir:/INBOX"))
  (mu4e-alert-enable-mode-line-display)
  (mu4e-alert-set-default-style 'libnotify)
  (mu4e-alert-enable-notifications))

(use-package mu4e-maildirs-extension
  :disabled
  :after mu4e
  :config (mu4e-maildirs-extension))

(use-package mu4e-conversation
  :disabled
  :after mu4e
  :config (global-mu4e-conversation-mode))

(use-package mu4e-goodies
  :disabled ;TODO: à configurer
  :straight '(mu4e-goodies :host github :repo "panjie/mu4e-goodies")
  :after mu4e)

(use-package smtpmail
  :straight nil
  :defer t
  :config (setq message-send-mail-function 'smtpmail-send-it
                starttls-use-gnutls t
                smtpmail-starttls-credentials '((my-private-smtp-server my-private-smtp-port nil nil))
                smtpmail-auth-credentials (expand-file-name my-private-auth-file)
                smtpmail-default-smtp-server my-private-smtp-server
                smtpmail-smtp-server my-private-smtp-server
                smtpmail-smtp-service my-private-smtp-port
                smtpmail-debug-info t))

(use-package eww
  :defer t
  :config
  (when (boundp 'my-web-browser-program)
    (setq browse-url-generic-program my-web-browser-program)) ;; set default browser to eww
  (when (boundp 'my-web-browser-function)
    (setq browse-url-browser-function my-web-browser-function))
  (setq shr-external-browser 'browse-url-firefox ;; use ‘&’ to open page in firefox
        shr-width 100 ;; keep sane layout
        shr-use-fonts t ;; no fancy fonts in text mode
        gnus-button-url 'browse-url-generic))

(use-package wiki-summary :defer t)


(use-package register
  :straight nil
  :chords ("jj" . jump-to-register))

(use-package hide-mode-line :defer t)

(use-package mlscroll
  :ensure t
  :hook (server-after-make-frame . mlscroll-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (when (boundp 'my-private-dashboard-banner-png)
    (setq dashboard-startup-banner my-private-dashboard-banner-png))
  (when (boundp 'my-private-dashboard-title)
    (setq dashboard-banner-logo-title my-private-dashboard-title))
  (setq dashboard-center-content t)
  (dashboard-modify-heading-icons '((recents . "file-text")
                                    (projects . "book")
                                    (bookmarks . "book")))
  (setq dashboard-items '((recents . 8)
                          (bookmarks . 5)
                          ;; (registers . 5)
                          ;; (agenda . 10)
                          (projects . 5)))
  (setq show-week-agenda-p nil)
  (when (boundp 'my-private-footer-messages)
    (nconc dashboard-footer-messages my-private-footer-messages)))

;; auto-save file-visiting buffers
(use-package super-save
  :disabled
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t
        auto-save-default nil))

;; Save a list of recent files visited.
(use-package desktop
  :disabled
  :defer t
  :config
  (setq desktop-save t
        ;; desktop-base-lock-name      "lock"
        ;; desktop-dirname             base-emacs-directory
        ;; desktop-path                (list desktop-dirname)
        ;; desktop-files-not-to-save   "^$" ;reload tramp paths
        ;; desktop-load-locked-desktop t
        ;; (add-to-list 'desktop-modes-not-to-save 'dired-mode)
        )
  (desktop-save-mode 1)
  (desktop-read))

(use-package edit-server
  :if (and
       (display-graphic-p)
       (or
        (not (fboundp 'server-running-p))
        (not (server-running-p))))
  :bind ("M-#" . server-edit)) ;; send back to server, quicker than C-x #

(use-package emacs-everywhere :disabled)

(use-package jenkinsfile-mode
  :mode ("\\.jenkinsfile$"))


(use-package command-log-mode
  :commands clm/toggle-command-log-buffer
  :init (setq command-log-mode-key-binding-open-log "C-c c")) ;; "C-c o" already used for bury-buffer

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode)
         ("\\.restclient\\'" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c n w" . widen)
              ("C-c C-f" . json-mode-beautify)))

;; (use-package ob-restclient :defer t)

(use-package impostman
  :defer t
  :straight '(impostman :host github :repo "flashcode/impostman"))

(use-package spray
  :disabled
  :bind (:map spray-mode-map
              ("-" . spray-slower)
              ("+" . spray-faster)
              ("<SPC>" . spray-start/stop)
              ("b" . spray-backward-word)
              ("p" . spray-backward-word)
              ("f" . spray-forward-word)
              ("n" . spray-forward-word)))

(use-package ereader
  :mode ("\\.epub\\'" . ereader-mode))

(use-package lorem-ipsum :defer t)

(use-package ten-hundred-mode :defer t)

(use-package darkroom :defer t)

(use-package speed-type :defer t)

(use-package monkeytype :defer t)

(use-package 2048-game :defer t)

(use-package vimgolf :commands vimgolf)



(add-hook 'emacs-startup-hook
          (lambda ()
            (display-battery-mode)
            (when (string-equal "darwin" system-type)
              (load-file
               (expand-file-name "elisp/macos-environment.el" base-emacs-directory)))
            (when (or (string-equal "ms-dos" system-type)
                      (string-equal "windows-nt" system-type)
                      (string-equal "cygwin" system-type))
              (load-file
               (expand-file-name "elisp/windows-environment.el" base-emacs-directory)))
            (cond
             ((daemonp)
              (progn
                (when (fboundp 'beacon-mode) (beacon-mode -1))
                (when (boundp 'my-private-frame-font-size)
                  (set-face-attribute 'default nil :height my-private-frame-font-size)
                  (set-fira-font-if-possible))))
             (t
              (progn
                (when (fboundp 'beacon-mode) (beacon-mode))
                (mouse-wheel-mode t))))))
