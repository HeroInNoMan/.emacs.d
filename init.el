;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    emacs config file                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALLATION & LOADING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package-style dependencies
(when (>= emacs-major-version 24)
  (require 'package)
  ;; (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t) ;; default
  ;; (add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)
(setq use-package-verbose t)

(require 'diminish) ;; for :diminish
(require 'bind-key) ;; for :bind

(use-package use-package-chords
  :config (key-chord-mode 1))

;;;;;;;;;;;;;;;;;;
;; LIBS & TOOLS ;;
;;;;;;;;;;;;;;;;;;

(use-package s)

(use-package my-functions ;; custom functions
  :ensure nil
  :load-path "elisp/"
  :bind
  ("C-c i" . iwb) ;; indent whole buffer
  ("M-«" . simplified-beginning-of-buffer) ;; useful when C-< does not work (windows/putty)
  ("M-»" . simplified-end-of-buffer)
  ("<C-M-down>" . duplicate-current-line)
  ("<up>" . up-arrow)
  ("<down>" . down-arrow)
  ("<f5>" . reload-file) ;; re-read file from disk
  ("C-<f5>" . copy-and-show-current-file-path) ;; copy current file path
  ("M-<f5>" . show-file-name) ;; show the file name in minibuffer
  ("C-x C-r" . sudo-edit) ;; sudo open file
  ("C-x |" . toggle-window-split)
  ("C-|" . toggle-window-split))

(use-package my-checks :ensure nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS, ERGONOMY & KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package better-defaults)

(global-set-key (kbd "C-S-b") 'bookmark-set) ;; easier eclipse-style bookmark setting

;; Killing emacs
(global-unset-key (kbd "C-x C-c")) ;; too easy to hit by accident, use “M-x kill-emacs” instead
(global-set-key (kbd "C-x r q") 'kill-emacs) ;; r·eally q·uit

(unless (display-graphic-p)
  (global-unset-key (kbd "C-z")) ;; used for tmux (C-z z for suspend-frame)
  (global-set-key (kbd "C-z z") 'suspend-frame)) ;; C-z is saved for tmux

(use-package god-mode
  :bind
  (("C-c g" . god-mode-all)
   ("<f8>" . god-mode-all)
   :map god-local-mode-map
   ("z" . repeat)
   ("i" . god-mode-all)
   ("." . repeat))
  :config
  (defun my-update-cursor ()
    (cond
     (god-local-mode
      (progn
        (set-cursor-color "red")
        (set-face-background 'mode-line "brown")))
     (t
      (progn
        (set-cursor-color "yellow")
        (set-face-background 'mode-line "#0a2832")))))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)
  (add-hook 'window-configuration-change-hook 'my-update-cursor)
  (add-to-list 'god-exempt-major-modes 'ibuffer-mode))

;; Answer questions with y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; UTF-8 everywhere
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; Lines should be 80 characters wide, not 70
(setq-default fill-column 80)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; use count-words instead of count-words-region as it works on buffer
;; if no region is selected
(global-set-key (kbd "M-=") 'count-words)

;; enable commands disabled by default
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'scroll-left 'disabled nil)

;; set default browser to firefox
(setq gnus-button-url 'browse-url-generic
      browse-url-generic-program "firefox"
      browse-url-browser-function gnus-button-url)

;;;;;;;;;;;;;;;;;;;;;;;;
;; THEME & APPEARANCE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package color-theme
  :config
  (color-theme-initialize)
  (color-theme-dark-laptop)
  (set-face-background 'mode-line "#0a2832"))

(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :config (add-hook 'css-mode-hook 'rainbow-mode))

(use-package volatile-highlights
  :config (volatile-highlights-mode t))

(use-package idle-highlight-mode)

(use-package highlight-line ;; highlight line in list buffers
  :ensure nil
  :load-path "elisp/")

(use-package zoom-frm
  :if (display-graphic-p)
  :bind
  ("C-+" . zoom-frm-in)
  ("C-=" . zoom-frm-unzoom))

(use-package sublimity
  :disabled t
  :config
  (require 'sublimity-scroll)
  (require 'sublimity-map))

;; colors, appearance
(require 'iso-transl) ;; some environments don’t handle dead keys
(global-font-lock-mode t) ;; enable syntax highlighting
(setq font-lock-maximum-decoration t) ;; all possible colours
(blink-cursor-mode -1) ;; no blinking cursor
(global-hl-line-mode -1) ;; don’t highlight current line
(highlight-line-mode 1) ;; except in “list” modes

;; progress in file
(use-package sml-modeline
  :config (sml-modeline-mode))

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; No splash screen
(setq inhibit-startup-screen t)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Show active region
(setq-default transient-mark-mode t)

;; No *scratch* message
(setq initial-scratch-message nil)

;; Always display line and column numbers
(setq line-number-mode t
      column-number-mode t)

;; don’t display linum except while goto-line
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; No flashing!
(setq visible-bell nil)

;; Don't soft-break lines for me, please
(setq-default truncate-lines t)

;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;

(use-package dired+
  :config (unbind-key "M-b" dired-mode-map))

(use-package dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-AlhGF") ;; dired human readable size format, hide group

;; Auto refresh dired
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      ;; always delete and copy recursively
      dired-recursive-deletes 'always
      dired-recursive-copies 'always)

;;;;;;;;;;;;
;; SEARCH ;;
;;;;;;;;;;;;

(use-package swiper)

(use-package swiper-helm
  :bind ("C-S-s" . swiper-helm))

(use-package smartscan
  :bind
  ("M-n". smartscan-symbol-go-forward) ;; find next occurence of word at point
  ("M-p". smartscan-symbol-go-backward) ;; find previous occurence of word at point
  ("M-'". smartscan-symbol-replace)) ;; replace all occurences of word at point

;; regexp-builder
(require 're-builder)
(setq reb-re-syntax 'string) ;; syntax used in the re-buidler

(use-package visual-regexp-steroids
  :bind
  (("M-s r" . vr/replace)
   ("M-s q" . vr/query-replace)
   ;; if you use multiple-cursors, this is for you:
   ("M-s m" . vr/mc-mark)
   ;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
   ("C-M-r" . vr/isearch-backward)
   ("C-M-s" . vr/isearch-forward))
  :config
  (require 'visual-regexp))
;; if the files are not already in the load path
;; (add-to-list 'load-path "folder-to/visual-regexp/")
;; (add-to-list 'load-path "folder-to/visual-regexp-steroids/")

;;;;;;;;;;;;;;;;
;; NAVIGATION ;;
;;;;;;;;;;;;;;;;

;; quickly switch to other window
(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package avy
  :bind
  ("M-à" . avy-goto-word-1) ;; quickly jump to word by pressing its first letter
  ("C-à" . avy-goto-char-timer)) ;; quickly jump to any char in word

(use-package imenu-anywhere
  :bind ("C-." . helm-imenu-anywhere))

(use-package dumb-jump
  :bind
  ("C-M-g" . dumb-jump-go)
  ("C-M-S-g" . dumb-jump-back)
  ("C-M-q" . dumb-jump-quick-look))

(use-package minimap
  :bind ("<f2>" . minimap-toggle))

;; Allow scrolling with mouse wheel
(when (display-graphic-p) (mouse-wheel-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INDENTATION, TABS, SPACES & FOLDING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

(use-package aggressive-indent
  :config (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;;Indentation
(setq-default tab-width 4
              c-auto-newline t
              c-basic-offset 4
              c-block-comment-prefix ""
              c-default-style "k&r"
              indent-tabs-mode nil ;; <tab> inserts spaces, not tabs and spaces
              sentence-end-double-space nil) ;; Sentences end with a single space

;; use tab to auto-comlete if indentation is right
(setq tab-always-indent 'complete)

(use-package shrink-whitespace
  :bind ("C-x C-o" . shrink-whitespace))

(use-package origami
  :bind ("C-%" . origami-recursively-toggle-node)
  :config (add-hook 'prog-mode-hook #'origami-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION & HELP ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package char-menu
  :bind ("<f7>" . char-menu)
  :config
  (setq char-menu '(("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                    ("Math"       "≈" "≡" "∞" "√" "∀" "∃")
                    ("cyrillic"   "а" "б" "в" "г" "д" "е" "ж" "з" "и" "й" "к" "л" "м" "н" "о" "п" "р" "с")
                    ("Smileys"    "☺" "☹")
                    ("Arrows"     "←" "→" "↑" "↓" "↔" "↕" "⇔" "⇐" "⇒"))))

(use-package company
  :diminish company-mode
  :config
  (global-company-mode) ;; enable company in all buffers
  (setq company-show-numbers t)
  (add-hook 'markdown-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode))

(use-package undo-tree ;; powerfull undo/redo mode
  :diminish undo-tree-mode
  :config (undo-tree-mode t))

(use-package which-key ;; which-key (replacement for guide-key)
  :config (which-key-mode))

(use-package helm
  :diminish helm-mode
  :bind
  ("M-x" . helm-M-x) ;; superior to M-x
  ("M-y" . helm-show-kill-ring)
  ("C-h v" . helm-apropos)
  ("C-h f" . helm-apropos)
  ("C-h a" . helm-apropos)
  ("C-x w" . helm-wikipedia-suggest) ;; quick wp lookup
  ("C-c h c" . helm-org-capture-templates)
  ("C-c h p" . helm-list-elisp-packages-no-fetch)
  ("C-c h P" . helm-apt)
  ("C-c h m" . helm-man-woman)
  ("C-c h o" . helm-occur)
  ("C-c h r" . helm-resume)
  ("C-c h s" . helm-google-suggest)
  ("C-c h t" . helm-top)
  ("C-c h w" . helm-wikipedia-suggest)
  ("C-c h x" . helm-run-external-command)
  ("C-c h g" . helm-do-ag)
  ("M-ç" . helm-for-files)
  ("C-ç" . helm-for-files)
  :chords (("bf" . helm-for-files) ;; helm-for-file looks everywhere, no need for anything else
           ("éè" . helm-do-ag-project-root)) ;; incremental grep in project
  :config
  ;; activate additional features
  (helm-mode 0) ;; helm-mode only on demand
  (helm-autoresize-mode t)
  (setq helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-ag-insert-at-point 'symbol
        helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  (setq
   helm-for-files-preferred-list '(helm-source-buffers-list
                                   helm-source-recentf
                                   helm-source-projectile-files-list
                                   helm-source-bookmarks
                                   helm-source-file-cache
                                   helm-source-files-in-current-dir
                                   helm-source-locate)))

(use-package helm-descbinds
  :bind ("C-h b" . helm-descbinds))

(use-package wgrep
  :bind (:map grep-mode-map
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(use-package helm-ag
  :bind (:map helm-ag-mode-map
              ("p" . previous-line)
              ("n" . next-line)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(use-package helm-mode-manager
  :bind
  ("C-c m" . helm-switch-major-mode)
  ("C-c n" . helm-enable-minor-mode)
  ("C-c d" . helm-disable-minor-mode))

(use-package hydra
  :config
  (defvar whitespace-mode nil)
  (defvar idle-highlight-mode nil)
  (defvar global-linum-mode nil)
  (defvar god-local-mode nil)
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

  (global-set-key (kbd "<f6>") 'hydra-arabic/body))

(use-package yasnippet
  :bind (:map yas-minor-mode-map ("<C-tab>" . yas-ido-expand))
  :config
  (yas-global-mode 1)
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
          (yas-expand))))))

;; case-insensitive policy
(setq read-file-name-completion-ignore-case t ;; case-insensitive completion
      read-buffer-completion-ignore-case t) ;; case-insensitive completion

;;;;;;;;;;;;;;;;;;;;;;;
;; TEXT MANIPULATION ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package expand-region
  :bind ("C-c e" . er/expand-region))

(use-package move-text
  :config (move-text-default-bindings)) ;; M-up / M-down to move line or region

(use-package zop-to-char
  :bind ("C-M-z" . zop-up-to-char))

(use-package region-bindings-mode
  :config (region-bindings-mode-enable))

(use-package multiple-cursors
  ;; Multiple cursors keybindings
  :bind
  (("M-é" . set-rectangular-region-anchor)
   :map region-bindings-mode-map
   ("a" . mc/mark-all-like-this) ;; new cursor on each occurence of current region
   ("d" . mc/mark-all-symbols-like-this-in-defun)
   ("D" . mc/mark-all-dwim)
   ("p" . mc/mark-previous-like-this) ;; new cursor on previous occurence of current region
   ("n" . mc/mark-next-like-this) ;; new cursor on next occurence of current region
   ("P" . mc/unmark-previous-like-this)
   ("N" . mc/unmark-next-like-this)
   ("é" . mc/edit-lines) ;; new cursor on each line of region
   ("(" . mc/cycle-backward)
   (")" . mc/cycle-forward)
   ("m" . mc/mark-more-like-this-extended)
   ("h" . mc-hide-unmatched-lines-mode)
   ("v" . mc/vertical-align)
   ("|" . mc/vertical-align-with-space)
   ("r" . mc/reverse-regions)
   ("s" . mc/sort-regions)
   ("#" . mc/insert-numbers) ; use num prefix to set the starting number
   ("^" . mc/edit-beginnings-of-lines)
   ("$" . mc/edit-ends-of-lines)
   ("<down>" . move-text-down)
   ("<up>" . move-text-up)))

(use-package smart-comment
  :bind ("M-;" . smart-comment))

;; Remove text in active region if inserting text
(pending-delete-mode t)

;; join lines below onto current line
(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BUFFER & WINDOW MANIPULATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package multi-scratch ;; scratch
  :ensure nil
  :load-path "elisp"
  :bind
  ("C-x \"" . multi-scratch-new) ;; create new scratch buffer named “new<#>”
  ("M-\"" . multi-scratch-new) ;; create new scratch buffer named “new<#>”
  ("C-x «" . multi-scratch-prev) ;; jump to previous scratch buffer
  ("C-x »" . multi-scratch-next) ;; jump to next scratch buffer
  :config (setq multi-scratch-buffer-name "new"))

;; better access to window manipulation commands
(global-set-key (kbd "C-\"") 'delete-other-windows)
(global-set-key (kbd "C-«") 'split-window-below)
(global-set-key (kbd "C-»") 'split-window-right)
(global-set-key (kbd "C-*") 'delete-window)

;; buffer & file handling
(key-chord-define-global (kbd "«»") 'ibuffer) ;; call ibuffer
(global-set-key (kbd "C-x C-b") 'electric-buffer-list) ;; electric buffer by default
(global-set-key (kbd "C-c o") 'bury-buffer) ;; put buffer at bottom of buffer list
(global-set-key (kbd "C-c k") 'kill-this-buffer) ;; kill buffer without confirmation
(key-chord-define-global (kbd "+-") 'kill-this-buffer) ;; kill buffer without confirmation

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 30 30 :left :elide) " "
              (size 9 -1 :right) " "
              (mode 16 16 :left :elide) " " filename-and-process)
        (mark " " (name 16 -1) " " filename)))

;;revert windows on ediff exit - needs winner mode
(use-package winner
  :config
  (winner-mode)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

;; Change how buffer names are made unique
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; Auto refresh buffers
(global-auto-revert-mode 1)

;;;;;;;;;;;;;;;;
;; GIT AND VC ;;
;;;;;;;;;;;;;;;;

(use-package git-timemachine
  :bind ("C-x g t" . git-timemachine))

(use-package git-messenger
  :bind
  (("C-x g g" . git-messenger:popup-message)
   :map git-messenger-map
   ("d" . git-messenger:popup-diff)
   ("s" . git-messenger:)
   ("c" . git-messenger:copy-commit-id))
  :config
  (add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)
  (setq git-messenger:show-detail t))

(use-package gitignore-mode)
(use-package gitconfig-mode
  :config
  (autoload 'gitconfig-mode "gitconfig-mode" "Major mode for editing gitconfig files." t)
  (add-to-list 'auto-mode-alist '(".gitconfig$" . gitconfig-mode)))

(use-package magit
  :chords ("qg" . magit-status) ;; run git status for current buffer
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (magit-define-popup-switch 'magit-log-popup ?w "date-order" "--date-order"))

;; A saner ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-projectile
  ;; :diminish projectile-mode
  :config
  (projectile-global-mode) ;; activate projectile-mode everywhere
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (setq projectile-enable-caching t) ;; enable caching for projectile-mode
  (setq projectile-switch-project-action 'projectile-vc) ;; magit-status or svn
  (def-projectile-commander-method ?d
    "Open project root in dired."
    (projectile-dired))
  (def-projectile-commander-method ?f
    "Git fetch."
    (magit-status)
    (call-interactively #'magit-fetch-current)))

(use-package jenkins ;; TODO voir si c’est mieux que butler
  :disabled t
  :config
  (setq jenkins-api-token "<api token can be found on user's configure page>")
  (setq jenkins-url "<jenkins url>")
  (setq jenkins-username "<your user name>")
  (setq jenkins-viewname "<viewname>"))

;;;;;;;;;;;;;;
;; ORG-MODE ;;
;;;;;;;;;;;;;;

(use-package org
  :bind
  (("\C-c l" . org-store-link)
   ("\C-c a" . org-agenda)
   ("\C-c b" . org-iswitchb)
   ("\C-c j" . jirify)
   :map org-mode-map
   ("\C-c t" . org-begin-template))
  :chords ("gx" . org-capture)
  :init (require 'org-agenda)
  :config
  ;; ORG-CAPTURE
  (setq org-default-notes-file (concat user-emacs-directory "notes.org"))
  (setq terminalcity-dir "~/Terminalcity/")
  (setq polopeche-home-dir "/sshx:polopeche:/home/duncan/")

  ;; org-capture-templates
  (setq org-capture-templates
        '(
          ;; local
          ("n" "local - Note" entry (file+datetree org-default-notes-file) "* %<%Hh%M>\n\t%i%?")
          ("y" "local - Code snippet" plain (file (concat user-emacs-directory "code-snippets.txt")) "\n%i%?")
          ;; remote
          ("D" "polopeche - Diary entry" entry (file+datetree (concat polopeche-home-dir "Terminalcity/Textes/diary.org")) "* %<%Hh%M>\n\t%i%?")
          ("T" "polopeche - TODO" entry (file+headline (concat polopeche-home-dir "Terminalcity/Todo/arthur.org") "VRAC") "* TODO %?\n\t%i")))

  (setq org-export-coding-system 'utf-8)
  (setq org-completion-use-ido t)

  ;; font and faces customization
  (setq org-todo-keyword-faces
        '(("INPR" . (:foreground "yellow" :weight bold))
          ("STARTED" . (:foreground "yellow" :weight bold))
          ("WAIT" . (:foreground "yellow" :weight bold))
          ("WIP" . (:foreground "yellow" :weight bold))
          ("INPROGRESS" . (:foreground "yellow" :weight bold))))

  ;; update cookies [1/2] when deleting lines
  (defun myorg-update-parent-cookie ()
    (when (equal major-mode 'org-mode)
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (org-update-parent-todo-statistics)))))

  (defadvice org-kill-line (after fix-cookies activate)
    (myorg-update-parent-cookie))

  (defadvice kill-whole-line (after fix-cookies activate)
    (myorg-update-parent-cookie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATE, TIME & CALENDAR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(use-package emacs-calfw
  :disabled t) ;; à tester

;; TODO configure weather in mode line
(use-package weatherline-mode
  :disabled t
  :ensure nil
  :load-path "elisp"
  :config
  (setq weatherline-location-id "2988507")
  (weatherline-mode))

(use-package engine-mode
  :config
  (engine/set-keymap-prefix (kbd "C-c s"))
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
  (defengine wordreference-en-fr "www.wordreference.com/enfr/%s" :keybinding "r")
  (defengine wordreference-fr-en "www.wordreference.com/fren/%s" :keybinding "R"))

;;;;;;;;;;
;; JAVA ;;
;;;;;;;;;;

(use-package flycheck-java ;; flycheck minor mode for java
  :ensure nil
  :load-path "elisp/")

(use-package malabar-mode ;; TODO à tester
  :disabled t
  :config
  ;; JAVA (malabar-mode)
  ;; mimic the IDEish compile-on-save behaviour
  ;; (load-file "~/outils/cedet/cedet-devel-load.el")
  (add-hook 'after-init-hook (lambda ()
                               (message "activate-malabar-mode")
                               (activate-malabar-mode)))

  (add-hook 'malabar-java-mode-hook 'flycheck-mode)
  (add-hook 'malabar-groovy-mode-hook 'flycheck-mode)
  (add-hook 'malabar-mode-hook (lambda () (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))
  (add-hook 'malabar-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'malabar-http-compile-file-silently
                        nil t))))


(use-package emacs-eclim
  :config
  (global-eclim-mode)
  (require 'eclimd)
  (setq eclim-eclipse-dirs "~/outils/eclipse/eclipse-mars"
        eclim-executable "~/outils/eclipse/eclipse-mars/eclim")
  (require 'company-emacs-eclim)
  (company-emacs-eclim-setup)
  (company-emacs-eclim-ignore-case t)
  (add-hook 'java-mode-hook (lambda () (setq flycheck-java-ecj-jar-path "/home/arthur/outils/java/ecj-4.5.jar"))))

;;;;;;;;;;;;;;;;;;;;;;
;; WEB & JAVASCRIPT ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode ;; HTML, XML, JSP (using web-mode)
  :config
  (setq web-mode-engines-alist '(("php" . "\\.phtml\\'")
                                 ("blade" . "\\.blade\\.")))
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.rhtml\\'"
         "\\.html\\'"
         "\\.tag\\'"
         "\\.xml\\'"
         "\\.xsd\\'"
         "\\.wsdl\\'"))
(use-package web-beautify
  :disabled t
  :bind-keymap (
                ;; :map js2-mode-map ("C-c b" . web-beautify-js)
                ;; Or if you're using 'js-mode' (a.k.a 'javascript-mode')
                :map js-mode-map ("C-c b" . web-beautify-js)
                     :map json-mode-map ("C-c b" . web-beautify-js)
                     :map html-mode-map ("C-c b" . web-beautify-html)
                     :map web-mode-map ("C-c b" . web-beautify-html)
                     :map css-mode-map ("C-c b" . web-beautify-css)))

;; JAVASCRIPT (to be tested)
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq js2-basic-offset 2)
;; (setq js2-use-font-lock-faces t)


(autoload 'json-pretty-print "json-pretty-print" "json-pretty-print" t)
(add-hook 'json-mode-hook 'json-pretty-print)
(add-hook 'js-mode-hook (lambda () (flycheck-mode t)))

;;;;;;;;;;
;; TEXT ;;
;;;;;;;;;;

(setq default-major-mode 'text-mode) ;; text-mode by default
(add-hook 'text-mode-hook 'flyspell-mode) ;; flyspell by default
(add-hook 'text-mode-hook 'visual-line-mode) ;; auto-wrapping (soft wrap) in text-mode
(add-hook 'text-mode-hook 'dubcaps-mode) ;; auto-correct double capitals
(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; visual-line-mode instead of auto-fill

;;;;;;;;;;
;; MAIL ;;
;;;;;;;;;;

;; (remove-hook 'html-helper-mode-hook 'flyspell-mode) ;; auto-correct disabled by default
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping in mail-mode

;;;;;;;;;;;
;; SHELL ;;
;;;;;;;;;;;

(add-hook 'sh-mode-hook (lambda () (setq tab-width 4 sh-basic-offset 4 indent-tabs-mode t)))
;;(autoload 'sh-mode "sh-mode" "Major mode for editing shell scripts." t)
(add-to-list 'auto-mode-alist '(".*rc$" . sh-mode))
(add-to-list 'auto-mode-alist '(".*bash.*$" . sh-mode))

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

;;;;;;;;;
;; SQL ;;
;;;;;;;;;

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

;;;;;;;;;;;;
;; GROOVY ;;
;;;;;;;;;;;;

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

;;;;;;;;;;
;; RUBY ;;
;;;;;;;;;;

;; Loads ruby mode when a .rb file is opened.
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(add-to-list 'auto-mode-alist '(".rb$" . ruby-mode))

;;;;;;;;;;
;; LISP ;;
;;;;;;;;;;

(define-key lisp-mode-map (kbd "C-c x") 'eval-and-replace) ;; eval sexp and replace it by its value
;; (global-set-key (kbd "C-c c") 'compile)

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t))))

;;;;;;;;;;;;
;; PYTHON ;;
;;;;;;;;;;;;

;; (add-hook 'python-mode-hook 'jedi:setup) ;; fire up jedi in python env
;; (setq jedi:complete-on-dot t) ;; optional

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMISATION & ENVIRONMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(load-file (expand-file-name "env.el" user-emacs-directory))

(setq private-file (expand-file-name "my-private.el" user-emacs-directory))
(when (file-exists-p private-file)
  (load-file "~/my-private.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SESSION SAVING & BACKUPS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't move files to trash when deleting
(setq delete-by-moving-to-trash nil)

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 500) ;; just 20 is too recent

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

;;;;;;;;;;;;;;;;;
;; SERVER MODE ;;
;;;;;;;;;;;;;;;;;

(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))
(global-set-key (kbd "M-#") 'server-edit) ;; send back to server, quicker than C-x #

(use-package edit-server
  :init (edit-server-start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE LINE (smart-mode-line) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO configure weather in mode line
(use-package weatherline-mode
  :disabled t
  :ensure nil
  :load-path "elisp"
  :config
  (setq weatherline-location-id "2988507")
  (weatherline-mode))

(use-package smart-mode-line
  :config
  (setq powerline-arrow-shape 'curve)
  (setq powerline-default-separator-dir '(right . left))
  (setq sml/theme 'dark)
  ;; (setq sml/no-confirm-load-theme t)
  (add-to-list 'sml/replacer-regexp-list '("^~/projets/" ":p:") t)
  (sml/setup))

;;;;;;;;;;;
;; GAMES ;;
;;;;;;;;;;;

;; spray mode (spritz)
(use-package spray
  :bind ("C-x g s" . spray-mode))

(use-package 2048-game :disabled t)
(use-package speed-type :disabled t)
(use-package typit :disabled t)
(use-package vimgolf :disabled t)
(use-package slime-volleyball :disabled t)

;;;;;;;;;;;;;;
;; EPILOGUE ;;
;;;;;;;;;;;;;;

;; always open init file!
(find-file (expand-file-name "init.el" user-emacs-directory))

;;; init.el ends here
