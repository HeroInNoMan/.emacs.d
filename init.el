;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                    emacs config file                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;
;; PRIVATE VARIABLES ;;
;;;;;;;;;;;;;;;;;;;;;;;
(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALLATION & LOADING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; package-style dependencies
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t
      use-package-verbose t)

(require 'diminish) ;; for :diminish

(use-package use-package-chords
  :config (key-chord-mode 1))

;;;;;;;;;;;;;;;;;;
;; LIBS & TOOLS ;;
;;;;;;;;;;;;;;;;;;

(use-package my-functions ;; custom functions
  :ensure nil
  :commands (ale-insert-ticket-prefix ale-jirify ale-find-init-file)
  :load-path "elisp/"
  :bind
  ("C-c i" . ale-indent-region-or-buffer) ;; indent whole buffer
  ("<C-M-down>" . duplicate-current-line)
  ("<f5>" . ale-revert-buffer-no-confirm) ;; re-read file from disk
  ("C-<f5>" . ale-copy-and-show-current-file-path) ;; copy current file path
  ("M-<f5>" . show-file-name) ;; show the file name in minibuffer
  ("C-x C-r" . sudo-edit) ;; sudo open file
  ("C-x |" . ale-toggle-window-split)
  ("C-|" . ale-toggle-window-split))

(use-package my-checks :ensure nil)

(use-package describe-number)

(use-package swagger-to-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFAULTS, ERGONOMY & KEYBINDINGS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package better-defaults)

(use-package crux
  :config
  (global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
  (crux-reopen-as-root-mode)
  ;; advices
  (crux-with-region-or-buffer comment-or-uncomment-region)
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-buffer tabify))

(global-set-key (kbd "C-S-b") 'bookmark-set) ;; easier eclipse-style bookmark setting

(global-set-key (kbd "M-«") 'beginning-of-buffer)
(global-set-key (kbd "M-»") 'end-of-buffer)


;; Killing emacs
(global-unset-key (kbd "C-x C-c")) ;; too easy to hit by accident, use “M-x kill-emacs” instead
(global-set-key (kbd "C-x r q") 'kill-emacs) ;; r·eally q·uit

(unless (display-graphic-p)
  (global-unset-key (kbd "C-z")) ;; used for tmux (C-z z for suspend-frame)
  (global-set-key (kbd "C-z z") 'suspend-frame)) ;; C-z is saved for tmux

(use-package god-mode
  :diminish god-local-mode
  :bind
  (("C-c g" . toggle-god-mode)
   ("<menu>" . toggle-god-mode)
   :map god-local-mode-map
   ("i" . toggle-god-mode)
   ("z" . repeat)
   ("." . repeat))
  :chords ("ii" . toggle-god-mode)
  :init (setq god-mode-colors nil)
  :config
  (defun toggle-god-mode ()
    (interactive)
    (god-mode-all)
    (my-update-cursor))
  (defun my-update-cursor ()
    "change cursor colour if god-mode is activated/deactivated effectively."
    (cond
     ((and god-local-mode (not god-mode-colors))
      (progn
        (set-cursor-color "red")
        (setq god-mode-colors t)))
     ((and (not god-local-mode) god-mode-colors)
      (progn
        (set-cursor-color "yellow")
        (setq god-mode-colors nil)))))
  (defadvice select-window (after update-cursor-color activate)
    (my-update-cursor))
  (add-to-list 'god-exempt-major-modes 'helm-major-mode)
  (add-to-list 'god-exempt-major-modes 'browse-kill-ring-mode)
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

;; fill or unfill paragraph (M-q as a toggle)
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))
(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000
      global-mark-ring-max 128
      mark-ring-max 128
      kill-ring-max 128)

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

(use-package anzu
  :diminish anzu-mode
  :config
  (global-anzu-mode t)
  (setq-default anzu-cons-mode-line-p nil))

(use-package all-the-icons)

(use-package spaceline
  :ensure t
  :config
  (defun spaceline-face-func-god (face active)
    (cond
     ((eq 'face1 face) (if active 'powerline-active1 'powerline-inactive1))
     ((eq 'face2 face) (if active 'powerline-active2 'powerline-inactive2))
     ((eq 'line face) (if active (if god-local-mode 'spaceline-god-face 'mode-line) 'mode-line-inactive))
     ((eq 'highlight face) (if active (funcall spaceline-highlight-face-func) 'powerline-inactive1))))

  (spaceline-define-segment ale/version-control
    "Show the current version control branch."
    (when vc-mode
      (substring vc-mode (+ 2 (length (symbol-name (vc-backend buffer-file-name)))))))
  (spaceline-define-segment ale/buffer-modified
    "Buffer status (read-only, modified), with color"
    (cond (buffer-read-only (propertize "" 'face 'spaceline-read-only))
          ((buffer-modified-p) (propertize " " 'face 'spaceline-modified))
          (t "")))
  (spaceline-define-segment ale/major-mode
    "The name of the major mode."
    (if god-local-mode
        (propertize (powerline-major-mode) 'face 'spaceline-god-face)
      (powerline-major-mode)))
  (defface spaceline-god-face
    `((t (:background "brown"
                      :foreground "#3E3D31"
                      :inherit 'mode-line)))
    "Face for god mode."
    :group 'spaceline)
  (defface spaceline-process-face
    `((t (:background "blue"
                      :foreground "bold"
                      :inherit 'mode-line)))
    "Face for process segment."
    :group 'spaceline)
  (defun spaceline-highlight-face-modified ()
    "Set the highlight face depending on the modified state.
Set `spaceline-highlight-face-func' to
`spaceline-highlight-face-modified' to use this."
    (cond
     (buffer-read-only 'spaceline-read-only)
     ((buffer-modified-p) 'spaceline-modified)
     (t 'spaceline-unmodified)))
  (setq-default spaceline-face-func 'spaceline-face-func-god
                mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-all-the-icons
  :after spaceline)
(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode)
  (spaceline-info-mode)
  (setq-default
   spaceline-minor-modes-separator " ⚫ "
   spaceline-highlight-face-func 'spaceline-highlight-face-modified
   powerline-default-separator 'arrow);; Valid Values: alternate, arrow, arrow-fade, bar, box, brace,butt, chamfer, contour, curve, rounded, roundstub, wave, zigzag, utf-8.
  ;; build mode line
  (spaceline-install
    'main
    '(((remote-host buffer-id line) :face highlight-face :separator ":" :priority 1)
      ((projectile-root ale/version-control) :separator " ⑂ ")
      (anzu :face mode-line)
      (process :when active :face spaceline-process-face)
      (erc-track))
    '((selection-info :face region :when mark-active)
      ((flycheck-error flycheck-warning flycheck-info) :when active)
      (python-pyenv)
      (python-pyvenv)
      (org-clock)
      (org-pomodoro)
      (major-mode :face highlight-face :priority 1)
      (minor-modes :face spaceline-evil-visual)
      (which-function)
      (line-column :priority 0)
      (point-position :priority 0)
      (buffer-size :priority 0)
      (buffer-encoding-abbrev :priority 0 :when active)
      (global :face spaceline-evil-visual :when active :priority 2)
      (window-number :priority 0)
      (workspace-number :priority 0)
      (battery :face powerline-active1 :priority 0 :when active)
      ;; (buffer-position :face highlight-face :priority 0)
      (hud :priority 0))))

(use-package color-theme
  :config
  (color-theme-initialize)
  (color-theme-dark-laptop))

(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package rainbow-delimiters
  :config (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-blocks)

(use-package rainbow-mode
  :diminish rainbow-mode
  :config (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

(use-package idle-highlight-mode)

(use-package highlight-line ;; highlight line in list buffers
  :ensure nil
  :load-path "elisp/")

(use-package fancy-narrow
  :diminish fancy-narrow-mode
  :config (fancy-narrow-mode t))

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-mode t))

(use-package zoom-frm
  :if (display-graphic-p)
  :bind
  ("C-+" . zoom-frm-in)
  ("C-=" . zoom-frm-unzoom))

;; (defun my--set-transparency (inc)
;;   "Increase or decrease the selected frame transparency"
;;   (let* ((alpha (frame-parameter (selected-frame) 'alpha))
;;          (next-alpha (cond ((not alpha) 100)
;;                            ((> (- alpha inc) 100) 100)
;;                            ((< (- alpha inc) 0) 0)
;;                            (t (- alpha inc)))))
;;     (set-frame-parameter (selected-frame) 'alpha next-alpha)))

;; (defhydra hydra-transparency (:columns 2)
;;   "
;; ALPHA : [ %(frame-parameter nil 'alpha) ]
;; "
;;   ("+" (lambda () (interactive) (my--set-transparency +1)) "+ more")
;;   ("-" (lambda () (interactive) (my--set-transparency -1)) "- less")
;;   ("j" (lambda () (interactive) (my--set-transparency +10)) "++ more")
;;   ("k" (lambda () (interactive) (my--set-transparency -10)) "-- less")
;;   ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 opaque:")
;;          (set-frame-parameter (selected-frame) 'alpha value)) "Set to ?" :color blue))

(use-package sublimity
  :config
  (require 'sublimity-scroll)
  (require 'sublimity-map))

;; colors, appearance
(use-package iso-transl ;; some environments don’t handle dead keys
  :ensure nil)

(global-font-lock-mode t) ;; enable syntax highlighting
(setq font-lock-maximum-decoration t) ;; all possible colours
(blink-cursor-mode -1) ;; no blinking cursor
(global-hl-line-mode -1) ;; don’t highlight current line
(highlight-line-mode 1) ;; except in “list” modes
(fringe-mode 0) ;; remove fringes on the sides

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

(setq-default truncate-string-ellipsis "…")

;; prettify-symbols
(defconst lisp--prettify-symbols-alist
  '(("lambda"  . ?λ)
    (">=" . ?≥)
    ("<=" . ?≤)))

;;;;;;;;;;;
;; DIRED ;;
;;;;;;;;;;;

(use-package dired+
  :config (unbind-key "M-b" dired-mode-map))

(use-package dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-AlhF") ;; dired human readable size format
(unbind-key "M-b" dired-mode-map)

;; Auto refresh dired
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil
      ;; always delete and copy recursively
      dired-recursive-deletes 'always
      dired-recursive-copies 'always)

(use-package find-dired
  :config (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(use-package disk
  :chords ("<f5><f5>" . disk)) ;; cannot use function keys for chords

;;;;;;;;;;;;
;; SEARCH ;;
;;;;;;;;;;;;

;; standard isearch by default
(global-set-key (kbd "C-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward)


(use-package isearch-dabbrev
  :bind (:map isearch-mode-map
              ("<tab>" . isearch-dabbrev-expand)
              ("M-/" . isearch-dabbrev-expand)))

;; (use-package swiper-helm
;;   :bind ("C-S-s" . swiper-helm))

(use-package smartscan
  :bind
  ("M-n". smartscan-symbol-go-forward) ;; find next occurence of word at point
  ("M-p". smartscan-symbol-go-backward) ;; find previous occurence of word at point
  ("M-'". smartscan-symbol-replace)) ;; replace all occurences of word at point

;; regexp-builder
(use-package re-builder
  :config (setq reb-re-syntax 'string)) ;; syntax used in the re-buidler

(use-package visual-regexp-steroids
  :bind
  (("M-s r" . vr/replace)
   ("M-s q" . vr/query-replace)
   ("C-M-%" . vr/query-replace)
   ;; if you use multiple-cursors, this is for you:
   ("M-s m" . vr/mc-mark)
   ;; to use visual-regexp-steroids's isearch instead of the built-in regexp isearch, also include the following lines:
   ("C-M-r" . vr/isearch-backward)
   ("C-M-s" . vr/isearch-forward))
  :config (require 'visual-regexp)) ;; TODO check if really necessary

;; ;; file finder
;; (use-package f3
;;   :bind
;;   ("C-ç f" . f3)
;;   ("C-c h f" . f3))

;;;;;;;;;;;;;;;;
;; NAVIGATION ;;
;;;;;;;;;;;;;;;;

;; quickly switch to other window
(use-package ace-window
  :bind ("M-o" . ace-window))

(use-package avy
  :chords ("àà" . avy-goto-char-timer)
  :bind
  ("M-à" . avy-goto-word-1) ;; quickly jump to word by pressing its first letter
  ("C-à" . avy-goto-char-timer)) ;; quickly jump to any char in word

(use-package imenu-anywhere
  :bind ("C-ç ." . helm-imenu-anywhere))

(use-package dumb-jump
  :bind (:map prog-mode-map
              ("C-." . dumb-jump-go)
              ("C-," . dumb-jump-back)
              ("C-;" . dumb-jump-quick-look)))

(use-package minimap)

;; Allow scrolling with mouse wheel
(when (display-graphic-p) (progn(mouse-wheel-mode t)
                                (mouse-avoidance-mode 'none)))

(use-package goto-last-change
  :bind
  ("C-x C-/" . goto-last-change)
  ("C-x /" . goto-last-change)
  :config (make-command-repeatable 'goto-last-change))

(use-package neotree
  :bind (:map neotree-mode-map
              ("<f2>" . neotree-copy-filepath-to-yank-ring)
              ("<f5>" . neotree-refresh)
              ("e" . neotree-stretch-toggle))
  :config (setq neo-window-width 50))

(use-package pfuture)
(use-package treemacs
  :disabled t
  :after pfuture
  :load-path "~/projets/treemacs/"
  :defer t
  :config
  (setq treemacs-header-function            #'treemacs--create-header-projectile
        treemacs-follow-after-init          t
        treemacs-width                      45
        treemacs-indentation                2
        treemacs-git-integration            t
        treemacs-change-root-without-asking nil
        treemacs-sorting                    'alphabetic-desc
        treemacs-show-hidden-files          t
        treemacs-never-persist              nil)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  :bind
  (:map
   global-map
   ([f8] . treemacs-toggle)))

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

(global-set-key (kbd "C-%") 'ale-toggle-selective-display)

;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION & HELP ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package browse-kill-ring
  :config
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package char-menu
  :bind (("<f7>" . char-menu)
         ("C-é" . char-menu))
  :config
  (setq char-menu '(("Typography" "•" "©" "†" "‡" "°" "·" "§" "№" "★")
                    ("Math"       "≈" "≡" "∞" "√" "∀" "∃")
                    ("cyrillic"   "а" "б" "в" "г" "д" "е" "ж" "з" "и" "й" "к" "л" "м" "н" "о" "п" "р" "с")
                    ("cyril. maj" "А" "Б" "В" "Г" "Д" "Е" "Ж" "З" "И" "Й" "К" "Л" "М" "Н" "О" "П" "Р" "С")
                    ("arabic"     "ا" "ب" "ت" "ث" "ج" "ح" "خ" "د" "ذ" "ر" "ز" "س" "ش" "ص" "ض" "ط" "ظ" "ع" "غ" "ف" "ق" "ك" "ل" "م" "ن" "ه" "و" "ي" "ء")
                    ("Greek"      "α" "β" "Y" "δ" "ε" "ζ" "η" "θ" "ι" "κ" "λ" "μ" "ν" "ξ" "ο" "π" "ρ" "σ" "τ" "υ" "φ" "χ" "ψ" "ω")
                    ("Greek Maj"  "Α" "Β" "Y" "Δ" "Ε" "Ζ" "Η" "Θ" "Ι" "Κ" "Λ" "Μ" "Ν" "Ξ" "Ο" "Π" "Ρ" "Σ" "Τ" "Υ" "Φ" "Χ" "Ψ" "Ω")
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
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t))

(use-package which-key ;; which-key (replacement for guide-key)
  :diminish which-key-mode
  :config (which-key-mode))

(use-package helm
  :diminish helm-mode
  :bind
  (("M-x" . helm-M-x) ;; superior to M-x
   ("C-x M-x" . execute-extended-command)
   ("C-ç A" . helm-apt)
   ("C-c h A" . helm-apt)
   ("C-ç P" . helm-list-elisp-packages-no-fetch)
   ("C-c h P" . helm-list-elisp-packages-no-fetch)
   ("C-ç a" . my-do-ag-project-root-or-dir)
   ("C-c h a" . my-do-ag-project-root-or-dir)
   ("C-ç c" . helm-org-capture-templates)
   ("C-c h c" . helm-org-capture-templates)
   ("C-ç g" . helm-do-ag)
   ("C-c h g" . helm-do-ag)
   ("C-ç m" . helm-man-woman)
   ("C-c h m" . helm-man-woman)
   ("C-ç o" . helm-occur)
   ("C-c h o" . helm-occur)
   ("C-ç p" . helm-projectile-switch-project)
   ("C-ç C-p" . helm-projectile-switch-project)
   ("C-c h p" . helm-projectile-switch-project)
   ("C-ç r" . helm-resume)
   ("C-c h r" . helm-resume)
   ("C-ç s" . helm-google-suggest)
   ("C-c h s" . helm-google-suggest)
   ("C-ç t" . helm-top)
   ("C-c h t" . helm-top)
   ("C-ç w" . helm-wikipedia-suggest)
   ("C-c h w" . helm-wikipedia-suggest)
   ("C-ç x" . helm-run-external-command)
   ("C-c h x" . helm-run-external-command)
   ("C-ç h" . helm-apropos)
   ("C-h a" . helm-apropos)
   ("C-h f" . helm-apropos)
   ("C-h v" . helm-apropos)
   ("C-ç C-ç" . helm-for-files)
   ("M-ç" . my/helm-find-files)
   :map helm-map
   ("M-«" . helm-beginning-of-buffer)
   ("M-»" . helm-end-of-buffer))
  :chords (("bf" . helm-for-files) ;; helm-for-file looks everywhere, no need for anything else
           ("éè" . my-do-ag-project-root-or-dir)) ;; incremental grep in project
  :config
  (defun my-do-ag-project-root-or-dir ()
    "call helm-do-ag-project-root if in project, helm-do-ag otherwise"
    (interactive)
    (require 'helm-ag)
    (let ((rootdir (helm-ag--project-root)))
      (unless rootdir (helm-do-ag))
      (helm-do-ag rootdir)))
  ;; activate additional features
  (helm-mode 0) ;; helm-mode only on demand
  (helm-autoresize-mode t)
  (setq helm-M-x-fuzzy-match t ;; optional fuzzy matching for helm-M-x
        helm-adaptive-mode t
        helm-buffer-max-length nil
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-ff-skip-boring-files t
        helm-candidate-number-limit 500
        helm-ag-insert-at-point 'symbol
        helm-ag-base-command "ag --nocolor --nogroup --smart-case"
        helm-for-files-preferred-list '(helm-source-buffers-list
                                        helm-source-recentf
                                        helm-source-projectile-files-list
                                        helm-source-bookmarks
                                        helm-source-file-cache
                                        helm-source-files-in-current-dir
                                        ;; helm-source-google-suggest
                                        helm-source-locate))
  (defun my/helm-find-files ()
    ;; https://stackoverflow.com/questions/11403862/how-to-have-emacs-helm-list-offer-files-in-current-directory-as-options
    (interactive)

    ;; From helm-buffers-list in helm-buffers.el
    (unless helm-source-buffers-list
      (setq helm-source-buffers-list
            (helm-make-source " Buffers" 'helm-source-buffers)))

    ;; From file:elpa/helm-20160401.1302/helm-files.el::(with-helm-temp-hook%20'helm-after-initialize-hook
    ;; This lets me bring up results from locate without having to
    ;; exit and run a separate command.  Now I just have to remember
    ;; to use it...
    (with-helm-temp-hook 'helm-after-initialize-hook
      (define-key helm-map (kbd "C-x C-l")
        'helm-multi-files-toggle-to-locate))

    (helm-other-buffer (list helm-source-buffers-list
                             helm-source-files-in-current-dir
                             helm-source-bookmarks
                             helm-source-recentf
                             helm-source-projectile-files-list)
                       " * my/helm-find-files *")))

(use-package ace-jump-helm-line
  :bind (:map helm-map ("M-à" . ace-jump-helm-line)))
(use-package helm-dash
  :bind ("C-ç d" . helm-dash))

(use-package helm-descbinds
  :bind
  ("C-ç b" . helm-descbinds)
  ("C-h b" . helm-descbinds))

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

(use-package hydra
  :config
  (defvar whitespace-mode nil)
  (defvar idle-highlight-mode nil)
  (defvar global-linum-mode nil)
  (defvar god-local-mode nil)

  (defhydra hydra-spell (:color teal)
    "spelling"
    ("t" endless/ispell-word-then-abbrev "corr. & add")
    ("f" flyspell-mode "flyspell")
    ("c" flyspell-buffer "flycheck buffer")
    ("F" flyspell-buffer "flycheck buffer")
    ("d" ispell-change-dictionary "change dictionary")
    ("w" define-word-at-point "word definition")
    ("q" nil "cancel"))
  (global-set-key (kbd "C-è") 'hydra-spell/body)

  (defhydra hydra-widgets (:color teal)
    "widgets"
    ("a" avandu-overview "avandu RSS")
    ("b" eww "eww-browser")
    ("B" ecb-activate "code browser")
    ("c" open-calendar "calendar")
    ("d" ale-find-diary-file "diary")
    ("e" eshell "eshell")
    ("E" elfeed "elfeed RSS")
    ("g" toggle-god-mode "god")
    ("G" gnus "gnus")
    ("i" ale-find-init-file "init file")
    ("I" highlight-indent-guides-mode "indent-guide")
    ("j" butler-status "jenkins")
    ("l" linum-mode "line number")
    ("m" minimap-mode "minimap")
    ("M" helm-spotify-plus "spotify")
    ("n" neotree-toggle "neotree")
    ;; ("n" treemacs-toggle "tree")
    ("o" org-mode "org-mode")
    ("p" list-packages "packages")
    ("P" prettify-symbols-mode "prettify symbols")
    ("r" ale-find-rest-client-file "rest-client")
    ("R" rainbow-blocks-mode "rainbow-blocks")
    ("s" sublimity-mode "sublimity")
    ("S" spray-mode "spritz")
    ("t" crux-visit-term-buffer "ansi-term")
    ("T" tomatinho "pomodoro (tomatinho)")
    ("v" visual-line-mode "visual-line")
    ("w" whitespace-mode "whitespace")
    ("W" wttrin "weather")
    ("y" play-youtube-video "youtube")
    ("Y" w3m-play-youtube-video "youtube at point")
    ("$" shell "shell")
    ("%" ansi-term "term")
    ("q" nil "cancel"))
  (key-chord-define-global (kbd "bj") 'hydra-widgets/body)

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

;; case-insensitive policy
(setq completion-ignore-case t
      pcomplete-ignore-case t
      read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;; IVY / SWIPER / COUNSEL
(use-package ivy
  :config (setq ivy-height 20))
(use-package swiper
  :bind ("C-S-s" . counsel-grep-or-swiper))
;; (use-package counsel
;;   :chords ("éè" . counsel-ag))

(defhydra hydra-counsel (:color teal)
  "call counsel functions"
  ("é" counsel-recentf "recentf")
  ("f" counsel-find-file "find file")
  ("l" counsel-locate "locate")
  ("r" ivy-resume "resume")
  ("s" counsel-grep-or-swiper "grep or swiper")
  ("u" counsel-unicode-char "unicode char")
  ("x" counsel-M-x "M-x")
  ("y" counsel-yank-pop "yank-pop")
  ("q" nil "cancel" :color blue))
(global-set-key (kbd "C-é") 'hydra-counsel/body)

(use-package yasnippet
  :config (yas-global-mode 1))

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
(setq-default select-enable-clipboard t
              x-select-enable-clipboard t)

(global-set-key (kbd "M-y") 'yank-pop)

(use-package paredit
  :disabled t
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode))

;; easier access to transposition commands
(global-set-key (kbd "C-x M-h") 'transpose-paragraphs)
(global-set-key (kbd "C-§") 'transpose-paragraphs)
(global-set-key (kbd "C-x M-s") 'transpose-sentences)
(global-set-key (kbd "C-x M-t") 'transpose-sexps)

(use-package repeatable
  :ensure nil
  :load-path "elisp")

(global-set-key (kbd "C-x _") 'ale-toggle-camel-snake-kebab-case)

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

(use-package temporary-persistent)

(use-package persistent-scratch
  :config (persistent-scratch-setup-default))

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

;; (setq ibuffer-formats
;;       '((mark modified read-only " "
;;               (name 30 30 :left :elide) " "
;;               (size 9 -1 :right) " "
;;               (mode 16 16 :left :elide) " " filename-and-process)
;;         (mark " " (name 16 -1) " " filename)))

(use-package ibuffer-vc
  :config
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))


  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
  (setq ibuffer-formats
        '((mark modified " " read-only " " vc-status-mini " "
                (name 50 50 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                ;; (vc-status 14 14 :left) " "
                filename-and-process))))

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

;; C-M-e to edit minibuffer in a full-size buffer
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

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

(use-package helm-git-grep
  :bind ("C-ç G" . helm-git-grep))

(use-package git-gutter
  :diminish git-gutter-mode
  :bind
  ("C-x g n" . git-gutter:next-hunk)
  ("C-x g p" . git-gutter:previous-hunk)
  ("M-N" . git-gutter:next-hunk)
  ("M-P" . git-gutter:previous-hunk)
  ("C-x g s" . git-gutter:stage-hunk)
  ("C-x g r" . git-gutter:revert-hunk)
  ("C-x g u" . git-gutter-mode)
  :config
  (global-git-gutter-mode +1)
  (setq git-gutter:ask-p nil
        git-gutter:hide-gutter t))

(use-package magit
  :chords ("qg" . magit-status) ;; run git status for current buffer
  :bind ("C-x g ." . magit-status)
  :bind ("C-x g b" . magit-blame)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (magit-define-popup-switch 'magit-log-popup ?w "date-order" "--date-order"))

;; A saner ediff
(setq ediff-diff-options "-w"
      ediff-split-window-function 'split-window-horizontally
      ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package vdiff
  :disabled t
  :bind (:map vdiff-mode-map
              ("C-c" . vdiff-mode-prefix-map))
  :init (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  :config
  (require 'vdiff)
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

;; always follow symbolic links for files under VC
(use-package vc
  :config (setq vc-follow-symlinks t))


;;;;;;;;;;;;;;;;;;;;;;;;
;; PROJECT MANAGEMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-projectile
  :diminish projectile-mode
  :config
  (projectile-global-mode) ;; activate projectile-mode everywhere
  (helm-projectile-on)
  (setq projectile-completion-system 'helm
        projectile-enable-caching t ;; enable caching for projectile-mode
        projectile-switch-project-action 'projectile-vc) ;; magit-status or svn
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
  (setq jenkins-api-token "<api token can be found on user's configure page>"
        jenkins-url "<jenkins url>"
        jenkins-username "<your user name>"
        jenkins-viewname "<viewname>"))

;;;;;;;;;;;;;;
;; ORG-MODE ;;
;;;;;;;;;;;;;;

(use-package org
  :bind
  (("\C-c a" . org-agenda)
   ("\C-c b" . org-iswitchb)
   :map org-mode-map
   ("\C-c l" . org-store-link)
   ("\C-c j" . ale-jirify)
   ("\C-c t" . org-begin-template))
  :chords ("gx" . org-capture)
  :init (require 'org-agenda)
  :config
  ;; ORG-CAPTURE
  (setq org-default-notes-file (concat user-emacs-directory "notes.org")
        terminalcity-dir "~/Terminalcity/"
        polopeche-home-dir "/sshx:polopeche:/home/duncan/")

  ;; active Babel languages
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . nil)
     (sh . t)
     (emacs-lisp . t)))

  ;; org-capture-templates
  (setq org-capture-templates
        '(
          ;; local
          ("n" "local - Note" entry (file+datetree org-default-notes-file) "* %<%Hh%M>\n\t%i%?")
          ("y" "local - Code snippet" plain (file (concat user-emacs-directory "code-snippets.txt")) "\n%i%?")
          ;; remote
          ("D" "polopeche - Diary entry" entry (file+datetree (concat polopeche-home-dir "Terminalcity/Textes/diary.org")) "* %<%Hh%M>\n\t%i%?")
          ("T" "polopeche - TODO" entry (file+headline (concat polopeche-home-dir "Terminalcity/Todo/arthur.org") "VRAC") "* TODO %?\n\t%i")))

  ;; specific agenda files
  (add-to-list 'org-agenda-files my-private-work-diary-org-file)

  (setq org-export-coding-system 'utf-8
        org-completion-use-ido t)

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
      european-calendar-style t ;; day/month/year format for calendar
      calendar-week-start-day 1 ;; start week on Monday
      display-time-string-forms '((propertize
                                   (format-time-string
                                    (or display-time-format
                                        (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
                                    now)
                                   'help-echo
                                   (format-time-string "%a %e %b %Y S%V" now)
                                   'face '(:foreground "blue" :weight bold)
                                   )
                                  (if
                                      (and
                                       (not display-time-format)
                                       display-time-day-and-date)
                                      (format-time-string ", %a %e %b %Y S%V" now)
                                    "")))
(display-time)
(global-set-key (kbd "C-c d") 'insert-todays-date)

(use-package calfw) ;; à tester
(use-package calfw-gcal)
(use-package calfw-ical :ensure nil)
(defun open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:ical-create-source "gcal AL" my-private-primary-gcal-url "Blue")
    (cfw:ical-create-source "gcal hellfest" my-private-secondary-gcal-url "Brown"))))

;; TODO configure weather in mode line
(use-package weatherline-mode
  :disabled t
  :ensure nil
  :load-path "elisp"
  :config
  (setq weatherline-location-id "2988507")
  (weatherline-mode))

;; weather from wttr.in
(use-package wttrin
  :commands (wttrin)
  :config
  (setq wttrin-default-cities
        '("Paris" "Londres" "Nantes" "Lyon" "Berlin" "Manchester" "Nice")))

;; avandu (gateway to tiny tiny RSS)
(use-package avandu
  :config (setq avandu-tt-rss-api-url my-private-personal-tt-rss-api-url
                avandu-user my-private-personal-tt-rss-username
                avandu-password my-private-personal-tt-rss-password)
  :bind (:map avandu-article-mode-map
              ("p" . previous-line)
              ("n" . next-line)
              ("l" . recenter-top-bottom)
              :map avandu-overview-map
              ("<tab>" . avandu-next-feed)
              ("<backtab>" . avandu-previous-feed)
              ("l" . recenter-top-bottom)
              ("v" . scroll-up-command)))

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
  (load-file "~/projets/malabar-mode/src/main/lisp/malabar-mode.el")
  (load-file "~/projets/cedet/cedet-devel-load.el")
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

(use-package eclim
  :disabled t
  :config
  (global-eclim-mode)
  (require 'eclimd)
  (setq eclim-eclipse-dirs "~/outils/eclipse/eclipse-mars"
        eclim-executable "~/outils/eclipse/eclipse-mars/eclim")
  (require 'company)
  (require 'company-emacs-eclim)
  (global-company-mode t)
  (company-emacs-eclim-setup)
  ;; (company-emacs-eclim-ignore-case t)
  (add-hook 'java-mode-hook (lambda () (setq flycheck-java-ecj-jar-path "/home/arthur/outils/java/ecj-4.5.jar")))
  (add-hook 'java-mode-hook 'eclim-mode))
(use-package ecb :disabled t) ;; TODO à tester

;;;;;;;;;
;; WEB ;;
;;;;;;;;;

;; ;; JAVASCRIPT
(use-package js2-mode
  :bind (:js2-mode-map ("C-c C-c" . compile))
  ;; :mode ("\\.js\\'\\|\\.json\\'" . js2-mode)
  :config
  (setq js2-basic-offset 2
        js-indent-level 2
        js2-use-font-lock-faces t)
  (add-hook 'json-mode-hook 'json-pretty-print)
  (add-hook 'js-mode-hook (lambda () (flycheck-mode t)))
  (autoload 'json-pretty-print "json-pretty-print" "json-pretty-print" t))

;; à tester
(use-package js-comint
  :config  (defun inferior-js-mode-hook-setup ()
             (add-hook 'comint-output-filter-functions 'js-comint-process-output))
  (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)
  (add-hook 'js2-mode-hook
            (lambda ()
              (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
              (local-set-key (kbd "C-M-x") 'js-send-last-sexp-and-go)
              (local-set-key (kbd "C-c b") 'js-send-buffer)
              (local-set-key (kbd "C-c C-b") 'js-send-buffer-and-go)
              (local-set-key (kbd "C-c l") 'js-load-file-and-go))))

(use-package web-mode ;; HTML, XML, JSP (using web-mode)
  :config
  (setq web-mode-markup-indent-offset 4
        web-mode-code-indent-offset 4
        web-mode-enable-auto-indentation t
        web-mode-enable-auto-quoting t
        web-mode-engines-alist '(("php" . "\\.phtml\\'")
                                 ("blade" . "\\.blade\\.")))
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.js\\'"
         "\\.jsx\\'"
         "\\.json\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.rhtml\\'"
         "\\.htm\\'"
         "\\.html\\'"
         "\\.tag\\'"
         "\\.tsx\\'"
         "\\.xml\\'"
         "\\.xsd\\'"
         "\\.wsdl\\'"))

(defun mu-xml-format ()
  "Format an XML buffer with `xmllint'."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           "xmllint -format -"
                           (current-buffer) t
                           "*Xmllint Error Buffer*" t))

(use-package gradle-mode
  :mode ("\\.gradle\\'" . gradle-mode))

;;;;;;;;;;;;;;;;
;; TYPESCRIPT ;;
;;;;;;;;;;;;;;;;
(use-package tide
  :bind
  (:map tide-mode-map
        ("C-." . tide-jump-to-definition)
        ("C-," . tide-jump-back)
        ("C-c C-c" . hydra-tide/body))
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    ;; flycheck-typescript-tslint-executable "tslint"
    (eldoc-mode +1)
    (company-mode +1))

  (setq company-tooltip-align-annotations t  ;; aligns annotation to the right hand side
        typescript-indent-level 2
        ;; format options
        tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
                                                                                    :placeOpenBraceOnNewLineForFunctions nil))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-hook 'js2-mode-hook #'setup-tide-mode)
  ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file ~/projets/tss.log"))

  (defhydra hydra-tide(:color blue)
    "tide"
    ("e" tide-project-errors "errors")
    ("f" tide-format "format")
    ("g" tide-references "references")
    ("r" tide-rename-symbol "rename")
    ("s" tide-restart-server "restart server")
    ("q" nil "cancel")))

(use-package typescript-mode
  :disabled t
  :mode ("\\.ts\\'"))

(use-package sass-mode
  :mode ("\\.sass$" . sass-mode))

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

(use-package tidy
  :config (setq sgml-validate-command "tidy"))

;;;;;;;;;;
;; TEXT ;;
;;;;;;;;;;

(setq default-major-mode 'text-mode) ;; text-mode by default
(add-hook 'text-mode-hook 'flyspell-mode) ;; flyspell by default
(add-hook 'text-mode-hook 'visual-line-mode) ;; auto-wrapping (soft wrap) in text-mode
(add-hook 'text-mode-hook 'dubcaps-mode) ;; auto-correct double capitals
(remove-hook 'text-mode-hook #'turn-on-auto-fill) ;; visual-line-mode instead of auto-fill

(use-package define-word)

(use-package yaml-mode
  :config (add-to-list 'auto-mode-alist '(".yml$" . yaml-mode)))
;;;;;;;;;;;;;;
;; MARKDOWN ;;
;;;;;;;;;;;;;;

(use-package markdown-mode)

;;;;;;;;;;
;; MAIL ;;
;;;;;;;;;;

;; (remove-hook 'html-helper-mode-hook 'flyspell-mode) ;; auto-correct disabled by default
(add-hook 'mail-mode-hook 'visual-line-mode) ;; wrapping in mail-mode

;;;;;;;;;;;
;; SHELL ;;
;;;;;;;;;;;

(add-hook 'sh-mode-hook (lambda () (setq tab-width 4 sh-basic-offset 4 indent-tabs-mode t)))
(add-hook 'sh-mode-hook 'flycheck-mode)
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

;; TODO tout passer en use-package
;; (use-package groovy
;;   :ensure nil
;;   :mode ("\\.groovy" . groovy-mode)
;;   :config
;;   ((add-hook 'groovy-mode-hook
;;              '(lambda ()
;;                 (require 'groovy-electric)
;;                 (groovy-electric-mode)))
;;    (autoload 'groovy-eval "groovy-eval" "Groovy Evaluation" t)
;;    (add-hook 'groovy-mode-hook 'groovy-eval)))

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
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-region)
(define-key lisp-mode-map (kbd "C-c C-c") 'eval-region)

;; (global-set-key (kbd "C-c c") 'compile)

(use-package elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :config (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t))))

;;;;;;;;;;;;
;; PYTHON ;;
;;;;;;;;;;;;

(use-package elpy)
(use-package jedi)
(use-package python
  :bind (:map python-mode-map
              ("M-g M-p" . elpy-flymake-previous-error)
              ("M-g M-n" . elpy-flymake-next-error)
              ("C-x C-e " . python-shell-send-defun))
  :config
  (require 'elpy)
  (require 'jedi)
  (add-hook 'python-mode-hook 'elpy-enable)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (setq elpy-rpc-backend "jedi") ;; fire up jedi in python env
  (setq jedi:complete-on-dot t)
  (setq tab-always-indent t))

;;;;;;;;;;;;;
;; CRONTAB ;;
;;;;;;;;;;;;;
(use-package crontab-mode
  :mode ("crontab$" . crontab-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CUSTOMISATION & ENVIRONMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(load-file (expand-file-name "env.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SESSION SAVING & BACKUPS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Save a list of recent files visited.
(recentf-mode 1)
(setq recentf-max-saved-items 500  ;; just 20 is too recent
      vc-make-backup-files t ;; make backups of files, even when they're in version control
      delete-by-moving-to-trash t ;; move files to trash when deleting
      desktop-base-lock-name      "lock"
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

(use-package edit-server
  :if (and
       (window-system)
       (or
        (not (fboundp 'server-running-p))
        (not (server-running-p))))
  :bind ("M-#" . server-edit) ;; send back to server, quicker than C-x #
  :init
  (add-hook 'after-init-hook 'server-start t)
  (add-hook 'after-init-hook 'edit-server-start t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; pomodoro technique ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tomatinho)

;; REST client
(use-package restclient
  :mode ("restclient" . restclient-mode)
  :bind
  (:map restclient-mode-map
        ("C-c n w" . widen)))

;;;;;;;;;;;
;; GAMES ;;
;;;;;;;;;;;

;; spray mode (spritz)
(use-package spray
  :bind (:map spray-mode-map
              ("-" . spray-slower)
              ("+" . spray-faster)
              ("<SPC>" . spray-start/stop)
              ("b" . spray-backward-word)
              ("p" . spray-backward-word)
              ("f" . spray-forward-word)
              ("n" . spray-forward-word)))

;; lorem ipsum filling
(use-package lorem-ipsum
  :disabled t
  :bind (("C-c C-l p" . lorem-ipsum-insert-paragraphs)
         ("C-c C-l s" . lorem-ipsum-insert-sentences)
         ("C-c C-l l" . lorem-ipsum-insert-list)))

(use-package helm-spotify-plus)
(use-package 2048-game :disabled t)
(use-package speed-type :disabled t)
(use-package typit :disabled t)
(use-package typing :disabled t)
(use-package vimgolf :disabled t)
(use-package slime-volleyball :disabled t)

(defun play-youtube-video (url)
  (interactive "sURL: ")
  (shell-command
   (concat "youtube-dl  -o - " url " | vlc -")))

(defun w3m-play-youtube-video ()
  (interactive)
  (play-youtube-video
   (w3m-print-this-url (point))))

;;;;;;;;;;;;;;
;; EPILOGUE ;;
;;;;;;;;;;;;;;

;; always open init file!
(ale-find-init-file)

;;; init.el ends here
