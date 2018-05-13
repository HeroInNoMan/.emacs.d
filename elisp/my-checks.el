;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SPELLING & ERROR CHECKING ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
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

;; spell-check
(use-package flyspell
  :config
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
  (dim-minor-name 'abbrev-mode " abrv")

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
  (define-key flyspell-mode-map (kbd "C-,") 'flyspell-goto-previous-error))

(provide 'my-checks)
;; my-checks.el ends here.
