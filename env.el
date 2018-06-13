;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; KUUTAMO (work)  ;;
;;;;;;;;;;;;;;;;;;;;;

(defun load-kuutamo-env ()
  (progn
    ;; hooking for specific functions
    (add-hook 'git-commit-setup-hook 'ale/insert-ticket-prefix)
    (add-hook 'git-commit-setup-hook 'ale/switch-to-en-dict)

    ;; capture templates
    (add-to-list 'org-capture-templates '("d" "work - log" entry (file+olp+datetree my-private-work-diary-org-file) "* TODO %i%?") t)
    (add-to-list 'org-capture-templates '("t" "work - TODO" entry (file+headline my-private-work-diary-org-file "À faire") "* TODO %?\n\t%i") t)

    ;; display battery level
    (use-package fancy-battery
      :config
      (setq fancy-battery-show-percentage t)
      (fancy-battery-mode))

    ;; default project root folder
    (when (featurep 'dumb-jump)
      (setq dumb-jump-default-project "~/projets"))

    ;; default browser
    (setq browse-url-generic-program "firefox")

    ;; confluence search
    (when (featurep 'engine-mode)
      (defengine confluence my-private-work-confluence-url :keybinding "c"))

    ;; gitlab interaction
    (use-package helm-gitlab
      :disabled t
      :ensure gitlab
      :config
      (setq gitlab-host my-private-gitlab-host
            gitlab-username my-private-gitlab-username
            gitlab-password my-private-gitlab-password
            gitlab-token-id my-private-gitlab-token-id))

    ;; jenkins interaction
    (use-package butler
      :bind (:map butler-mode-map
                  ("n" . next-line)
                  ("p" . previous-line))
      :config
      (add-to-list 'god-exempt-major-modes 'butler-mode)
      (add-to-list 'butler-server-list
                   '(jenkins my-private-work-jenkins-url
                             (server-address . my-private-work-server-address)
                             (auth-file . my-private-work-auth-file)))) ;; machine SERVER-NAME login my_login password my_pass

    (use-package jenkins ;; TODO compare to butler
      :disabled t
      :config
      (setq jenkins-api-token "<api token can be found on user's configure page>"
            jenkins-url "<jenkins url>"
            jenkins-username "<your user name>"
            jenkins-viewname "<viewname>"))

    ;; let access projects quickly in a convenient layout
    (defhydra hydra-projects(:color teal)
      ("q" nil "cancel" :column "Projets")
      ("e" (lambda () (interactive)(ale/open-project user-emacs-directory)) "emacs" :column "Perso")
      ("E" (lambda () (interactive)(ale/open-project "~/outils/emacs")) "emacs source"))
    (global-set-key (kbd "<f9>") 'hydra-projects/body)
    (global-set-key (kbd "C-c C-j") 'hydra-projects/body)))

;;;;;;;;;;;;;;;;;;;;;;;
;; YUNOHOST (remote) ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun load-yuno-env ()
  (progn
    ;; don’t highlight current line
    (global-hl-line-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;
;; HIGHLANDER (home) ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun load-highlander-env ()
  (progn
    ;; display battery level
    (use-package fancy-battery
      :config
      (setq fancy-battery-show-percentage t)
      (fancy-battery-mode))

    (use-package transmission
      :config
      (setq transmission-host my-private-transmission-host
            transmission-service my-private-transmission-service
            transmission-rpc-path my-private-transmission-rpc-path))

    ;; let access projects quickly in a convenient layout
    (defhydra hydra-projects(:color teal :columns 3)
      "projects"
      ("d" (lambda () (interactive)(ale/open-project "~/Terminalcity/dotfiles")) "dotfiles")
      ("e" (lambda () (interactive)(ale/open-project user-emacs-directory)) "emacs")
      ("q" nil "cancel"))
    (global-set-key (kbd "<f9>") 'hydra-projects/body)
    (global-set-key (kbd "C-c C-j") 'hydra-projects/body)))

;;;;;;;;;;;;;
;; DEFAULT ;;
;;;;;;;;;;;;;
(defun load-default-env ()
  (progn
    (cond
     ;; MAC OS X
     ((equal "darwin" system-type)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (setq ns-function-modifier 'hyper)))

     ;; windows
     ((or (equal "ms-dos" system-type)
          (equal "windows-nt" system-type)
          (equal "cygwin" system-type))
      (ale/load-windows-specific-conf)
      ;; useful when C-/ does not work (windows/putty)
      (global-set-key (kbd "C-M-z") 'undo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD APPROPRIATE ENVIRONMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((equal "kuutamo" (system-name))
  (load-kuutamo-env))
 ((or (equal "scw-0f23ec" (system-name))
      (equal "ns301170.ip-91-121-73.eu" (system-name)))
  (load-yuno-env))
 ((equal "highlander" (system-name))
  (load-highlander-env))
 (t (load-default-env)))

;;; env.el ends here
