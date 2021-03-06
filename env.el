;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-work-org-capture-templates ()
  "Add work-related capture templates & org variables."
  (add-to-list 'org-refile-targets '(my-private-work-diary-org-file :maxlevel . 3))
  (add-to-list 'org-refile-targets '(my-private-local-todo-org-file :maxlevel . 2))

  (add-to-list 'org-agenda-files my-private-work-diary-org-file)
  (add-to-list 'org-agenda-files my-private-local-notes-org-file)

  ;; capture templates
  (add-to-list 'org-capture-templates
               '("l" "Log       (work)"
                 entry (file+olp+datetree my-private-work-diary-org-file)
                 "* TODO %i%?"
                 :time-prompt t :kill-buffer t) t)
  (add-to-list 'org-capture-templates
               '("w" "TODO      (work)"
                 entry (file+headline my-private-work-diary-org-file "À faire")
                 "* TODO %i%?\n\tSCHEDULED: %t\n%a\n"
                 :prepend t :kill-buffer t) t)
  (add-to-list 'org-capture-templates
               '("v" "Revue     (work)"
                 entry (file+headline my-private-work-diary-org-file "À faire")
                 "* Review %i%?\n\tSCHEDULED: %t"
                 :prepend t :kill-buffer t) t)
  (add-to-list 'org-capture-templates
               '("r" "Référence (work)\n"
                 entry (file+headline my-private-work-diary-org-file "Références")
                 "* %?\n%i"
                 :prepend t :kill-buffer t) t))

;;;;;;;;;;;;;;;;;;;;;
;; KUUTAMO (work)  ;;
;;;;;;;;;;;;;;;;;;;;;

(defun load-kuutamo-env ()
  (set-fira-font-if-possible)

  ;; hooking for specific functions
  (add-hook 'git-commit-setup-hook 'ale/insert-ticket-prefix)
  (add-hook 'git-commit-setup-hook 'ale/switch-to-all-dict)

  ;; org
  (if (fboundp 'org-capture)
      (add-work-org-capture-templates)
    (add-hook
     'org-load-hook
     #'add-work-org-capture-templates))

  ;; display battery level
  (display-battery-mode)

  ;; default project root folder
  (when (featurep 'dumb-jump)
    (setq dumb-jump-default-project "~/projets"))

  ;; default browser
  (setq browse-url-generic-program "firefox")
  (setq browse-url-browser-function 'browse-url-firefox)
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
    :disabled t
    :bind (:map butler-mode-map
                ("n" . next-line)
                ("p" . previous-line))
    :config
    (add-to-list 'god-exempt-major-modes 'butler-mode)
    (add-to-list 'butler-server-list
                 '(jenkins my-private-work-jenkins-url
                           (server-address . my-private-work-server-address)
                           (auth-file . my-private-work-auth-file)))) ;; machine SERVER-NAME login my_login password my_pass

  (use-package travis
    :defer t
    :config (setq travis--token-id my-private-work-travis-token-id
                  travis-website my-private-work-travis-website
                  travis--host my-private-work-travis-host))

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
    ("c" (lambda () (interactive)(ale/open-project "~/projets/Coopener")) "Coopener" :column "Coopener")
    ("v" (lambda () (interactive)(ale/open-project "~/projets/Coopener-v5")) "Coopener-v5")
    ("k" (lambda () (interactive)(ale/open-project "~/outils/axelor-development-kit")) "ADK" :column "Tools")
    ("b" (lambda () (interactive)(ale/open-project "~/projets/coopener-build")) "Coopener-build")
    ("a" (lambda () (interactive)(ale/open-project "~/projets/architecture")) "Architecture")
    ("s" (lambda () (interactive)(ale/open-project "~/projets/Sinapse")) "Sinapse" :column "Sinapse")
    ("C" (lambda () (interactive)(ale/open-project "~/projets/sinapse-config")) "Sinapse-config" :column "Sinapse")
    ("$" (lambda () (interactive)(ale/open-project "~/projets/sinapse-scripts")) "Sinapse-scripts")
    ("m" (lambda () (interactive)(ale/open-project "~/projets/moulinette")) "Moulinette" :column "Other")
    ("p" (lambda () (interactive)(ale/open-project "~/projets/coopener-printer")) "EDI (printer)")
    ("u" (lambda () (interactive)(ale/open-project "~/projets/mailjet-updater")) "Mailjet Updater")
    ("w" (lambda () (interactive)(ale/open-project "~/projets/CalCon")) "CalCon")
    ("d" (lambda () (interactive)(ale/open-project my-private-dotfiles)) "dotfiles" :column "Perso")
    ("e" (lambda () (interactive)(ale/open-project user-emacs-directory)) "emacs config")
    ("E" (lambda () (interactive)(ale/open-project "~/outils/emacs")) "emacs source"))
  (global-set-key (kbd "<f9>") 'hydra-projects/body)
  (global-set-key (kbd "C-c C-j") 'hydra-projects/body))

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
  ;; hooking for specific functions
  (add-hook 'git-commit-setup-hook 'ale/switch-to-all-dict)

  ;; org
  (add-hook
   'org-load-hook
   #'(lambda () (progn
                  (add-to-list 'org-refile-targets '(my-private-local-notes-org-file :maxlevel . 2))
                  (add-to-list 'org-agenda-files my-private-local-todo-org-file))))


  ;; display battery level
  (display-battery-mode)

  (mouse-wheel-mode -1)

  ;; default project root folder
  (when (featurep 'dumb-jump)
    (setq dumb-jump-default-project "~/projets"))

  ;; default browser
  (setq browse-url-generic-program "firefox")

  (use-package transmission
    :defer t
    :config
    (setq transmission-host my-private-transmission-host
          transmission-service my-private-transmission-service
          transmission-rpc-path my-private-transmission-rpc-path))


  (pretty-hydra-define hydra-projects
    (:color teal :quit-key "q")
    ("Config"
     (("d" (funcall (lambda () (interactive)(ale/open-project "~/repos/dotfiles"))) "dotfiles")
      ("e" (funcall (lambda () (interactive)(ale/open-project "~/repos/.emacs.d"))) "emacs")
      ("E" (funcall (lambda () (interactive)(ale/open-project "~/repos/emacs"))) "emacs source")
      ("x" (funcall (lambda () (interactive)(ale/open-project "~/repos/ergodox"))) "Ergodox EZ"))
     "Curriculum"
     (("v" (funcall (lambda () (interactive)(ale/open-project "~/repos/cv"))) "cv")
      ("V" (funcall (lambda () (interactive)(ale/open-project "~/repos/CV"))) "CV")
      ("c" (funcall (lambda () (interactive)(ale/open-project "~/repos/curriculum"))) "curriculum")
      ("a" (funcall (lambda () (interactive)(ale/open-project "~/repos/cv-antoine"))) "cv antoine"))
     "Travaux"
     (("t" (funcall (lambda () (interactive)(ale/open-project "~/repos/traduction-omsi-2020"))) "trad OMSI")
      ("b" (funcall (lambda () (interactive)(ale/open-project "~/repos/blog"))) "blog dibalo")
      ("g" (funcall (lambda () (interactive)(ale/open-project "~/repos/gameoflife"))) "Game of Life")
      ("s" (funcall (lambda () (interactive)(ale/open-project "~/repos/commune_sub"))) "subs commune")
      ("m" (funcall (lambda () (interactive)(ale/open-project "~/repos/metoo"))) "textes Manue"))))

  ;; let access projects quickly in a convenient layout
  ;; (defhydra hydra-projects (:color teal :columns 3)
  ;;   "projects"
  ;;   ("d" (lambda () (interactive)(ale/open-project "~/Terminalcity/dotfiles")) "dotfiles")
  ;;   ("c" (lambda () (interactive)(ale/open-project "~/Terminalcity/curriculum")) "curriculum")
  ;;   ("e" (lambda () (interactive)(ale/open-project user-emacs-directory)) "emacs")
  ;;   ("E" (lambda () (interactive)(ale/open-project "~/outils/emacs")) "emacs source")
  ;;   ("E" (lambda () (interactive)(ale/open-project "~/outils/emacs")) "emacs source")
  ;;   ("q" nil "cancel"))
  (global-set-key (kbd "<f9>") 'hydra-projects/body)
  (global-set-key (kbd "C-c C-j") 'hydra-projects/body))

;;;;;;;;;;;;;
;; DEFAULT ;;
;;;;;;;;;;;;;
(defun load-default-env ()
  "Load the default environment depending on system type."
  (cond
   ;; MAC OS X
   ((string-equal "darwin" system-type)
    (load-file (expand-file-name "elisp/macos-environment.el" base-emacs-directory)))

   ;; windows
   ((or (string-equal "ms-dos" system-type)
        (string-equal "windows-nt" system-type)
        (string-equal "cygwin" system-type))
    (load-file (expand-file-name "elisp/windows-environment.el" base-emacs-directory)))

   ;; GNU
   (t (message "Unknown GNU systme detected. No specific conf applied."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD APPROPRIATE ENVIRONMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((equal "ene-natl-pc-075" (system-name))
  (load-kuutamo-env))
 ((or (equal "scw-0f23ec" (system-name))
      (equal "ns301170.ip-91-121-73.eu" (system-name)))
  (load-yuno-env))
 ((equal "highlander" (system-name))
  (load-highlander-env))
 (t (load-default-env)))

;;; env.el ends here
