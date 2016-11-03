;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;
;; DIJON ;;
;;;;;;;;;;;
(defun load-kuutamo-env ()
  (progn
    ;; specific agenda files
    (add-to-list 'org-agenda-files "~/Terminalcity/Renault.org")

    ;; hooking for specific functions
    (add-hook 'git-commit-setup-hook 'insert-ticket-prefix)

    ;; org-capture-templates
    (add-to-list 'org-capture-templates '("d" "Renault - work log" entry (file+datetree (concat terminalcity-dir "Renault.org") "Diary") "* %i%?"))
    (add-to-list 'org-capture-templates '("t" "Renault - TODO" entry (file+headline (concat terminalcity-dir "Renault.org") "À faire") "* TODO %?\n\t%i"))

    ;; display battery level
    (use-package fancy-battery
      :config
      (setq fancy-battery-show-percentage t)
      (fancy-battery-mode))

    ;; default project root folder
    (when (featurep 'dumb-jump)
      (setq dumb-jump-default-project "~/projets"))

    ;; open work log file
    (find-file (expand-file-name "~/Terminalcity/Renault.org"))

    ;; change theme
    (color-theme-oswald)))

(defun load-dijon-env ()
  (progn
    ;; set default browser to chromium-browser
    (setq browse-url-generic-program "chromium-browser")

    ;; specific agenda files
    (add-to-list 'org-agenda-files "~/Terminalcity/SFR.org")

    ;; hooking for specific functions
    (add-hook 'git-commit-setup-hook 'insert-ticket-prefix)

    ;; confluence search
    (when (featurep 'engine-mode)
      (defengine confluence "http://confluence.sfrdev.fr/dosearchsite.action?queryString=%s" :keybinding "c"))


    ;; gitlab interaction
    (use-package gitlab
      :config
      (setq gitlab-host my-private-gitlab-host
            gitlab-username my-private-gitlab-username
            gitlab-password my-private-gitlab-password
            gitlab-token-id my-private-gitlab-token-id))

    (use-package helm-gitlab)

    ;; jenkins interaction
    (use-package butler
      :bind (:map butler-mode-map
                  ("n" . next-line)
                  ("p" . previous-line))
      :config
      (add-to-list 'god-exempt-major-modes 'butler-mode)
      (add-to-list 'butler-server-list
                   '(jenkins "SERVER-NAME"
                             (server-address . "https://jenkins.sfrdev.fr/view/ecomfixe-git/")
                             (auth-file . "~/.authinfo-jenkins.gpg")))) ;; machine SERVER-NAME login my_login password my_pass

    ;; org-capture-templates
    (add-to-list 'org-capture-templates '("d" "SFR - work log" entry (file+datetree (concat terminalcity-dir "SFR.org") "Diary") "* %i%?"))
    (add-to-list 'org-capture-templates '("t" "SFR - TODO" entry (file+headline (concat terminalcity-dir "SFR.org") "À faire") "* TODO %?\n\t%i"))

    ;; smaller font by default on dijon
    (when (featurep 'zoom-frm)
      (progn
        (zoom-frm-unzoom)
        (dotimes (number 4) (zoom-frm-out))))

    ;; default project root folder
    (when (featurep 'dumb-jump)
      (setq dumb-jump-default-project "~/projets"))

    ;; open work log file
    (find-file (expand-file-name "~/Terminalcity/SFR.org"))

    ;; change theme
    (color-theme-oswald)))

;;;;;;;;;;;;;;;
;; POLOPECHE ;;
;;;;;;;;;;;;;;;
(defun load-polopeche-env ()
  (progn
    ;; don’t highlight current line
    (global-hl-line-mode -1)

    ;; change theme
    (color-theme-calm-forest)))

;;;;;;;;;;;;;;;;
;; HIGHLANDER ;;
;;;;;;;;;;;;;;;;
(defun load-highlander-env ()
  (progn
    ;; bigger font by default on laptop
    (when (featurep 'zoom-frm)
      (progn
        (zoom-frm-unzoom)
        (dotimes (number 3) (zoom-frm-in))))

    ;; display battery level
    (use-package fancy-battery
      :config
      (setq fancy-battery-show-percentage t)
      (fancy-battery-mode))

    (use-package transmission
      :config
      (setq
       transmission-host my-private-transmission-host
       transmission-service my-private-transmission-service
       transmission-rpc-path my-private-transmission-rpc-path))

    ;; change theme
    (color-theme-oswald)))

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
      ;; useful when C-/ does not work (windows/putty)
      (global-set-key (kbd "C-M-z") 'undo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOAD APPROPRIATE ENVIRONMENT ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cond
 ((equal "dijon" (system-name))
  (load-dijon-env))
 ((equal "kuutamo" (system-name))
  (load-kuutamo-env))
 ((equal "ns301170.ip-91-121-73.eu" (system-name))
  (load-polopeche-env))
 ((equal "highlander" (system-name))
  (load-highlander-env))
 (t (load-default-env)))

;;; env.el ends here
