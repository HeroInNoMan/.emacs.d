;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
 ;;;;;;;;
 ;; DIJON
 ((equal "dijon" (system-name))
  (progn
    ;; set default browser to chromium-browser
    (setq browse-url-generic-program "chromium-browser")

    ;; specific agenda files
    (add-to-list 'org-agenda-files "~/Terminalcity/SFR.org")

    ;; confluence search
    (when (featurep 'engine-mode)
      (defengine confluence "http://confluence.sfrdev.fr/dosearchsite.action?queryString=%s" :keybinding "c"))

    ;; jenkins interaction
    (use-package butler
      :bind (:map butler-mode-map
                  ("n" . next-line)
                  ("p" . previous-line))
      :chords ("jb" . butler-status)
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

    ;; open work log file
    (find-file (expand-file-name "~/Terminalcity/SFR.org"))

    ;; change theme
    (color-theme-oswald)))

 ;;;;;;;;;;;;
 ;; POLOPECHE
 ((equal "ns301170.ip-91-121-73.eu" (system-name))
  (progn
    (global-hl-line-mode -1) ;; don’t highlight current line
    (color-theme-calm-forest)))

 ;;;;;;;;;;;;;
 ;; HIGHLANDER
 ((equal "highlander" (system-name))
  (progn
    ;; bigger font by default on laptop
    (when (featurep 'zoom-frm)
      (progn
        (zoom-frm-unzoom)
        (dotimes (number 3) (zoom-frm-in))))
    ;; display battery level
    (use-package fancy-battery
      :config
      (add-hook 'after-init-hook #'fancy-battery-mode)
      (setq fancy-battery-show-percentage t))
    (color-theme-oswald)))

 ;;;;;;;;;;
 ;; DEFAULT
 (t (progn
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
        (global-set-key (kbd "C-M-z") 'undo))) ;; useful when C-/ does not work (windows/putty)

      (message "unknown system"))))

;;; env.el ends here
