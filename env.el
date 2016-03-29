;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
 ;;;;;;;;;;;;;;
 ;; machine SFR
 ((equal "dijon" (system-name))
  (progn
    ;; set default browser to chromium-browser
    (setq browse-url-generic-program "chromium-browser")

    ;; specific agenda files
    (add-to-list 'org-agenda-files "~/Terminalcity/SFR.org")

    ;; confluence search
    (if (featurep 'engine-mode)
        (defengine confluence "http://confluence.sfrdev.fr/dosearchsite.action?queryString=%s" :keybinding "c"))

    ;; jenkins interaction
    (use-package butler
      :bind (:map butler-mode-map
                  ("n" . next-line)
                  ("p" . previous-line))
      :chords ("jb" . butler-status)
      :config (add-to-list 'butler-server-list
                           '(jenkins "SERVER-NAME"
                                     (server-address . "https://jenkins.sfrdev.fr/view/ecomfixe-git/")
                                     (auth-file . "~/.authinfo-jenkins.gpg")))) ;; machine SERVER-NAME login my_login password my_pass

    ;; org-capture-templates
    (add-to-list 'org-capture-templates '("d" "SFR - work log" entry (file+datetree (concat terminalcity-dir "SFR.org") "Diary") "* %i%?"))
    (add-to-list 'org-capture-templates '("t" "SFR - TODO" entry (file+headline (concat terminalcity-dir "SFR.org") "À faire") "* TODO %?\n\t%i"))

    ;; smaller font by default on dijon
    (if (featurep 'zoom-frm)
        (dotimes (number 2) (zoom-frm-out)))

    ;; open work log file
    (find-file (expand-file-name "~/Terminalcity/SFR.org"))))

 ;;;;;;;;;;;;;;;;
 ;; machine perso
 ((equal "highlander" (system-name))
  (progn
    ;; bigger font by default on laptop
    (if (featurep 'zoom-frm)
        (dotimes (number 5) (zoom-frm-in)))))

 ;;;;;;;;;;
 ;; default
 (t (progn
      (message "unknown system"))))

;;; env.el ends here
