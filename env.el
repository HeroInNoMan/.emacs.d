;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; environment specific customisations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond
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

    ;; org-capture-templates
    (add-to-list 'org-capture-templates
                 '("d" "SFR - work log" entry (file+datetree (concat terminalcity-dir "SFR.org") "Diary") "* %i%?")
                 '("t" "SFR - TODO" entry (file+headline (concat terminalcity-dir "SFR.org") "À faire") "* TODO %?\n\t%i"))
    (message "machine dijon detected")))

 ;; machine perso
 ((equal "highlander" (system-name))
  (progn
    (message "machine highlander detected")))

 ;; default
 (t (progn
      (message "no specific machine detected"))))

;;; env.el ends here
