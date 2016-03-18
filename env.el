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

    ;; open work log file
    (find-file (expand-file-name "~/Terminalcity/SFR.org"))))

 ;; machine perso
 ((equal "highlander" (system-name))
  (progn
    ;; bigger font by default on laptop
    (dotimes (number 5) (zoom-frm-in))))

 ;; default
 (t (progn
      (message "unknown system"))))

;;; env.el ends here
