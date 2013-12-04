
;; Org-mode key bindings (to remove from key-bindings.el)
(global-set-key (kbd "\C-cl") 'org-store-link)
(global-set-key (kbd "\C-ca") 'org-agenda)
(global-set-key (kbd "\C-cb") 'org-iswitchb)

;; notes in
(setq org-default-notes-file
      `(("." . ,(expand-file-name
                 (concat conf-dir ".notes")))))

;; font and faces customization
(setq org-todo-keyword-faces
      '(
        ("INPR" . (:foreground "yellow" :weight bold))
        ("STARTED" . (:foreground "yellow" :weight bold))
        ("WAIT" . (:foreground "yellow" :weight bold))
        ("INPROGRESS" . (:foreground "yellow" :weight bold))
        ))
