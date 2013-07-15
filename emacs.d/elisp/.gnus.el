;; LOCAL FILE FOR LOADING LOCAL STUFF!

;; got this line from one of the tutorials. Seemed interesting enough
(setq gnus-invalid-group-regexp "[:`'\"]\\|^$")
;; Make Gnus NOT ignore [Gmail] mailboxes
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; standard way of getting imap going
(setq gnus-select-method
      '(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-server-port 993)
               (nnimap-stream ssl)))

;; set up smtp so we can send from gmail too:
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "arthur.leothaud@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;;http://www.emacswiki.org/cgi-bin/wiki/GnusGmail
;;http://linil.wordpress.com/2008/01/18/gnus-gmail/

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; Threads are nice!
(setq gnus-summary-thread-gathering-function
      'gnus-gather-threads-by-subject)

(setq gnus-posting-styles
      '((".*"
         (name "Arthur Léothaud")
         )))

(setq user-full-name "Arthur Léothaud")
(setq user-mail-address "arthur.leothaud@gmail.com")
(setq send-mail-function 'smtpmail-send-it)
