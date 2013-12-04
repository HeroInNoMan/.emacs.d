(require 'newsticker)

; W3M HTML renderer isn't essential, but it's pretty useful.
(require 'w3m)
(setq newsticker-html-renderer 'w3m-region)

; We want our feeds pulled every 10 minutes.
(setq newsticker-retrieval-interval 3600)

; Setup the feeds. We'll have a look at these in just a second.
;; (setq newsticker-url-list-defaults nil)
(setq newsticker-url-list '(
                            ("xkcd" "http://xkcd.com/atom.xml" nil nil nil)
                            ("zen habits" "http://feeds.feedburner.com/zenhabits?format=xml" nil nil nil)
))

; Optionally bind a shortcut for your new RSS reader.
(global-set-key (kbd "C-c r") 'newsticker-treeview)

; Don't forget to start it!
;; (newsticker-start)
