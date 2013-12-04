

;; (defun assoc-or-error (what where &optional err-string)
;;   (or (assoc what where)
;;       (error (or err-string
;;                  "Suddenly errors! Thousands of them!"))))

;; (defun google-reader-to-newsticker (filename)
;;   "Parses your google-reader feed subscription XML
;;  and returns newsticker compatible feed alist."
;;   (with-temp-buffer
;;     (insert-file-contents filename)
;;     (let* ((opml (assoc-or-error
;;                   'opml
;;                   (xml-parse-region (point-min)
;;                                     (point-max))
;;                   "Not a valid feed XML!"))
;;            (body (assoc-or-error
;;                   'body
;;                   opml
;;                   "Not a valid feed XML!"))
;;            (outlines (delq nil
;;                            (map 'list
;;                                 (lambda (element)
;;                                   (and (consp element)
;;                                        (equal (car element)
;;                                               'outline)
;;                                        element))
;;                                 body)))
;;            (alist
;;             (map 'list
;;                  (lambda (outline)
;;                    (let ((title (assoc-or-error
;;                                  'title
;;                                  outline
;;                                  (concat "No title in an outline "
;;                                          "- malformed feed XML?"))
;;                                 (feed (assoc-or-error
;;                                        'xmlUrl
;;                                        outline
;;                                        (concat "No feed link in an outline "
;;                                                '"- check your feed XML!")))
;;                                 (list (cdr title) (cdr feed))))
;;                      (map 'list
;;                           #'cadr
;;                           outlines))))
;;             alist)))))

;; (setq newsticker-url-list
;;   (google-reader-to-newsticker "/media/Sauvegarder/Arthur/subscriptions.xml"))
