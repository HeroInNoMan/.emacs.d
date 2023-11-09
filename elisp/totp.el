;; Taken from Jürgen Hötzel's `totp.el':
;; https://github.com/juergenhoetzel/emacs-totp
(require 'bindat)
(require 'gnutls)
(require 'hexl)
(require 'auth-source)

(defun totp--hex-decode-string (string)
  "Hex-decode STRING and return the result as a unibyte string."
  (apply #'unibyte-string
         (seq-map (lambda (s) (hexl-htoi (aref s 0) (aref s 1)))
                  (seq-partition string 2))))

(defun totp (string &optional time digits)
  "Return a TOTP token using the secret hex STRING and current time.
TIME is used as counter value instead of current time, if non-nil.
DIGITS is the number of pin digits and defaults to 6."
  (let* ((key-bytes (totp--hex-decode-string (upcase string)))
         (counter (truncate (/ (or time (time-to-seconds)) 30)))
         (digits (or digits 6))
         (format-string (format "%%0%dd" digits))
         ;; we have to manually split the 64 bit number (u64 not supported in Emacs 27.2)
         (counter-bytes (bindat-pack  '((:high u32) (:low u32))
                                      `((:high . ,(ash counter -32)) (:low . ,(logand counter #xffffffff)))))
         (mac (gnutls-hash-mac 'SHA1 key-bytes counter-bytes))
         (offset (logand (bindat-get-field (bindat-unpack '((:offset u8)) mac 19) :offset) #xf)))
    (format format-string
            (mod
             (logand (bindat-get-field (bindat-unpack '((:totp-pin u32)) mac  offset) :totp-pin)
                     #x7fffffff)
             (expt 10 digits)))))

(defconst base32-alphabet
  (let ((tbl (make-char-table nil)))
    (dolist (mapping '(("A" . 0) ("B" . 1) ("C" . 2) ("D" . 3)
                       ("E" . 4) ("F" . 5) ("G" . 6)
                       ("H" . 7) ("I" . 8) ("J" . 9) ("K" . 10)
                       ("L" . 11) ("M" . 12) ("N" . 13)
                       ("O" . 14) ("P" . 15) ("Q" . 16) ("R" . 17)
                       ("S" . 18) ("T" . 19) ("U" . 20)
                       ("V" . 21) ("W" . 22) ("X" . 23) ("Y" . 24)
                       ("Z" . 25) ("2" . 26) ("3" . 27)
                       ("4" . 28) ("5" . 29) ("6" . 30) ("7" . 31)))
      (aset tbl (string-to-char (car mapping)) (cdr mapping)))
    tbl)
  "Base-32 mapping table, as defined in RFC 4648.")

(defun base32-hex-decode (string)
  "The cheats' version of base-32 decode.

This is not a 100% faithful implementation of RFC 4648. The
concept of encoding partial quanta is not implemented fully.

No attempt is made to pad the output either as that is not
required for HMAC-TOTP."
  (unless (mod (length string) 8)
    (error "Padding is incorrect"))
  (setq string (upcase string))
  (let ((trimmed-array (append (string-trim-right string "=+") nil)))
    (format "%X" (seq-reduce
                  (lambda (acc char) (+ (ash acc 5) (aref base32-alphabet char)))
                  trimmed-array 0))))

(defun totp-display (auth)
  "Select a TOTP AUTH from `auth-sources' and display its TOTP."
  (auth-source-forget-all-cached)
  (interactive
   (list
    (let ((candidates
           (mapcar
            (lambda (auth)
              (cons
               (format
                "User '%s' on %s"
                (propertize (plist-get auth :user) 'face 'font-lock-keyword-face)
                (propertize (plist-get auth :host) 'face 'font-lock-string-face))
               auth))
            (seq-filter
             (lambda (auth) (string-prefix-p "TOTP:" (plist-get auth :host)))
             (auth-source-search :max 10000)))))
      (cdr (assoc (completing-read "Pick a TOTP> " candidates) candidates)))))
  (let ((code (totp (base32-hex-decode (funcall (plist-get auth :secret))))))
    (message "Your TOTP for '%s' is: %s (sent to kill ring)"
             (propertize (plist-get auth :host) 'face font-lock-keyword-face)
             (propertize code 'face 'font-lock-string-face))
    (kill-new code)
    code))

(provide 'totp)
;; EOF
