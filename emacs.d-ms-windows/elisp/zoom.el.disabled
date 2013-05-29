
;; Firefox-like zooming of fonts. (does not work in Windows Emacs)
(setq default-font-zoom-index 2)
(setq font-zoom-index default-font-zoom-index)
(setq font-zoom-list
      (list "-*-fixed-*-*-*-*-6-*-*-*-*-*-*-*"
            "-*-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
            "-*-fixed-*-*-*-*-13-*-*-*-*-*-*-*"
            "-*-fixed-*-*-*-*-15-*-*-*-*-*-*-*"
            "-*-fixed-*-*-*-*-17-*-*-*-*-*-*-*"
            "-*-fixed-*-*-*-*-18-*-*-*-*-*-*-*"
            "-*-fixed-*-*-*-*-24-*-*-*-*-*-*-*"
            "-*-fixed-*-*-*-*-36-*-*-*-*-*-*-*"
            "-*-fixed-*-*-*-*-48-*-*-*-*-*-*-*"))
(defun font-zoom-increase-font-size ()
  (interactive)
  (progn
    (setq font-zoom-index (min (- (length font-zoom-list) 1)
                               (+ font-zoom-index 1)))
    (set-frame-font (nth font-zoom-index font-zoom-list))))

(defun font-zoom-decrease-font-size ()
  (interactive)
  (progn
    (setq font-zoom-index (max 0
                               (- font-zoom-index 1)))
    (set-frame-font (nth font-zoom-index font-zoom-list))))

(defun font-zoom-reset-font-size ()
  (interactive)
  (progn
    (setq font-zoom-index default-font-zoom-index)
    (set-frame-font (nth font-zoom-index font-zoom-list))))

(set-frame-font (nth font-zoom-index font-zoom-list))
