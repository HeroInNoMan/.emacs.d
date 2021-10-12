
(defun syracuse-recursive (digit)
  "Apply the syracuse (or 3N+1) rules to DIGIT recursively until it reaches 1."
  (message (number-to-string digit))
  (cond ((= 1 digit) (message "Done!"))
        ((= (mod digit 2) 0) (syracuse (/ digit 2)))
        (t (syracuse (1+ (* digit 3))))))

(defun syracuse (digit)
  "Run the syracuse algorithm starting with DIGIT and show some
statistics ( number of steps, upper bound ) ."
  (interactive "NEnter a number: ")
  (message "Starting Syracuse suite with %s" digit)
  (let ((steps '())
        (current digit)
        (counter 0))
    (while (not (member current steps))
      (progn
        (push current steps)
        (message (number-to-string current))
        (cl-incf counter)
        (setq current (cond ((= (mod current 2) 0) (/ current 2))
                            (t (1+ (* current 3)))))))
    (message "Loop reached in %s steps (upper bound: %s)" (1- counter) (reduce #'max steps))))

;; (defun swap-sort (elements)
;;   "Sort ELEMENTS as a list using the swap sort algorithm."
;;   (interactive "P")
;;   (let ((sorted-elements elements)
;;         (n (length sorted-elements)))
;;     (cl-loop
;;      for i from 1 to n
;;      do (cl-loop
;;          for j from 1 to n
;;          do (if (< (nth i) (nth j)))
;;          (swap-elements sorted-elements i j)))))

;; (defun swap-elements (collection index1 index2)
;;   "Swap elements INDEX1 and INDEX2 in COLLECTION."
;;   (let ((v (vconcat collection))
;;         (a (aref 'v index1))
;;         (b (aref 'v index2)))
;;     (aset v index1 b)
;;     (aset v index2 a)
;;     (append v nil)))

;; (swap-elements '(a b c d e f g h i j) '4 '8 )
