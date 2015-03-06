; binary search
; returns the position of an element in a sorted array

(defun binary-search (lst n)
  (let ((l 0)
        (r (- (length lst) 1))
        (mid))
    (loop while (<= l r)
          do (setf mid (/ (+ l r) 2))
          (cond (< n (nth mid lst)) (setf r (- mid 1))
                (> n (nth mid lst)) (setf l (+ mid 1))
                (t return mid)))))

