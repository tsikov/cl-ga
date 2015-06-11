; define constants

(defvar *crossover-rate* 0.6)
(defvar *mutation-rate* 0.001)

; define helper functions

(defun normalize-list (lst)
  (let ((sum-of-list (reduce #'+ lst)))
    (mapcar
      (lambda (num)
        (/ num sum-of-list))
      lst)))

(defun accumulate-list (lst)
  (let ((sum 0))
    (mapcar
      (lambda (num)
        (incf sum num)
        sum)
      lst)))

(defun rotate (list-of-lists)
  "Transposes an array. How it works? Magic.
  http://stackoverflow.com/questions/3513128/transposing-lists-in-common-lisp"
  (apply #'mapcar #'list list-of-lists))

(defun unzip (list)
  "Get a list and return two lists of the even elements and the odd elements"
  (loop for (x y) on list by #'cddr
        collect x into evens
        collect y into odds
        finally (return (list evens odds))))

(defun in-groups-of-two (lst)
  "Groups the elements of the list in lists of two. Returns the list of lists.
  If the list is of odd length, the last element of the last nested list is NIL."
  (rotate (unzip lst)))

; the bigger the number -> the closest to zero
; the smallest the number -> the closest to 1
(defun normalize-number (n)
  (/ 1 (+ n 1)))

; transform a number into a list of it's bits
; needed for the fitness fn
; shamelessly stolen from Stack Overflow
(defun number-to-binary-list (n &optional acc)
  (cond ((zerop n) (or acc (list 0)))
        ((plusp n)
         (number-to-binary-list (ash n -1) (cons (logand 1 n) acc)))
        (t (error "~S: non-negative argument required, got ~s" 'number-to-binary-list n))))

(defun add-zeroes (chromosome dataset)
  (append
    (make-list
      ; TODO add assert. number shoud be positive
      ; how much zeroes should we add?
      (-
        (length dataset)
        (integer-length chromosome))
      :initial-element 0)
    (number-to-binary-list chromosome)))

; end of helper functions

; TODO memoization
; TODO this function is too imperative. Must be rewritten in more functional style
(defun fitness-fn (chromosome)
  "Calculate the fitness of the chromosome."
  (let ((dataset '(1 6 7 3 4 3 6 9 2 5 10 5 5 9 7 3 2 6 1 1 1 10 4 3 7 7 3 8 1 10 3 1 8 2))
        (binary-chromosome)
        (sum 0)
        (product 1)
        (goal-sum 30)
        (goal-product 40))

    ; let defines the variables in paralel so...
    (setf binary-chromosome (add-zeroes chromosome dataset))

    ; calculate sum and product
    ; for each bit == 0 -> sum     += dataset[i]
    ; for each bit == 1 -> product *= dataset[i]

    (mapcar
      (lambda (b n)
        (if (= b 0)
          (incf sum n)
          (setf product (* product n))))
        binary-chromosome
        dataset)

    ; finally evaluate score = sqrt(((sum - goal-sum)^2) + ((product - goal-product)^2))
    ; and normalize the score = 1/(score + 1)
    (normalize-number
      (sqrt
        (+
          (expt (- sum goal-sum) 2)
          (expt (- product goal-product) 2))))))

; generate a random number between 0 and (2 ^ chromosome-length) - 1
(defun generate-chromosome (length-of-chromosome)
  (random
    (expt 2 length-of-chromosome)))

(defun generate-population (population-count length-of-chromosome)
  ; create a list of nils and generate a chromosome for each element of the list
  (mapcar
    (lambda (_)
      (generate-chromosome length-of-chromosome))
    (make-list population-count)))

(defun accumulated-normalized-fitnesses (population)
  (accumulate-list
    (normalize-list
      ; calculate each fitness
      (mapcar #'fitness-fn population))))

(defun draw (population accumulated-normalized-fitnesses)
  "Draw a chromosome from the population with a probability relative to it's fitness function"
  (let ((random-number (random 1.0)))
    (nth
      ; TODO implement binary search and use it here
      (loop
        for i from 0
        for af in accumulated-normalized-fitnesses
        do
        (when (<= random-number af) (return i)))
      population)))

(defun cross (chromosomes)
  (if (< (random 1.0) *crossover-rate*)
    ()
    chromosomes))

(defun next-generation (population)
  (let ((new-population)
        (accumulated-normalized-fitnesses (accumulated-normalized-fitnesses population)))
   (dotimes (i (length population) new-population)
     (push (draw population accumulated-normalized-fitnesses) new-population))
   ; cross and mutate
   (mapcar #'cross (in-groups-of-two new-population))))

(defun run (generations population-count length-of-chromosome)
  ; init a population
  (let (population (generate-population population-count length-of-chromosome))
    ; itarate next-genration generation-count times
    (dotimes (_ generation-count population)
      (setf population (next-generation population)))))

; lets do this!
(run 100 100 34)


