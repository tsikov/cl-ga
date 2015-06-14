; define constants

(defvar *crossover-rate* 0.5)
(defvar *mutation-rate* 0.15)

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

(defun best-fitness (population)
  (apply #'max (mapcar (memd-fitness-fn) population)))

(defun memoize (fn)
  "From Wikipedia: Memoization is an optimization technique used primarily to
  speed up computer programs by storing the results of expensive function
  calls and returning the cached result when the same inputs occur again.
  This implementation gets a function and returns another function from whitin
  a lexical scope that contains a *cache* variable which we use to store the
  arguments and corresponding results from function calls. The lexical scope is
  important for us in order to be able to access the variable the second/third/etc...
  time the anonymous, inner function is called."
  (let ((*cache* (make-hash-table)))
   (lambda (x)
     ; how many entries are in the *cache*? Uncomment to check.
     ; (print (hash-table-count *cache*))

     ; check if the argument is in the cache. (gethash) has multiple return
     ; values. to see if the key is present in the hash-table we should get
     ; the second value.
     (if (multiple-value-bind (value present) (gethash x *cache*) present)
       (gethash x *cache*)
       (setf (gethash x *cache*) (funcall fn x))))))

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

; needed for the fitness fn
; shamelessly stolen from Stack Overflow
(defun number-to-binary-list (n &optional acc)
  "Transform a number into a list of it's bits."
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

(defun memd-fitness-fn () (memoize #'fitness-fn))

(defun generate-chromosome (length-of-chromosome)
  "Generate a random number between 0 and (2 ^ chromosome-length) - 1."
  (random
    (expt 2 length-of-chromosome)))

(defun generate-population (population-count length-of-chromosome)
  "Create a list of nils and generate a chromosome for each element of the list."
  (mapcar
    (lambda (_) _ (generate-chromosome length-of-chromosome))
    (make-list population-count)))

(defun accumulated-normalized-fitnesses (population)
  (accumulate-list
    (normalize-list
      ; calculate each fitness
      (mapcar (memd-fitness-fn) population))))

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

(defun split-chromosome-by-index (chromosome idx)
  "Given a chromosome and an index, return two chromosomes for the split placed."
  (let ((first-part (logand chromosome (- (expt 2 idx) 1))))
    (list first-part (- chromosome first-part))))

(defun mutate (length-of-chromosome chromosomes)
  "Every bit in the chromosome has a *mutation-rate* chance of being flipped."
  (mapcar
    (lambda (chromosome)
      (dotimes (i length-of-chromosome chromosome)
        (when (< (random 1.0) *mutation-rate*)
          ; bitflip the bit in question
          (setf chromosome (logxor (expt 2 i) chromosome)))))
    chromosomes))

(defun cross (chromosomes length-of-chromosome)
  "Has a *crossover-rate* chance to cross two cromosomes. If it doesn't -> just returns the
  chromosomes. If it does -> randomly splits the chromosomes in two and swaps their bits. After
  this operation is completed, initiate mutation. Only crossed chromosomes have a chance of being
  mutated."
  (if (< (random 1.0) *crossover-rate*)
    (let ((lst (mapcar #'split-chromosome-by-index
              chromosomes

              ; create a list with two random indexes for the chromosomes to be split
              ; idx is in [0..chromosome-length-1]
              ; TODO off by 1 error with indexes - sometimes no change in chromosomes (when 0)
              (make-list 2 :initial-element (random (- length-of-chromosome 1))))))

      ; sum the first element of first list with second element of second list and SF with FS
      ; mutate and return the result
      (mutate length-of-chromosome (list (+ (caar lst) (cadadr lst)) (+ (cadar lst) (caadr lst)))))

    ; no crossover -> return the chromosomes intact
    chromosomes))

(defun next-generation (population length-of-chromosome)
  "Get a population and return the new population."
  ; print the the best fitness from the population
  (print (best-fitness population))

  (let ((new-population)
        (accumulated-normalized-fitnesses (accumulated-normalized-fitnesses population)))
   (dotimes (i (length population) new-population)
     (push (draw population accumulated-normalized-fitnesses) new-population))
   ; cross and mutate
   (apply #'append (mapcar #'cross
           (in-groups-of-two new-population)
           (make-list (/ length-of-chromosome 2) :initial-element length-of-chromosome)))))

(defun run (generations population-count length-of-chromosome)
  "Iterate 'generations' times over 'next-generation' function."
  ; init a population
  (let ((population (generate-population population-count length-of-chromosome)))
    ; itarate next-genration generation-count times
    (dotimes (_ generations population)
      (setf population (next-generation population length-of-chromosome)))))

; lets do this!
(run 100 100 34)


