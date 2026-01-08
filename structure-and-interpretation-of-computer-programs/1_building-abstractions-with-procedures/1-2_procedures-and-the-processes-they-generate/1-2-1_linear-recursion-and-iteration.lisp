;; Linear recursive process
; Deferred operations
; 1. Wishful thinking
; 2. Decompose the problem
; 3. Identify non-decomposable (smallest) problems
(defun factorial (n)
    (if (= n 1)                         ; Test for base case
        1                               ; Base case
        (* n (factorial (- n 1)))))     ; Recursive case

;; Linear iterative process
; Iterative algorithms uses constant space and have no pending operations when the procedure calls itself
; - Figure out a way to accumulate partial answers
; - Write out a table to analyze precisely
;     - Initialization of first row
;     - Update rules for other rows
;     - How to know when to stop
(defun fact-iter (product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))

(defun factorialProduct (n)
    (fact-iter 1 1 n))

;; Block structure
(defun factProd (n)
    (labels ((iter (product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1)))))
        (iter 1 1)))

; - Find the base case and create solution
; - Determine how to reduce to a simpler version of same problem, plus some additional operations
; - Assume code will work for simpler problem and design solution to extended problem