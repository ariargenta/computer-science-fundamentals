;; A language that supports the concept that "equals can be substituted for equals" in an expression without changing the value of the expression is said to be <<referentially transparent>>.

; Mutation is a powerful idea... But introduces substantial complexity
; - Can have unexpected surprising side effects
; - Enables new and efficient data structures
; - Breaks our "functional" programming (substitution) model because is no longer sufficient to explain behaviour

(defun make-simplified-withdraw (balance)
    (lambda (amount)
        (setf balance (- balance amount))
        balance))

(defun make-decrementer (balance)
    (lambda (amount)
        (- balance amount)))

;; Pitfalls of imperative programming
(defun factorial (n)
    (let ((product 1)
          (counter 1))
        (labels ((iter ()
            (if (> counter n)
                product
                (progn (setf product (* counter product))
                       (setf counter (+ counter 1))
                       (iter)))))
            (iter))))