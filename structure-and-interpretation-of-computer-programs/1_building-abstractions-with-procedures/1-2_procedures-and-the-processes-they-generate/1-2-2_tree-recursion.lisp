; Tree recursion
(defun fib (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (t (+ (fib (- n 1))
                (fib (- n 2))))))

; Linear iterative
(defun fib-iter (a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

(defun fibbonaci (n)
    (fib-iter 1 0 n))

;; How many different ways can we make change of $1.00 USD, given half-dollars, quarters, dimes, nickels and pennies?
; The number of ways to change amount a using n kinds of coins equals:
; * The number of ways to change amount a using all but the first quind of coin plus
; * The number of ways to change amount a - d using all n kinds of coins, where d is the denomination of the first kind of coin
; Edge cases:
; * If a is exactly 0, we should count that as 1 way to make change
; * If a is less than 0, we should count that as 0 ways to make change
; * If n is 0, we should count that as 0 ways to make change
(defun count-change (amount) (cc amount 5))

(defun cc (amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (t (+ (cc amount
                (- kinds-of-coins 1))
             (cc (- amount
                    (first-denomination
                     kinds-of-coins))
                 kinds-of-coins)))))

(defun first-denomination (kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))

(princ (count-change 100)) (terpri)