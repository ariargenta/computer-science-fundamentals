; Searching for divisors
(defun divides-p (a b) (= (mod b a) 0))

(defun find-divisor (n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides-p test-divisor n) test-divisor)
          (t (find-divisor n (+ test-divisor 1)))))

(defun smallest-divisor (n) (find-divisor n 2))

(defun prime-p (n)
    (= n (smallest-divisor n)))

; Fermat's little theorem: If n is a prime number and a is any positive integer less than n, then a raised to the n^th power is congruent to a modulo n.
(defun is-even-p (n)
    (= (mod n 2) 0))

(defun expmod (base exponent m)
    (cond ((= exponent 0) 1)
          ((is-even-p exponent)
              (mod
                  (square (expmod base (/ exponent 2) m))
                   m))
          (t
            (mod
                (* base (expmod base (- exponent 1) m))
                m))))

(defun fermat-test (n)
    (labels ((try-it (a)
        (= (expmod a n n) a)))
    (try-it (+ 1 (random (- n 1))))))

(defun fast-prime-p (n times)
    (cond ((= times 0) T)
          ((fermat-test n) (fast-prime-p n (- times 1)))
          (t NIL)))