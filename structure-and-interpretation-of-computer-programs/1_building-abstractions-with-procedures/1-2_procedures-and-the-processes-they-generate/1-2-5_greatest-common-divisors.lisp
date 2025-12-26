; Euclid's Algorithm
(defun greatestCommonDivisor (a b)
    (if (= b 0)
        a
        (greatestCommonDivisor b (mod a b))))

; Lame's Theorem: If Euclid's Algorithm requires k steps to compute the greatest common divisor of some pair, then the smallesr number in the pair must be grater than or equal to the kth Fibonacci number
