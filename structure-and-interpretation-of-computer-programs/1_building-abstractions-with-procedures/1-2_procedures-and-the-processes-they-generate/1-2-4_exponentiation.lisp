(defun square (x) (* x x))

; Recursive definition
; b^n = b * b^(n - 1),
; b^0 = 1
(defun recursive-exponentiation (b n)
    (if (= n 0)
        1
        (* b (recursive-exponentiation b (- n 1)))))

; Linear iteration
(defun expt-iter (b counter product)
    (if (= counter 0)
        product
        (expt-iter b
                   (- counter 1)
                   (* b product))))

(defun iterative-exponentiation (b n)
    (expt-iter b n 1))

; Successive squaring definition
; b^n = (b^(n/2))^2     if n is even
; b^n = (b * b^(n - 1))     if n is odd;
(defun is-even-p (n)
    (= (mod n 2) 0))

(defun fast-exp (b n)
    (cond ((= n 0) 1)
          ((is-even-p n) (square (fast-exp b (/ n 2))))
          (t (* b (fast-exp b (- n 1))))))