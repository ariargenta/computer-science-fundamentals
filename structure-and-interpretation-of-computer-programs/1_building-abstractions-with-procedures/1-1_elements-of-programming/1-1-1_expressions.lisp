;; Language elements
; - Primitives
;     - Primitive data
;     - Primitive procedures
; - Means of combination
;     - Procedure application
;     - Compound data
; - Means of abstraction
;     - Naming
;     - Compound procedures
;         - Block structure
;         - Higher order procedures
;     - Conventional interfaces (lists)
;     - Data abstraction
(princ "The value of a combination is obtained by applying the procedure specified by the operator to the arguments that are the values of the operands") (terpri)
(princ "([operator] [operand x] [operand y])") (terpri)
(princ "(+ 137 349)") (terpri)
(princ (+ 137 349)) (terpri)
(princ "(- 1000 334)") (terpri)
(princ (- 1000 334)) (terpri)
(princ "(* 5 99)") (terpri)
(princ (* 5 99)) (terpri)
(princ "(/ 10 5)") (terpri)
(princ (/ 10 5)) (terpri)
(princ "(+ 2.7 10)") (terpri)
(princ (+ 2.7 10)) (terpri)
(princ "(+ 21 35 12 7)") (terpri)
(princ (+ 21 35 12 7)) (terpri)
(princ "(* 25 4 12)") (terpri)
(princ (* 25 4 12)) (terpri)
(princ "(+ (* 3 5) (- 10 6))") (terpri)
(princ (+ (* 3 5) (- 10 6))) (terpri)   ; nested combinations

(princ "(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))") (terpri)

(princ (+ (* 3
    (+ (* 2 4)
        (+ 3 5)))
    (+ (- 10 7)
        6)))
    (terpri)