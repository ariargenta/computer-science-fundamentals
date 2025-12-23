;;;; Chapter 1 - Building Abstractions with Procedures
;;; 1.1 - Elements of Programming
;; 1.1.1 - Expressions
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

;; 1.1.2 - Naming and the Environment
(terpri)
(princ "Variables") (terpri)

(defvar \*size\* 2)

(princ \*size\*) (terpri)
(princ (* 5 \*size\*)) (terpri)

(defvar \*pi\* 3.14159)
(defvar \*radius\* 10)

(princ (* pi (* \*radius\* \*radius\*))) (terpri)

(defvar \*circumference\* (* 2 \*pi\* \*radius\*))

(princ \*circumference\*) (terpri)

;; 1.1.3 - Evaluating Combinations
(terpri)
(princ (* (+ 2 (* 4 6)) (+ 3 5 7))) (terpri)

;                            390
;                           / / \
;                          / /   \
;                         / /     \
;                        / /       \
;                       * 26        15
;                        /|\       /|\ \
;                       / | \     / | \ \
;                      +  2 24   +  3  5  7
;                           /|\
;                          / | \
;                         *  4  6