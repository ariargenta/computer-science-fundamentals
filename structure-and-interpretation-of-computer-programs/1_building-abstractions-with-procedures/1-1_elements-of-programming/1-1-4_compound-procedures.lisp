;; 1.1.4 Compound Procedures
; (define (⟨name⟩ ⟨formal parameters⟩)
;     ⟨body⟩)
(defun square (x) (* x x))

(princ "21^2 = ")
(princ (square 21)) (terpri)
(princ "(2 + 5)^2 = ")
(princ (square (+ 2 5))) (terpri)
(princ "((3)^2)^2 = ")
(princ (square (square 3))) (terpri)

(defun sum-of-squares (x y)
    (+ (square x) (square y)))

(princ "3^2 + 4^2 = ")
(princ (sum-of-squares 3 4)) (terpri)

(defun f (a)
    (sum-of-squares (+ a 1) (* a 2)))

(princ "Compound procedure for 5: ")
(princ (f 5)) (terpri)