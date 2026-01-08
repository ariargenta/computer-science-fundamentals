;; Compound Procedures
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

;; Substitution model
; - If expression is self-evaluating, just return value
; - If expression is a name, replace with value associated with that name
; - If expression is a lambda, create procedure and return
; - If expression is a special form, follow specific rules for evaluating (or not) subexpressions
; - If expression is a combination
;     - Evaluate subexpressions in any order
;     - If first subexpressions is a primitive (or build-in) procedure, just apply it to values of other subexpressions
;     - If first subexpression is a compound procedure (created by lambda), substitute value of each subexpression for corresponding procedure parameter in body of procedure, then repeat on body