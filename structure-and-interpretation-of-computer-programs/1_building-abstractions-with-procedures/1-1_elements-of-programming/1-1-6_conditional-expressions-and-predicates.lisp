;; 1.1.6 - Conditional Expressions and Predicates
; (cond (⟨predicate 1⟩ ⟨consequent expression 1⟩)
;     (⟨p_2⟩ ⟨e_2⟩)  -> clauses
;     ...
;     (⟨p_n⟩ ⟨e_n⟩))
(defun absoluteValue (x)
    (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(princ "The absolute value of 7 is: ")
(princ (absoluteValue 7)) (terpri)
(princ "The absoulte value of 0 is: ")
(princ (absoluteValue 0)) (terpri)
(princ "The absolute value of -7 is: ")
(princ (absoluteValue -7)) (terpri)

(defun absVal (x)
    (cond ((< x 0) (- x))
        (t x)))

(princ "The absolute of 9 is: ")
(princ (absVal 9)) (terpri)
(princ "The absolute of 0 is: ")
(princ (absVal 0)) (terpri)
(princ "The absolute of -9 is: ")
(princ (absVal -9)) (terpri)

; (if ⟨predicate⟩ ⟨consecuent⟩ ⟨alternative⟩)
(defun modulus (x)
    (if (< x 0)
        (- x)
        x))

(princ "The modulus of 3 is: ")
(princ (modulus 3)) (terpri)
(princ "The modulus of 0 is: ")
(princ (modulus 0)) (terpri)
(princ "The modulus of -3 is: ")
(princ (modulus -3)) (terpri)

; (and ⟨e_1⟩ ... ⟨e_n⟩)
; (or ⟨e_1⟩ ... ⟨e_n⟩)
; (not ⟨e⟩)
(defun greaterOrEqualTo (x y)
    (or (> x y) (= x y)))   ; It can also be expressed as...

(defun greaterThanOrEqualTo (x y)
    (not (< x y)))