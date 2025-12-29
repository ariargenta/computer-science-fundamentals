;;; Equivalence between data and procedures
;; Data is behaviour, not storage
; A datum is defined by its interface, not its representation. Any entity that satisfies the contract of a data type IS a that data type, regardless of its underlying implementation.
; Message passing
; cons
(defun construct (x y)
    (lambda (m)
        (cond ((= m 0) x)
              ((= m 1) y)
              (t (error "Argument not 0 or 1: CONS ~a" m)))))

; car
(defun contents-of-address-part-of-register (z)
    (funcall z 0))

; cdr
(defun contents-of-decrement-part-of-register (z)
    (funcall z 1))