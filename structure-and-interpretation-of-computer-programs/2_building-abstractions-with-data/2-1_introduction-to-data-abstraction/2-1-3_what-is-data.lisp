;;; Equivalence between data and procedures
;; Data is behaviour, not storage
; A datum is defined by its interface, not its representation. Any entity that satisfies the contract of a data type IS a that data type, regardless of its underlying implementation.

; Treat a pair as a single unit
; - can pass a pair as argument
; - can return a pair as a value

;;; Pair abstraction
; Message passing
; Abstraction: A way of capturing computational elements and treating them as if they were primitives. Set in another way, a method of isolating the details of a computation from the use of a computation
; - Need a way of gluing data elements together into a unit that can be treated as a simple data element
; - Need ways of getting the pieces back out
; - Need a contract between the "glue" and the "unglue"

;; Constructor
; cons: A, B -> A X B
; cons: A, B -> Pair<A, B>
; (cons <x-exp> <y-exp> ==> <P>)
(defun construct (x y)
    (lambda (m)
        (cond ((= m 0) x)
              ((= m 1) y)
              (t (error "Argument not 0 or 1: CONS ~a" m)))))

;; Accessors
; car: Pair<A, B> -> A
; (car <P>) ==> <x-val>
(defun contents-of-address-part-of-register (z)
    (funcall z 0))

; cdr: Pair<A, B> -> B
; (cdr <P>) ==> <y-val>
(defun contents-of-decrement-part-of-register (z)
    (funcall z 1))

; Predicate
; pair? anytype -> boolean
; (pair? <z>)
;    ==> #t if <z> evaluates to a pair, else #f