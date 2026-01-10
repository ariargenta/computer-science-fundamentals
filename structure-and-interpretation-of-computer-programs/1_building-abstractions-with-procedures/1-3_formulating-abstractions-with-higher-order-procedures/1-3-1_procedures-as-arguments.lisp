; Types and Higher-order operations and procedures
; Since procedures operate objects, and return values; we can define their types as well
; We will denote a procedures type by indicating the types of each of its arguments, and the type of the returned value, plus the symbol -> to indicate that the arguments are mapped to the return value. Procedure types  indicate
; - Number of arguments required
; - Type of each argument
; - Type of result of the procedure
;
; The type of a procedure is a contract. A type describes a set of values. Every value has a type
; - If the operands have the specified types, the procedure will result in a value of the specified type
; - Otherwise, its behaviour is undefined

(defun sumIntegers (a b)
    (if (> a b)
        0
        (+ a (sum-integers (+ 1 a) b))))

(defun sum-squares (a b)
    (if (> a b)
        0
        (+ (square a)
           (sum-squares (+ 1 a) b))))

(defun piSum (a b)
    (if (> a b)
        0
        (+ (/ 1 (square a))
           (piSum (+ a 2) b))))

(defun cube (x) (* x x x))

(defun sum-integers (a b)
    (if (> a b)
        0
        (+ a (sum-integers (+ a 1) b))))

(defun sum-cubes (a b)
    (if (> a b)
        0
        (+ (cube a)
           (sum-cubes (+ a 1) b))))

(defun pi-sum (a b)
    (if (> a b)
        0
        (+ (/ 1.0 (* a (+ a 2)))
           (pi-sum (+ a 4) b))))

; (define (⟨name⟩ a b)
;   (if (> a b)
;       0
;       (+ (⟨term⟩ a)
;       (⟨name⟩ (⟨next⟩ a) b))))
(defun sum (term a next b)
    (if (> a b)
        0
        (+ (funcall term a)
           (sum term (funcall next a) next b))))

(defun inc (n) (+ n 1))

(defun sum-cubics (a b)
    (sum #'cube a #'inc b))

(princ (sum-cubics 1 10)) (terpri)

(defun sum-int (a b)
    (sum #'identity a #'1+ b))

(princ (sum-int 1 10)) (terpri)

(defun sum-pi (a b)
    (labels ((pi-term (x)
                      (/ 1.0 (* x (+ x 2))))
             (pi-next (x)
                      (+ x 4)))
        (sum #'pi-term a #'pi-next b)))

(princ (* 8 (sum-pi 1 1000))) (terpri)

(defun integral (f a b dx)
    (labels ((add-dx (x)
                     (+ x dx)))
        (* (sum f (+ a (/ dx 2.0)) #'add-dx b)
            dx)))

(princ (integral #'cube 0 1 0.01)) (terpri)
(princ (integral #'cube 0 1 0.001)) (terpri)