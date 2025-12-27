; (lambda (⟨formal-parameters⟩) ⟨body⟩)
(lambda (x) (+ x 4))
(lambda (x) (/ 1.0 (* x (+ x 2))))

(defun square (x) (* x x))

(defun sum (term a next b)
    (if (> a b)
        0
        (+ (funcall term a)
           (sum term (funcall next a) next b))))

(defun pi-sum (a b)
    (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
         a
         (lambda (x) (+ x 4))
         b))

(defun integral (f a b dx)
    (* (sum f
            (+ a (/ dx 2.0))
            (lambda (x) (+ x dx))
            b)
       dx))

; Equivalent declaration
(defun plus4 (x) (+ x 4))
(defparameter plus-four (lambda (x) (+ x 4)))

(princ ((lambda (x y z) (+ x y (square z)))
            1 2 3)) (terpri)