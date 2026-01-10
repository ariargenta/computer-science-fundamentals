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

(defun summatory (term a next b)
    (if (> a b)
        0
        (+ (funcall term a)
           (summatory term
                      (funcall next a)
                      next b))))

(defun sum-integers1 (a b)
    (sum (lambda (x) x)
         a
         (lambda (x) (+ x 1))
         b))

(defun sum-squares1 (a b)
    (funcall #'sum #'square a (lambda (x) (+ x 1)) b))

(defun pi-sum1 (a b)
    (sum (lambda (x) (/ 1 (square x))) a
         (lambda (x) (+ x 2)) b))

; Equivalent declaration
(defun plus4 (x) (+ x 4))
(defparameter plus-four (lambda (x) (+ x 4)))

(princ ((lambda (x y z) (+ x y (square z)))
            1 2 3)) (terpri)

; (let ((⟨var_1⟩ ⟨exp_1⟩)
;       (⟨var_2⟩ ⟨exp_2⟩)
;           ...
;       (⟨var_n⟩ ⟨exp_n⟩))
;   ⟨body⟩)
(defun f (x y)
    (labels ((f-helper (a b)
        (+ (* x (square a))
           (* y b)
           (* a b))))
    (f-helper (+ 1 (* x y))
                (- 1 y))))

(defun fn (x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
        (+ (* x (square a))
           (* y b)
           (* a b))))

(defun funct (x y)
    ((lambda (a b)
         (+ (* x (square a))
            (* y b)
            (* a b)))
     (+ 1 (* x y))
     (- 1 y)))

(defun fun (x y)
    (let ((a (+ 1 (* x y)))
          (b (- 1 y)))
        (+ (* x (square a))
           (* y b)
           (* a b))))