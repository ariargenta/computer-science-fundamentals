;; Finding roots of equations
(defun average (x y)
    (/ (+ x y) 2.0))

(defun close-enough-p (x y)
    (< (abs (- x y)) 0.001))

(defun search-root (f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough-p neg-point pos-point)
            midpoint
            (let ((test-value (funcall f midpoint)))
                (cond ((plusp test-value)
                       (search-root f neg-point midpoint))
                      ((minusp test-value)
                       (search-root f midpoint pos-point))
                      (t midpoint))))))

(defun half-interval-method (f a b)
    (let ((a-value (funcall f a))
          (b-value (funcall f b)))
        (cond ((and (minusp a-value) (plusp b-value))
                  (search-root f a b))
              ((and (minusp b-value) (plusp a-value))
                  (search-root f b a))
              (t
                  (error "Values are not of opposite sign")))))

(princ (half-interval-method #'sin 2.0 4.0)) (terpri)

(princ (half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)) (terpri)

;;Finding fiexd points of functions
(defparameter tolerance 0.00001)

(defun fixed-point (f first-guess)
    (labels ((close-enough-p (v1 v2)
                             (< (abs (- v1 v2)) tolerance))
             (try (guess)
                  (let ((next (funcall f guess)))
                      (if (close-enough-p guess next)
                          next
                          (try next)))))
        (try first-guess)))

(princ (fixed-point #'cos 1.0)) (terpri)
(princ (fixed-point (lambda (y) (+ (funcall #'sin y) (funcall #'cos y))) 1.0)) (terpri)

; Average damping
(defun square-root (x)
    (fixed-point (lambda (y)
                         (average y (/ x y)))
                 1.0))