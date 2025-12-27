(defun average (x y)
    (/ (+ x y) 2))

(defun square (x) (* x x))

(defparameter tolerance 0.00001d0)

(defun fixed-point (f first-guess)
    (labels ((close-enough-p (v1 v2)
                             (< (abs (- v1 v2)) tolerance))
             (try (guess)
                  (let ((next (funcall f guess)))
                      (if (close-enough-p guess next)
                          next
                          (try next)))))
        (try first-guess)))

(defun average-damp (f)
    (lambda (x) (average x (funcall f x))))

(princ (funcall (average-damp #'square) 10)) (terpri)

(defun square-root (x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
                 1.0))

(defun cube-root (x)
    (fixed-point (average-damp (lambda (y) (/ x (square y))))
                 1.0))

;; Newton method
(defparameter dx 1.0d-5)

(defun cube (x) (* x x x))

(defun deriv (g)
    (lambda (x) (/ (- (funcall g (+ x dx)) (funcall g x)) dx)))

(princ (funcall (deriv #'cube) 5)) (terpri)

(defun newton-transform (g)
    (lambda (x) (- x (/ (funcall g x) (funcall (deriv g) x)))))

(defun newton-method (g guess)
    (fixed-point (newton-transform g) guess))

(defun squareRoot (x)
    (newton-method
     (lambda (y) (- (square y) x)) 1.0))

;; Abstractios and first-class procedures
(defun fixed-point-of-transform (g transform guess)
    (fixed-point (funcall transform g) guess))

(defun square-root-of (x)
    (fixed-point-of-transform
     (lambda (y) (/ x y)) #'average-damp 1.0))

(defun squareRootOf (x)
    (fixed-point-of-transform
    (lambda (y) (- (square y) x)) #'newton-transform 1.0))