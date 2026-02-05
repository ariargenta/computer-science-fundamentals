(defun square (x) (* x x))

;; Rectangular form
(defun real-part (z) (car z))

(defun imag-part (z) (cdr z))

(defun magnitude (z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))

(defun angle (z)
    (atan (imag-part z) (real-part z)))

(defun make-from-real-imag (x y) (cons x y))

(defun make-from-mag-ang (r a)
    (cons (* r (cos a)) (* r (sin a))))

;; Polar form
(defun magnintude-polar (z) (car z))

(defun real_part (z) (* (magnitude z) (cos (angle z))))

(defun imag_part (z) (* (magnitude z) (sin (angle z))))

(defun angle-polar (z) (cdr z))

(defun makeFromRealImag (x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

(defun makeFromMagAng (a r) (cons r a))

;; Complex operations
(defun add-complex (z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))

(defun sub-complex (z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))

(defun mul-complex (z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))

(defun div-complex (z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))