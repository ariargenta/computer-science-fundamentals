(defclass numeric-object () ())

(defclass scheme-number (numeric-object)
        ((value :initarg :value
                :accessor scheme-number-value)))

(defclass rational-number (numeric-object)
        ((numerator :initarg :numerator
                    :accessor numer)
         (denominator :initarg :denominator
                      :accessor denom)))

(defclass complex-number (numeric-object)
        ((real-part :initarg :real
                    :accessor real-part)
         (imag-part :initarg :imag
                    :accessor imag-part)))

(defgeneric add (x y))

(defgeneric sub (x y))

(defgeneric mul (x y))

(defgeneric div (x y))

(defun make-scheme-number (n)
    (make-instance 'scheme-number :value n))

(defmethod add ((x scheme-number) (y scheme-number))
    (make-scheme-number (+ (scheme-number-value x)
                           (scheme-number-value y))))

(defmethod sub ((x scheme-number) (y scheme-number))
    (make-scheme-number (- (scheme-number-value x)
                           (scheme-number-value y))))

(defmethod mul ((x scheme-number) (y scheme-number))
    (make-scheme-number (* (scheme-number-value x)
                           (scheme-number-value y))))

(defmethod div ((x scheme-number) (y scheme-number))
    (make-scheme-number (/ (scheme-number-value x)
                           (scheme-number-value y))))

;; Rational numbers
(defun make-rational (n d)
    (let ((g (gcd n d)))
        (make-instance 'rational-number
            :numerator (/ n g)
            :denominator (/ d g))))

(defmethod add-rat ((x rational-number) (y rational-number))
    (make-rational (+ (* (numer x) (denom y))
                      (* (numer y) (denom x)))
                   (* (denom x) (denom y))))

(defmethod sub-rat ((x rational-number) (y rational-number))
    (make-rational (- (* (numer x) (denom y))
                      (* (numer y) (denom x)))
                   (* (denom x) (denom y))))

(defmethod mul-rat ((x rational-number) (y rational-number))
    (make-rational (* (numer x) (numer y))
                   (* (denom x) (denom y))))

(defmethod div-rat ((x rational-number) (y rational-number))
    (make-rational (* (numer x) (denom y))
                   (* (denom x) (numer y))))

;; Complex numbers
(defgeneric magnitude (z))

(defgeneric angle (z))

(defmethod magnitude ((z complex-number))
    (sqrt (+ (* (real-part z) (real-part z))
             (* (imag-part z) (imag-part z)))))

(defmethod angle ((z complex-number))
    (atan (imag-part z) (real-part z)))

(defun make-complex-from-real-imag (x y)
    (make-instance 'complex-number :real x :imag y))

(defun make-complex-from-mag-ang (r a)
    (make-instance 'complex-number
        :real (* r (cos a))
        :imag (* r (sin a))))

(defmethod add-complex ((z1 complex-number) (z2 complex-number))
    (make-complex-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))

(defmethod sub-complex ((z1 complex-number) (z2 complex-number))
    (make-complex-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))

(defmethod mul-complex ((z1 complex-number) (z2 complex-number))
    (make-complex-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))

(defmethod div-complex ((z1 complex-number) (z2 complex-number))
    (make-complex-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))

;; Representation of `3 + 4i` in rectangular form
;    +---+---+   +---+---+    +---+---+
; -->| o | o-+-->| o | o-+--> | o | o |
;    +---+---+   +---+---+    +---+---+
;      ↓           ↓            ↓   ↓
;   complex    rectangular      3   4