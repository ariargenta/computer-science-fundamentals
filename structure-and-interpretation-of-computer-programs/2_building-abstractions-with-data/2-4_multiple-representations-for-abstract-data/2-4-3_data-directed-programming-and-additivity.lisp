(defclass complex-number () ())

(defclass rectangular-complex (complex-number)
        ((real :initarg :real
               :accessor real-part)
         (imag :initarg :imag
               :accesor imag-part)))

(defclass polar-complex (complex-number)
        ((magnitude :initarg :magnitude
                    :accessor magnitude)
         (angle :initarg :angle
                :accessor angle)))

(defgeneric real-part (z))
(defgeneric imag-part (z))
(defgeneric magnitude (z))
(defgeneric angle (z))

(defmethod magnitude ((z rectangular-complex))
    (sqrt (+ (expt (real-part z) 2)
             (expt (imag-part z) 2))))

(defmethod angle ((z rectangular-complex))
    (atan (imag-part z)
          (real-part z)))

(defmethod real-part ((z polar-complex))
    (* (magnitude z)
       (cos (angle z))))

(defmethod imag-part ((z polar-complex))
    (* (magnitude z)
       (sin (angle z))))

(defun make-from-real-imag (x y)
    (make-instance 'rectangular-complex
        :real x
        :imag y))

(defun make-from-mag-ang (r a)
    (make-instance 'polar-complex
        :magnitude r
        :angle a))