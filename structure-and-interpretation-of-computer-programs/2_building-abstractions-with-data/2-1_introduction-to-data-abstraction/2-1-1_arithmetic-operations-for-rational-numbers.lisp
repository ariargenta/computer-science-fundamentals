(defun make-rat (n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

(defun numer (x) (car x))
(defun denom (x) (cdr x))

(defun linear-combination (a b x y)
    (+ (* a x) (* b y)))

(defun add-rat (x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(defun sub-rat (x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(defun mul-rat (x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(defun div-rat (x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(defun equal-rat-p (x y)
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

(defun print-rat (x)
    (terpri)
    (princ (numer x))
    (princ "/")
    (princ (denom x)))

(defparameter one-half (make-rat 1 2))
(defparameter one-third (make-rat 1 3))

(print-rat one-half)
(print-rat one-third)
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))
(print-rat (add-rat one-third one-third))