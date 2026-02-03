(defun variable-p (x) (symbolp x))

(defun same-variable-p (v1 v2)
    (and (variable-p v1) (variable-p v2) (eq v1 v2)))

(defun =number? (exp num) (and (numberp exp) (= exp num)))

(defun make-sum (a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (numberp a1) (numberp a2))
              (+ a1 a2))
          (t (list '+ a1 a2))))

(defun make-product (m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (numberp m1) (numberp m2)) (* m1 m2))
          (t (list '* m1 m2))))

(defun sum-p (x) (and (consp x) (eq (car x) '+)))

(defun addend (s) (cadr s))

(defun augend (s) (caddr s))

(defun product-p (x) (and (consp x) (eq (car x) '*)))

(defun multiplier (p) (cadr p))

(defun multiplicand (p) (caddr p))

(defun deriv (exp var)
    (cond ((numberp exp) 0)
          ((variable-p exp) (if (same-variable-p exp var) 1 0))
          ((sum-p exp) (make-sum (deriv (addend exp) var)
                                 (deriv (augend exp) var)))
          ((product-p exp)
              (make-sum
               (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var)
                             (multiplicand exp))))
          (t
              (error "Unknown expression type: DERIV ~A" exp))))