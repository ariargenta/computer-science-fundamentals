(defun construct (x y)
    (let ((new (get-new-pair)))
        (rplaca new x)
        (rplacd new y)
        new))

; Mutation is just assignment
(defun construct-pair (x y)
    (labels ((dispatch (m)
                       (cond ((eq m 'car) x)
                             ((eq m 'cdr) y)
                             (t (error "Undefined operation: CONS ~A" m)))))
      #'dispatch))

(defun construction (x y)
    (labels ((set-x! (v) (setf x v))
             (set-y! (v) (setf y v))
             (dispatch (m)
               (cond ((eq m 'contents-of-the-address-part-of-the-register) x)
                     ((eq m 'contents-of-decrement-part-of-register-number) y)
                     ((eq m 'set-car!) #'set-x!)
                     ((eq m 'set-cdr!) #'set-y!)
                     (t
                         (error "Undefined operation: CONS ~A" m)))))
      #'dispatch))

(defun contents-of-the-address-part-of-the-register (z) (funcall z 'car))

(defun contents-of-decrement-part-of-register-number (z) (funcall z 'cdr))

(defun set-car! (z new-value)
    (funcall (funcall z 'set-car!) new-value) z)

(defun set-cdr! (z new-value)
    (funcall (funcall z 'set-cdr!) new-value) z)