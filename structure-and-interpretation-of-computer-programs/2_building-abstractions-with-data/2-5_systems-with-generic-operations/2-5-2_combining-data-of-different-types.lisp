(defun add-complex-to-schemenum (z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))

(put 'add '(complex scheme-number)
     #'(lambda (z x)
           (tag (add-complex-to-schemenum z x))))

(defun scheme-number->complex (n)
    (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              #'scheme-number->complex)

(defun apply-generic (op &rest args)
    (let ((type-tags (mapcar #'type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (mapcar #'contents args))
                (if (= (length args) 2)
                    (let ((type1 (first type-tags))
                          (type2 (second type-tags))
                          (a1 (first args))
                          (a2 (second args)))
                        (let ((t1->t2 (get-coercion type1 type2))
                              (t2->t1 (get-coercion type2 type1)))
                            (cond
                             (t1->t2
                                 (apply-generic op (funcall t1->t2 a1) a2))
                             (t2->t1
                                 (apply-generic op a1 (funcall t2->t1 a2)))
                             (t
                                 (error "No method for these types: ~A ~A" op type-tags)))))
                    (error "No method for these types ~A ~A" op type-tags))))))