(defparameter C (make-connector))
(defparameter F (make-connector))

(defun celsius-farenheit-converter (c f)
    (let ((u (make-connector))
          (v (make-connector))
          (w (make-connector))
          (x (make-connector))
          (y (make-connector)))
        (multiplier c w u)
        (multiplier v x u)
        (adder v y f)
        (constant 9 w)
        (constant 5 x)
        (constant 32 y)
        'ok))

; (has-value-p ⟨connector⟩)                             tells whether the connector has a value
;
; (get-value ⟨connector⟩)                               returns the connector's current value
;
; (n-set-value ⟨connector⟩ ⟨new-value⟩ ⟨informant⟩)     indicates that the informant is requesting the connector to set its value to the new value
;
; (n-forget-value ⟨connector⟩ ⟨retractor⟩)              tells the connector that the retractor is requesting it to forget its value
;
; (connect ⟨connector⟩ ⟨new-constraint⟩)                tells the connector to participate in the new constraint

(defun adder (a1 a2 sum)
    (labels ((process-new-value ()
                                (cond ((and (has-value-p a1) (has-value-p a2))
                                          (n-set-value sum
                                                       (+ (get-value a1) (get-value a2))
                                                       #'me))
                                      ((and (has-value-p a1) (has-value-p sum))
                                          (n-set-value a2
                                                       (- (get-value sum) (get-value a1))
                                                       #'me))
                                      ((and (has-value-p a2) (has-value-p sum))
                                          (n-set-value a1
                                                       (- (get-value sum) (get-value a2))
                                                       #'me))))
             (process-forget-value ()
                                   (n-forget-value sum #'me)
                                   (n-forget-value a1 #'me)
                                   (n-forget-value a2 #'me)
                                   (process-new-value))
             (me (request)
                 (cond ((eq request 'I-have-a-value) (process-new-value))
                       ((eq request 'I-lost-my-value) (process-forget-value))
                       (t (error "Unknown request: ADDER ~A" request)))))
        (connect a1 #'me)
        (connect a2 #'me)
        (connect sum #'me)
        #'me))

(defun inform-about-value (constraint)
    (funcall constraint 'I-have-a-value))

(defun inform-about-no-value (constraint)
    (funcall constraint 'I-lost-my-value))

(defun multiplier (m1 m2 product)
    (labels ((process-new-value ()
                                (cond ((or (and (has-value-p m1) (= (get-value m1) 0))
                                           (and (has-value-p m2) (= (get-value m2) 0)))
                                          (n-set-value product 0 #'me))
                                      ((and (has-value-p m1) (has-value-p m2))
                                          (n-set-value product
                                                       (* (get-value m1) (get-value m2))
                                                       #'me))
                                      ((and (has-value-p product) (has-value-p m1))
                                          (n-set-value m2
                                                       (/ (get-value product)
                                                          (get-value m1))
                                                       #'me))
                                      ((and (has-value-p product) (has-value-p m2))
                                          (n-set-value m1
                                                       (/ (get-value product)
                                                          (get-value m2))
                                                       #'me))))
             (process-forget-value ()
                                   (n-forget-value product #'me)
                                   (n-forget-value m1 #'me)
                                   (n-forget-value m2 #'me)
                                   (process-new-value))
             (me (request)
                 (cond ((eq request 'I-have-a-value) (process-new-value))
                       ((eq request 'I-lost-my-value) (process-forget-value))
                       (t (error "Unknown request: MULTIPLIER ~A" request)))))
             (connect m1 #'me)
             (connect m2 #'me)
             (connect product #'me)
             #'me))

(defun constant (value connector)
    (labels ((me (request)
                 (error "Unknown request: CONSTANT ~A" request)))
        (connect connector #'me)
        (n-set-value connector value #'me)
        #'me))

(defun probe (name connector)
    (labels ((print-probe (value)
                          (terpri) (princ "Probe: ") (princ name)
                          (princ " = ") (princ value))
             (process-new-value ()
                                (print-probe (get-value connector)))
             (process-forget-value () (print-probe "?"))
             (me (request)
                 (cond ((eq request 'I-have-a-value) (process-new-value))
                       ((eq request 'I-lost-my-value) (process-forget-value))
                       (t (error "Unknown request: PROBE ~A" request)))))
             (connect connector #'me)
             #'me))

(defun make-connector ()
    (let ((value nil) (informant nil) (constraints '()))
        (labels ((set-my-value (newval setter)
                               (cond ((not (has-value-p #'me))
                                         (setf value newval)
                                         (setf informant setter)
                                         (for-each-except setter
                                                          #'inform-about-value
                                                          constraints))
                                     ((not (= value newval))
                                         (error "Contradiction ~A" (list value newval)))
                                     (t 'ignored)))
                 (forget-my-value (retractor)
                                  (if (eq retractor informant)
                                      (progn (setf informant nil)
                                             (for-each-except retractor
                                                              #'inform-about-no-value
                                                              constraints))
                                      'ignored))
                 (connect (new-constraint)
                          (if (not (member new-constraint constraints :test #'eq))
                              (setf constraints
                                  (cons new-constraint constraints)))
                          (if (has-value-p #'me)
                              (inform-about-value new-constraint))
                          'done)
                 (me (request)
                     (cond ((eq request 'has-value-p)
                               (if informant T nil))
                           ((eq request 'value) value)
                           ((eq request 'n-set-value) #'set-my-value)
                           ((eq request 'forget) #'forget-my-value)
                           ((eq request 'connect) #'connect)
                           (t (error "Unknown operation: CONNECTOR ~A" request)))))
            #'me)))

(defun for-each-except (exception procedure lst)
    (labels ((looping (items)
                      (cond ((null items) 'done)
                            ((eq (car items) exception) (looping (cdr items)))
                            (t (funcall procedure (car items))
                               (looping (cdr items))))))
        (looping lst)))

(defun has-value-p (connector)
    (funcall connector 'has-value-p))

(defun get-value (connector)
    (funcall connector 'value))

(defun n-set-value (connector new-value informant)
    (funcall (funcall connector 'n-set-value) new-value informant))

(defun n-forget-value (connector retractor)
    (funcall (funcall connector 'forget) retractor))

(defun connect (connector new-constraint)
    (funcall (funcall connector 'connect) new-constraint))