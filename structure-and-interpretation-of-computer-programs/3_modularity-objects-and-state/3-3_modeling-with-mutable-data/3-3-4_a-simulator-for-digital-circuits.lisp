(defun half-adder (a b s c)
    (let ((d (make-wire)) (e (make-wire)))
        (or-gate a b d)
        (and-gate a b c)
        (inverter c e)
        (and-gate d e s)
        'ok))

(defun full-adder (a b c-in sum c-out)
    (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
        (half-adder b c-in s c1)
        (half-adder a s sum c2)
        (or-gate c1 c2 c-out)
        'ok))

; (get-signal ⟨wire⟩)                                   returns the current value of the signal on the wire
;
; (n-set-signal ⟨wire⟩ ⟨new value⟩)                     changes the value of the signal on the wire to the new value
;
; (n-add-action ⟨wire⟩ ⟨procedure of no arguments⟩)     asserts that the designated procedure should be run whenever the signal on the wire changes value

(defun inverter (input output)
    (labels ((invert-input ()
                           (let ((new-value (logical-not (get-signal input))))
                               (after-delay inverter-delay
                                            (lambda () (n-set-signal output new-value)))))
             (logical-not (s)
                          (cond ((= s 0) 1)
                                ((= s 1) 0)
                                (t (error "Invalid signal ~A" s)))))
        (n-add-action input #'invert-input)
        'ok))

(defun and-gate (a1 a2 output)
    (labels ((and-action-procedure ()
                                   (let ((new-value
                                          (logical-and (get-signal a1) (get-signal a2))))
                                       (after-delay
                                        and-gate-delay
                                        (lambda () (n-set-signal output new-value))))))
        (n-add-action a1 #'and-action-procedure)
        (n-add-action a2 #'and-action-procedure)
        'ok))

(defun make-wire ()
    (let ((signal-value 0) (action-procedures '()))
        (labels ((n-set-my-signal (new-value)
                                  (if (not (= signal-value new-value))
                                      (progn (setf signal-value new-value)
                                             (call-each action-procedures))
                                      'done))
                 (n-accept-action-procedure (proc)
                                            (setf action-procedures
                                                (cons proc action-procedures))
                                            (funcall proc))
                 (dispatch (m)
                           (cond ((eq m 'get-signal) signal-value)
                                 ((eq m 'n-set-signal) #'n-set-my-signal)
                                 ((eq m 'n-add-action) #'n-accept-action-procedure)
                                 (t (error "Unknown operation: WIRE ~A" m)))))
            #'dispatch)))

(defun call-each (procedures)
    (if (null procedures)
        'done
        (progn (funcall (car procedures))
               (call-each (cdr procedures)))))

(defun get-signal (wire) (funcall wire 'get-signal))

(defun n-set-signal (wire new-value) (funcall (funcall wire 'n-set-signal) new-value))

(defun n-add-action (wire action-procedure) (funcall (funcall wire 'n-add-action) action-procedure))

; (make-agenda)                                 returns a new empty agenda
;
; (n-empty-agenda ⟨agenda⟩)                     is true if the specified agenda is empty
;
; (first-agenda-item ⟨agenda⟩)                  returns the first item on the agenda
;
; (n-remove-first-agenda-item ⟨agenda⟩)         modifies the agenda by removing the first item
;
; (n-add-to-agenda ⟨time⟩ ⟨action⟩ ⟨agenda⟩)    modifies the agenda by adding the given action procedure to be run at the specified time
;
; (current-time ⟨agenda⟩)                       returns the current simulation time

(defun after-delay (delay action)
    (n-add-to-agenda (+ delay (current-time the-agenda))
                     action
                     the-agenda))

(defun propagate ()
    (if (empty-agenda-p the-agenda)
        'done
        (let ((first-item (first-agenda-item the-agenda)))
            (funcall first-item)
            (n-remove-first-agenda-item the-agenda)
            (propagate))))

(defun probe (name wire)
    (n-add-action wire
                  (lambda ()
                      (terpri)
                      (princ name) (princ " ")
                      (princ (current-time the-agenda))
                      (princ " New-value = ")
                      (princ (get-signal wire)))))

(defparameter the-agenda (make-agenda))
(defparameter inverter-delay 2)
(defparameter and-gate-delay 3)
(defparameter or-gate-delay 5)
(defparameter input-1 (make-wire))
(defparameter input-2 (make-wire))
(defparameter sum (make-wire))
(defparameter carry (make-wire))

(defun make-time-segment (time queue) (cons time queue))

(defun segment-time (s) (car s))

(defun segment-queue (s) (cdr s))

(defun make-agenda () (list 0))

(defun current-time (agenda) (car agenda))

(defun n-set-current-time (agenda time) (rplaca agenda time))

(defun segments (agenda) (cdr agenda))

(defun n-set-segments (agenda segments) (rplacd agenda segments))

(defun first-segment (agenda) (car (segments agenda)))

(defun rest-segments (agenda) (cdr (segments agenda)))

(defun empty-agenda-p (agenda) (null (segments agenda)))

(defun n-add-to-agenda (timming action agenda)
    (labels ((belongs-before-p (segments)
                               (or (null segments)
                                   (< timming (segment-time (car segments)))))
             (make-new-time-segment (timming action)
                                    (let ((q (make-queue)))
                                        (n-insert-queue q action)
                                        (make-time-segment timming q)))
             (n-add-to-segments (segments)
                                (if (= (segment-time (car segments)) timming)
                                    (n-insert-queue (segment-queue (car segments))
                                                    action)
                                    (let ((rest (cdr segments)))
                                        (if (belongs-before-p rest)
                                            (rplacd
                                                segments
                                                (cons (make-new-time-segment timming action)
                                                      (cdr segments)))
                                            (n-add-to-segments rest))))))
             (let ((segments (segments agenda)))
                 (if (belongs-before-p segments)
                     (n-set-segments
                      agenda
                      (cons (make-new-time-segment timming action)
                            segments))
                     (n-add-to-segments segments)))))

(defun n-remove-first-agenda-item (agenda)
    (let ((q (segment-queue (first-segment agenda))))
        (n-delete-queue q)
        (if (empty-queue-p q)
            (n-set-segments agenda (rest-segments agenda)))))

(defun first-agenda-item (agenda)
    (if (empty-agenda-p agenda)
        (error "Agenda is empty: FIRST-AGENDA-ITEM")
        (let ((first-seg (first-segment agenda)))
            (n-set-current-time agenda
                                (segment-time first-seg))
            (front-queue (segment-queue first-seg)))))