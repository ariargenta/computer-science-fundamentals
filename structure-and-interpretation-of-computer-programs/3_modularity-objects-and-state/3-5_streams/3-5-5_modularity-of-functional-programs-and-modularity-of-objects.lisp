(defun random-numbers ()
    (cons-stream
        random-init
        (stream-map rand-update random-numbers)))

(defun cesaro-stream ()
    (map-successive-pairs
     (lambda (r1 r2) (= (gcd r1 r2) 1))
     random-numbers))

(defun map-successive-pairs (f s)
    (cons-stream
        (f (stream-car s) (stream-car (stream-cdr s)))
        (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(defun monte-carlo (experiment-stream passed failed)
    (labels ((next (passed failed)
                   (cons-stream
                       (/ passed (+ passed failed))
                       (monte-carlo
                        (stream-cdr experiment-stream) passed failed))))
             (if (stream-car experiment-stream)
                 (next (+ passed 1) failed)
                 (next passed (+ failed 1)))))

(defun pi-estimate ()
    (stream-map
     (lambda (p) (sqrt (/ 6 p)))
     (monte-carlo #'cesaro-stream 0 0)))

(defun stream-withdraw (balance amount-stream)
    (cons-stream
        balance
        (stream-withdraw (- balance (stream-car amount-stream))
                         (stream-cdr amount-stream))))