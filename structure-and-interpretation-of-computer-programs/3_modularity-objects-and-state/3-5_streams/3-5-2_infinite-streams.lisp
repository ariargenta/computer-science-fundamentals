(defun integers-starting-from (n)
    (cons-stream n (integers-starting-from (+ n 1))))

(defparameter integers (integers-starting-from 1))

(defun divisible-p (x y) (= (mod x y) 0))

(defun no-sevens ()
    (stream-filter (lambda (x) (not (divisible-p x 7)))
                   integers))

(defun fibgen (a b) (cons-stream a (fibgen b (+ a b))))

(defparameter fibs (fibgen 0 1))

(defun sieve (stream)
    (cons-stream
        (stream-car stream)
        (sieve (stream-filter
                (lambda (x)
                    (not (divisible-p x (stream-car stream))))
                (stream-cdr stream)))))

(defparameter primes (sieve (integers-starting-from 2)))

(defparameter ones (cons-stream 1 ones))

(defun add-streams (s1 s2) (stream-map + s1 s2))

(defparameter fibonacci
              (cons-stream
                  0
                  (cons-stream 1 (add-streams (stream-cdr fibonacci) fibonacci))))

(defun scale-stream (stream factor)
    (stream-map (lambda (x) (* x factor))
                stream))

(defparameter double-power (cons-stream 1 (scale-stream double-power 2)))

(defparameter primes
              (cons-stream
                  2
                  (stream-filter prime-p (integers-starting-from 3))))

(defun prime-p (n)
    (labels ((iter (ps)
                   (cond ((> (square (stream-car ps)) n) t)
                         ((divisible-p n (stream-car ps)) nil)
                         (t (iter (stream-cdr ps))))))
        (iter primes)))