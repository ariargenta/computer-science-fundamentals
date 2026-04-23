(defun stream-ref (s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

(defun stream-map (proc s)
    (if (stream-null-p s)
        the-empty-stream
        (cons-stream (funcall proc (stream-car s))
                     (stream-map proc (stream-cdr s)))))

(defun stream-for-each (proc s)
    (if (stream-null-p s)
        'done
        (progn (funcall proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

(defun display-stream (s)
    (stream-for-each #'display-line s))

(defun display-line (x) (terpri) (princ x))

(defun stream-car (stream) (car stream))

(defun stream-cdr (stream) (force (cdr stream)))

(defun stream-enumerate-interval (low high)
    (if (> low high)
        the-empty-stream
        (cons-stream
         low
         (stream-enumerate-interval (+ low 1) high))))

(defun stream-filter (pred stream)
    (cond ((stream-null-p stream) the-empty-stream)
                                  ((pred (stream-car stream))
                                   (cons-stream (stream-car stream)
                                                (stream-filter
                                                 pred
                                                 (stream-cdr stream))))
                                  (t (stream-filter pred (stream-cdr stream)))))

(defun force (delayed-object) (funcall delayed-object))

(defun memo-proc (proc)
    (let ((already-run-p nil) (result nil))
        (lambda ()
            (if (not already-run-p)
                (progn (setf result (funcall proc))
                       (setf already-run-p t)
                       result)
                result))))

; Lazy evaluations
(defmacro delay (expr)
    `(lambda () ,expr))

(defmacro cons-stream (x y)
    `(cons ,x (delay ,y)))