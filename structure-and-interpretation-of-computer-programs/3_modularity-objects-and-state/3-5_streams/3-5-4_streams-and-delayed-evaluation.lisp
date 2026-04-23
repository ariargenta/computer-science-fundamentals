(defun integral (delayed-integrand initial-value dt)
    (labels ((int ()
                  (cons-stream
                      initial-value
                      (let ((integrand (force delayed-integrand)))
                          (add-streams (scale-stream integrand dt) #'int)))))
        #'int))

(defun solve (f y0 dt)
    (let (y dy)
        (setf y (integral (delay dy) y0 dt))
        (setf dy (stream-map f y))
        y))