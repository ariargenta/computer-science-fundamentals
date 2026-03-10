(defun rand ()
    (let ((x random-init))
        (lambda () setf x (rand-update x)) x))

(defun estimate-pi (trials)
    (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(defun cesaro-test ()
    (= (gcd (rand) (rand)) 1))

(defun monte-carlo (trials experiment)
    (labels ((iter (trials-remaining trials-passed)
               (cond ((= trials-remaining 0)
                      (/ trials-passed trials))
                     ((funcall experiment)
                      (iter (- trials-remaining 1)
                            (+ trials-passed 1)))
                     (t
                      (iter (- trials-remaining 1)
                            trials-passed)))))
      (iter trials 0)))

(defun estimate_pi (trials)
    (sqrt (/ 6 (random-gcd-test trials random-init))))

(defun random-gcd-test (trials initial-x)
    (labels ((iter (trials-remaining trials-passed x)
               (let* ((x1 (rand-update x))
                      (x2 (rand-update x1)))
                 (cond ((= trials-remaining 0)
                        (/ trials-passed trials))
                       ((= (gcd x1 x2) 1)
                        (iter (- trials-remaining 1)
                              (+ trials-passed 1)
                              x2))
                       (t
                        (iter (- trials-remaining 1)
                              trials-passed
                              x2))))))
        (iter trials 0 initial-x)))