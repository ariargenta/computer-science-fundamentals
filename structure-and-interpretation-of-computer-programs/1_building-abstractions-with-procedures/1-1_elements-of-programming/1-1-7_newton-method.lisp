(defun square (x) (* x x))

(defun average (x y)
    (/ (+ x y) 2))

(defun improve (guess x)
    (average guess (/ x guess)))

(defun good-enoughp (guess x)
    (< (abs (- (square guess) x)) 0.001))

(defun sqrt-iter (guess x)
    (if (good-enoughp guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(defun squareRoot (x)
    (sqrt-iter 1.0 x))

(princ (squareRoot 9)) (terpri)
(princ (squareRoot ( + 100 37))) (terpri)
(princ (squareRoot (+ (squareRoot 2) (squareRoot 3)))) (terpri)
(princ (square (squareRoot 1000))) (terpri)
(princ (square (sqrt 1000))) (terpri)