;             squareRoot
;                  |
;              sqrt-iter
;                /   \
;               /     \
;              /       \
;             /         \
;            /           \
;           /             \
;          /               \
;         /                 \
;   good-enoughp          improve
;       /  \                 |
;      /    \                |
;     /      \               |
;    /        \              |
;   /          \             |
;square   absoluteValue   average

(defun squared (x) (* x x))
(defun squared (x) (exp (doubled (log x))))
(defun doubled (x) (+ x x))
; Any procedure that achieve the desired result is equally good. A user should not need to know how the procedure is implemented in order to use it, this is called procedural abstraction

(defun square (x) (* x x))
(defun square (y) (* y y))  ; Parameter names must be local to the body of the procedure

; Block structure, lexical scoping
(defun squareRoot (x)
    (labels ((goodEnoughp (guess)
                (< (abs (- (square guess) x)) 0.001))
            (improve (guess)
                (average guess (/ x guess)))
            (sqrtIter (guess)
                (if (goodEnoughp guess)
                guess
                (sqrtIter (improve guess)))))
        (sqrtIter 1.0)))