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
; Input: typye -> Procedure: details of contract for converting input to output -> Output: type

(defun square (x) (* x x))
(defun square (y) (* y y))  ; Parameter names must be local to the body of the procedure

;; Block structure, lexical scoping
;              +---------------------------+
;              |       square-root         |
;              |  +---------------------+  |
;              |  |    good-enough-p    |  |
; x: number -> |  +---------------------+  | -> √x: number
;              |  |      improve        |  |
;              |  +---------------------+  |
;              |  |     sqrt-iter       |  |
;              |  +---------------------+  |
;              +---------------------------+
(defun square-root (x)
    (labels ((good-enough-p (guess)
                (< (abs (- (square guess) x)) 0.001))
            (improve (guess)
                (average guess (/ x guess)))
            (sqrt-iter (guess)
                (if (good-enough-p guess)
                guess
                (sqrt-iter (improve guess)))))
        (sqrt-iter 1.0)))

; Procedural abstractions isolate details of process from its use. Designer has choice of which ideas to isolate, in order to support general patterns of computation