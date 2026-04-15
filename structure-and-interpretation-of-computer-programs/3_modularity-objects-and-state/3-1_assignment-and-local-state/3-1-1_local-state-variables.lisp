(defparameter balance 100)

(defun withdraw (amount)
    (if (>= balance amount)
        (progn (setf balance (- balance amount))
               balance)
        "Insufficient funds"))

(defun new-withdraw ()
    (let ((balance 100))
        (lambda (amount)
            (if (>= balance amount)
                (progn (setf balance (- balance amount))
                       balance)
                "Insufficient funds"))))

(defun make-withdraw (balance)
    (lambda (amount)
        (if (>= balance amount)
            (progn (setf balance (- balance amount))
                   balance)
            "Insufficient funds")))

(defun make-account (balance)
    (let ((current-balance balance))
        (lambda (m)
            (cond ((eq m 'withdraw)
                      (lambda (amount)
                          (if (>= current-balance amount)
                              (setf current-balance (- current-balance amount))
                              "Insufficient funds")))
                  ((eq m 'deposit)
                      (lambda (amount)
                          (setf current-balance (+ current-balance amount))
                          current-balance))
                  (t (error "Unknown request: MAKE_ACCOUNT ~A" m))))))

(defparameter acc (make-account 100))

; Functional programming
; (defparameter x 10)       - Expression has same value each time it is evaluated in same scope as binding
; (+ x 5) ==> 15
; . . .
; (+ x 5) ==> 15
; ...
; (setf (car x) 94)         - Expression "value" depends on when it is evaluated
; . . .
; (+ x 5) ==> 99
;
;; Compound data
; - Constructor:
;   (cons x y)                                      Creates a new pair p
;
; - Selectors:
;   (car p)                                         Returns car part of pair
;   (cdr p)                                         Returns cdr part of pair
;
; - Mutators:
;   (setf (car p) new-x) || (rplaca p new-x)        Changes car pointer in pair
;   (setf (cdr p) new-y) || (rplacd p new-y)        Changes cdr pointer in pair
;   Pair, anytype -> undef                          Side-effect only!
;
;; Sharing, Equivalence and Identity
; How can we tell if two things are equivalent? What do we mean by "equivalent"?
;   1. The same object: test with `eq?`
;       (eq? a b) ==> #t
;   2. Objects that "look" the same: test with `equal?`
;       (equal? (list 1 2) (list 1 2)) ==> #t
;       (eq? (list 1 2) (list 1 2)) ==> #f
;
; If we change an object, is it the same object?
;   - Yes, if we retain the same pointer to the object
; How tell if parts of an object is shared with another?
;   - If we mutate one, see if other also changes
;
; Mutation introduces substantial complexity
;   - Unexpected side effects
;   - Substitution model is no longer sufficient to explain behaviour