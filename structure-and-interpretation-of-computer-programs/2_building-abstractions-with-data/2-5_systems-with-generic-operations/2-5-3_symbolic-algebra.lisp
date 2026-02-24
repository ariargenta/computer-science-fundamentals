(defun install-polynomial-package ()
    (let ((the-empty-termlist '())
          (polynomial-tag 'polynomial))
        (labels ((make-poly (variables term-list) (cons variables term-list))
                 (variables (p) (car p))
                 (term-list (p) (cdr p))
                 (variable-p (x) (symbolp x))
                 (same-variable-p (v1 v2) (and (variable-p v1) (variable-p v2) (eq v1 v2)))
                 (adjoin-term (term term-list)
                              (if (equals-zero-p (coeff term))
                                  term-list
                                  (cons term term-list)))
                 (first-term (term-list) (car term-list))
                 (rest-terms (term-list) (cdr term-list))
                 (empty-termlist-p (term-list) (null term-list))
                 (make-term (order coeff) (list order coeff))
                 (order (term) (car term))
                 (coeff (term) (cadr term)))
            (add-poly (p1 p2)
                      (if (same-variable-p (variable p1) (variable p2))
                          (make-poly (variable p1)
                                     (add-terms (term-list p1) (term-list p2)))
                          (error "Polys not in same var: ADD-POLY ~A" (list p1 p2))))
            (add-terms (L1 L2)
                       (cond ((empty-termlist-p L1) L2)
                             ((empty-termlist-p L2) L1)
                             (t
                                 (let ((t1 (first-term L1))
                                       (t2 (first-term L2)))
                                     (cond ((> (order t1) (order t2))
                                               (adjoin-term
                                                t1 (add-terms (rest-terms L1) L2)))
                                           ((< (order t1) (order t2))
                                               (adjoin-term
                                                t2 (add-terms L1 (rest-terms L2))))
                                           (t
                                               (adjoin-term
                                                (make-term (order t1)
                                                           (add (coeff t1) (coeff t2)))
                                                (add-terms (rest-terms L1)
                                                           (rest-terms L2)))))))))
            (mul-poly (p1 p2)
                      (if (same-variable-p (variables p1) (variables p2))
                          (make-poly (variables p1)
                                     (mul-terms (term-list p1) (term-list p2)))
                          (error "Polys not in same var: MUL-POLY ~A" (list p1 p2))))
            (mul-terms (L1 L2)
                       (if (empty-termlist-p L1)
                           (the-empty-termlist)
                           (add-terms (mul-term-by-all-terms (first-term L1) L2)
                                      (mul-terms (rest-terms L1) L2))))
            (mul-term-by-all-terms (t1 L)
                                   (if (empty-termlist-p L)
                                       (the-empty-termlist)
                                       (let ((t2 (first-term L)))
                                           (adjoin-term
                                            (make-term (+ (order t1) (order t2))
                                                       (mul (coeff t1) (coeff t2)))
                                            (mul-term-by-all-terms t1 (rest-terms L))))))
            (tag (p) (cons polynomial-tag p))
            )))

(defun make-polynomial (var terms)
    ((get #'make #'polynomial) var terms))

(defun gcd-terms (a b)
    (if (empty-termlist-p b)
        a
        (gcd-terms b (remainder-terms a b))))

;;; Environment Model
; A precise, completely mechanical description of:
; - name-rule       looking up the value of a variable
; - define-rule     creating a new definition of a var
; - nset-rule       changing the value of a variable
; - lambda-rule     creating a procedure
; - application     applying a procedure
;
;; Viewpoint shift
; - Variable:
;   OLD - name for value
;   NEW - place into which one can store things
; - Procedure:
;   OLD - functional description
;   NEW - object with inherited context
; - Expressions:
;   Now only have maning with respect to an environment
;
;; Frame: a table of bindings
; Binding: a pairing of a name and a value
;
;; Environment: a sequence of frames
;
;; Evaluation in the environment model
; All evaluation occurs in an environment
; The current environment changes when the interpreter applies a procedure
; The top environment is called the global environment, only this has no enclosing environment
; To evaluate a combination
; - Evaluate the subexpressions in the current environment
; - Apply the value of the first to the values of the rest
;
;; Name-rule
; A name X evaluated in environment E gives the value of X in the first frame of E where X is bound
;
;; Define-rule
; A define special form evaluated in environment E creates or replaces a binding in the first frame of E
;
;; Set!-rule
; A nset of variable X evaluated in environment E changes the binding of X in the first frame of E where X is bound
;
;; Double bubble: How to draw a procedure
;
; (lambda (x) (* x x))
;    ↓
;   eval              #[compound-...]
;    ↓                      ↑                 +---+---+
; A compound proc that squares its argument → | * | *-|-→ environment pointer
;                                             +-|-+---+
;                                               ↓
;                                           code pointer
;
;; Lambda-rule
; A lambda special form evaluated in environment E creates aprocedure whose environment pointer is E
; Evaluating a lambda actually returns a pointer to the procedure object
;
; To apply a compound procedure P to arguments:
;   1. Create a new frame A
;   2. Make A into an environment E:
;       A's encolosing environment pointer goes to the same frame as the environment pointer of P
;   3. In A, bind the parameters of P to the argument values
;   4. Evaluate the body of P with E as the current environment
;
; - Environment model does not show the complete state of the interpreter. Missing the stack of pending operations
; - The global environment contains all standard bindings (*, cons, etc). Omitted from environment model drawings
; - Useful to link environment pointer of each frame to the procedure that created it

(defun make-counter (n)
    (lambda () (setf n (+ n 1))
        n))

(defun ca () (make-counter 0))

(defun cb () (make-counter 0))

; Environment diagrams get complicated quickly, rules are meant for the computer to follow, not to help humans
; A lambda inside a procedure body captures the frame that was active when the lambda was evaluated, this effect can be used to store local state