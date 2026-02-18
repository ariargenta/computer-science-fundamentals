(defun add-complex-to-schemenum (z x)
    (make-from-real-imag (+ (real-part z) x)
                         (imag-part z)))

(put 'add '(complex scheme-number)
     #'(lambda (z x)
           (tag (add-complex-to-schemenum z x))))

(defun scheme-number->complex (n)
    (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number
              'complex
              #'scheme-number->complex)

(defun apply-generic (op &rest args)
    (let ((type-tags (mapcar #'type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (mapcar #'contents args))
                (if (= (length args) 2)
                    (let ((type1 (first type-tags))
                          (type2 (second type-tags))
                          (a1 (first args))
                          (a2 (second args)))
                        (let ((t1->t2 (get-coercion type1 type2))
                              (t2->t1 (get-coercion type2 type1)))
                            (cond
                             (t1->t2
                                 (apply-generic op (funcall t1->t2 a1) a2))
                             (t2->t1
                                 (apply-generic op a1 (funcall t2->t1 a2)))
                             (t
                                 (error "No method for these types: ~A ~A" op type-tags)))))
                    (error "No method for these types ~A ~A" op type-tags))))))

;; Functional programming
; (defparameter x 10)   - Expression has same value each time it is evaluated in same scope as binding
; (+ x 5) ==> 15
; . . .
; (+ x 5) ==> 15
; ...
; (setf (car x) 94)           - Expression "value" depends on when it is evaluated
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
;
;;; Stack Data Abstraction (Last In, First Out)
; - Constructor:
;   (make-stack)                    Returns an empty stack
; - Selectors:
;   (top stack)                     Returns current top element from a stack
; - Operations:
;   (insert-stack stack elt)        Returns a new stack with the element added to the top of the stack
;   (delete-stack stack)            Returns a new stack with the top element removed from the stack
;   (empty-stack-p stack)           Returns #t if no elements, #f otherwise
;
;; Stack contract
; If `s` is a stack, created by `(make-stack)` and subsequent stack procedures, where `i` is the number of insertions and `j` is the number of deletions then
;   1. If j > i     Then it is an error
;   2. If j = i     Then `(empty-stack-p s)` is true, and `(top s)` and `(delete-stack s)` are errors
;   3. If j < i     Then `(empty-stack-p s)` is false and `(top (delete-stack (insert-stack s val))) = (top s)`
;   4. If j <= i    Then `(top (insert-stack s val)) = val` for any val

(defparameter make-stack nil)

(defun empty-stack-p (stack) (null stack))

(defun insert (stack elt) (cons elt stack))

(defun delete-stack (stack)
    (if (empty-stack-p stack)
        (error "Stack underflow - delete")
        (cdr stack)))

(defun top (stack)
    (if (empty-stack-p stack)
        (error "Stack underflow - top")
        (car stack)))

; Attach a type tag - defensive programming.
; Provides an object whose identity remains even as the object mutates
; Note that this is a change to the abstraction! User should know if the object mutates or not in order to use the abstraction correctly

(defun make_stack () (cons 'stack nil))

(defun stack-p (stack) (and (consp stack) (eq 'stack (car stack))))

(defun empty_stack-p (stack)
    (if (not (stack-p stack))
        (error "Object not a stack: ~A" stack)
        (null (cdr stack))))

(defun ninsert (stack elt)
    (cond ((not (stack-p stack))
              (error "Object not a stack: ~A" stack))
          (t
              (setf (cdr stack) (cons elt (cdr stack)))
                  stack)))

(defun ndelete (stack)
    (if (empty-stack-p stack)
        (error "Stack underflow - delete")
        (setf (cdr stack) (cddr stack)))
    stack)

(defun top-stack (stack)
    (if (empty-stack-p stack)
        (error "Stack underflow - top")
        (cadr stack)))

;;; Queue Data Abstraction (Last In, Last Out)
; - Constructor:
;   (make-queue)                Returns an empty queue
; - Accessors:
;   (front-queue q)             Returns the object at the front of the queue. If queue is empty signals error
; - Mutators:
;   (insert-queue q elt)        Returns a new queue with elt at the rear of the queue
;   (delete-queue q)            Returns a new queue with the item at the front of the queue removed
; - Operations:
;   (empty-queue q)             Tests if the queue is empty
;
;; Queue contract
; If `q` is a queue, created by `(make-queue)` amd subsequent queue procedures, where `i` is the number of insertions, `j` is the number of deletions and `x_i` is the ith item inserted into `q`, then
;   1. If j > i     Then it is an error
;   2. If j = i     Then ``(empty-queue-p q)` is true, and `(front-queue q)` and `(delete-queue q)` are errors
;   3. If j < i     Then `(front-queue q) = x_{j + 1}`

(defparameter make-queue nil)

(defun empty-queue-p (q) (null q))

(defun front-queue (q)
    (if (empty-queue-p q)
        (error "Front of empty queue: ~A" q)
        (car q)))

(defun delete-queue (q)
    (if (empty-queue-p q)
        (error "Delete of empty queue: ~A" q)
        (cdr q)))

(defun insert-queue (q elt)
    (if (empty-queue-p q)
        (cons elt nil)
        (cons (car q) (insert-queue (cdr q) elt))))

; The front of the queue is the first element in the list
; To insert an element at the tail of the queue, we need to "copy" the existing queue onto the front of the new element
;
; - Mutators:
;   (ninsert-queue q elt)       Inserts the elt at the rear of the queue and returns the modified queue
;   (ndelete-queue q)           Removes the elt at the front of the queue and returns the modified queue
; - Operations:
;   (queue-p q)                 Tests if the object is a queue
;
; Maintain queue identity. Build a structure to hold:
; - A list of items in the queue
; - A pointer to the front of the queue
; - A pointer to the rear of the queue

(defun front-ptr (q) (cadr q))

(defun rear-ptr (q) (caddr q))

(defun nset-front-ptr (q item) (setf (cadr q) item))

(defun nset-rear-ptr (q item) (setf (cddr q) item))

(defun make-queue () (cons 'queue (cons nil nil)))

(defun queue-p (q) (and (consp q) (eq 'queue (car q))))

(defun empty-queue-p (q)
    (if (not (queue-p q))
        (error "Object not a queue: ~A" q)
        (null (front-ptr q))))

(defun front-queue (q)
    (if (empty-queue-p q)
        (error "Front of empty queue: ~A" q)
        (car (front-ptr q))))

(defun ninsert-queue (q elt)
    (let ((new-pair (cons elt nil)))
        (cond ((empty-queue-q q)
                  (nset-front-ptr q new-pair)
                  (nset-rear-ptr q new-pair)
                  q)
              (t
                  (rplacd (rear-ptr q) new-pair)
                  (nset-rear-ptr q new-pair)
                  q))))

(defun ndelete-queue (q)
    (cond ((empty-queue-p q)
              (error "Delete of empty queue: ~A" q))
          (t
              (nset-front-ptr q
                              (cdr (front-ptr q)))
              q)))

; Mutation is a powerful idea
; - Enables new and efficient data structures
; - Can have surprising side effects
; - Breaks our "functional" programming (substitution) model