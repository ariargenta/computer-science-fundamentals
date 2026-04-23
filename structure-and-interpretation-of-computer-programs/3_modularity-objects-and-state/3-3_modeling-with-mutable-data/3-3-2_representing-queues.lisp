;;; Queue Data Abstraction (Last In, Last Out)
; - Constructor:
;   (make-queue)                    Returns an empty queue
; - Accessors:
;   (front-queue ⟨queue⟩)           Returns the object at the front of the queue. If queue is empty signals error
; - Mutators:
;   (insert-queue ⟨queue⟩ ⟨item⟩)   Returns a new queue with elt at the rear of the queue
;   (delete-queue ⟨queue⟩)          Returns a new queue with the item at the front of the queue removed
; - Operations:
;   (empty-queue-p ⟨queue⟩)         Tests if the queue is empty
;
;; Queue contract
; If `q` is a queue, created by `(make-queue)` amd subsequent queue procedures, where `i` is the number of insertions, `j` is the number of deletions and `x_i` is the ith item inserted into `q`, then
;   1. If j > i     Then it is an error
;   2. If j = i     Then ``(empty-queue-p q)` is true, and `(front-queue q)` and `(delete-queue q)` are errors
;   3. If j < i     Then `(front-queue q) = x_{j + 1}`

(defparameter make-queue nil)

(defun empty-queue-p (queue) (null queue))

(defun front-queue (queue)
    (if (empty-queue-p queue)
        (error "Front of empty queue: ~A" queue)
        (car queue)))

(defun delete-queue (queue)
    (if (empty-queue-p queue)
        (error "Delete of empty queue: ~A" queue)
        (cdr queue)))

(defun insert-queue (queue elt)
    (if (empty-queue-p queue)
        (cons elt nil)
        (cons (car queue) (insert-queue (cdr queue) elt))))

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

(defun front-ptr (queue) (cadr queue))

(defun rear-ptr (queue) (caddr queue))

(defun nset-front-ptr (queue item) (setf (cadr queue) item))

(defun nset-rear-ptr (queue item) (setf (cddr queue) item))

(defun make-queue () (cons 'queue (cons nil nil)))

(defun queue-p (queue) (and (consp queue) (eq 'queue (car queue))))

(defun empty_queue-p (queue)
    (if (not (queue-p queue))
        (error "Object not a queue: ~A" queue)
        (null (front-ptr queue))))

(defun front_queue (queue)
    (if (empty-queue-p queue)
        (error "Front called with an empty queue: ~A" queue)
        (car (front-ptr queue))))

(defun n-insert-queue (queue item)
    (let ((new-pair (cons item nil)))
        (cond ((funcall #'empty_queue-p queue)
                  (nset-front-ptr queue new-pair)
                  (nset-rear-ptr queue new-pair)
                  queue)
              (t
                  (rplacd (rear-ptr queue) new-pair)
                  (nset-rear-ptr queue new-pair)
                  queue))))

(defun n-delete-queue (queue)
    (cond ((empty_queue-p queue)
              (error "Delete called with an empty queue: ~A" queue))
          (t
              (nset-front-ptr queue
                              (cdr (front-ptr queue)))
              queue)))

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