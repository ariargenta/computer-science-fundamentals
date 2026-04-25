;;; Class: Defined by a `make-<type>` procedure
; Capture common behaviour
; - Defines what is common to all instances of that class
;       - Provides local state variables (must have "self" as first argument)
;       - Provides methods which implement desired behaviours
;       - Provides a message handler to implement methods
;       - Specifies what superclasses and methods are inherited
; - Inheritance enables inclusion of other class variables and methods
;       - Root class: Root object
;           - All user defined classes should inherit from either root-object class or from some other superclass.
;       - Each class should specialise the TYPE method
;       - Subclass vs superclass
;           - The subclass specialises the superclass by extending the state/behaviour of the superclass
;       - Classes have "is-a" relationships with other classes
;           - Establishes a type hierarchy
;               - Inheritance of state and behaviour from superclass
;               - Multiple inheritance: rules for finding methods
;           - In local methods can "ask" internal parts to do something.
;           - Use get-method on superclass parts to find method if needed
;
;;; Instance: Created by a `create-<type>` procedure
; - An object created to the "plan" given by a class definition
; - Each instances has its own identity
;   - Local state: The instance can perform based on its own state
; - An instance has a type corresponding to the class(es)
;
;; Message Handlers
; - Object behaviours are specified using message-handlers
; - Response to every message is a method
; - A method is a procedure that can be applied to actually do the work

(defun make-named-object-handler (name)
    (lambda (message)
        (cond ((eq message 'NAME)
                  (lambda () name))
              ((eq message 'CHANGE-NAME)
                  (lambda (new-name) (setf name new-name)))
              (t (no-method)))))

; create-book: symbol, number -> book
(defun create-book (name copyright)
    (create-instance #'make-book name copyright))

(defun make-book (self name copyright)
    (let ((named-object-part (make-named-object self name)))
        (lambda (message)
            (case message
                ((TYPE) (lambda ()
                            (type-extend 'book named-object)))
                ((YEAR) (lambda () copyright))
                (t (get-method message named-object-part))))))

; symbol -> named-object
(defun create-named-object (name)
    (create-instance #'make-named-object name))

(defun make-named-object (self name)
    (let ((root-part (make-root-object self)))
        (lambda (message)
            (case message
                ((TYPE)
                 (lambda () (type-extend 'named-object root-part)))
                ((NAME)
                 (lambda () name))
                ((CHANGE-NAME)
                 (lambda (newname) (setf name newname)))
                (t (get-method message root-part))))))

;; Instance creation
; - User should provide a `create-<type>` procedure for each class, the instance is created by applying this procedure.
;   - Uses the `create-instance` higher order procedure to
;       - Generate an instance object
;       - Make and add the message handler for the object
;       - Return the instance object
;
;; Using an instance
; - Method lookup: get-method for <MESSAGE> from instance
; - Method application: apply that method to method arguments
; - Can do both steps at once, ask an instance to do something
;
;; Type system
; With inheritance, an instance can have multiple types.
; - All objects respond to TYPE message.
; - All objects respond to IS-A message

(defun make_instance ()
    (let ((handler nil))
        (lambda (message)
            (case message
                ((N-SET-HANDLER)
                 (lambda (handler-proc)
                     (setf handler handler-proc)))
                (t (get-method message handler))))))

(defun create-instance (maker &rest args)
    (let* ((instance (make_instance))
           (handler (apply maker instance args)))
        (ask instance 'N-SET-HANDLER handler)
        instance))

(defun get-method (message &rest objects)
    (labels ((try (objects)
                  (if (null objects)
                      (no-method)
                      (let ((method (funcall (car objects) message)))
                          (if (not (eq method (no-method)))
                              method
                              (try (cdr objects)))))))
             (try objects)))

(defun ask (object message &rest args)
    (let ((method (get-method message object)))
        (if (method-p method)
            (apply method args)
            (error "No method for message ~A" message))))

;; Why a "self" variable?
; Every class definition has access to a "self" variable. `self` is a pointer to the entire instance.
; When implementing a method, sometimes you "ask" a parto fo yourself to do something, however sometimes we want to ask the whole instance to do something. This mostly matters when we have subclass methods that shadow superclass methods and we want to invoke one of those shadowing methods from inside the superclass

(defun create-person (name)
    (create-instance #'make-person name))

(defun make-person (self name)
    (let ((root-part (make-root-object self)))
        (lambda (message)
            (case message
                ((TYPE) (lambda () (type-extend 'person root-part)))
                ((WHOAREYOU?) (lambda () name))
                ((SAY) (lambda (stuff) stuff))
                ((QUESTION) (lambda (of-whom query)
                                (ask of-whom 'answer self query)))
                ((ANSWER) (lambda (whom query)
                              (ask self 'say
                                   (cons (ask whom 'whoareyou?)
                                         (append '(i do not know about)
                                             query)))))
                       (t (get-method message root-part))))))

(defun create-professor (name)
    (create-instance #'make-professor name))

(defun make-professor (self name)
    (let ((person-part (make-person self name)))
        (lambda (message)
            (case message
                ((TYPE)
                 (lambda () (type-extend 'professor person-part)))
                ((WHOAREYOU?) (lambda () (list 'prof name)))
                ((LECTURE) (lambda (notes)
                   (cons 'therefore
                                     (ask person-part 'say notes))))
                (t (get-method message person-part))))))

(defun create-arrogant-prof (name)
    (create-instance #'make-arrogant-prof name))

(defun make-arrogant-prof (self name)
    (let ((prof-part (make-professor self name)))
        (lambda (message)
            (case message
                ((TYPE)
                 (lambda () (type-extend 'arrogant-prof prof-part)))
                ((SAY) (lambda (stuff)
                           (append (ask prof-part 'say stuff)
                               (list 'obviously))))
                ((ANSWER) (lambda (whom query)
                              (cond ((ask whom 'is-a 'student)
                                        (ask self 'say
                                             '(this should be obvious to you)))
                                    ((ask whom 'is-a 'professor)
                                        (ask self 'say
                                             (append '(but you wrote a paper about)
                                                 query)))
                                    (t (ask prof-part 'answer whom query)))))
                (t (get-method message prof-part))))))

(defun create-student (name)
    (create-instance #'make-student name))

(defun make-student (self name)
    (let ((person-part (make-person self name)))
        (lambda (message)
            (case message
                ((TYPE)
                 (lambda () (type-extend 'student person-part)))
                ((SAY) (lambda (stuff)
                           (append '(excuse me but)
                               (ask person-part 'say stuff))))
                (t (get-method message person-part))))))

(defun create-singer ()
    (create-instance #'make-singer))

(defun make-singer (self)
    (let ((root-part (make-root-object self)))
        (lambda (message)
            (case message
                ((TYPE) (lambda () (type-extend 'singer root-part)))
                ((SAY) (lambda (stuff) (append stuff '(tra la la))))
                ((SING) (lambda () (ask self 'say '(the hills are alive))))
                (t (get-method message root-part))))))

(defun create-singing-arrogant-prof (name)
    (create-instance
     #'make-singing-arrogant-prof name))

(defun make-singing-arrogant-prof (self name)
    (let ((singer-part (make-singer self))
          (arr-prof-part (make-arrogant-prof self name)))
        (lambda (message)
            (case message
                ((TYPE) (lambda () (type-extend 'singing-arrogant-prof
                                                singer-part
                                                arr-prof-part)))
                (t (get-method message singer-part
                               arr-prof-part))))))

;;; Object-oriented programming systems
; - Abstract view: class and instance diagrams
; - User view: how to define classes, create instances
; - Implementation view: how we layer notion of object, classes, instances, and inheritance on top of standard language

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