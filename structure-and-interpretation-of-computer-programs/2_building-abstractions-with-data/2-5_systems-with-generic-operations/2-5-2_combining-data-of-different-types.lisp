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