(defun construct (x y)
    (let ((new (get-new-pair)))
        (rplaca new x)
        (rplacd new y)
        new))

; Mutation is just assignment
(defun construct-pair (x y)
    (labels ((dispatch (m)
                       (cond ((eq m 'car) x)
                             ((eq m 'cdr) y)
                             (t (error "Undefined operation: CONS ~A" m)))))
      #'dispatch))

(defun construction (x y)
    (labels ((n-set-x (v) (setf x v))
             (n-set-y (v) (setf y v))
             (dispatch (m)
               (cond ((eq m 'contents-of-address-part-of-register) x)
                     ((eq m 'contents-of-decrement-part-of-register) y)
                     ((eq m 'n-set-car) #'n-set-x)
                     ((eq m 'n-set-cdr) #'n-set-y)
                     (t
                         (error "Undefined operation: CONS ~A" m)))))
      #'dispatch))

(defun contents-of-address-part-of-register (z) (funcall z 'car))

(defun contents-of-decrement-part-of-register (z) (funcall z 'cdr))

(defun n-set-car (z new-value)
    (funcall (funcall z 'n-set-car) new-value) z)

(defun n-set-cdr (z new-value)
    (funcall (funcall z 'n-set-cdr) new-value) z)

; How easy is it to break system into abstraction modules?
; How easy is it to extend the system?
; - Adding new data types?
; - Adding new methods?
;
;; Tagged data
; - Some complex structure from cons cells
; - Explicit tags to keep track of data types
; - Implement a data abstraction as set of procedures that operate the data
;
; Dispatch on type
; - Adding new data types:
;   - Must change every generic operation
;   - Must keep names distinct
; - Adding new methods
;   - Just create generic operations
;- Works best when:
;   - There are a small number of data abstractions
;   - The changes are mostly new methods or operations
;   - The different kinds of data structures in the system are independent or unrelated to one another
;
; Generic operations by looking at types
(defun scale (x factor)
    (cond ((number-p x) (* x factor))
          ((line-p x) (line-scale x factor))
          ((shape-p x) (shape-scale x factor))
          (t (error "Unknown type"))))

;;; Procedures with state
; - A procedure has
;   - Parameters and body as specified by λ expression
;   - Environment, which can hold name-value bindings
;
; - We can use a procedure to represent a data object with some state, encapsulate and hide data, and provide controlled acess to that data.
;   - Procedure application creates private environment
;   - Need access to that environment
; Procedure capturing local state, not only can it return information about state but if we send it a message, it can give us back a procedure that causes changes in that state.

(defun cons-pair (x y)
    (lambda (msg)
        (cond ((eq msg 'CAR) x)
              ((eq msg 'CDR) y)
              ((eq msg 'PAIR) t)
              ((eq msg 'N_SET_CAR) (lambda (new-car) (setf x new-car)))
              ((eq msg 'N_SET_CDR) (lambda (new-cdr) (setf y new-cdr)))
              (t (error "Pair cannot ~A" msg)))))

(defun contents_of_address_part_of_register (p) (funcall p 'CAR))

(defun contents_decrement_part_of_register (p) (funcall p 'CDR))

(defun pair-p (p)
    (and (procedure-p p) (funcall p 'PAIR)))

(defun n_set_car (p new-car)
    (funcall (funcall p 'N_SET_CAR) new-car))

(defun n_set_cdr (p new-cdr)
    (funcall (funcall p 'N_SET_CDR) new-cdr))

; Lexical scoping for private state and private procedures
(defun construct_pair (x y)
    (labels ((change-car (new-car) (setf x new-car))
             (change-cdr (new-cdr) (setf y new-cdr)))
      (lambda (msg &rest args)
        (cond ((eq msg 'CAR) x)
              ((eq msg 'CDR) y)
              ((eq msg 'PAIR) t)
              ((eq msg 'N_SET_CAR)
               (change-car (first args)))
              ((eq msg 'N_SET_CDR)
               (change-cdr (first args)))
              (t (error "Pair cannot ~A" msg))))))

;;; Procedural programming
; Organise system around procedures that operate on data. They are good when we're dealing with numerical operations or with systems that have small numbers of data structures.
;   (do-something <data> <arg> ...)
;   (do-another-thing <data>)
;
;;; Object-oriented programming
; They are good for things like simulation or for systems with large numbers of objects where the objects themselves are characterized by small amount of state information and the computation basically involves interaction between the objects causing that state to change
; - Organise system around objects that receive messages
;   (<object> 'do-something <arg>)
;   (<object> 'do-another-thing)
; - An object encapsulates data and operations
;
;;; Object: "Smart" data structure, set of state variables and set of methods for manipulating state variables
;;; Class: Specifies the common structure and behaviour of entities
;;; Instance: A particular object or entitiy of a given class
;
;; Class Diagram
;
; |   PAIR    |     <-- class
; | --------- |
; | x:        |     <-- Private state
; | y:        |
; | --------- |
; | CAR       |     <-- Public messages
; | CDR       |
; | PAIR-P    |
; | N-SET-CAR |
; | N-SET-CDR |

(defun make-ship (ship-position ship-velocity num-torps)
    (labels ((move ()
                   (setf ship-position (add-vect ship-position ship-velocity)))
             (fire-torp ()
                        (cond ((> num-torps 0) 
                                  ;...
                                  )
                              (t 'FAIL))))
        (lambda (msg)
            (cond ((eq msg 'POSITION) ship-position)
                  ((eq msg 'VELOCITY) ship-velocity)
                  ((eq msg 'MOVE) (move))
                  ((eq msg 'ATTACK) (fire-torp))
                  (t (error "Ship cannot ~A" msg))))))

(defun make-station (station-position)
    (lambda (msg)
        (cond ((eq msg 'POSITION) station-position)
              ((eq msg 'DISPLAY) (draw
                                  ;...
                                  ))
              (t (error "Station cannot ~A" msg)))))

(defun make-torpedo (torpedo-position torpedo-velocity)
    (labels ((explode (torp)
                      (terpri "Torpedo goes off!")
                      (remove-from-universe torp))
             (move ()
                   (setf torpedo-position (add-vect torpedo-position torpedo-velocity))
                       ;...
                       )))
        (lambda (msg &rest args)
            (cond ((eq msg 'POSITION) torpedo-position)
                  ((eq msg 'VELOCITY) torpedo-velocity)
                  ((eq msg 'MOVE) (move))
                  ((eq msg 'EXPLODE) (explode (car args)))
                  ((eq msg 'DISPLAY) (draw
                                      ;...
                                      ))
                  (t (error "No method ~A" msg)))))