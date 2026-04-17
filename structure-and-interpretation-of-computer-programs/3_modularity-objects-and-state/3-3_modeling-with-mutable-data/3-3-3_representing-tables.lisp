; Environment diagrams get complicated quickly, rules are meant for the computer to follow, not to help humans
; A lambda inside a procedure body captures the frame that was active when the lambda was evaluated, this effect can be used to store local state
;
;; Abstract interface to a table
; - make                Create a new table
; - put! key value      Insert a new binding, replaces any previous binding of that key
; - get key             Look up the key, return the corresponding value
;
;; This definition IS the table abstract type
;
;; Association List: A list where each element is a list of the key and value
;
; i.e.:
; Represent the table:
; | var | val |
; | ---- | ---- |
; | x | 15 |
; | y | 20 |
;
; As the list: ((x 15) (y 20))
;
;    +---+---+                +---+---+
; -->| o | o-|--------------->| o | / |
;    +-+-+---+                +-+-+---+
;      ↓                        ↓
;    +---+---+   +---+---+    +---+---+   +---+---+
;    | o | o-|-->| o | / |    | o | o-|-->| o | / |
;    +-+-+---+   +-+-+---+    +-+-+---+   +-+-+---+
;      ↓           ↓            ↓           ↓
;      x           15           y           20
;
;; Alists are not an abstract data type
; - Missing a constructor
; - There is no abstraction barrier:
;   - Definition in Lisp: An alist is a list of pairs, each of which is called and assotiation. The car of an association is called the key
; - Therefore, the implementation is exposed. User may operate on alists using list operations
;
;; Why do we care that alists are not an abstract data type?
; - Modularity is essential for software engineering
;   - Build a program by sticking modules together
;   - Can change one module without affecting the rest
; - Alists have poor modularity
;   - Programs may use list ops like `filter` and `map` on alists
;   - These ops will fail if the implementations of alists change
;   - Must change whole program if you want a different table
; - To achieve modularity, hide information
;   - Hide the fact that the table is implemented as a list
;   - Do not allow rest of program to use list operations
;   - ADT techniques exist in order to do this

(defun find-assoc (key alist)
    (cond
     ((null alist) nil)
     ((equal key (caar alist)) (cadar alist))
     (t (find-assoc key (cdr alist)))))

(defun add-assoc (key val alist) (cons (list key val) alist))

(defparameter table1-tag 'table1)

;; void -> Table1<anytype, anytype>
(defun make-table1 () (cons table1-tag nil))

;; Table1<k, v>, k -> (v | null)
(defun table1-get (tbl key) (find-assoc key (cdr tbl)))

;; Table1<k, v> k, v -> undef
(defun table1-put! (tbl key val) 
    (rplacd tbl (add-assoc key val (cdr tbl))))

; Only implementation knows:
; Table1<k, v> = symbol x Alist<k, v>
; Alist<k, v> = list<k x v x null>
;
; The rest of the program does not apply any functions to Abstract Data Type objects other than the functions specified in the ADT.
; The implementation (as an alist) is hidden from the rest of the program, so it can be changed easily.
;
;; Information hidding in types: Opaque names
; Opaque: Type name that is defined but unspecified, denote information hiding. No functions will match except the functions of the ADT.
;
;; Hash Tables: Standard data structure for fast table lookup. The idea is to keep a bunch of associations lists rather than one and to choose which association list to use based on the hash function

(defun lookup (key table)
    (let ((record (associated key (cdr table))))
        (if record
            (cdr record)
            nil)))

(defun associated (key records)
    (cond ((null records) nil)
          ((equal key (caar records)) (car records))
          (t (associated key (cdr records)))))

(defun ninsert (key value table)
    (let ((record (associated key (cdr table))))
        (if record
            (rplacd record value)
            (rplacd table
                      (cons (cons key value)
                            (cdr table)))))
    'ok)

(defun make-table () (list '*table*))


(defun look-up (key-1 key-2 table)
    (let ((subtable
           (associated key-1 (cdr table))))
        (if subtable
            (let ((record
                   (associated key-2 (cdr subtable))))
                (if record
                    (cdr record)
                    nil))
            nil)))

(defun n-insert (key-1 key-2 value table)
    (let ((subtable (associated key-1 (cdr table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
                (if record
                    (rplacd record value)
                    (rplacd subtable
                               (cons (cons key-2 value)
                                     (cdr subtable)))))
            (rplacd table
                       (cons (list key-1
                                   (cons key-2 value))
                             (cdr table)))))
    'ok)

(defun make_table ()
    (let ((local-table (list '*table*)))
        (labels ((lookup (key-1 key-2)
                         (let ((subtable
                                (associated key-1 (cdr local-table))))
                             (if subtable
                                 (let ((record
                                        (associated key-2 (cdr subtable))))
                                     (if record (cdr record) nil))
                                 nil)))
                 (n-insert (key-1 key-2 value)
                           (let ((subtable
                                  (associated key-1 (cdr local-table))))
                               (if subtable
                                   (let ((record
                                          (associated key-2 (cdr subtable))))
                                       (if record
                                           (rplacd record value)
                                           (rplacd subtable
                                                   (cons (cons key-2 value)
                                                         (cdr subtable)))))
                                   (rplacd local-table
                                           (cons (list key-1 (cons key-2 value))
                                                 (cdr local-table)))))
                               'ok)
                 (dispatch (m)
                           (cond ((eq m 'lookup-proc) #'lookup)
                                 ((eq m 'n-insert-proc) #'n-insert)
                                 (t (error "Unknown operation: TABLE ~A" m)))))
            #'dispatch)))

(defun operation-table (m) (funcall (make_table) m))

(defun get-table () (operation-table 'lookup-proc))

(defun put-table () (operation-table 'n-insert-proc))

;; Vector: Fixed size collection with indexed access

;; number, A -> vector<A>
(defun make-vector (size value))

;; vector<A>, number -> A
(defun vector-ref (v index))

;; vector<A>, number, A -> undef
(defun vector-set! (v index val))

(defparameter t2-tag 'table2)

(defun make-table2 (size hashfunc)
    (let ((buckets (make-vector size nil)))
        (list t2-tag size hashfunc buckets)))

(defun size-of (tbl) (cadr tbl))

(defun hashfunc-of (tbl) (caddr tbl))

(defun buckets-of (tbl) (cadddr tbl))

(defun table2-get (tbl key)
    (let ((index
           (funcall (hashfunc-of tbl) key (size-of tbl))))
        (find-assoc key
                    (vector-ref (buckets-of tbl) index))))

(defun table2-put! (tbl key val)
    (let ((index
           (funcall (hashfunc-of tbl) (size-of tbl)))
          (buckets (buckets-of tbl)))
        (vector-set! buckets index
                     (add-assoc key val
                                (vector-ref buckets index)))))