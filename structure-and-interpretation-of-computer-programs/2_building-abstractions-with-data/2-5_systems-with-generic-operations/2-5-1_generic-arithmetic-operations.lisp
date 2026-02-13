(defclass numeric-object () ())

(defclass scheme-number (numeric-object)
        ((value :initarg :value
                :accessor scheme-number-value)))

(defclass rational-number (numeric-object)
        ((numerator :initarg :numerator
                    :accessor numer)
         (denominator :initarg :denominator
                      :accessor denom)))

(defclass complex-number (numeric-object)
        ((real-part :initarg :real
                    :accessor real-part)
         (imag-part :initarg :imag
                    :accessor imag-part)))

(defgeneric add (x y))

(defgeneric sub (x y))

(defgeneric mul (x y))

(defgeneric div (x y))

(defun make-scheme-number (n)
    (make-instance 'scheme-number :value n))

(defmethod add ((x scheme-number) (y scheme-number))
    (make-scheme-number (+ (scheme-number-value x)
                           (scheme-number-value y))))

(defmethod sub ((x scheme-number) (y scheme-number))
    (make-scheme-number (- (scheme-number-value x)
                           (scheme-number-value y))))

(defmethod mul ((x scheme-number) (y scheme-number))
    (make-scheme-number (* (scheme-number-value x)
                           (scheme-number-value y))))

(defmethod div ((x scheme-number) (y scheme-number))
    (make-scheme-number (/ (scheme-number-value x)
                           (scheme-number-value y))))

;; Rational numbers
(defun make-rational (n d)
    (let ((g (gcd n d)))
        (make-instance 'rational-number
            :numerator (/ n g)
            :denominator (/ d g))))

(defmethod add-rat ((x rational-number) (y rational-number))
    (make-rational (+ (* (numer x) (denom y))
                      (* (numer y) (denom x)))
                   (* (denom x) (denom y))))

(defmethod sub-rat ((x rational-number) (y rational-number))
    (make-rational (- (* (numer x) (denom y))
                      (* (numer y) (denom x)))
                   (* (denom x) (denom y))))

(defmethod mul-rat ((x rational-number) (y rational-number))
    (make-rational (* (numer x) (numer y))
                   (* (denom x) (denom y))))

(defmethod div-rat ((x rational-number) (y rational-number))
    (make-rational (* (numer x) (denom y))
                   (* (denom x) (numer y))))

;; Complex numbers
(defgeneric magnitude (z))

(defgeneric angle (z))

(defmethod magnitude ((z complex-number))
    (sqrt (+ (* (real-part z) (real-part z))
             (* (imag-part z) (imag-part z)))))

(defmethod angle ((z complex-number))
    (atan (imag-part z) (real-part z)))

(defun make-complex-from-real-imag (x y)
    (make-instance 'complex-number :real x :imag y))

(defun make-complex-from-mag-ang (r a)
    (make-instance 'complex-number
        :real (* r (cos a))
        :imag (* r (sin a))))

(defmethod add-complex ((z1 complex-number) (z2 complex-number))
    (make-complex-from-real-imag
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))

(defmethod sub-complex ((z1 complex-number) (z2 complex-number))
    (make-complex-from-real-imag
     (- (real-part z1) (real-part z2))
     (- (imag-part z1) (imag-part z2))))

(defmethod mul-complex ((z1 complex-number) (z2 complex-number))
    (make-complex-from-mag-ang
     (* (magnitude z1) (magnitude z2))
     (+ (angle z1) (angle z2))))

(defmethod div-complex ((z1 complex-number) (z2 complex-number))
    (make-complex-from-mag-ang
     (/ (magnitude z1) (magnitude z2))
     (- (angle z1) (angle z2))))

;; Representation of `3 + 4i` in rectangular form
;    +---+---+   +---+---+    +---+---+
; -->| o | o-+-->| o | o-+--> | o | o |
;    +---+---+   +---+---+    +---+---+
;      ↓           ↓            ↓   ↓
;   complex    rectangular      3   4

(defun find-assoc (key alist)
    (cond
     ((null alist) nil)
     ((equal key (caar alist)) (cadar alist))
     (t (find-assoc key (cdr alist)))))

(defun add-assoc (key val alist) (cons (list key val) alist))

;; Table: A set of bindings
; Binding: A pairing of a key and a value
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

(defparameter table1-tag 'table1)

;; void -> Table1<anytype, anytype>
(defun make-table1 () (cons table1-tag nil))

;; Table1<k, v>, k -> (v | null)
(defun table1-get (tbl key) (find-assoc key (cdr tbl)))

;; Table1<k, v> k, v -> undef
(defun table1-put! (tbl key val) 
    (set-cdr! tbl (add-assoc key val (cdr tbl))))

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
;
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