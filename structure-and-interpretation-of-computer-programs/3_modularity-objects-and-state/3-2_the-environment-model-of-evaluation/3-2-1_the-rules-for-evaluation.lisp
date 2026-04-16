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
;; Table: A set of bindings
;; Frame: a table of bindings
;; Binding: a pairing of a key and a value
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