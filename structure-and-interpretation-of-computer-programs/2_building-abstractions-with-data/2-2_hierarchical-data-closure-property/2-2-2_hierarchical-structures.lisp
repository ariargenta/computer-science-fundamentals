; Groups - User knows groups are also lists
; Lists - User knows list are also pairs
; Pairs - Contract for cons, car, cdr
; Specified the implementations for lists and groups: Weak data abstraction

;; Pair abstraction
; 1. Constructor
;   cons: A, B -> Pair<A, B>;  A & B = anytype
;   (cons <x> <y>) ==> <p>
; 2. Accessors
;   (car <p>); car: Pair<A, B> -> A
;   (cdr <p>); cdr: Pair<A, B> -> B
; 3. Contract
;   (car (cons <x> <y>)) ==> <x>
;   (cdr (cons <x> <y>)) ==> <y>
; 4. Operations
;   pair?: anytype -> boolean
;   (pair? <p>)
; 5. Abstraction barrier
;   Say nothing about implementation
; 6. Concrete representation & implementation
;   Could have alternative implementations

;;; Trees
;; Type definition:
; List<C> = Pair<C, List<C>> | null
; Tree<C> = List<Tree<C>> | Leaf<C>
; Leaf<C> = C

(defparameter x (cons (list 1 2) (list 3 4)))

;           ((1 2) 3 4)
;              /   |  \
;           (1 2)  3   4
;            / \
;           1   2

;                +---+---+   +---+---+   +---+---+
; ((1 2) 3 4) -->| o | o-+-->| o | o-+-->| o | / |
;                +---+---+   +---+---+   +---+---+
;                  ↓           ↓           ↓
;                (1 2)         3           4
;                  ↓
;                +---+---+   +---+---+
;                | o | o-+-->| o | / |
;                +---+---+   +---+---+
;                  ↓           ↓
;                  1           2

(defun count-leaves (x)
    (cond ((null x) 0)      ; The order of the first two clauses matters
          ((not (consp x)) 1)
          (t (+ (count-leaves (car x))  ; The recursive structure of the procedure will tend to reflect the recursive structure of the data structure itself
                (count-leaves (cdr x))))))

(defun leaf-p (x)
    (not (consp x)))

(defun countLeaves (tree)
    (cond ((null tree) 0)   ; Base case
          ((leaf-p tree) 1) ; Base case
          (t                ; Recursive case
              (+ (countLeaves (car tree))
                 (countLeaves (cdr tree))))))

(princ (length x)) (terpri)
(princ (count-leaves x)) (terpri)
(princ (list x x)) (terpri)
(princ (length (list x x))) (terpri)
(princ (count-leaves (list x x))) (terpri)

;; Mapping over trees
(defun scale-tree (tree factor)
    (cond ((null tree) nil)
          ((not (consp tree)) (* tree factor))
          (t (cons (scale-tree (car tree) factor)
                   (scale-tree (cdr tree) factor)))))

(princ (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)) (terpri)

(defun scaleTree (tree factor)
    (mapcar (lambda (sub-tree)
                (if (consp sub-tree)
                    (scaleTree sub-tree factor)
                    (* sub-tree factor)))
            tree))

(defun tree-map (proc tree)
    (if (null tree)
        nil
        (if (leaf-p tree)
            (funcall proc tree)
            (cons (tree-map proc (car tree))
                  (tree-map proc (cdr tree))))))

(defun tree-manip (leaf-op init merge tree)
    (if (null tree)
        init
            (if (leaf-p tree)
                (funcall leaf-op tree)
                (funcall merge (tree-manip leaf-op init merge (car tree))
                               (tree-manip leaf-op init merge (cdr tree))))))