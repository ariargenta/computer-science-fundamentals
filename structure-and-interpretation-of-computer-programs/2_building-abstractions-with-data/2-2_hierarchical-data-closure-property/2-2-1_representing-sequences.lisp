;;; Conventional interfaces - lists
; A list is a data object that can hold an arbitrary number of ordered items
; Lists are closed under operations of cons and cdr
; - car-part of a pair in sequence - holds an item
; - cdr-part of a pair in sequence - holds a pointer to rest of list
; - Empty-list nil - signals no more pairs, end of the list
;
; (list ⟨a_1⟩ ⟨a_2⟩... ⟨a_n⟩)
; ==
; (cons ⟨a_1⟩
;       (cons ⟨a_2⟩)
;             (cons ...
;                    (cons ⟨a_n⟩
;                           nil)...)))
;
; (cons <el1> <el2>)
;    +-----+-----+
;    |     |     |
; -->|     |   --|--> <el2>
;    |  |  |     |
;    +--|--+-----+
;       |
;       v
;     <el1>
;
; (list <el1> <el2> ... <eln>)
;    +---+---+   +---+---+           +---+---+
; -->| o | o-+-->| o | o-+--> ... -->| o | / |
;    +---+---+   +---+---+           +---+---+
;      ↓           ↓                   ↓
;    <el1>       <el2>               <eln>
;
; (null? <z>)
;     ==> #t if <z> evaluates to empty list
(defparameter one-through-four (list 1 2 3 4))

(princ one-through-four) (terpri)

;(cadr ⟨arg⟩) = (car (cdr ⟨arg⟩))
(princ (car one-through-four)) (terpri)
(princ (cdr one-through-four)) (terpri)
(princ (car (cdr one-through-four))) (terpri)
(princ (cons 10 one-through-four)) (terpri)
(princ (cons 5 one-through-four)) (terpri)

;; List operations
; cons'ing up a list
(defun enumerate-interval (from to)
    (if (> from to)
        nil
        (adjoin from (enumerate-interval (+ 1 from) to))))

; cdr'ing down a list
(defun list-reference (lst n)
    (if (= n 0)
        (first lst)
        (list-ref (rest lst)
                  (- n 1))))

(defun length-list (lst)
    (if (null lst)
        0
        (+ 1 (length-list (rest lst)))))

(defun copy (lst)
    (if (null lst)
        nil                             ; base case
        (adjoin (first lst)             ; recursion
                (copy (rest lst)))))

(defun append-list (list1 list2)
    (cond ((null list1) list2)                  ; base
          (t
              (adjoin (first list1)             ; recursion
                      (append-list (rest list1)
                                   list2)))))

(defparameter group (list p1 p2 p3 p4 p5 p6 p7 p8 p9))

(defun stretch-group (gp sc)
    (if (null gp)
        nil
        (adjoin (stretch-point (first gp) sc)
                (stretch-group (rest gp) sc))))

; Transforming a list
(defun add-x (gp)
    (if (null gp)
        0
        (+ (point-x (first gp))
           (add-x (rest gp)))))

(defun add-y (gp)
    (if (null gp)
        0
        (+ (point-y (first gp))
           (add-y (rest gp)))))

(defun centroid (gp)
    (let ((x-sum (add-x gp))
          (y-sum (add-y gp))
          (how-many (length-list gp)))
      (make-point (/ x-sum how-many)
                  (/ y-sum how-many))))

(defun list-ref (items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (- n 1))))

(defparameter squares (list 1 4 9 16 25))

(princ (list-ref squares 3)) (terpri)

(defun length-items (items)
    (if (null items)
        0
        (+ 1 (length-items (cdr items)))))

(defparameter odds (list 1 3 5 7))

(princ (length-items odds)) (terpri)

(defun lengthItems (items)
    (labels ((length-iter (a count)
                          (if (null a)
                              count
                              (length-iter (cdr a) (+ 1 count)))))
      (length-iter items 0)))

(princ (append squares odds)) (terpri)
(princ (append odds squares)) (terpri)

(defun appendList (list1 list2)
    (if (null list1)
        list2
        (cons (car list1) (appendList (cdr list1) list2))))

;; Mapping over lists
(defun square-list (lst)
    (if (null lst)
        nil
        (adjoin (square (first lst))
                (square-list (rest lst)))))

(defun double-list (lst)
    (if (null lst)
        nil
        (adjoin (* 2 (first lst))
                (double-list (rest lst)))))

(defun mapping (proc lst)
    (if (null lst)
        nil
        (adjoin (funcall proc (first lst))
                (mapping proc (rest lst)))))

(defun squareList (lst)
    (mapping #'square lst))

(defun doubleList (lst)
    (mapping (lambda (x) (* 2 x)) lst))

(defun scale-list (items factor)
    (if (null items)
        nil
        (cons (* (car items) factor)
              (scale-list (cdr items)
                          factor))))

(princ (scale-list (list 1 2 3 4 5) 10)) (terpri)

(defun map-car (proc items)
    (if (null items)
        nil
        (cons (funcall proc (car items))
              (map-car proc (cdr items)))))

(princ (map-car #'abs (list -10 2.5 -11.6 17))) (terpri)
(princ (map-car (lambda (x) (* x x)) (list 1 2 3 4))) (terpri)

(defun scaleList (items factor)
    (mapcar (lambda (x) (* x factor))
            items))

; - There are conventional ways of grouping elements together into compound data structures
; - The procedures that manipulate these data structures tend to have a form that mimics the actual data structure
; - Compound data structures rely on an inductive format in much the same way recursive procedures do. We can often deduce properties of compound data structures in analogy to our analysis of recursive procedures by using induction

; - Pair<A, B>
;   A compound data structure formed by a cons pair, in which the first element is of type A, and the second of type B
; - List<A> = Pair<A, List<A> or nil>
;   A compound data structure that is recursively defined as a pair, whose first element is of type A, and whose second element is either a list of type A or the empty list