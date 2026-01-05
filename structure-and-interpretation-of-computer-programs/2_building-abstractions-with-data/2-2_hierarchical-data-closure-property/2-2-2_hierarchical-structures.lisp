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
          (t (+ (count-leaves (car x))
                (count-leaves (cdr x))))))

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