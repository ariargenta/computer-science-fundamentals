;;; Sequences as conventional interfaces (signal-flow processing)
;; Computation as data flowing through composable stages
; Sequence abstraction localizes data-structure dependencies, allowing processing modules to combine freely.
(defun square (x) (* x x))

(defun fib (n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (t (+ (fib (- n 1))
                (fib (- n 2))))))

(defun sum-odd-squares (tree)
    (cond ((null tree) 0)
          ((not (consp tree))
              (if (oddp tree) (square tree) 0))
          (t (+ (sum-odd-squares (car tree))
                (sum-odd-squares (cdr tree))))))

(defun even-fibs (n)
    (labels ((next (k)
                   (if (> k n)
                       nil
                       (let ((f (fib k)))
                           (if (evenp f)
                               (cons f (next (+ k 1)))
                               (next (+ k 1)))))))
        (next 0)))

;; Sequence operations
; Enumerate (generate), Filter (select), Map (transform), Accumulate (combine)
(princ (mapcar #'square (list 1 2 3 4 5))) (terpri)

(defun filter (predicate sequence)
    (cond ((null sequence) nil)
          ((funcall predicate (car sequence))
              (cons (car sequence)
                    (filter predicate (cdr sequence))))
          (t (filter predicate (cdr sequence)))))

(princ (filter #'oddp (list 1 2 3 4 5))) (terpri)

(defun accumulate (op initial sequence)
    (if (null sequence)
        initial
        (funcall op (car sequence)
            (accumulate op initial (cdr sequence)))))

(princ (accumulate #'+ 0 (list 1 2 3 4 5))) (terpri)
(princ (accumulate #'* 1 (list 1 2 3 4 5))) (terpri)
(princ (accumulate #'cons nil (list 1 2 3 4 5))) (terpri)

(defun enumerate-interval (low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))

(princ (enumerate-interval 2 7)) (terpri)

(defun enumerate-tree (tree)
    (cond ((null tree) nil)
          ((not (consp tree)) (list tree))
          (t (append (enumerate-tree (car tree))
                 (enumerate-tree (cdr tree))))))

(princ (enumerate-tree (list 1 (list 2 (list 3 4)) 5))) (terpri)

(defun sumOddSquares (tree)
    (accumulate
     #'+ 0 (mapcar #'square (filter #'oddp (enumerate-tree tree)))))

(defun evenFibs (n)
    (accumulate
     #'cons
     nil
     (filter #'evenp (mapcar #'fib (enumerate-interval 0 n)))))

(defun list-fib-squares (n)
    (accumulate
     #'cons
     nil
     (mapcar #'square (mapcar #'fib (enumerate-interval 0 n)))))

(princ (list-fib-squares 10)) (terpri)

(defun product-of-squares-of-odd-elements (sequence)
    (accumulate #'* 1 (mapcar #'square (filter #'oddp sequence))))

(princ (product-of-squares-of-odd-elements (list 1 2 3 4 5))) (terpri)