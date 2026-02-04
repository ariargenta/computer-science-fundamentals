;; Sets as unordered lists
(defun element-of-set? (x set)
    (cond ((null set) nil)
          ((equal x (car set)) t)
          (t (element-of-set-p x (cdr set)))))

(defun adjoin-set (x set)
    (if (element-of-set? x set)
        set
        (cons x set)))

(defun intersection-set (set1 set2)
    (cond ((or (null set1) (null set2)) '())
          ((element-of-set? (car set1) set2)
              (cons (car set1) (intersection-set (cdr set1) set2)))
          (t (intersection-set (cdr set1) set2))))

;; Sets as ordered lists
(defun element-of-set-p (x set)
    (cond ((null set) nil)
          ((= x (car set)) t)
          ((< x (car set)) nil)
          (t (element-of-set? x (cdr set)))))

(defun intersectionSet (set1 set2)
    (if (or (null set1) (null set2))
        '()
        (let ((x1 (car set1)) (x2 (car set2)))
            (cond ((= x1 x2)
                      (cons x1 (intersectionSet (cdr set1)
                                                 (cdr set2))))
                  ((< x1 x2)
                      (intersectionSet (cdr set1) set2))
                  ((< x2 x1)
                      (intersectionSet set1 (cdr set2)))))))

;; Sets as binary trees
(defun entry (tree) (car tree))

(defun left-branch (tree) (cadr tree))

(defun right-branch (tree) (caddr tree))

(defun make-tree (entry left right)
    (list entry left right))

(defun elementOfSet (x set)
    (cond ((null set) nil)
          ((= x (entry set)) t)
          ((< x (entry set))
              (elementOfSet x (left-branch set)))
          ((> x (entry set))
              (elementOfSet x (right-branch set)))))

(defun adjoinSet (x set)
    (cond ((null set) (make-tree x '() '()))
          ((= x (entry set)) set)
          ((< x (entry set))
              (make-tree (entry set)
                         (adjoinSet x (left-branch set))
                         (right-branch set)))
          ((> x (entry set))
              (make-tree (entry set) (left-branch set)
                         (adjoinSet x (right-branch set))))))

;; Sets and information retrieval
(defun lookup (given-key set-of-records)
    (cond ((null set-of-records) nil)
          ((equal given-key (key (car set-of-records)))
              (car set-of-records))
          (t (lookup given-key (cdr set-of-records)))))