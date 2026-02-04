(defun make-leaf(symbolic weight) (list 'leaf symbolic weight))

(defun leaf? (object) (eq (car object) 'leaf))

(defun symbol-leaf (x) (cadr x))

(defun weight-leaf (x) (caddr x))

(defun left-branch (tree) (car tree))

(defun right-branch (tree) (cadr tree))

(defun symbols (tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

(defun weight (tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (caddr tree)))

(defun make-code-tree (left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

(defun choose-branch (bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (t (error "Bad bit: CHOOSE-BRANCH ~A" bit))))

(defun decode (bits tree)
    (labels ((decode-1 (bits current-branch)
                       (if (null bits)
                           '()
                           (let ((next-branch
                                  (choose-branch (car bits) current-branch)))
                               (if (leaf? next-branch)
                                   (cons (symbol-leaf next-branch)
                                         (decode-1 (cdr bits) tree))
                                   (decode-1 (cdr bits) next-branch))))))
        (decode-1 bits tree)))

(defun adjoin-set (x set)
    (cond ((null set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (t (cons (car set)
                   (adjoin-set x (cdr set))))))

(defun make-leaf-set (pairs)
    (if (null pairs)
        '()
        (let ((pair (car pairs)))
            (adjoin-set (make-leaf (car pair)       ; symbol
                                   (cadr pair))     ; frequency
                        (make-leaf-set (cdr pairs))))))