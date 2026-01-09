;; Compound data
(defun make-point (x y)
    (cons x y))

(defun point-x (point)
    (car point))

(defun point-y (point)
    (cdr point))

(defun make-seg (pt1 pt2)
    (cons pt1 pt2))

(defun start-point (seg)
    (car seg))

(defparameter p1 (make-point 1 2))
(defparameter p2 (make-point 4 3))
(defparameter s1 (make-seg p1 p2))

(defun stretch-point (pt scale)
    (make-point (* scale (point-x pt))
                (* scale (point-y pt))))

(defun stretch-seg (seg sc)
    (make-seg (stretch-point (start-pt seg) sc)
              (stretch-point (end-pt seg) sc)))

(defun seg-length (seg)
    (sqrt (+ (square (- (point-x (start-point seg))
                        (point-x (end-point seg))))
             (square (- (point-y (start-point seg))
                        (point-y (end-point seg)))))))

(defun add-interval (x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(defun mul-interval (x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (lower-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(defun div-interval (x y)
    (mul-interval
     x
     (make-interval (/ 1.0 (upper-bound y))
                    (/ 1.0 (lower-bound y)))))

(defun make-interval (a b)
    (cons a b))

(defun make-center-width (c w)
    (make-interval (- c w) (+ c w)))

(defun center (i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(defun width (i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(defun par1 (r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

(defun par2 (r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval
         one (add-interval (div-interval one r1)
                           (div-interval one r2)))))