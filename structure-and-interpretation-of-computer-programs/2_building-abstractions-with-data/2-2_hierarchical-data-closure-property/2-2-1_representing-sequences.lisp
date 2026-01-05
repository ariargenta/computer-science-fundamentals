; (list ⟨a_1⟩ ⟨a_2⟩... ⟨a_n⟩)
; ==
; (cons ⟨a_1⟩
;       (cons ⟨a_2⟩)
;             (cons ...
;                    (cons ⟨a_n⟩
;                           nil)...)))
(defparameter one-through-four (list 1 2 3 4))

(princ one-through-four) (terpri)

;(cadr ⟨arg⟩) = (car (cdr ⟨arg⟩))
(princ (car one-through-four)) (terpri)
(princ (cdr one-through-four)) (terpri)
(princ (car (cdr one-through-four))) (terpri)
(princ (cons 10 one-through-four)) (terpri)
(princ (cons 5 one-through-four)) (terpri)

;; List operations
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

(defun append-list (list1 list2)
    (if (null list1)
        list2
        (cons (car list1) (append-list (cdr list1) list2))))

;; Mapping over lists
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