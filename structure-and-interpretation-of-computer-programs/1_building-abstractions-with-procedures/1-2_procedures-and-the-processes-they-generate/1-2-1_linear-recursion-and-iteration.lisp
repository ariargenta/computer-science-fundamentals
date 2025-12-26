; Linear recursive process
(defun factorial (n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

; Linear iterative process
(defun fact-iter (product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))

(defun factorialProduct (n)
    (fact-iter 1 1 n))

; Block structure
(defun factProd (n)
    (labels ((iter (product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1)))))
        (iter 1 1)))