(defun memq (item x)
    (cond ((null x) nil)
          ((eq item (car x)) x)
          (t (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))