;; A data abstraction consist of:
; - Constructors
; - Selectors
; - Mutators
; - Operations
; - Contract

(defun memq (item x)
    (cond ((null x) nil)
          ((eq item (car x)) x)
          (t (memq item (cdr x)))))

(memq 'apple '(pear banana prune))
(memq 'apple '(x (apple sauce) y apple pear))

;; Two quote expressions with the same symbol return the same symbol object
; (list ('delta) ('delta))
;    +---+---+   +---+---+ 
; -->| o | o-+-->| o | / +
;    +---+---+   +---+---+ 
;      ↓           |
;   symbol <-------
;   delta