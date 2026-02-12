;; Data-directed programming:
; Functions that decide what to do based on the arguments
; id est, exempli gratia:
;   area: triangle|square|circle -> number

;; Defensive programming:
; Functions that fail gracefully if given bad arguments, much better to give an error message than to return garbage

;; Build modular systems: Start simple, then extend
; Build eval in a way that it will extend easily & safely
; - Easily: Requires data-directed programming
; - Safely: Requires defensive programming

(defclass complex-number () ())

(defclass rectangular-complex (complex-number)
        ((real :initarg :real
               :accessor real-part)
         (imag :initarg :imag
               :accesor imag-part)))

(defclass polar-complex (complex-number)
        ((magnitude :initarg :magnitude
                    :accessor magnitude)
         (angle :initarg :angle
                :accessor angle)))

(defgeneric real-part (z))

(defgeneric imag-part (z))

(defgeneric magnitude (z))

(defgeneric angle (z))

(defmethod magnitude ((z rectangular-complex))
    (sqrt (+ (expt (real-part z) 2)
             (expt (imag-part z) 2))))

(defmethod angle ((z rectangular-complex))
    (atan (imag-part z)
          (real-part z)))

(defmethod real-part ((z polar-complex))
    (* (magnitude z)
       (cos (angle z))))

(defmethod imag-part ((z polar-complex))
    (* (magnitude z)
       (sin (angle z))))

(defun make-from-real-imag (x y)
    (make-instance 'rectangular-complex
        :real x
        :imag y))

(defun make-from-mag-ang (r a)
    (make-instance 'polar-complex
        :magnitude r
        :angle a))

;; Abstract Data Type for sums
; Type: Exp, Exp -> SumExp
(defun make-sum (addend augend)
    (list '+ addend augend))

; Type: anytype -> boolean
(defun sum-exp-p (e)
    (and (consp e) (eq (car e) '+)))

; Type: SumExp -> Exp
(defun sum-addend (sum) (cadr sum))

(defun sum-augend (sum) (caddr sum))

; Type: number | SumExp -> number
(defun eval-1 (exp)
    (cond
     ((numberp exp) exp)
     ((sum-exp-p exp)
         (+ (eval-1 (sum-addend exp))
            (eval-1 (sum-augend exp))))
     (t
         (error "Unknown expression ~A" exp))))

;; Abstract Data Types for ranges (no tags)
; Type: number, number -> range2
(defun make-range-2 (min max) (list min max))

; Type: range2 -> number
(defun range-min-2 (range) (car range))

(defun range-max-2 (range) (cadr range))

; Type: range2, range2 -> range2
(defun range-add-2 (r1 r2)
    (make-range-2
     (+ (range-min-2 r1) (range-min-2 r2))
     (+ (range-max-2 r1) (range-max-2 r2))))

; Type: number|range2|SumExp -> number|range2
(defun eval-2 (exp)
    (cond
     ((numberp exp) exp)
     ((sum-exp-p exp)
         (let ((v1 (eval-2 (sum-addend exp)))
               (v2 (eval-2 (sum-augend exp))))
             (if (and (numberp v1) (numberp v2))
                 (+ v1 v2)
                 (range-add-2 v1 v2))))
     ((consp exp) exp)  ; a range
     (t (error "Unknown expression ~A" exp))))

;; Common bug: Calling a function on the wrong type of data
; - typos
; - brainos
; - changing one part of the program and not another
;; Common result: The function returns garbage
; - Why? Primary predicates (numberp, consp) are ambiguous
; - Something fails later, but cause is hard to track down
; - Worst case: Program produces incorrect output

(defparameter sum-tag '+)

; Type: Exp, Exp -> SumExp
(defun make_sum (addend augend) (list sum-tag addend augend))

; Type: anytype -> boolean
(defun sum_exp-p (e) (and (consp e) (eq (car e) sum-tag)))

(defparameter constant-tag 'const)

; Type: number -> ConstantExp
(defun make-constant (val) (list constant-tag val))

; Type: anytype -> boolean
(defun constant-exp-p (e) (and (consp e) (eq (car e) constant-tag)))

; Type: ConstantExp -> number
(defun constant-val (const) (cadr const))

; Type: ConstantExp | SumExp -> number
(defun eval-3 (exp)
    (cond
     ((constant-exp-p exp) (constant-val exp))
     ((sum_exp-p exp)
         (+ (eval-3 (sum-addend exp))
            (eval-3 (sum-augend exp))))
     (t (error "Unknown expr type: ~A" exp))))

; Type: ConstantExp, ConstantExp -> ConstantExp
(defun constant-add (c1 c2)
    (make-constant (+ (constant-val c1)
                      (constant-val c2))))

; Type: ConstantExp | SumExp -> ConstantExp
(defun eval-4 (exp)
    (cond
     ((constant-exp-p exp) exp)
     ((sum_exp-p exp)
         (constant-add (eval-4 (sum-addend exp))
                       (eval-4 (sum-augend exp))))
     (t (error "Unknown expr type: ~A" exp))))

;; Standard pattern for an ADT with tagged data
; - A variable in the ADT implementation stores the tag
; - Attach the tag in the constructor
; - Write a predicate that checks the tag
;   - Determines whether an object belongs to the ADT
; - Operations strip the tags, operate, attach the tag again
;; Must use tagged data everywhere to get full benefits including return values

(defparameter range-tag 'range)

; Type: number, number -> RangeExp
(defun make-range (min max) (list range-tag min max))

; Type: anytype -> boolean
(defun range-exp-p (e) (and (consp e) (eq (car e) range-tag)))

; Type: RangeExp -> number
(defun range-min (range) (cadr range))

(defun range-max (range) (caddr range))

; Type: ConstantExp | RangeExp | SumExp -> ConstantExp | RangeExp
(defun eval-5 (exp)
    (cond
     ((constant-exp-p exp) exp)
     ((range-exp-p exp) exp)
     ((sum_exp-p exp)
         (let ((v1 (eval-5 (sum-addend exp)))
               (v2 (eval-5 (sum-augend exp))))
             (if (and (constant-exp-p v1) (constant-exp-p v2))
                 (constant-add v1 v2)
                 (range-add-2 (val2range v1) (val2range v2)))))
     (t (error "Unknown expr type: ~A" exp))))

; ValueExp = ConstantExp | RangeExp
(defun value-exp-p (v) (or (constant-exp-p v) (range-exp-p v)))

; Type: ValueExp, ValueExp -> ValueExp
(defun value-add-6 (v1 v2)
    (if (and (constant-exp-p v1) (constant-exp-p v2))
        (constant-add v1 v2)
        (range-add-2 (val2range v1) (val2range v2))))

; If argument is a range, return it. Else, make the range [x, x] from a constant x
; Type: ValueExp | RangeExp -> RangeExp
(defun val2range (v)
    (if (range-exp-p v)
        v
        (make-range v v)))

; ValueExp = ConstantExp | RangeExp
; Type: ValueExp | SumExp -> VauleExp
(defun eval-6 (exp)
    (cond
     ((value-exp-p exp) exp)
     ((sum-exp-p exp)
         (value-add-6 (eval-6 (sum-addend exp))
                      (eval-6 (sum-augend exp))))
     (t (error "Unknown expr type: ~A" exp))))

(defparameter limited-tag 'limited)

(defun make-limited-precision (val err) (list limited-tag val err))

(defun limited-exp-p (exp) (and (consp exp) (eq (car exp) limited-tag)))

; Value|Limited|SumExp -> ValueExp|Limited
(defun eval-7 (exp)
    (cond
     ((value-exp-p exp) exp)
     ((limited-exp-p exp) exp)
     ((sum-exp-p exp)
         (value-add-6 (eval-7 (sum-addend exp))
                      (eval-7 (sum-augend exp))))
     (t (error "Unknown expr type: ~A" exp))))

; Type: ValueExp, ValueExp -> ValueExp
(defun value-add-7 (v1 v2)
    (cond
     ((and (constant-exp-p v1) (constant-exp-p v2))
         (constant-add v1 v2))
     ((and (value-exp-p v1) (value-exp-p v2))
         (range-add-2 (val2range v1) (val2range v2)))
     (t
         (error "Unknown exp: ~A or ~A" v1 v2))))

;; Rule of thumb: When checking types, use the else branch only for errors
;; Data directed programming can simplify highger level code
;; Using tagged data is only defensive programming. If you check the tags don't use the else branch of `if` or `cond`
;; Traditionally, ADT operators and accessors don't check tags
; - Ommited for efficiency: Assume checked at the higher level
; - A check in `constant-val` would have trapped this bug
; - Add checks into your ADT implementation to be paraoind
; > Andy Grove: Only the paranoid survive