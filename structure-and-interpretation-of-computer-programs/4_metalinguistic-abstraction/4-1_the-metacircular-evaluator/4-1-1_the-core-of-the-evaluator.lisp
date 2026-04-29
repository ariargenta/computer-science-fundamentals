(defpackage "METALINGUISTIC-ABSTRACTION"
    (:nicknames "CAP4")
    (:use "COMMON-LISP")
    (:shadow "EVAL" "APPLY"))

(in-package "CAP4")

(defun eval (exp env) (funcall (analyse exp) env))

(defun analyse (exp)
    (cond ((self-evaluating-p exp) (analyse-self-evaluating exp))
          ((variable-p exp) (analyse-variable exp env))
          ((quoted-p exp) (analyse-quoted exp))
          ((assignment-p exp) (analyse-assignment exp))
          ((definition-p exp) (analyse-definition exp))
          ((if-p exp) (analyse-if exp))
          ((lambda-p exp) (analyse-lambda exp))
          ((begin-p exp) (analyse-sequence (begin-actions exp)))
          ((cond-p exp) (analyse (expand-cond exp)))
          ((application-p exp) (analyse-application exp))
          (t (error "Unknown expression type: ANALYSE ~A" exp))))

(defun apply (procedure arguments)
    (cond ((primitive-procedure-p procedure)
              (apply-primitive-procedure procedure arguments))
          ((compound-procedure-p procedure)
              (eval-sequence
               (procedure-body procedure)
               (extend-environment
                (procedure-parameters procedure)
                arguments
                (procedure-environment procedure))))
          (t
              (error
                      "Unknown procedure type: APPLY ~A" procedure))))

(defun list-of-values (exps env)
    (if (no-operands-p exps)
        '()
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

(defun eval-if (exp env)
    (if (true-p (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

(defun eval-sequence (exps env)
    (cond ((last-exp-p exps)
              (eval (first-exp exps) env))
          (t
              (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(defun eval-assignment (exp env)
    (set-variable-value (assignment-variable exp)
                        (eval (assignment-value exp) env)
                        env)
    'ok)

(defun eval-definition (exp env)
    (define-variable (definition-variable exp)
                     (eval (definition-value exp) env)
                     env)
    'ok)

;;; 4.1.2 - Representing expressions

(defun self-evaluating-p (exp)
    (cond ((numberp exp) t)
          ((stringp exp) t)
          (t nil)))

(defun variable-p (exp) (symbolp exp))

(defun quoted-p (exp) (tagged-list-p exp 'quote))

(defun text-of-quotation (exp) (cadr exp))

(defun tagged-list-p (exp tag)
    (if (consp exp)
        (eq (car exp) tag)
        nil))

(defun assignment-p (exp) (tagged-list-p exp 'setf))

(defun assignment-variable (exp) (cadr exp))

(defun assignment-value (exp) (caddr exp))

(defun definition-p (exp) (tagged-list-p exp 'defun))

(defun definition-variable (exp)
    (if (symbolp (cadr exp))
        (cadr exp)
        (caadr exp)))

(defun definition-value (exp)
    (if (symbolp (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)    ; formal parameters
                     (cddr exp))))  ; body

(defun lambda-p (exp) (tagged-list-p exp 'lambda))

(defun lambda-parameters (exp) (cadr exp))

(defun lambda-body (exp) (cddr exp))

(defun make-lambda (parameters body) (cons 'lambda (cons parameters body)))

(defun if-p (exp) (tagged-list-p exp 'if))

(defun if-predicate (exp) (cadr exp))

(defun if-consequent (exp) (caddr exp))

(defun if-alternative (exp)
    (if (not (null (cdddr exp)))
        (cadddr exp)
        'nil))

(defun make-if (predicate consequent alternative) (list 'if predicate consequent alternative))

(defun begin-p (exp) (tagged-list-p exp 'begin))

(defun begin-actions (exp) (cdr exp))

(defun last-exp-p (seq) (null (cdr seq)))

(defun first-exp (seq) (car seq))

(defun rest-exps (seq) (cdr seq))

(defun expand-sequence (seq)
    (cond ((null seq) seq)
          ((last-exp-p seq) (first-exp seq))
          (t (make-begin seq))))

(defun make-begin (seq) (cons 'begin seq))

(defun application-p (exp) (consp exp))

(defun operator (exp) (car exp))

(defun operands (exp) (cdr exp))

(defun no-operands-p (ops) (null ops))

(defun first-operand (ops) (car ops))

(defun rest-operands (ops) (cdr ops))

(defun cond-p (exp) (tagged-list-p exp 'cond))

(defun cond-clauses (exp) (cdr exp))

(defun cond-else-clause-p (clause) (eq (cond-predicate clause) 'else))

(defun cond-predicate (clause) (car clause))

(defun cond-actions (clause) (cdr clause))

(defun expand-cond (exp) (expand-clauses (cond-clauses exp)))

(defun expand-clauses (clauses)
    (if (null clauses)
        'false
        (let ((first (car clauses))
              (rest (cdr clauses)))
            (if (cond-else-clause-p first)
                (if (null rest)
                    (expand-sequence (cond-actions first))
                    (error "ELSE clause is not last: EXPAND-COND ~A" clauses))
                (make-if (cond-predicate first)
                         (expand-sequence (cond-actions first))
                         (expand-clauses rest))))))

;;; 4.1.3 - Evaluator Data Structures

(defun true-p (x) (not (eq x nil)))

(defun false-p (x) (eq x nil))

(defun make-procedure (parameters body env) (list 'procedure parameters body env))

(defun compound-procedure-p (p) (tagged-list-p p 'procedure))

(defun procedure-parameters (p) (cadr p))

(defun procedure-body (p) (caddr p))

(defun procedure-environment (p) (cadddr p))

(defun enclosing-environment (env) (cdr env))

(defun first-frame (env) (car env))

(defparameter the-empty-environment '())

(defun make-frame (variables values) (cons variables values))

(defun frame-variables (frame) (car frame))

(defun frame-values (frame) (cdr frame))

(defun add-binding-to-frame (var val frame)
    (setf (car frame) (cons var (car frame)))
    (setf (cdr frame) (cons val (cdr frame))))

(defun extend-environment (vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied ~A ~B" vars vals)
            (error "Too few arguments supplied ~A ~B" vars vals))))

(defun lookup-variable-value (var env)
    (labels ((env-loop (env)
                       (labels ((scan (vars vals)
                                      (cond ((null vars)
                                                (env-loop (enclosing-environment env)))
                                            ((eq var (car vars)) (car vals))
                                            (t (scan (cdr vars) (cdr vals))))))
                           (if (eq env the-empty-environment)
                               (error "Unbound variable ~A" var)
                               (let ((frame (first-frame env)))
                                   (scan (frame-variables frame)
                                         (frame-values frame)))))))
        (env-loop env)))

(defun set-variable-value (var val env)
    (labels ((env-loop (env)
                       (labels ((scan (vars vals)
                                      (cond ((null vars)
                                                (env-loop (enclosing-environment env)))
                                            ((eq var (car vars)) (setf (car vals) val))
                                            (t (scan (cdr vars) (cdr vals))))))
                           (if (eq env the-empty-environment)
                               (error "Unbound variable: SETF ~A" var)
                               (let ((frame (first-frame env)))
                                   (scan (frame-variables frame)
                                         (frame-values frame)))))))
        (env-loop env)))

(defun define-variable (var val env)
    (let ((frame (first-frame env)))
        (labels ((scan (vars vals)
                       (cond ((null vars)
                                 (add-binding-to-frame var val frame))
                             ((eq var (car vars)) (setf (car vals) val))
                             (t (scan (cdr vars) (cdr vals))))))
            (scan (frame-variables frame) (frame-values frame)))))

;;; 4.1.4 - Running the Evaluator as a Program

(defun setup-environment ()
    (let ((initial-env
           (extend-environment (primitive-procedure-names)
                               (primitive-procedure-objects)
                               the-empty-environment)))
        (define-variable 'true t initial-env)
        (define-variable 'false nil initial-env)
        initial-env))

(defun primitive-procedure-p (proc) (tagged-list-p proc 'primitive))

(defun primitive-implementation (proc) (cadr proc))

(defun primitive-procedures ()
    (list (list 'car #'car)
          (list 'cdr #'cdr)
          (list 'cons #'cons)
          (list 'null #'null)
          ; ⟨more primitives⟩
          ))

(defun primitive-procedure-names () (mapcar #'car (primitive-procedures)))

(defun primitive-procedure-objects ()
    (mapcar (lambda (proc) (list 'primitive (cadr proc)))
            (primitive-procedures)))

(defun apply-primitive-procedure (proc args)
    (apply-in-underlying-lisp
     (primitive-implementation proc) args))

(defun apply-in-underlying-lisp (function arguments) (cl:apply function arguments))

(defparameter input-prompt ";;; M-Eval input: ")
(defparameter output-prompt ";;; M-Eval value: ")

(defun driver-loop ()
    (prompt-for-input input-prompt)
    (let ((input (read)))
        (let ((output (eval input the-global-environment)))
            (announce-output output-prompt)
            (user-print output)))
    (driver-loop))

(defun prompt-for-input (string)
    (terpri) (terpri) (princ string) (terpri))

(defun announce-output (string)
    (terpri) (princ string) (terpri))

(defun user-print (object)
    (if (compound-procedure-p object)
        (princ (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
        (princ object)))

;;; 4.1.7 - Separating Syntactic Analysis from Execution

(defun analyse-self-evaluating (exp)
    (lambda (env) (declare (ignore env)) exp))

(defun analyse-quoted (exp)
    (let ((qval (text-of-quotation exp)))
        (lambda (env) (declare (ignore env)) qval)))

(defun analyse-variable (exp) (lambda (env) (lookup-variable-value exp env)))

(defun analyse-assignment (exp)
    (let ((var (assignment-variable exp))
          (vproc (analyse (assignment-value exp))))
        (lambda (env)
            (set-variable-value var (funcall vproc env) env)
            'ok)))

(defun analyse-definition (exp)
    (let ((var (definition-variable exp))
          (vproc (analyse (definition-value exp))))
        (lambda (env)
            (define-variable var (funcall vproc env) env)
            'ok)))

(defun analyse-if (exp)
    (let ((pproc (analyse (if-predicate exp)))
          (cproc (analyse (if-consequent exp)))
          (aproc (analyse (if-alternative exp))))
        (lambda (env) (if (true-p (funcall pproc env))
                          (funcall cproc env)
                          (funcall aproc env)))))

(defun analyse-lambda (exp)
    (let ((vars (lambda-parameters exp))
          (bproc (analyse-sequence (lambda-body exp))))
        (lambda (env) (make-procedure vars bproc env))))

(defun analyse-sequence (exps)
    (labels ((sequentially (proc1 proc2)
                           (lambda (env) (funcall proc1 env) (funcall proc2 env)))
             (looping (first-proc rest-procs)
                      (if (null rest-procs)
                          first-proc
                          (looping (sequentially first-proc (car rest-procs))
                                   (cdr rest-procs)))))
        (let ((procs (map 'list #'analyse exps)))
            (if (null procs) (error "Emptyy sequence: ANALYSE"))
            (looping (car procs) (cdr procs)))))

(defun analyse-application (exp)
    (let ((fproc (analyse (operator exp)))
          (aprocs (map 'list #'analyse (operands exp))))
        (lambda (env)
            (execute-application
             (funcall fproc env)
             (map 'list (lambda (aproc) (funcall aproc env))
                     aprocs)))))

(defun execute-application (proc args)
    (cond ((primitive-procedure-p proc)
              (apply-primitive-procedure proc args))
          ((compound-procedure-p proc)
              (funcall (procedure-body proc)
                       (extend-environment
                        (procedure-parameters proc)
                        args
                        (procedure-environment proc))))
          (t (error "Unknown procedure type: EXECUTE-APPLICATION ~A" proc))))

(defparameter the-global-environment (setup-environment))