(defpackage "METALINGUISTIC-ABSTRACTION"
    (:nicknames "CAP4")
    (:use "COMMON-LISP")
    (:shadow "EVAL" "APPLY"))

(in-package "CAP4")
(defun eval (exp env)
    (cond ((self-evaluating-p exp) exp)
          ((variable-p exp) (lookup-variable-value exp env))
          ((quoted-p exp) (text-of-quotation exp))
          ((assignment-p exp) (eval-assignment exp env))
          ((definition-p exp) (eval-definition exp env))
          ((if-p exp) (eval-if exp env))
          ((lambda-p exp) (make-procedure (lambda-parameters exp)
                                          (lambda-body exp)
                                          env))
          ((begin-p exp)
              (eval-sequence (begin-actions exp) env))
          ((cond-p exp) (eval (expand-cond exp) env))
          ((application-p exp)
              (apply (eval (operator exp) env)
                      (list-of-values (operands exp) env)))
          (t (error "Unknown expression type: EVAL ~A" exp))))

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