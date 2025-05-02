;; Core AST definitions and predicates
(define-record-type ast-node
  (make-ast-node type data)
  ast-node?
  (type ast-node-type)
  (data ast-node-data))

(define (variable? expr) (symbol? expr))
(define (literal? expr) (or (number? expr) (boolean? expr)))
(define (lambda? expr) (and (pair? expr) (eq? (car expr) 'lambda)))
(define (typed-lambda? expr) (and (lambda? expr) (not (eq? (caddr expr) #f))))
(define (untyped-lambda? expr) (and (lambda? expr) (eq? (caddr expr) #f)))
(define (application? expr) (and (pair? expr) (eq? (car expr) 'app)))
(define (typed-application? expr)
  (and (application? expr)
       (or (typed-lambda? (operator expr))
           (typed-application? (operator expr))
           (typed-application? (operand expr)))))

;; Expression accessors
(define (operator expr) (cadr expr))
(define (operand expr) (caddr expr))
(define (lambda-param expr) (cadr expr))
(define (lambda-type expr) (if (> (length expr) 3) (caddr expr) #f))
(define (lambda-body expr) (if (> (length expr) 3) (cadddr expr) (caddr expr)))

;; Type system interface
(define-record-type type-system
  (make-type-system base-type? function-type? type-compatible? type-of)
  type-system?
  (base-type? type-system-base-type?)
  (function-type? type-system-function-type?)
  (type-compatible? type-system-compatible?)
  (type-of type-system-infer))

;; Type environment
(define (make-type-env) '())
(define (type-env-extend env var type) (cons (cons var type) env))
(define (type-env-lookup env var)
  (cond ((null? env) '|Any|)
        ((eq? (caar env) var) (cdar env))
        (else (type-env-lookup (cdr env) var))))

;; Default type system implementation
(define default-type-system
  (make-type-system
   (lambda (expr) (symbol? expr))
   (lambda (expr) (and (pair? expr) (eq? (car expr) '->)))
   (lambda (expected actual)
     (cond ((and (symbol? expected) (symbol? actual))
            (eq? expected actual))
           ((and (pair? expected) (pair? actual)
                 (eq? (car expected) '->) (eq? (car actual) '->))
            (and (type-compatible? (cadr expected) (cadr actual))
                 (type-compatible? (caddr expected) (caddr actual))))
           ((and (pair? expected) (eq? (car expected) '->))
            (type-compatible? (caddr expected) actual))
           (else #f)))
   (lambda (expr)
     (let type-of-aux ((expr expr) (env (make-type-env)))
       (cond ((variable? expr) (type-env-lookup env expr))
             ((literal? expr)
              (cond ((number? expr) '|Int|)
                    ((boolean? expr) '|Bool|)
                    (else '|Any|)))
             ((untyped-lambda? expr) '|Any|)
             ((typed-lambda? expr)
              (let* ((param (lambda-param expr))
                     (param-type (lambda-type expr))
                     (body (lambda-body expr))
                     (new-env (type-env-extend env param param-type))
                     (body-type (type-of-aux body new-env)))
                (list '-> param-type body-type)))
             ((application? expr)
              (let ((op-type (type-of-aux (operator expr) env))
                    (arg-type (type-of-aux (operand expr) env)))
                (cond ((not (pair? op-type))
                       (error "Type error: expected function type but got" op-type))
                      ((not (eq? (car op-type) '->))
                       (error "Type error: expected function type but got" op-type))
                      ((and (pair? arg-type) (eq? (car arg-type) '->))
                       (if (type-compatible? (cadr op-type) arg-type)
                           (caddr op-type)
                           (error "Type error in application: expected" 
                                  (cadr op-type) 
                                  "but got" 
                                  arg-type)))
                      ((not (type-compatible? (cadr op-type) arg-type))
                       (error "Type error in application: expected" 
                              (cadr op-type) 
                              "but got" 
                              arg-type))
                      (else (caddr op-type)))))
             (else '|Any|))))))

;; Global type inference function
(define (type-of expr)
  ((type-system-infer default-type-system) expr))

;; Global type compatibility check
(define (type-compatible? expected actual)
  ((type-system-compatible? default-type-system) expected actual))

;; Reduction predicates and functions
(define (alpha-reducible? expr)
  (and (lambda? expr)
       (let ((param (lambda-param expr))
             (body (lambda-body expr)))
         (and (lambda? body)
              (eq? param (lambda-param body))))))

(define (alpha-reduce expr)
  (if (not (alpha-reducible? expr))
      expr
      (let* ((outer-param (lambda-param expr))
             (inner-lambda (lambda-body expr))
             (inner-param (lambda-param inner-lambda))
             (inner-body (lambda-body inner-lambda))
             (inner-type (lambda-type inner-lambda))
             (new-param (generate-fresh-var inner-param)))
        (list 'lambda outer-param (lambda-type expr)
              (list 'lambda new-param inner-type
                    (subst inner-param new-param inner-body))))))

;; Variable handling
(define fresh-counter 0)
(define (generate-fresh-var var)
  (set! fresh-counter (+ fresh-counter 1))
  (string->symbol
   (string-append (symbol->string var)
                  "_"
                  (number->string fresh-counter))))

(define (occurs-in? var expr)
  (cond ((variable? expr) (eq? expr var))
        ((literal? expr) #f)
        ((lambda? expr) (occurs-in? var (lambda-body expr)))
        ((application? expr)
         (or (occurs-in? var (operator expr))
             (occurs-in? var (operand expr))))
        (else #f)))

;; Substitution with alpha-renaming
(define (subst var val expr)
  (cond ((variable? expr)
         (if (eq? expr var) val expr))
        ((literal? expr) expr)
        ((lambda? expr)
         (let ((param (lambda-param expr))
               (typ (lambda-type expr))
               (body (lambda-body expr)))
           (if (eq? param var)
               expr
               (let ((new-param (if (occurs-in? param val)
                                   (generate-fresh-var param)
                                   param)))
                 (let ((renamed-body (subst param new-param body)))
                   (if (typed-lambda? expr)
                       (list 'lambda new-param typ 
                             (subst var val renamed-body))
                       (list 'lambda new-param 
                             (subst var val renamed-body))))))))
        ((application? expr)
         (list 'app
               (subst var val (operator expr))
               (subst var val (operand expr))))
        (else (error "subst: unknown expr type" expr))))

;; Generic procedures
(define g:eval (simple-generic-procedure 'eval 1 
  (lambda (expr) (error "No evaluation strategy defined for expression type:" expr))))
(define g:apply (simple-generic-procedure 'apply 2 
  (lambda (proc args) (error "No application strategy defined for procedure type:" proc))))

;; Generic procedure handlers
(define-generic-procedure-handler g:eval
  (match-args variable?)
  (lambda (expr) expr))

(define-generic-procedure-handler g:eval
  (match-args literal?)
  (lambda (expr) expr))

(define-generic-procedure-handler g:eval
  (match-args lambda?)
  (lambda (expr)
    (let ((reduced (alpha-reduce expr)))
      (if (not (eq? reduced expr))
          (begin
            (trace-step expr reduced "α")
            (g:eval reduced))
          expr))))

(define-generic-procedure-handler g:eval
  (match-args typed-application?)
  (lambda (expr)
    (let* ((raw-op (operator expr))
           (raw-arg (operand expr))
           (proc (g:eval raw-op))
           (arg raw-arg)
           (redex (list 'app proc arg))
           (new-expr (g:apply proc (list arg))))
      (trace-step redex new-expr "β")
      (g:eval new-expr))))

(define-generic-procedure-handler g:apply
  (match-args typed-lambda? list?)
  (lambda (proc args)
    (if (not (= (length args) 1))
        (error "Wrong number of operands, expected 1, got" (length args)))
    (let* ((param (lambda-param proc))
           (typ (lambda-type proc))
           (body (lambda-body proc))
           (arg (car args)))
      (let ((arg-type (type-of arg)))
        (if (not (type-compatible? typ arg-type))
            (error "Type error: expected" typ "but got" arg-type)))
      (subst param arg body))))

;; Pretty printing
(define (trace-step redex result reduction-type)
  (display "; ")
  (display reduction-type)
  (display ": ")
  (pp-expr redex)
  (display " => ")
  (pp-expr result)
  (newline))

(define (pp-expr expr)
  (cond ((variable? expr) (display expr))
        ((literal? expr) (display expr))
        ((lambda? expr)
         (display "λ")
         (display (lambda-param expr))
         (if (typed-lambda? expr)
             (begin
               (display ":")
               (pp-type (lambda-type expr))))
         (display ".")
         (pp-expr (lambda-body expr)))
        ((application? expr)
         (display "(")
         (pp-expr (operator expr))
         (display " ")
         (pp-expr (operand expr))
         (display ")"))
        (else (error "pp: unknown expr type" expr))))

(define (pp-type typ)
  (cond ((symbol? typ) (display typ))
        ((and (pair? typ) (eq? (car typ) '->))
         (display "(")
         (pp-type (cadr typ))
         (display "->")
         (pp-type (caddr typ))
         (display ")"))
        (else (error "pp-type: unknown type" typ))))
