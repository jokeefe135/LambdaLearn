;; AST predicates and accessors
(define (variable? expr) (symbol? expr))
(define (lambda? expr)
  (and (pair? expr) (eq? (car expr) 'lambda)))
(define (application? expr)
  (and (pair? expr) (eq? (car expr) 'app)))
(define (operator expr) (cadr expr))
(define (operand expr) (caddr expr))

;; Generate fresh variable names
(define fresh-counter 0)
(define (generate-fresh-var var)
  (set! fresh-counter (+ fresh-counter 1))
  (string->symbol
   (string-append (symbol->string var)
                  "_"
                  (number->string fresh-counter))))

;; Check if variable occurs in expression
(define (occurs-in? var expr)
  (cond ((variable? expr)    (eq? expr var))
        ((lambda? expr)      (occurs-in? var (caddr expr)))
        ((application? expr)
         (or (occurs-in? var (operator expr))
             (occurs-in? var (caddr expr))))
        (else #f)))

;; Substitution with alpha-renaming
(define (subst var val expr)
  (cond ((variable? expr)
         (if (eq? expr var) val expr))
        ((lambda? expr)
         (let ((param (cadr expr))
               (body  (caddr expr)))
           (if (eq? param var)
               expr
               (let ((new-param (if (occurs-in? param val)
                                    (generate-fresh-var param)
                                    param)))
                 (let ((renamed-body (subst param new-param body)))
                   (list 'lambda new-param (subst var val renamed-body)))))))
        ((application? expr)
         (list 'app
               (subst var val (operator expr))
               (subst var val (operand expr))))
        (else
         (error "subst: unknown expr type" expr))))

;; Default eval/apply
(define (default-eval expr)
  (error "Unknown expression type in eval:" expr))
(define g:eval
  (simple-generic-procedure 'eval 1 default-eval))

(define (default-apply proc args)
  (error "Unknown application type" proc args))
(define g:apply
  (simple-generic-procedure 'apply 2 default-apply))

;; Pretty printing for AST
(define (pp-expr expr)
  (cond ((variable? expr) (display expr))
        ((lambda? expr)
         (display "λ")
         (display (cadr expr))
         (display ".")
         (pp-expr (caddr expr)))
        ((application? expr)
         (display "(")
         (pp-expr (operator expr))
         (display " ")
         (pp-expr (operand expr))
         (display ")"))
        (else (error "pp: unknown expr type" expr))))

;; Trace a beta-reduction step
(define (trace-step redex result)
  (display "; β: ")
  (pp-expr redex)
  (display " => ")
  (pp-expr result)
  (newline))

;; Eval handlers
(define-generic-procedure-handler g:eval
  (match-args variable?)
  (lambda (expr) expr))

(define-generic-procedure-handler g:eval
  (match-args lambda?)
  (lambda (expr) expr))

(define-generic-procedure-handler g:eval
  (match-args application?)
  (lambda (expr)
    (let* ((raw-op   (operator expr))
           (raw-arg  (operand expr))
           (proc     (g:eval raw-op))
           (arg      raw-arg)
           (redex    (list 'app proc arg))
           (new-expr (g:apply proc (list arg))))
      (trace-step redex new-expr)
      (g:eval new-expr))))

;; Apply handler for lambda
(define-generic-procedure-handler g:apply
  (match-args lambda? list?)
  (lambda (proc args)
    (if (not (= (length args) 1))
        (error "Wrong number of operands, expected 1, got" (length args)))
    (let ((var  (cadr proc))
          (body (caddr proc))
          (arg  (car args)))
      (subst var arg body))))
