;; Typed parser

;; Grammar:
;;   type        ::= type‑simple [ '->' type ]
;;   type‑simple ::= identifier | '(' type ')' | '|' identifier '|'

(define (parse-type tokens)
  (let-values (((lhs rest1) (parse-type-simple tokens)))
    (if (and (pair? rest1) (eq? (car rest1) sym-arrow))
        (let-values (((rhs rest2) (parse-type (cdr rest1))))
          (values (list '-> lhs rhs) rest2))
        (values lhs rest1))))

(define (parse-type-simple tokens)
  (cond
    ((eq? (car tokens) sym-bar)
     (unless (and (pair? (cdr tokens)) (identifier? (cadr tokens)))
       (error "parse-type: expected identifier after '|'"))
     (unless (and (pair? (cddr tokens)) (eq? (caddr tokens) sym-bar))
       (error "parse-type: expected '|' after identifier"))
     (values (cadr tokens) (cdddr tokens)))
    ((identifier? (car tokens))
     (values (car tokens) (cdr tokens)))
    ((eq? (car tokens) sym-open)
     (let-values (((t rest) (parse-type (cdr tokens))))
       (unless (and (pair? rest) (eq? (car rest) sym-close))
         (error "parse-type: missing ')'") )
       (values t (cdr rest))))
    (else
     (error "parse-type: unexpected token" (car tokens)))))

;; Now we define the functions that parse the tokens

(define (parse-expression-typed tokens)
  (if (and (pair? tokens) (eq? (car tokens) sym-lambda))
      (parse-abstraction-typed tokens)
      (parse-application-typed tokens)))

;; abstraction ::= 'λ' variable ':' type '.' expression
(define (parse-abstraction-typed tokens)
  (unless (identifier? (cadr tokens))
    (error "typed λ: expected variable"))
  (unless (eq? (caddr tokens) sym-colon)
    (error "typed λ: expected ':'"))
  (let-values (((typ rest1) (parse-type (cdddr tokens))))
    (unless (and (pair? rest1) (eq? (car rest1) sym-period))
      (error "typed λ: expected '.'"))
    (let-values (((body rest2) (parse-expression-typed (cdr rest1))))
      (values (list 'lambda (cadr tokens) typ body) rest2))))

;; application ::= atomic { atomic }
(define (parse-application-typed tokens)
  (let-values (((head rest) (parse-atomic-typed tokens)))
    (let loop ((acc head) (ts rest))
      (if (and (pair? ts)
               (or (identifier? (car ts))
                   (eq? (car ts) sym-open)
                   (eq? (car ts) sym-lambda)
                   (literal? (car ts))))
          (let-values (((arg rest2) (parse-atomic-typed ts)))
            (loop (list 'app acc arg) rest2))
          (values acc ts)))))

(define (parse-atomic-typed tokens)
  (cond
    ((identifier? (car tokens))
     (values (car tokens) (cdr tokens)))
    ((literal? (car tokens))
     (values (car tokens) (cdr tokens)))
    ((eq? (car tokens) sym-open)
     (let-values (((expr rest) (parse-expression-typed (cdr tokens))))
       (unless (and (pair? rest) (eq? (car rest) sym-close))
         (error "typed atom: missing ')'"))
       (values expr (cdr rest))))
    (else
     (error "typed atom: unexpected token" (car tokens)))))
