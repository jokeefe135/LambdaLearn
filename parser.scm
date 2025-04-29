;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRAMMAR
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; expression   ::= abstraction
;;                | application
;; 
;; abstraction  ::= "λ" variable "." expression
;; 
;; application  ::= atomic { atomic }      ; one or more atomics
;; 
;; atomic       ::= variable
;;                | "(" expression ")"
;; 
;; variable     ::= [a-zA-Z_][a-zA-Z0-9_]*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TOKENIZER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tokenize : String -> (Listof Symbol)
;;   splits on whitespace and these single-char tokens:
;;     ( )  λ  .
;;   everything else matching [A-Za-z_][A-Za-z0-9_]* becomes an identifier symbol.
(define (tokenize str)
  (let ((len (string-length str)))
    ;; character tests:
    (define (is-letter? c)
      (or (and (char>=? c #\A) (char<=? c #\Z))
          (and (char>=? c #\a) (char<=? c #\z))))
    (define (is-digit? c)
      (and (char>=? c #\0) (char<=? c #\9)))
    (define (is-ident-start? c)
      (or (is-letter? c) (char=? c #\_)))
    (define (is-ident-part? c)
      (or (is-letter? c) (is-digit? c) (char=? c #\_)))

    (let loop ((i 0) (tokens '()))
      (if (>= i len)
          (reverse tokens)
          (let ((c (string-ref str i)))
            (cond
              ;; skip whitespace
              ((or (char=? c #\space)
                   (char=? c #\tab)
                   (char=? c #\newline)
                   (char=? c #\return))
               (loop (+ i 1) tokens))

              ;; single-char: ( ) λ .
              ((or (char=? c #\()
                   (char=? c #\))
                   (char=? c #\λ)
                   (char=? c #\.))
               (loop (+ i 1)
                     (cons (string->symbol (string c)) tokens)))

              ;; variable identifier
              ((is-ident-start? c)
               (let loop2 ((j i))
                 (if (and (< j len) (is-ident-part? (string-ref str j)))
                     (loop2 (+ j 1))
                     (let ((name (substring str i j)))
                       (loop j (cons (string->symbol name) tokens))))))
              
              ;; anything else is an error
              (else
               (error "tokenize: unexpected character" c))))))))

;; Tests
;; (tokenize "λx.x")
;; (tokenize "  λ   x  .  (  x   x )   ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARSER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; AST representation:
;;   - variable   : just a Scheme symbol, e.g. 'x
;;   - abstraction: (lambda var body)
;;   - application: (app    func arg)

;; Constants to help with clarity and bugs
(define sym_open (string->symbol (string #\( )))
(define sym_close (string->symbol (string #\) )))
(define sym_lambda (string->symbol (string #\λ )))
(define sym_period (string->symbol (string #\. )))

;; Entry point: parse a full expression and ensure no leftover tokens.
(define (parse tokens)
  (let-values (((expr rest) (parse-expression tokens)))
    (if (null? rest)
        expr
        (error "parse: extra tokens after end of input:" rest))))

;; expression ::= abstraction | application
(define (abstraction? tokens)
  (eq? (car tokens) sym_lambda))

(define (parse-expression tokens)
  (if (abstraction? tokens)
      (parse-abstraction tokens)
      (parse-application  tokens)))

;; abstraction ::= "λ" variable "." expression
(define (parse-abstraction tokens)
  (unless (symbol? (cadr tokens))
    (error "parse-abstraction: expected variable, got" (cadr tokens)))
  (unless (eq? (caddr tokens) sym_period)
    (error "parse-abstraction: expected '.', got" (caddr tokens)))
  (let-values (((body rest)
                (parse-expression (cdddr tokens))))
    (values (list 'lambda (cadr tokens) body) rest)))

;; application ::= atomic { atomic }
(define (parse-application tokens)
  (let-values (((head rest) (parse-atomic tokens)))
    (parse-application-loop head rest)))

;; true iff token is a user variable, not punctuation
(define (identifier? tok)
  (and (symbol? tok)
       (not (member tok (list sym_open sym_close sym_lambda sym_period)))))

;; helper to absorb as many atomics as possible
(define (parse-application-loop head tokens)
  (if (and (pair? tokens)
           (or (identifier? (car tokens))
               (eq?    (car tokens) sym_lambda)  ; <-- re-allow λ here
               (eq?    (car tokens) sym_open)))
      (let-values (((next-expr rest2) (parse-atomic tokens)))
        (parse-application-loop
          (list 'app head next-expr)
          rest2))
      (values head tokens)))

;; atomic ::= variable | "(" expression ")"
(define (parse-atomic tokens)
  (cond
    ;; parenthesised sub-expression
    ((eq? (car tokens) sym_open)
     (let-values (((expr rest) (parse-expression (cdr tokens))))
       (unless (and (pair? rest) (eq? (car rest) sym_close))
         (error "parse-atomic: missing ')'"))
       (values expr (cdr rest))))

    ;; variable
    ((identifier? (car tokens))
     (values (car tokens) (cdr tokens)))

    (else
     (error "parse-atomic: unexpected token" (car tokens)))))

;; Tests
;; (define toks (tokenize "λx.x y"))
;; (define ast (parse toks))
;; (pp ast)
;;
;; (define toks (tokenize   "  λcompose.  λf.λg. λx. compose f (g x) (λid.λy.y_x1) arg_1  "))
;; (define ast (parse toks))
;; (pp ast)
;;
;; (define toks (tokenize "(λx. x x) (λy.y)"))
;; (define ast (parse toks))
;; (pp ast)
