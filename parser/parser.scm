;; tokenize : String -> (Listof Symbol)
;; Splits on whitespace and these tokens:
;;   single-char  : ( ) λ . : =
;;   two-char     : ->
;; Identifiers match [A-Za-z_][A-Za-z0-9_]*.
(define (tokenize str)
  (let* ((len (string-length str)))
    ;; character predicates
    (define (is-letter? c)
      (or (and (char>=? c #\A) (char<=? c #\Z))
          (and (char>=? c #\a) (char<=? c #\z))))
    (define (is-digit? c)
      (and (char>=? c #\0) (char<=? c #\9)))
    (define (is-ident-start? c) (or (is-letter? c) (char=? c #\_)))
    (define (is-ident-part?  c) (or (is-ident-start? c) (is-digit? c)))

    (let loop ((i 0) (tokens '()))
      (if (>= i len)
          (reverse tokens)
          (let ((c (string-ref str i)))
            (cond
              ;; 1. skip whitespace
              ((memv c '(#\space #\tab #\newline #\return))
               (loop (+ i 1) tokens))

              ;; 2. two-char token "->"
              ((and (char=? c #\-) (< (+ i 1) len)
                    (char=? (string-ref str (+ i 1)) #\>))
               (loop (+ i 2) (cons '-> tokens)))

              ;; 3. one-char tokens: ( ) λ . : = |
              ((memv c '(#\( #\) #\λ #\. #\: #\= #\|))
               (loop (+ i 1)
                     (cons (string->symbol (string c)) tokens)))

              ;; 4. numeric literal
              ((is-digit? c)
               (let scan ((j i))
                 (if (and (< j len) (is-digit? (string-ref str j)))
                     (scan (+ j 1))
                     (let ((num (string->number (substring str i j))))
                       (loop j (cons num tokens))))))
              ;; 5. boolean literal
              ((is-ident-start? c)
               (let scan ((j i))
                 (if (and (< j len) (is-ident-part? (string-ref str j)))
                     (scan (+ j 1))
                     (let ((name (substring str i j)))
                       (cond
                         ((string=? name "true") (loop j (cons #t tokens)))
                         ((string=? name "false") (loop j (cons #f tokens)))
                         (else (loop j (cons (string->symbol name) tokens))))))))

              ;; 6. anything else -> error
              (else (error "tokenize: unexpected character" c))))))))

;; Constants to avoid some annoying ahh bugs
(define sym-open    (string->symbol (string #\()    ))
(define sym-close   (string->symbol (string #\))    ))
(define sym-lambda  (string->symbol (string #\λ)    ))
(define sym-period  (string->symbol (string #\.)    ))
(define sym-colon   (string->symbol (string #\:)    ))
(define sym-arrow   (string->symbol (string #\- #\>)))
(define sym-equal   (string->symbol (string #\=)))
(define sym-bar     (string->symbol (string #\|)    ))

(define (identifier? tok)
  (and (symbol? tok)
       (not (member tok (list sym-open sym-close sym-lambda
                               sym-period sym-colon sym-arrow sym-equal sym-bar)))))

;; Presence of : or -> tokens means typed.
(define (typed-tokens? toks)
  (or (member sym-colon toks) (member sym-arrow toks)))

;; Detect assignment syntax <identifier> = <expression>
(define (parser-assignment? toks)
  (and (pair? toks)
       (identifier? (car toks))
       (pair? (cdr toks))
       (eq? (cadr toks) sym-equal)))

;; Parse assignment, returns (assign var expr).
(define (parse-assignment tokens)
  (let ((var (car tokens))
        (rest1 (cddr tokens)))
    (if (typed-tokens? rest1)
        (let-values (((expr rest2) (parse-expression-typed rest1)))
          (values (list 'assign var expr) rest2))
        (let-values (((expr rest2) (parse-expression-untyped rest1)))
          (values (list 'assign var expr) rest2)))))

(define (parse-tokens toks)
  (cond
    ;; assignment: X = expr
    ((parser-assignment? toks)
     (let-values (((expr rest) (parse-assignment toks)))
       (if (null? rest)
           expr
           (error "assignment parse: extra tokens" rest))))
    ;; typed lambda expression
    ((typed-tokens? toks)
     (let-values (((expr rest) (parse-expression-typed toks)))
       (if (null? rest)
           expr
           (error "typed parse: extra tokens" rest))))
    ;; untyped lambda expression
    (else
     (let-values (((expr rest) (parse-expression-untyped toks)))
       (if (null? rest)
           expr
           (error "untyped parse: extra tokens" rest))))))

(define (parse str)
  (parse-tokens (tokenize str)))
