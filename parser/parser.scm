(load "parser/parse-typed.scm")
(load "parser/parse-untyped.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TOKENIZER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tokenize : String -> (Listof Symbol)
;; Splits on whitespace and these single‑char tokens:  ( ) λ . : -> |
;; Recognises numeric literals, boolean literals, and identifiers
;; Identifiers match  [A‑Za‑z_][A‑Za‑z0‑9_]*  .

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
              ((and (char=? c #\-)           ; first char
                    (< (+ i 1) len)
                    (char=? (string-ref str (+ i 1)) #\>))
               (loop (+ i 2) (cons '-> tokens)))

              ;; 3. one-char tokens: ( ) λ . : |
              ((memv c '(#\( #\) #\λ #\. #\: #\|))
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

              ;; 6. anything else → error
              (else (error "tokenize: unexpected character" c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COMMON SYMBOL CONSTANTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sym-open    (string->symbol (string #\()    ))
(define sym-close   (string->symbol (string #\))    ))
(define sym-lambda  (string->symbol (string #\λ)    ))
(define sym-period  (string->symbol (string #\.)    ))
(define sym-colon   (string->symbol (string #\:)    ))
(define sym-arrow   (string->symbol (string #\- #\>)))
(define sym-bar     (string->symbol (string #\|)    ))

(define (identifier? tok)
  (and (symbol? tok)
       (not (member tok (list sym-open sym-close sym-lambda
                               sym-period sym-colon sym-arrow sym-bar)))))

(define (literal? tok)
  (or (number? tok) (boolean? tok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FRONT‑END DISPATCH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Heuristic: presence of ':' OR '->' tokens ⇒ typed.
(define (typed-tokens? toks)
  (or (member sym-colon toks) (member sym-arrow toks)))

(define (parse-tokens toks)
  (if (typed-tokens? toks)
      (let-values (((expr rest) (parse-expression-typed toks)))
        (if (null? rest)
            expr
            (error "typed parse: extra tokens" rest)))
      (let-values (((expr rest) (parse-expression-untyped toks)))
        (if (null? rest)
            expr
            (error "untyped parse: extra tokens" rest)))))

;; User‑facing entry pt: takes a raw string.
(define (parse str)
  (parse-tokens (tokenize str)))
