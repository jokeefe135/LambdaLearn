;;; -
;;; A drop‑in replacement for your existing repl.scm that adds an interactive
;;; teaching mode.  Type `learn!` at the prompt to enter a guided tutorial that
;;; walks through the λ‑calculus, Church encodings, and the Y‑combinator.
;;;
;;;   (load "lambda_tutorial_repl.scm")
;;;   (repl)            ; normal REPL with tutorial support
;;;
;;; The file assumes the following globals are already loaded in the image:
;;;   * parse   – string -> AST      (from parser.scm)
;;;   * g:eval  – AST -> AST         (from interpreter.scm)
;;;   * pp-expr – pretty‑printer    (from interpreter.scm)
;;;
;;; -

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Helper utilities                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-whitespace? c)
  (memv c '(#\space #\tab #\newline #\return)))

(define (string-trim str)
  (let* ((len (string-length str)))
    (let loop-left ((i 0))
      (if (or (>= i len) (not (char-whitespace? (string-ref str i))))
          (let loop-right ((j (- len 1)))
            (if (or (< j i) (not (char-whitespace? (string-ref str j))))
                (substring str i (+ j 1))
                (loop-right (- j 1))))
          (loop-left (+ i 1))))))

(define (normalize str)
  ;; remove leading / trailing spaces and collapse runs of internal space
  (let loop ((chars (string->list (string-trim str))) (acc '()) (last-space? #f))
    (cond ((null? chars)
           (list->string (reverse acc)))
          ((char-whitespace? (car chars))
           (loop (cdr chars) (if last-space? acc (cons #\space acc)) #t))
          (else
           (loop (cdr chars) (cons (car chars) acc) #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tutorial data                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each lesson is a record (make-lesson prompt check after)
(define-record-type lesson
  (make-lesson prompt check after)
  lesson?
  (prompt  lesson-prompt)   ; => thunk, so we can re-display fresh each time
  (check   lesson-check)    ; string -> boolean
  (after   lesson-after))   ; (expr result) -> void

(define lessons
  (vector
   ;; ──────────────────────────────────────────────────────────────────────
   ;; 0. Identity and β-reduction
   (make-lesson
     (lambda ()
       (display "Lesson 1: Identity\n")
       (display "The identity function returns its argument unchanged.\n")
       (display "Type exactly:  λx.x\n"))
     (lambda (line) (string=? (normalize line) "λx.x"))
     (lambda (expr result)
       (display "Excellent: you constructed λx.x.\n")
       (display "A β-reduction means applying (λx.M) N -> M[x:=N].\n")
       (display "\nLesson 2: Notation for application\n")
       (display "Watch it reduce: Type:  (λx.x) FOO\n")))

   ;; 1. β-reduction demo
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "(λx.x) FOO"))
     (lambda (expr result)
       (display "Great! (λx.x) FOO -> FOO by one β-reduction.\n")
       (display "\nLesson 3: Church encodings\n")
       (display "Church encodings represent data like booleans & numerals as pure functions.\n")
       (display "Define TRUE.  Type:\n")
       (display "  TRUE = λa.λb.a\n")))

   ;; 2. TRUE
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "TRUE = λa.λb.a"))
     (lambda (expr result)
       (display "TRUE selects its first argument.\n")
       (display "Now define FALSE.  Type:\n")
       (display "  FALSE = λa.λb.b\n")))

   ;; 3. FALSE
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "FALSE = λa.λb.b"))
     (lambda (expr result)
       (display "Excellent – you have Church booleans!\n")
       (display "Next: Build an IF combinator.  Type:\n")
       (display "  IF = λp.λa.λb.p a b\n")))

   ;; 4. IF combinator
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "IF = λp.λa.λb.p a b"))
     (lambda (expr result)
       (display "Great – IF applies the predicate to two branches.\n")
       (display "\nLesson 4: Church numerals – start with ZERO\n")
       (display "Type:\n")
       (display "  ZERO = λf.λx.x\n")))

   ;; 5. ZERO
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "ZERO = λf.λx.x"))
     (lambda (expr result)
       (display "Good.  Now define SUCC (successor).  Type:\n")
       (display "  SUCC = λn.λf.λx.f (n f x)\n")))

   ;; 6. SUCC
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "SUCC = λn.λf.λx.f (n f x)"))
     (lambda (expr result)
       (display "Nice.  Build ONE by applying SUCC to ZERO.  Type:\n")
       (display "  ONE = SUCC ZERO\n")))

   ;; 7. ONE
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "ONE = SUCC ZERO"))
     (lambda (expr result)
       (display "You now have ONE!\n")
       (display "\nLesson 5: Simple type annotations\n")
       (display "In typed λ-calculus, we enforce types.  Define:\n")
       (display "  ID_BOOL = λx:Bool.x\n")))

   ;; 8. Typed identity
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "ID_BOOL = λx:Bool.x"))
     (lambda (expr result)
       (display "Typed identity accepted – types are enforced!\n")
       (display "Back to numerals: build TWO, THREE, FOUR:\n")
       (display "  TWO = SUCC ONE\n")))

   ;; 9. TWO
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "TWO = SUCC ONE"))
     (lambda (expr result)
       (display "Great.  Now THREE.  Type:\n")
       (display "  THREE = SUCC TWO\n")))

   ;; 10. THREE
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "THREE = SUCC TWO"))
     (lambda (expr result)
       (display "And now FOUR.  Type:\n")
       (display "  FOUR = SUCC THREE\n")))

   ;; 11. FOUR
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "FOUR = SUCC THREE"))
     (lambda (expr result)
       (display "Excellent – you’ve built FOUR.\n")
       (display "Next, define multiplication.  Type:\n")
       (display "  MUL = λm.λn.λf.λx.m (n f) x\n")))

   ;; 12. MUL
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "MUL = λm.λn.λf.λx.m (n f) x"))
     (lambda (expr result)
       (display "Multiplication ready.\n")
       (display "Define predecessor.  Type:\n")
       (display "  PRED = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)\n")))

   ;; 13. PRED
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "PRED = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)"))
     (lambda (expr result)
       (display "Predecessor defined.\n")
       (display "Define ISZERO.  Type:\n")
       (display "  ISZERO = λn.n (λx.FALSE) TRUE\n")))

   ;; 14. ISZERO
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "ISZERO = λn.n (λx.FALSE) TRUE"))
     (lambda (expr result)
       (display "All core pieces in place!\n")
       (display "\nLesson 6: The Y-combinator (fixed-point)\n")
       (display "Define Y.  Type:\n")
       (display "  Y = λf.(λx.f (x x)) (λx.f (x x))\n")))

   ;; 15. Y combinator
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "Y = λf.(λx.f (x x)) (λx.f (x x))"))
     (lambda (expr result)
       (display "Y is the fixed-point combinator enabling recursion via self-application.\n")
       (display "In an eager evaluator, naive use of Y causes endless expansions (infinite reductions).\n")
       (display "\nLesson 7: Use the Z-combinator instead\n")
       (display "Type:\n")
       (display "  Z = λf.(λx.f (λv.(x x) v)) (λx.f (λv.(x x) v))\n")))

   ;; 16. Z combinator
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "Z = λf.(λx.f (λv.(x x) v)) (λx.f (λv.(x x) v))"))
     (lambda (expr result)
       (display "Z defined – delayed self-application avoids infinite unfolding.\n")
       (display "Now define factorial with Z.  Type:\n")
       (display "  FACT = Z (λfact.λn.IF (ISZERO n) ONE (MUL n (fact (PRED n))))\n")))

   ;; 17. FACT via Z
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "FACT = Z (λfact.λn.IF (ISZERO n) ONE (MUL n (fact (PRED n))))"))
     (lambda (expr result)
       (display "Great – FACT is now recursive without infinite expansion.\n")
       (display "Finally, compute 4!.  Type:\n")
       (display "  FACT FOUR\n")))

   ;; 18. Compute 4!
   (make-lesson
     (lambda () #f)
     (lambda (line) (string=? (normalize line) "FACT FOUR"))
     (lambda (expr result)
       (display "Well done! That λ-expression encodes 24 in Church style.\n")
       (display "Verify by applying SUCC and ZERO:\n")
       (display "  (FACT FOUR) SUCC ZERO\n")))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tutorial engine                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tutorial-active? #f)
(define tutorial-idx       0)

(define (start-tutorial)
  (set! tutorial-active? #t)
  (set! tutorial-idx 0)
  ((lesson-prompt (vector-ref lessons 0))))

(define (handle-tutorial-line line)
  (let ((lesson (vector-ref lessons tutorial-idx)))
    (if ((lesson-check lesson) line)
        (begin
          (let* ((expr   (parse line))
                 (result (g:eval expr)))
            (display "; Result: ")
            (pp-expr result)
            (newline)
            ((lesson-after lesson) expr result))
          (set! tutorial-idx (+ tutorial-idx 1))
          (when (< tutorial-idx (vector-length lessons))
            (let ((next (lesson-prompt (vector-ref lessons tutorial-idx))))
              (when next (next))))
          (when (>= tutorial-idx (vector-length lessons))
            (set! tutorial-active? #f)
            (display "\nExiting tutorial…\n")))
        ;; else
        (display "That's not what I asked for. Please try again.\n"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Enhanced REPL                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repl)
  ;; reset tutorial state on each entry
  (set! tutorial-active? #f)
  (set! tutorial-idx       0)

  (display "LambdaLearn REPL - type \'learn!\' for the guided tutorial")
  (newline)
  (let main-loop ()
    (display (if tutorial-active? "» " "> "))
    (flush-output)
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        (display "Goodbye!") (newline))
       ((string=? line "")
        (main-loop))
       ((and (not tutorial-active?) (string=? (string-trim line) "learn!"))
        (start-tutorial) (main-loop))
       (tutorial-active?
        (handle-tutorial-line line) (main-loop))
       (else
        (let* ((expr   (parse line))
               (result (g:eval expr)))
          (display "; Input: ") (pp-expr expr) (newline)
          (write result) (newline)
          (main-loop)))))))
