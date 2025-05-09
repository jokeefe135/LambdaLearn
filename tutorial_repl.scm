;;; --------------------------------------------------------------------------
;;; lambda_tutorial_repl.scm - Interactive λ-calculus tutor
;;; --------------------------------------------------------------------------
;;; A drop-in replacement for your existing repl.scm that adds an interactive
;;; teaching mode.  Type `learn!` at the prompt to enter a guided tutorial that
;;; walks through the λ-calculus, Church encodings, and the Y-combinator.
;;;
;;;   (load "lambda_tutorial_repl.scm")
;;;   (repl)            ; normal REPL with tutorial support
;;;
;;; The file assumes the following globals are already loaded in the image:
;;;   * parse   – string → AST      (from parser.scm)
;;;   * g:eval  – AST → AST         (from interpreter.scm)
;;;   * pp-expr – pretty-printer    (from interpreter.scm)
;;;
;;; New in this version
;;; -------------------
;;;  • Added Lesson 3 demonstrating α-conversion using the term
;;;      ((λx.(λx.x)) f y)
;;;  • Replaced `help!` with `skip!`, which automatically enters the canonical
;;;    answer and advances the tutorial.
;;;  • Beefed-up normalisation: *all* whitespace is ignored when checking the
;;;    learner’s input, so extra spaces, tabs, or newlines no longer matter.
;;;
;;; --------------------------------------------------------------------------

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Helper utilities                                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-whitespace? c)
  (memv c '(#\space #\tab #\newline #\return)))

;; Trim leading/trailing whitespace (kept for REPL convenience)
(define (string-trim str)
  (let* ((len (string-length str)))
    (let loop-left ((i 0))
      (if (or (>= i len) (not (char-whitespace? (string-ref str i))))
          (let loop-right ((j (- len 1)))
            (if (or (< j i) (not (char-whitespace? (string-ref str j))))
                (substring str i (+ j 1))
                (loop-right (- j 1))))
          (loop-left (+ i 1))))))

;; Normalise a string for *equality testing only* by stripping *all* whitespace.
(define (normalize str)
  (list->string (filter (lambda (c) (not (char-whitespace? c)))
                        (string->list str))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tutorial data                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Each lesson is a record (make-lesson prompt check after)
(define-record-type lesson
  (make-lesson prompt check after)
  lesson?
  (prompt lesson-prompt)
  (check  lesson-check)
  (after  lesson-after))

;; Canonical answers – used internally by `skip!`
(define lesson-answers
  (vector
   ;; 0 ─ Identity (untyped)
   "λx.x"
   ;; 1 ─ β-reduction demo
   "(λx.x) FOO"
   ;; 2 ─ α-reduction demo
   "((λx.(λx.x)) f y)"
   ;; 3 ─ Typed demo
   "(λx:Int.x) 5"
   ;; 4–18 ─ rest of the journey
   "TRUE = λa.λb.a"
   "FALSE = λa.λb.b"
   "IF = λp.λa.λb.p a b"
   "IF TRUE FOO BAR"
   "ZERO = λf.λx.x"
   "SUCC = λn.λf.λx.f (n f x)"
   "ONE = SUCC ZERO"
   "TWO = SUCC ONE"
   "THREE = SUCC TWO"
   "MUL = λm.λn.λf.λx.m (n f) x"
   "Y = λf.(λx.f (x x)) (λx.f (x x))"
   "PRED = λn.λf.λx.n (λg.λh.h (g f)) (λu.x) (λu.u)"
   "ISZERO = λn.n (λx.FALSE) TRUE"
   "FACT = Y (λfact.λn.IF (ISZERO n) ONE (MUL n (fact (PRED n))))"
   "ISZERO (PRED PRED PRED PRED PRED PRED (FACT THREE))"))

;; Convenience
(define (hint) (display "   (type skip! to skip this exercise)\n"))

;; Vector of lessons ---------------------------------------------------------
(define lessons
  (vector
   ;; 0 ─ Identity -----------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 1: Identity\nRecall that in λ-calculus, λ<var>.<body> defines a function that
returns <body> when applied to <var>.  Write the identity
function using x as the variable.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 0)))
    (lambda (_e _r) (display "Good! You have the identity.\n\n")))

   ;; 1 ─ β-reduction demo ---------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 2: β-reduction\nApply your identity to the atomic \"FOO\" so we can
watch it reduce.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 1)))
    (lambda (_e _r) (display "Nice! One β-reduction and we're left with the argument.\n\n")))

   ;; 2 ─ α-reduction demo ---------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 3: α-reduction\nTwo lambda terms that differ only by variable names are
alpha equivalent.  Enter ((λx.(λx.x)) f y) to observe how the
interpreter safely renames the inner x before reducing, i.e., does an α-reduction.")
      (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 2)))
    (lambda (_e _r) (display "Great! You just witnessed α-reduction in action.\n\n")))

   ;; 3 ─ Typed demo ---------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 4: Static types\nProvide a statically-typed identity that only accepts integers,
then apply it to 5 using \"x\" as the variable.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 3)))
    (lambda (_e _r)
      (display "Type-checking passed and the term reduces to 5.  Nice!\n\n")))

   ;; 4 ─ TRUE ---------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 5: Church booleans - TRUE\nEncode TRUE as a function that takes two arguments a and b and
returns a.  Bind it to TRUE.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 4)))
    (lambda (_e _r) (display "Nice!\n\n")))

   ;; 5 ─ FALSE --------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 6: Church booleans - FALSE\nNow encode FALSE as a function that returns its second
argument.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 5)))
    (lambda (_e _r) (display "Good!\n\n")))

   ;; 6 ─ IF -----------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 7: IF\nDefine IF so that IF p a b selects between a and b based on the boolean p.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 6)))
    (lambda (_e _r) (display "Great!\n\n")))

   ;; 7 ─ Test IF ------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 8: Test IF with TRUE, FOO, and BAR.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 7)))
    (lambda (_e _r) (display "Branching works!\n\n")))

   ;; 8 ─ ZERO ---------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 9: Church numerals - ZERO\nA Church numeral applies a function f exactly n times.
Define ZERO.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 8)))
    (lambda (_e _r) (display "Good!\n\n")))

   ;; 9 ─ SUCC ---------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 10: Successor (SUCC)\nWrite SUCC, which turns n into n+1.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 9)))
    (lambda (_e _r) (display "Nice!\n\n")))

   ;; 10 ─ ONE ---------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 11: Build ONE using SUCC.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 10)))
    (lambda (_e _r) (display "Good work!\n\n")))

   ;; 11 ─ TWO ---------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 12: Define TWO.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 11)))
    (lambda (_e _r) (display "\n")))

   ;; 12 ─ THREE -------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 13: Define THREE.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 12)))
    (lambda (_e _r) (display "\n")))

   ;; 13 ─ MUL ---------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 14: Multiplication (MUL).") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 13)))
    (lambda (_e _r) (display "\n")))

   ;; 14 ─ Y combinator ------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 15: Y combinator\nCombinators are closed lambda terms, i.e., functions without any\nfree variables. Fixed-point combinators are special combinators\nF such that for any function f, F(f) gives a value x\nsatisfying f(x) = x, which provides a way to define\nrecursion. The most famous of these is the\nY combinator. Define the Y combinator.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 14)))
    (lambda (_e _r) (display "\n")))

   ;; 15 ─ PRED --------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 16: Predecessor (PRED).\nImplement the opposite of SUCC.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 15)))
    (lambda (_e _r) (display "Nice!\n\n")))

   ;; 16 ─ ISZERO ------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 17: Zero test (ISZERO).") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 16)))
    (lambda (_e _r) (display "\n")))

   ;; 17 ─ FACT --------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 18: Factorial (FACT) using Y.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 17)))
    (lambda (_e _r) (display "\n")))

   ;; 18 ─ Finale ------------------------------------------------------------
   (make-lesson
    (lambda ()
      (display "Lesson 19 (last one): Verify that six PREDs of FACT THREE
reduce to zero.") (newline) (hint))
    (lambda (l) (string=? (normalize l) (vector-ref lesson-answers 18)))
    (lambda (_e _r)
      (display "Success! Factorial of 3 is 6 and six predecessors hit zero.\n")
      (display "Yippee, you're all done. Go get crazy with some lambda calculus!\n")))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Tutorial engine                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tutorial-active? #f)
(define tutorial-idx      0)

(define (start-tutorial)
  (set! tutorial-active? #t)
  (set! tutorial-idx 0)
  ((lesson-prompt (vector-ref lessons 0))))

(define (advance-to-next-lesson expr result)
  ((lesson-after (vector-ref lessons tutorial-idx)) expr result)
  (set! tutorial-idx (+ tutorial-idx 1))
  (if (< tutorial-idx (vector-length lessons))
      ((lesson-prompt (vector-ref lessons tutorial-idx)))
      (begin
        (set! tutorial-active? #f)
        (display "\nExiting tutorial…\n"))))

(define (handle-tutorial-line line)
  (let ((norm (normalize line)))
    (cond
     ;; Skip the current lesson ------------------------------------------------
     ((string=? norm "skip!")
      (let* ((answer (vector-ref lesson-answers tutorial-idx))
             (expr   (parse answer)))
        (display "answer: ") (display answer) (newline)
        (let ((result (g:eval expr)))
          (display "; Result: ") (pp-expr result) (newline)
          (advance-to-next-lesson expr result))))

     ;; Normal submission ------------------------------------------------------
     (else
      (let ((lesson (vector-ref lessons tutorial-idx)))
        (if ((lesson-check lesson) line)
            (let* ((expr   (parse line))
                   (result (g:eval expr)))
              (display "; Result: ") (pp-expr result) (newline)
              (advance-to-next-lesson expr result))
            (display "That's not what I asked for. Please try again.\n")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Enhanced REPL                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repl)
  ;; Reset tutorial state on each entry
  (set! tutorial-active? #f)
  (set! tutorial-idx      0)

  (display "LambdaLearn REPL - type 'learn!' to start the guided tutorial")
  (newline)
  (let main-loop ()
    (display (if tutorial-active? "» " "> "))
    (flush-output)
    (let ((line (read-line)))
      (cond
       ((eof-object? line)
        (display "Goodbye!") (newline))
       ((string=? line "") (main-loop))
       ((and (not tutorial-active?)
             (string=? (string-trim line) "learn!"))
        (start-tutorial) (main-loop))
       (tutorial-active? (handle-tutorial-line line) (main-loop))
       (else
        (let* ((expr   (parse line))
               (result (g:eval expr)))
          (display "; Input: ") (pp-expr expr) (newline)
          (write result) (newline)
          (main-loop)))))))
