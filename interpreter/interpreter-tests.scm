;; Test suite for typed lambda calculus interpreter

(load "parser/parser.scm")
(load "parser/parse-typed.scm")
(load "parser/parse-untyped.scm")
(load "interpreter/interpreter.scm")

;; Helper function to run a test case
(define (run-test name input expected)
  (display "Test: ")
  (display name)
  (newline)
  (display "Input: ")
  (display input)
  (newline)
  (let ((expr (parse input)))
    (display "Parsed: ")
    (pp-expr expr)
    (newline)
    (if (eq? expected 'type-error)
        (begin
          (display "Expected: Type error")
          (newline)
          (display "Actual: ")
          (let ((result (with-exception-handler
                        (lambda (exn)
                          (display "Type error: ")
                          (display (condition/report-string exn))
                          (newline)
                          (display "PASSED")
                          (newline)
                          #t)
                        (lambda ()
                          (g:eval expr)
                          (display "No type error occurred (test failed)")
                          (newline)
                          #f))))
            (if (not result)
                (display "FAILED")
                (newline))))
        (begin
          (display "Expected: ")
          (display expected)
          (newline)
          (display "Actual: ")
          (let ((result (g:eval expr)))
            (write result)
            (newline)
            (if (equal? result expected)
                (display "PASSED")
                (display "FAILED"))
            (newline))))
    (newline)))

;; Test cases
(define (test-typed-interpreter)
  (display "Running typed lambda calculus interpreter tests...")
  (newline)
  (newline)

  ;; Basic typed identity functions
  (run-test "Int identity" "(λx:|Int|.x) 5" 5)
  (run-test "Bool identity" "(λx:|Bool|.x) true" true)
  (run-test "Function identity" "(λf:|Int|->|Int|.f) (λx:|Int|.x)" '(lambda x |Int| x))

  ;; Function composition
  (run-test "Function composition"
            "(λf:|Int|->|Int|.λg:|Int|->|Int|.λx:|Int|.g (f x)) (λx:|Int|.x) (λx:|Int|.x) 5"
            5)

  ;; Higher-order functions
  (run-test "Apply function"
            "(λf:|Int|->|Int|.λx:|Int|.f x) (λx:|Int|.x) 5"
            5)

  ;; Alpha reduction tests
  (run-test "Alpha reduction: nested identity"
            "(λx:|Int|.λx:|Int|.x) 5"
            '(lambda x |Int| x))
  (run-test "Alpha reduction: shadowed variable"
            "(λx:|Int|.λy:|Int|.λx:|Int|.x) 3 4"
            '(lambda x |Int| x))

  ;; Type errors (should fail)
  ;;(run-test "Type mismatch: Int vs Bool"
           ;; "(λx:|Int|.x) true"
         ;;   'type-error)
  ;;(run-test "Type mismatch: wrong function type"
      ;;      "(λf:|Int|->|Int|.f) (λx:|Bool|.x)"
      ;;      'type-error)
  ;;(run-test "Type mismatch: wrong argument count"
        ;;    "(λx:|Int|.λy:|Int|.x) 5"
           ;; 'type-error)

  ;; Nested functions
  (run-test "Nested function application"
            "((λf:|Int|->|Int|.λg:|Int|->|Int|.λx:|Int|.g (f x)) (λx:|Int|.x) (λx:|Int|.x)) 5"
            5)

  ;; Multiple arguments
  (run-test "Multiple arguments"
            "(λx:|Int|.λy:|Int|.x) 3 4"
            3)

  ;; Type inference
  (run-test "Type inference in application"
            "(λf:|Int|->|Int|.f 5) (λx:|Int|.x)"
            5)

  ;; Currying
  (run-test "Curried function"
            "((λx:|Int|.λy:|Int|.x) 3) 4"
            3)

  ;; Complex nested types
  (run-test "Complex nested types"
            "(λf:(|Int|->|Int|)->(|Int|->|Int|).λg:|Int|->|Int|.λx:|Int|.f g x) (λh:|Int|->|Int|.h) (λx:|Int|.x) 5"
            5)

  ;; Type errors with complex types
  ;;(run-test "Type error: complex type mismatch"
            ;;"(λf:(|Int|->|Int|)->(|Int|->|Int|).λg:|Int|->|Int|.λx:|Int|.f g x) (λh:|Bool|->|Bool|.h) (λx:|Int|.x) 5"
            ;;'type-error)

  (display "Test suite completed.")
  (newline))

;; Run the tests
(test-typed-interpreter)