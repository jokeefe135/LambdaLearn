;; Alpha Reduction Examples
;; This file demonstrates alpha reductions in lambda calculus

;; Load the interpreter
(load "interpreter/interpreter.scm")

;; Example 1: Basic alpha reduction
;; Lambda expression with nested lambdas using the same parameter name
;; (λx.(λx.x)) will be alpha-reduced to (λx.(λx_1.x_1))
(display "\nExample 1: Basic alpha reduction\n")
(define example1 '(lambda x (lambda x x)))
(display "Before alpha reduction: ")
(pp-expr example1)
(newline)
(display "After alpha reduction: ")
(pp-expr (alpha-reduce example1))
(newline)

;; Example 2: Alpha reduction with types
;; (λx:Int.(λx:Bool.x)) will be alpha-reduced to (λx:Int.(λx_1:Bool.x_1))
(display "\nExample 2: Alpha reduction with types\n")
(define example2 '(lambda x |Int| (lambda x |Bool| x)))
(display "Before alpha reduction: ")
(pp-expr example2)
(newline)
(display "After alpha reduction: ")
(pp-expr (alpha-reduce example2))
(newline)

;; Example 3: Alpha reduction in a more complex expression
;; (λx.(λx.(λy.(x y)))) will be alpha-reduced to (λx.(λx_1.(λy.(x_1 y))))
(display "\nExample 3: Alpha reduction in a more complex expression\n")
(define example3 '(lambda x (lambda x (lambda y (app x y)))))
(display "Before alpha reduction: ")
(pp-expr example3)
(newline)
(display "After alpha reduction: ")
(pp-expr (alpha-reduce example3))
(newline)

;; Example 4: Evaluating an expression with alpha reduction
;; When we evaluate a lambda expression, alpha reduction is performed automatically
(display "\nExample 4: Evaluating an expression with alpha reduction\n")
(display "Expression: ")
(pp-expr example1)
(newline)
(display "Evaluation result: ")
(pp-expr (g:eval example1))
(newline)

;; Example 5: Alpha reduction with multiple nested lambdas with the same parameter
;; (λx.(λx.(λx.x))) will be alpha-reduced multiple times
(display "\nExample 5: Alpha reduction with multiple nested lambdas\n")
(define example5 '(lambda x (lambda x (lambda x x))))
(display "Before alpha reduction: ")
(pp-expr example5)
(newline)
(display "First alpha reduction: ")
(define first-reduction (alpha-reduce example5))
(pp-expr first-reduction)
(newline)
(display "Second alpha reduction: ")
(pp-expr (alpha-reduce first-reduction))
(newline)

;; Example 6: Alpha reduction doesn't happen when parameter names are different
;; (λx.(λy.x)) doesn't need alpha reduction
(display "\nExample 6: No alpha reduction needed\n")
(define example6 '(lambda x (lambda y x)))
(display "Before: ")
(pp-expr example6)
(newline)
(display "After (unchanged): ")
(pp-expr (alpha-reduce example6))
(newline) 