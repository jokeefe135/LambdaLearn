;; You gotta use sdf and load generic-procedures before use

;; let eval be used for 
;; gonna use eval apply to make extensible for types, potentially future forms such as vars, numbers etc

#|
(define (default-eval expression)
  (cond ((lambda? expression))
	expression
	(else
	 (error "Unknown expression type"
		expression))))
|#

(define (default-eval expression)
  (error "Unknown expression type" expression))

(define g:eval
  (simple-generic-procedure 'eval 1 default-eval))


(define (default-apply procedure operands)
  (error "Unknown application type"))

(define g:apply
  (simple-generic-procedure 'eval 1 default-apply))
