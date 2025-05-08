
(load "~/6.5150/sdf/manager/load")
(manage 'new 'generic-interpreter)

(load (string-append (->namestring (pwd)) "parser/parse-typed.scm"))
(load (string-append (->namestring (pwd)) "parser/parse-untyped.scm"))
(load (string-append (->namestring (pwd)) "parser/parser.scm"))
(load (string-append (->namestring (pwd)) "interpreter/interpreter.scm"))
;;(load (string-append (->namestring (pwd)) "repl.scm"))
(load (string-append (->namestring (pwd)) "tutorial_repl.scm"))

(repl)
