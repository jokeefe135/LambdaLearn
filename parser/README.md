## Lambda‑Calculus Parser (Typed + Untyped + Assignments)

---

### Features

* **Automatic detection** of whether the input contains type annotations:

  * If the input contains `:` or the arrow `->`, it dispatches to the **typed** parser.
  * Otherwise, it uses the **untyped** parser.
* **Top-level assignments** of the form `X = <lambda-calculus expression>` are now supported.

  * Parsed into `(assign var expr)` AST nodes.

### Public Entry Point

```scheme
(parse "λx.x")
;; ⇒ AST for untyped abstraction

(parse "λx:Nat. x")
;; ⇒ AST for typed abstraction

(parse "F = λx.x")
;; ⇒ (assign 'F <untyped-λ-ast>)

(parse "G = λx:Bool. x")
;; ⇒ (assign 'G <typed-λ-ast>)
```

---

### AST Representation (common to all flavours)

* **Variable**: Scheme symbol, e.g. `'x`
* **Abstraction**:

  ```scheme
  (lambda var [type] body)
  ```

  * `[type]` is either a type-AST or `#f`
* **Application**:

  ```scheme
  (app func arg)
  ```
* **Assignment**:

  ```scheme
  (assign var expr)
  ```

  * `var` is a symbol, `expr` is any lambda AST

---

### Type AST

* **Identifier Type**: symbol, e.g. `'Nat`
* **Arrow Type**:

  ```scheme
  (-> from to)
  ```

---

### Grammar

#### Top-Level

```
program      ::= assignment
              | expression

assignment   ::= variable "=" expression
```

#### Untyped Lambda Calculus

```
expression   ::= abstraction
              | application

abstraction  ::= ("λ" | "\\") variable "." expression

application  ::= atomic { atomic }      ; one or more atomics

atomic       ::= variable
              | "(" expression ")"

variable     ::= [A-Za-z_][A-Za-z0-9_]*
```

#### Typed Lambda Calculus

```
expression   ::= abstraction
              | application

abstraction  ::= ("λ" | "\\") variable ":" type "." expression

application  ::= atomic { atomic }      ; one or more atomics

atomic       ::= variable
              | "(" expression ")"

type         ::= base-type
              | "(" type "->" type ")"

base-type    ::= [A-Za-z_][A-Za-z0-9_]*

variable     ::= [A-Za-z_][A-Za-z0-9_]*
```
