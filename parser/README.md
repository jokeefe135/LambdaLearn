## Lambda‑Calculus Parser (Typed + Untyped)

---

### Features

- **Automatic detection** of whether the input contains type annotations:
  - If the input contains `:` or the arrow `->`, it dispatches to the **typed** parser.
  - Otherwise, it uses the **untyped** parser.

### Public Entry Point

```scheme
(parse "λx.x")         ;; ⇒ AST
(parse "λx:Nat. x")    ;; ⇒ AST
```

---

### AST Representation (common to both flavours)

- **Variable**: Scheme symbol, e.g. `'x`
- **Abstraction**: 
  ```scheme
  (lambda var [type] body)
  ```
  - `[type]` is either a type-AST or `#f`

- **Application**:
  ```scheme
  (app func arg)
  ```

---

### Type AST

- **Identifier Type**: symbol, e.g. `'Nat`
- **Arrow Type**:
  ```scheme
  (-> from to)
  ```

---

### Grammar

#### Untyped Lambda Calculus

```
expression   ::= abstraction
              | application

abstraction  ::= ("λ" | "\") variable "." expression

application  ::= atomic { atomic }      ; one or more atomics

atomic       ::= variable
              | "(" expression ")"

variable     ::= [a-zA-Z_][a-zA-Z0-9_]*
```

#### Typed Lambda Calculus

```
expression   ::= abstraction
              | application

abstraction  ::= ("λ" | "\") variable ":" type "." expression

application  ::= atomic { atomic }      ; one or more atomics

atomic       ::= variable
              | "(" expression ")"

type         ::= base-type
              | "(" type "->" type ")"

base-type    ::= [a-zA-Z_][a-zA-Z0-9_]*

variable     ::= [a-zA-Z_][a-zA-Z0-9_]*
```
