### 📗 Subsystem: `rewrite`

#### Role

Walks expressions and rewrites **referenced symbols** to their mangled equivalents using `lookup-symbol`.

#### Behavior

| Type         | Behavior                                 |
|--------------|------------------------------------------|
| `symbol`     | Rewritten using `lookup-symbol`          |
| `pair`       | - If special form: preserves head, rewrites args<br>- Otherwise: rewrites entire structure |
| `vector`     | Rewritten element-wise                   |
| `number`, `string`, etc. | Returned as-is                       |

#### Special Form Awareness

A static list of built-in special forms is respected, including:

```scheme
'(if lambda define set! begin cond let let* quote quasiquote
  unquote unquote-splicing ns ns-inline ns-set)
```

Only their **arguments** are rewritten, preserving structure.

#### Example

```scheme
(rewrite '(if x y x))     ⇒ (if demo__x demo__y demo__x)
(rewrite '(foo x))        ⇒ (demo__foo demo__x)
```

---

Let me know if you’d like this documentation exported to a markdown file or left inline in the `.scm` source headers.

### Binding Form Handling

The `rewrite` function handles special binding forms by explicitly managing the lexical environment during traversal. This ensures variables introduced by binding constructs do not get rewritten, while free references do.

Supported forms and their handling:

| Form       | Scope Introduced For | RHS Can Reference Peers? |
|------------|----------------------|---------------------------|
| `let`      | Body only            | ❌ No                     |
| `let*`     | Body + earlier vars  | ✅ Yes                    |
| `letrec`   | RHS and body         | ✅ Yes (all pre-bound)    |
| `letrec*`  | RHS and body         | ✅ Yes (sequential)       |

During rewrite:
- `let`: RHS expressions are rewritten with current env; then names are bound for body.
- `let*`: each binding is processed in sequence, both binding and RHS rewriting step by step.
- `letrec` and `letrec*`: all names are bound first before rewriting RHS or body.

### Shadowing and Free Variables

If a symbol is already defined in the current scope, `rewrite` preserves it unmodified. Otherwise, it is considered free and is rewritten using the current namespace (via `lookup-symbol`).

Shadowing is handled by pushing a new scope for each binding context. Symbols in inner bindings override outer bindings for the duration of that scope.

### Debugging Output

During development, `rewrite` and the binding form handlers emit debugging output such as:

```
(rewrite) expr = ...
[rewrite-letrec] binding = (f (lambda ...))
```

This output is used to trace the order of evaluation and binding visibility across nested scopes.

### Current Limitations

- No macro-expansion: macros expanding into `define`, `lambda`, etc., are not rewritten correctly unless pre-expanded.
- Quote/quasiquote/unquote are treated as opaque; nested symbol rewriting inside them is not yet supported.
- All special forms must be declared statically in `special-forms`.

### Advanced Examples

```scheme
;; let binding with no RHS visibility
(rewrite '(let ((x 1) (y x)) (+ x y)))
;; => (let ((x 1) (y demo__x)) (demo__+ x y))

;; let* with shadowing
(rewrite '(let* ((x 1) (x (+ x 1))) x))
;; => (let* ((x 1) (x (demo__+ x 1))) x)

;; letrec with mutual recursion
(rewrite
 '(letrec ((f (lambda (n) (g n)))
           (g (lambda (n) (+ n 1))))
    (f demo)))
;; => (letrec ((f (lambda (n) (g n)))
;;             (g (lambda (n) (+ n 1))))
;;      (f demo__demo))
```