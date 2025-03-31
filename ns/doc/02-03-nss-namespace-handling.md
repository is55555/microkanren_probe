# 02-03-nss-namespace-handling.md

## 📂 Namespace Handling Forms: `ns`, `ns-inline`

This module explains how `(ns ...)` and `(ns-inline ...)` work inside `.nss` source files, and how they control both scoping and symbol mangling.

---

### ✨ Purpose

- `(ns name ...)` introduces a new nested namespace and lexical scope.
- `(ns-inline name ...)` behaves similarly, but in addition exposes defined symbols back into the parent scope using alias definitions.

---

### 🏗 Internal Behavior

Both forms:
- Add `name` to the `namespace-stack` (via `push-namespace!`)
- Push a new environment frame (via `push-scope!`)
- Rewrite all inner forms under the new namespace and scope
- Pop the environment and namespace stack on exit

Inline-specific behavior:
- Enables `inline-mode?`
- For each defined symbol, also defines an alias in the parent namespace using `alias-definition`:
  ```scheme
  (define parent__symbol child__symbol)
  ```

---

### 🔍 Example

```scheme
(ns-set 'separator "__")

(define global-x 1)

(ns "util"
  (define a 10)

  (ns-inline "short"
    (define b 20)
    (define (add x) (+ x b a global-x))))
```

This produces:

```scheme
(define global__x 1)

(define util__a 10)

(define util__short__b 20)
(define util__short__add
  (lambda (x) (+ x util__short__b util__a global__x)))

(define util__b util__short__b)
(define util__add util__short__add)
```

---

### ⚠️ Constraints

- Namespace names must not be empty.
- `inline-mode?` is not recursive — nested `ns-inline` blocks still emit their own aliases but restore the previous mode on exit.
- Multiple inline blocks can coexist and overwrite aliases if names conflict.

---

### 🧪 Debugging Output

Expect trace logs during processing such as:

```
[handle-ns] entering with name = "util"
[handle-ns] --> Subform: (define a 10)
(mangle 'a) => util__a
```

---

This mechanism guarantees both nested organization and optional symbol flattening when `ns-inline` is used.
