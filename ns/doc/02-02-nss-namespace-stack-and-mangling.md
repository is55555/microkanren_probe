# 02-02-nss-namespace-stack-and-mangling.md

## 🧭 Namespace Stack and Symbol Mangling

This module controls how symbol names are transformed (`mangled`) based on their enclosing namespace context.

---

### 📚 Data Structures

#### `namespace-stack`

A global list of strings or symbols that represents the current nesting of namespaces, from innermost (head) to outermost.

Example:
```scheme
(set! namespace-stack '(short util geometry))
```
Represents the current namespace: `geometry::util::short` (depending on separator).

#### `separator`

A string (e.g. `"::"` or `"__"`) used to join namespace components in mangled names.

---

### 🔁 Stack Management

#### `(push-namespace! name)`

Adds a new namespace component to the stack. Typically called when entering an `(ns ...)` or `(ns-inline ...)` form.

#### `(pop-namespace!)`

Removes the top component from the stack. Called when exiting the namespace block.

---

### 🧪 Symbol Mangling

#### `(mangle sym)`

Returns a symbol that is qualified using the current namespace stack and the separator.

If `namespace-stack` is empty:
```scheme
(mangle 'x) => x
```

If `namespace-stack` is `'(util geometry)` and separator is `"__"`:
```scheme
(mangle 'x) => geometry__util__x
```

This ensures unique global symbol names in the output `.scm` file.

---

### 🧩 Inline Aliasing and Parent Namespace

#### `(parent-namespace)`

Computes the parent namespace string (all but the top of the stack), used for inserting alias definitions in inline namespaces.

Used like:
```scheme
(define outer__x inner__x)
```

Only applied when `inline-mode?` is true and aliases are enabled.

---

### 🔐 Constraints

- Namespace names must not be the empty string or `()`. These are rejected explicitly.
- Namespaces can be re-entered multiple times: mangling is deterministic and based on the current path, not declaration order.
- The separator must not clash with valid Scheme symbols used in user code. Users are responsible for picking a safe one.

---

### 💬 Debugging Output

During development, the system emits logs like:

```scheme
[handle-ns] entering with name = "math"
[handle-ns] --> Subform: (define pi 3.14159)
(mangle 'pi) => math__pi
```

These logs help diagnose incorrect nesting or mangling behavior.

---

### ✅ Example

```scheme
(ns-set 'separator "__")

(ns "geometry"
  (define pi 3.14159)

  (ns "area"
    (define (circle r)
      (* pi r r))))
```

Produces:

```scheme
(define geometry__pi 3.14159)
(define (geometry__area__circle r)
  (* geometry__pi r r))
```

---

This completes the specification of the namespace stack and symbol mangling subsystem.
