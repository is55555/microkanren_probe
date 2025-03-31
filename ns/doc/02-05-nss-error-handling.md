# 02-05-nss-error-handling.md

## 🚨 Error Handling Strategy

This module outlines how the NSS preprocessor handles malformed input forms and enforces validation before transformation.

---

### 🎯 Goal

- **Fail gracefully** on invalid or unexpected input.
- Provide meaningful error messages that identify the failing form.
- Minimise Scheme interpreter crashes due to malformed structure.

---

### ⚠️ Common Issues Identified

- Crashes when encountering malformed `(define)` forms.
- `(ns)` or `(ns-inline)` blocks with empty names cause unhelpful trace-backs.
- Lack of form context in exception messages.
- Line numbers or file locations are not currently tracked.

---

### 🛡️ Current Protections

- `(ns ...)` and `(ns-inline ...)` handlers check for:
  - Empty strings
  - `()` or `null?` names
  - Missing body

Example:
```scheme
(ns "")
;; Raises: (error 'ns "Namespace name cannot be empty")
```

- `ns-set` is strictly validated for:
  ```scheme
  (ns-set 'separator "...")
  ```

- `process-form` dispatch ensures:
  - Only forms with known shape are destructured
  - Unknown or unexpected forms are passed through or raise a guarded error

---

### 🧪 Future Plans

- Add `validate-form` helpers for defensive shape checking:
  ```scheme
  (and (pair? form) (eq? (car form) 'define) ...)
  ```

- Print error messages that include:
  - Offending form
  - Explanation of the expected pattern
  - Possibly add location hints if port tracking is implemented later

- Detect and error on unrecognized binding shapes like:
  ```scheme
  (define)
  (define (a b c)) ;; missing body
  (ns)
  ```

- Hook into `read-all` to detect malformed top-level constructs or unterminated input.

---

### 🔬 Design Constraint

The system is designed to run across multiple Scheme implementations with R5RS/R7RS compatibility. Because of this, detailed location tracking (e.g. line/column numbers) is not currently portable. Error diagnostics will rely on the form itself for now.

---

### ✅ Examples of Good Practice

```scheme
(define x) ; → ❌ ERROR: define form must have a variable and value
(ns)       ; → ❌ ERROR: namespace name cannot be empty
```

These cases are now intercepted explicitly instead of crashing on `car` or `cdr` access.

---

This subsystem will evolve as macros, includes, or locations are introduced. Until then, all destructuring code must include validation first.
