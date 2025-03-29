## 📘 Subsystem: Environment Management

### Purpose

This component tracks the **mapping between original and mangled symbol names** across namespace scopes. It simulates lexical scope using a stack of environments, where each environment frame is an association list mapping unqualified names to their mangled equivalents.

---

### Main Concepts

#### ✅ `scoped-env`
A stack of frames, each frame being an alist like:
```scheme
((a . geometry::a)
 (b . geometry::short::b))
```

#### ✅ Inheritance
When a new scope is pushed, it copies the current top environment:
```scheme
(push-scope!) → (cons (copy of top frame) scoped-env)
```
This ensures that inner forms still resolve outer bindings — crucial for inlined or nested namespace forms.

---

### Public Functions

#### `push-scope!`
Pushes a new frame on top of the environment stack.  
Inherits all previous bindings.

#### `pop-scope!`
Removes the top frame.  
Should match each `push-scope!` one-to-one.

#### `define-symbol! name mangled`
Adds a `(name . mangled)` entry to the top environment frame.

#### `lookup-symbol sym`
Looks up a symbol name from the top of the stack downward.
If no match is found, returns the original symbol (assumed global).

---

### 🧪 Expected Behavior

#### Example 1 — Nested Scope with Inheritance
```scheme
(push-scope!) ; env = ()
(define-symbol! 'a 'ns::a)
(push-scope!) ; inherits a
(lookup-symbol 'a) ⇒ ns::a
(pop-scope!)
(pop-scope!)
```

#### Example 2 — Shadowing
```scheme
(push-scope!)
(define-symbol! 'x 'outer::x)
(push-scope!)
(define-symbol! 'x 'inner::x)
(lookup-symbol 'x) ⇒ inner::x
(pop-scope!)
(lookup-symbol 'x) ⇒ outer::x
```

---

### 🧪 Suggested Tests

- [ ] Lookup from empty env returns symbol unchanged
- [ ] Lookup after `define-symbol!` returns mangled name
- [ ] Pushed frame inherits previous bindings
- [ ] Nested shadowing resolves to innermost
- [ ] Pop restores previous scope

[TODO: draft a test suite for this next]
[NEXT: symbol mangling]

---

### 🛠️ Internal Notes and Mechanics

#### Scope Stack Discipline

- The environment stack (`scoped-env`) must never be empty while processing forms.
- Each scope frame is a flat alist, not a tree.
- Pushed frames copy outer bindings, not reference them, to preserve shadowing and prevent mutation leaks.

#### Symbol Definition Timing

Symbols must be defined **before** rewriting their references in the body:
- In `let`, `define-symbol!` happens after rewriting RHS.
- In `let*`, it happens immediately before rewriting RHS.
- In `letrec`, all symbols are pre-bound before rewriting.

#### Debugging Support

For development, `define-symbol!` and `lookup-symbol` emit logs like:
```scheme
(define-symbol! x → geometry__x)
(lookup-symbol x) → geometry__x
```
These help visualize symbol resolution and validate proper shadowing and inheritance.

#### Free Symbol Handling

If a symbol is not found in any frame, `lookup-symbol` returns it unchanged. This lets free variables be mangled in `rewrite`, depending on the current namespace context.

#### Invariant

All rewrite-time bindings must flow through this subsystem. This ensures uniformity between variable names, function names, and potential macro identifiers (when supported later).