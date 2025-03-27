## 📘 Documentation Structure

### 1. **System Overview**
- What `.nss` files are
- What the preprocessor is supposed to produce
- Core goals: namespace scoping, symbol mangling, optional inlining

---

### 2. **Component Modules**

#### ✅ Environment Management
- Stack of scopes
- Inheritance behavior (especially for inline)
- `define-symbol!`, `lookup-symbol`, `push-scope!`, `pop-scope!`

#### ✅ Symbol Mangling
- How `mangle` constructs names from the namespace stack
- What separators are valid and how they affect output
- Aliases in inline mode

#### ✅ Namespace Handling
- Structure of `ns`, `ns-inline`
- Effects on `scoped-env`, `namespace-stack`, and symbol visibility
- Expected behavior with nesting

#### ✅ Rewrite System
- What gets rewritten
- What **shouldn’t** (special forms, macros, etc.)
- Limitations and design choices

#### ✅ Error Handling
- Malformed `define`, `ns`, or `ns-inline`
- Unexpected symbol use
- How errors should be reported (e.g., form + line number)

---

### 3. **Macro Handling**
- When macros are expanded (if at all)
- Why macro expansion may require interpreter or Scheme-specific support
- How to safely preprocess without breaking macro logic

---

### 4. **Testing Strategy**
- Each component with unit tests:
  - Env stack tests
  - Symbol mangling cases
  - `rewrite` behavior
  - `ns` and `ns-inline` nesting
- Error injection and malformed input tests

---

### 5. **Appendix: Known-Broken Current Source**
- Include `nss-lib.scm` and `nss-cli.scm` exactly as they are
- Annotate crash behavior, where debugging was attempted
- Use as reference point for incremental refactor

---

See nss-sys-overview.md