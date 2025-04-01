# 00-nss-doc-overview.md

## 📘 Documentation Structure

### 1. **System Overview**
- What `.nss` files are
- What the preprocessor is supposed to produce
- Core goals: namespace scoping, symbol mangling, optional inlining
- See: `01-nss-sys-overview.md`

---

### 2. **Component Modules**

#### ✅ Environment Management
- Stack of lexical scopes
- Shadowing and inheritance behaviour (especially for inline)
- `define-symbol!`, `lookup-symbol`, `push-scope!`, `pop-scope!`
- See: `02-01-nss-environment-management.md`

#### ✅ Namespace Stack and Symbol Mangling
- How `mangle` constructs names from the namespace stack
- What separators are valid and how they affect output
- Aliases in inline mode
- See: `02-02-nss-namespace-stack-and-mangling.md`

#### ✅ Namespace Handling Forms
- Structure of `(ns ...)`, `(ns-inline ...)`
- Effects on `scoped-env`, `namespace-stack`, and symbol visibility
- Expected behaviour with nesting
- See: `02-03-nss-namespace-handling.md`

#### ✅ Rewrite Subsystem
- What gets rewritten and how scoping is enforced
- What **shouldn’t** (special forms, macros, etc.)
- Let-forms, shadowing, free symbols
- See: `02-04-nss-rewrite-subsystem.md`

#### ✅ Error Handling
- Malformed `define`, `ns`, or `ns-inline`
- Unexpected symbol use
- How errors are reported (e.g., form content)
- See: `02-05-nss-error-handling.md`

#### ✅ CLI Entrypoint
- File read/write flow
- Form dispatch
- Integration of all subsystems
- See: `02-06-nss-cli-entrypoint.md`

---

### 3. **Testing Strategy**
- Each component has corresponding test files in `/test`:
  - `test/test-env-stack.scm`
  - `test/test-rewrite.scm`
  - `test/test-rewrite2.scm`
  - `test/test-rewrite-extra-let-bindings.scm`

---

### 4. **Future Subsystems**
- Macro-aware preprocessing
- Source annotations and `.nss` to `.scm` traceability
- Inclusion handling (`include-nss`)
- Optional namespace introspection in runtime Scheme
