# 02-06-nss-cli-entrypoint.md

## 🚀 NSS CLI Entry Point

This module documents the CLI interface and control flow of the NSS preprocessor, typically invoked using `nss-cli.scm`.

---

### 🧾 Usage

The CLI is designed to be executed as a Scheme script using `chezscheme`, `chicken`, or another R7RS-compatible interpreter.

```bash
chezscheme --script nss-cli.scm input.nss output.scm
```

---

### 🧩 File Structure

```scheme
(load "nss-lib.scm") ; loads the core transformation logic
```

- The CLI script handles:
  - Command-line argument validation
  - Input/output port management
  - Reading and dispatching all forms through `process-form`
  - Writing processed output to the `.scm` target file

---

### 🔁 Compilation Flow

#### `(main)`

Main entry point:
```scheme
(command-line) => (nss-cli.scm input.nss output.scm)
```
Dispatches to `(compile-nss input output)`.

#### `(compile-nss input-filename output-filename)`

Steps:
1. Opens input `.nss` file for reading
2. Opens output file for writing (with `'replace` mode if supported)
3. Writes header:
   ```scheme
   ;; This file was automatically generated...
   ```
4. Reads all forms via `read-all`
5. Processes each with `process-form`
6. Writes each transformed form into output file

---

### 🧪 Portability Notes

- The second argument to `open-output-file` must be `'replace` in Chez, but `#:replace` in CHICKEN. This is a known portability divergence.
- Line numbers are not preserved.
- Paths to loaded files (like `nss-lib.scm`) are currently relative and must match script location.

---

### 📂 Example Output

Input `.nss`:
```scheme
(ns-set 'separator "__")
(ns "math" (define pi 3.14))
```

Output `.scm`:
```scheme
;; This file was automatically generated...
(define math__pi 3.14)
```

---

### 🧱 Limitations

- No support for `stdin`/`stdout` streaming
- No file existence checking (output is overwritten)
- No macro-expansion support

---

### 🔍 Future Improvements

- Allow options for separator override via CLI
- Support nested directory mapping for namespaces
- Optional line comment annotations with source `.nss` line numbers
