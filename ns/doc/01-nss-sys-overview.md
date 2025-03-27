## NSS (Namespace Scheme) Preprocessor System

### Overview

The NSS system is a preprocessor for R5RS/R7RS-compatible Scheme implementations that allows developers to define nested namespaces using custom forms like `(ns ...)` and `(ns-inline ...)`. It transforms `.nss` files into `.scm` files, mangling symbols based on their namespace context.

### Goals

- Structured, nested namespace handling (C++-like scoping).
- Inline sub-namespaces with outer visibility.
- Non-intrusive to existing Scheme compilers/interpreters.
- Widely compatible with R5RS/R7RS - Chez Scheme and CHICKEN used for reference.
- Customisable separator for mangled names.

---

### Components and Responsibilities

#### 1. Environment Management

**Key Procedures:**

- `push-scope!`
- `pop-scope!`
- `define-symbol!`
- `lookup-symbol`

**Purpose:**
Maintain a stack of environments (scopes). Each scope maps original symbols to their mangled names. Used during both definition and rewriting.

**Behavior:**

- `ns` and `ns-inline` push a new scope.
- Inline scopes inherit from the outer scope.
- `define-symbol!` mutates the top scope.
- `lookup-symbol` traverses down to find the nearest mapping.

#### 2. Symbol Mangling

**Key Procedure:** `mangle`

**Purpose:**
Mangle symbols based on the active namespace stack and separator.

**Behavior:**

- Joins reversed `namespace-stack` with the configured `separator`, then appends symbol.
- Top-level symbols are not mangled.

#### 3. Namespace Handling

**Forms:** `(ns name ...)`, `(ns-inline name ...)`

**Purpose:**

- `(ns ...)` introduces a new scope and namespace path.
- `(ns-inline ...)` behaves the same, but inner symbols are aliased into the parent namespace.

**Behavior:**

- Adds `name` to `namespace-stack`.
- Pushes a new `scoped-env` frame.
- For inline, preserves parent visibility.
- On exit, pops scope and namespace.

#### 4. Rewrite System

**Key Procedure:** `rewrite`

**Purpose:**
Recursively transform expressions, replacing symbols with their mangled equivalents.

**Current Issues:**

- Rewrites *all* lists, including special forms like `if`, `cond`, `lambda`, `define`.
- Needs smart skipping or mode-aware rewriting.

**To Do:**

- Recognize special forms.
- Avoid rewriting syntax-position symbols.
- Possibly treat macros as opaque.

#### 5. Error Handling

**Goal:**

- Fail gracefully on malformed input.
- Provide useful error messages including form content.

**Issues Identified:**

- Crashes on malformed `(define)` and `(ns)`.
- No location or line number info available.

**Plan:**

- Validate all input forms before destructuring.
- Improve error messages.

---

### Macro Handling (Planned)

**Issue:**
Scheme macros may expand to special forms or custom syntax.

**Complications:**

- Macros can hide special forms in expansions.
- They may also expand to `ns` or `define`, which the preprocessor must recognize *after* macro expansion.

**Options:**

- Require `.nss` input to be macro-expanded by user.
- Hook into interpreter/compiler macroexpander (if available).
- Treat unknown forms conservatively.

---

### Testing Plan

**Unit-level:**

- Environment stack behavior.
- Mangling under various separator and nesting configurations.
- Rewriting of simple and nested expressions.
- Valid and invalid uses of `define`, `ns`, `ns-inline`.

**Integration-level:**

- Correct output `.scm` for input `.nss` files.
- Input with macros, special forms, and global references.

**Error injection tests:**

- Malformed forms like `(define)`, `(ns)`, `(ns-inline)`.
- Undefined variables in rewrite phase.

---

### Reference: Current Known-Broken Source (to start fresh from)

#### `nss-lib.scm`

```
;; nss-lib.scm - Namespace Preprocessor Lib for Scheme 

(define separator "__")
(define namespace-stack '())
(define scoped-env '())
(define inline-mode? #f)

;;; Environment Management ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (push-scope!)
  (let ((inherited (if (null? scoped-env) '() (car scoped-env))))
    (set! scoped-env (cons (append inherited '()) scoped-env))))

(define (pop-scope!)
  (set! scoped-env (cdr scoped-env)))

(define (define-symbol! name mangled)
(begin ; debug
(display "(define-symbol! ") (write name) (display " → ") (write mangled) (display ")\n")
(let ((top (car scoped-env)))
  (set-car! scoped-env (cons (cons name mangled) top))))
)

(define (lookup-symbol sym)

(begin
(display "(lookup-symbol ") (write sym) (display ") → ")
(let loop ((frames scoped-env))
  (cond
    ((null? frames) (begin (write sym) (newline) sym))
    ((assoc sym (car frames)) => (lambda (pair) (write (cdr pair)) (newline) (cdr pair)))
    (else (loop (cdr frames))))))
    )

;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-join lst sep)
  (if (null? lst)
      ""
      (let loop ((items (cdr lst)) (acc (car lst)))
        (if (null? items)
            acc
            (loop (cdr items) (string-append acc sep (car items)))))))

(define (current-namespace)
  (string-join (reverse namespace-stack) separator))

(define (parent-namespace)
  (if (> (length namespace-stack) 1)
      (string-join (reverse (cdr namespace-stack)) separator)
      ""))

(define (mangle sym)
  (string->symbol
   (if (null? namespace-stack)
       (symbol->string sym)
       (string-append (current-namespace) separator (symbol->string sym)))))

(define (alias-definition outer-sym inner-sym)
  `(define ,outer-sym ,inner-sym))

(define (rewrite expr)
  (display "(rewrite) expr = ") (write expr) (newline)
  (cond
    ((symbol? expr) (lookup-symbol expr))
    ((pair? expr) (cons (rewrite (car expr)) (rewrite (cdr expr))))
    ((vector? expr) (list->vector (map rewrite (vector->list expr))))
    ((null? expr) expr)
    (else expr)))

;;; Form Handlers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (handle-ns-set form)
  (let ((args (cdr form)))
    (cond
      ((and (= (length args) 2)
            (pair? (car args))
            (eq? (car (car args)) 'quote)
            (symbol? (cadr (car args)))
            (string=? (symbol->string (cadr (car args))) "separator")
            (string? (cadr args)))
       (set! separator (cadr args)))
      (else
       (error 'ns-set "Invalid usage of ns-set: expected (ns-set 'separator \"...\")"))))
  '())

(define (handle-ns name body inline?)
    (display "[handle-ns] entering with name = ") (write name) (newline)
    (display "[handle-ns] body = ") (write body) (newline)

    (when (or (null? name) ; no empty namespace names
        (and (string? name) (string=? name ""))
        (and (symbol? name) (string=? (symbol->string name) "")))
        (error 'ns "Namespace name cannot be empty"))


    (set! namespace-stack (cons name namespace-stack))
    (push-scope!)
    (let ((was-inline inline-mode?))
    (when inline? (set! inline-mode? #t))
    (let loop ((forms body) (results '()))
        (if (null? forms)
            (begin
            (set! inline-mode? was-inline)
            (pop-scope!)
            (set! namespace-stack (cdr namespace-stack))
            (reverse results))
            (let ((current (car forms)))
            (display "[handle-ns] --> Subform: ") (write current) (newline)
            (let ((processed (process-form current)))
                (loop (cdr forms)
                    (append (reverse processed) results))))))))


(define (handle-define-lambda form)
    (display ">>> entering handle-define-lambda with form: ") (write form) (newline)
    (let* ((head (cadr form))
            (fname (car head))
            (args (cdr head))
            (body (cddr form))
            (mangled (mangle fname))
            (outer-ns (parent-namespace))
            (alias-sym (if (string=? outer-ns "")
                            #f
                            (string->symbol (string-append outer-ns separator (symbol->string fname))))))
        (define-symbol! fname mangled)
        (push-scope!)
        (for-each (lambda (arg) (define-symbol! arg arg)) args)
        (let ((rewritten-body (map rewrite body)))
        (pop-scope!)
        (if (and inline-mode? alias-sym)
            (list `(define (,mangled ,@args) ,@rewritten-body)
                    (alias-definition alias-sym mangled))
            (list `(define (,mangled ,@args) ,@rewritten-body))))))
    

(define (handle-define-var form)
  (let* ((sym (cadr form))
         (val (caddr form))
         (mangled (mangle sym))
         (outer-ns (parent-namespace))
         (alias-sym (if (string=? outer-ns "") #f (string->symbol (string-append outer-ns separator (symbol->string sym))))))
    (define-symbol! sym mangled)
    (let ((rewritten-val (rewrite val)))
      (if (and inline-mode? alias-sym)
          (list `(define ,mangled ,rewritten-val)
                (alias-definition alias-sym mangled))
          (list `(define ,mangled ,rewritten-val))))))

;;; Dispatcher ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (process-form form)
(display "(process-form) form = ") (write form) (newline)
(cond
  ;; (ns-set 'separator "...")
  ((and (pair? form)
        (eq? (car form) 'ns-set))
   (handle-ns-set form))

  ;; (ns "name" ...)
  ((and (pair? form)
        (eq? (car form) 'ns)
        (>= (length form) 2))
   (handle-ns (cadr form) (cddr form) #f))

  ;; (ns-inline "name" ...)
  ((and (pair? form)
        (eq? (car form) 'ns-inline)
        (>= (length form) 2))
   (handle-ns (cadr form) (cddr form) #t))

  ;; (define (fn ...) ...)
  ((and (pair? form)
        (eq? (car form) 'define)
        (pair? (cadr form))
        (not (null? (cadr form)))
        (symbol? (car (cadr form))))
   (handle-define-lambda form))

  ;; (define var val)
  ((and (pair? form)
        (eq? (car form) 'define)
        (symbol? (cadr form)))
   (handle-define-var form))

  ;; fallback
  (else (list form))))



; (define (process-form form)
; (display "(process-form) raw form = ") (write form) (newline)
; (if (and (pair? form)
;          (eq? (car form) 'define))
;     (display "→ define form detected\n")
;     (display "→ not a define\n"))
; '())

```

#### `nss-cli.scm`

```
;; nss-cli.scm - Namespace Preprocessor cli for Scheme
;; Usage: chezscheme --script nss-cli.scm input.nss output.scm

(load "nss-lib.scm")


#;(define (read-all port)
(let loop ((forms '()))
  (let ((form (read port)))
    (if (eof-object? form) (reverse forms) (loop (cons form forms))))))

(define (read-all port)
(let loop ((forms '()))
  (let ((form (read port)))
    (if (eof-object? form)
        (reverse forms)
        (begin
          (display "[read-all] >>> read: ") (write form) (newline)
          (loop (cons form forms)))))))

        (define (compile-nss input-filename output-filename)
      (let ((in (open-input-file input-filename))
            (out (open-output-file output-filename 'replace)))
        (display ";; This file was automatically generated from a `.nss` source.\n" out)
        (display ";; Do not modify this file directly — edit the original `.nss` file instead.\n\n" out)
    
        ;; Read and inspect forms before processing
        (let ((forms (read-all in)))
          (display "[compile-nss] >>> forms = ") (write forms) (newline)
    
          (for-each
           (lambda (form)
             (for-each
              (lambda (out-form)
                (write out-form out)
                (newline out)
                (newline out))
              (process-form form)))
           forms))
    
        (close-input-port in)
        (close-output-port out)))
    
(define (main)
(let ((args (command-line)))
  (if (< (length args) 3)
      (begin
        (display "Usage: chezscheme --script nss.scm input.nss output.scm\n")
        (exit 1))
      (compile-nss (cadr args) (caddr args)))))

(main)
```

#### Minimal Input `.nss` That Crashes

```
(ns-set 'separator "__")

(define outside-var 5)

(ns "util"
  (define a 1)
  (ns-inline "short"
    (define b 2)
    (define (c x) 
      (* (+ a x b)
         outside-var))))
```

The third form causes a crash during processing. Future work will address parsing guarantees, form validation, and safe handling of rewrite and scope logic.

