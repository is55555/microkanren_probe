;; nss.scm - Namespace Preprocessor for Scheme
;; Usage: chezscheme --script nss.scm input.nss output.scm

(define separator "__") ; default separator between namespace levels
(define namespace-stack '()) ; stack of active nested namespaces

;; Environment for scope-aware rewriting
(define scoped-env '()) ; stack of ((original-symbol . mangled-symbol) ...) frames

;; Push a new scope, inheriting visible bindings
(define (push-scope!)
  ;; Instead of pushing an empty frame, we push a copy of the top frame (to inherit outer bindings)
  (let ((inherited (if (null? scoped-env) '() (car scoped-env))))
    (set! scoped-env (cons (append inherited '()) scoped-env))))

;; Pop the top scope
(define (pop-scope!)
  (set! scoped-env (cdr scoped-env)))

;; Add a new binding to the current scope
(define (define-symbol! name mangled)
  (let ((top (car scoped-env)))
    (set-car! scoped-env (cons (cons name mangled) top))))

;; Look up a symbol in the scoped environment
(define (lookup-symbol sym)
  (let loop ((frames scoped-env))
    (cond
      ((null? frames) sym) ; not found, return as-is
      ((assoc sym (car frames)) => cdr)
      (else (loop (cdr frames))))))

;; Recursively rewrite an expression by substituting known identifiers
(define (rewrite expr)
  (cond
    ((symbol? expr) (lookup-symbol expr))
    ((pair? expr) (cons (rewrite (car expr)) (rewrite (cdr expr))))
    ((vector? expr)
     (list->vector (map rewrite (vector->list expr))))
    ((null? expr) expr)
    (else expr)))

;; Utility: join strings in a list with a separator
(define (string-join lst sep)
  (if (null? lst)
      ""
      (let loop ((items (cdr lst))
                 (acc (car lst)))
        (if (null? items)
            acc
            (loop (cdr items)
                  (string-append acc sep (car items)))))))

;; Returns the current full namespace path as a string
(define (current-namespace)
  (string-join (reverse namespace-stack) separator))

;; Mangles a symbol using the current namespace path
(define (mangle sym)
  (string->symbol
   (if (null? namespace-stack)
       (symbol->string sym)
       (string-append (current-namespace) separator (symbol->string sym)))))

;; Reads all top-level forms from an input port
(define (read-all port)
  (let loop ((forms '()))
    (let ((form (read port)))
      (if (eof-object? form)
          (reverse forms)
          (loop (cons form forms))))))

(define inline-mode? #f) ; flag to track if we're inside an inline namespace

;; Computes the namespace string one level up
(define (parent-namespace)
  (if (> (length namespace-stack) 1)
      (string-join (reverse (cdr namespace-stack)) separator)
      ""))

;; Creates an alias definition from outer-scope name to inner-scope name
(define (alias-definition outer-sym inner-sym)
  `(define ,outer-sym ,inner-sym))

;; Main recursive form processor
(define (process-form form)
  (cond
    ;; Handle (ns-set 'separator "...")
    ((and (pair? form) (eq? (car form) 'ns-set))
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

    ;; Handle regular (ns "name" ...) blocks
    ((and (pair? form) (eq? (car form) 'ns))
     (let ((name (cadr form))
           (body (cddr form)))
       (set! namespace-stack (cons name namespace-stack))
       (push-scope!)
       (let ((expanded (map process-form body)))
         (pop-scope!)
         (set! namespace-stack (cdr namespace-stack))
         (apply append expanded))))

    ;; Handle inline namespaces (ns-inline "name" ...)
    ((and (pair? form) (eq? (car form) 'ns-inline))
     (let ((name (cadr form))
           (body (cddr form)))
       (set! namespace-stack (cons name namespace-stack))
       (push-scope!)
       (let ((was-inline inline-mode?))
         (set! inline-mode? #t)
         (let ((expanded (map process-form body)))
           (set! inline-mode? was-inline)
           (pop-scope!)
           (set! namespace-stack (cdr namespace-stack))
           (apply append expanded)))))

    ;; Handle (define (fn args ...) body ...)
    ((and (pair? form)
          (eq? (car form) 'define)
          (pair? (cadr form)))
     (let* ((fname (caadr form))
            (args (cdadr form))
            (body (cddr form))
            (mangled (mangle fname))
            (outer-ns (parent-namespace))
            (alias-sym (if (string=? outer-ns "")
                           #f
                           (string->symbol (string-append outer-ns separator (symbol->string fname))))))
       (define-symbol! fname mangled)
       (push-scope!)
       (for-each (lambda (arg)
                   (define-symbol! arg arg)) ; function args are local, unmangled
                 args)
       (let ((rewritten-body (map rewrite body)))
         (pop-scope!)
         (if (and inline-mode? alias-sym)
             (list `(define (,mangled ,@args) ,@rewritten-body)
                   (alias-definition alias-sym mangled))
             (list `(define (,mangled ,@args) ,@rewritten-body))))))

    ;; Handle (define var val)
    ((and (pair? form)
          (eq? (car form) 'define)
          (symbol? (cadr form)))
     (let* ((sym (cadr form))
            (val (caddr form))
            (mangled (mangle sym))
            (outer-ns (parent-namespace))
            (alias-sym (if (string=? outer-ns "")
                           #f
                           (string->symbol (string-append outer-ns separator (symbol->string sym))))))
       (define-symbol! sym mangled)
       (let ((rewritten-val (rewrite val)))
         (if (and inline-mode? alias-sym)
             (list `(define ,mangled ,rewritten-val)
                   (alias-definition alias-sym mangled))
             (list `(define ,mangled ,rewritten-val))))))

    ;; Otherwise leave it alone
    (else (list form))))

;; Compile .nss → .scm
(define (compile-nss input-filename output-filename)
  (let ((in (open-input-file input-filename))
        (out (open-output-file output-filename 'replace)))
    ;; Add header comment
    (display ";; This file was automatically generated from a `.nss` source.\n" out)
    (display ";; Do not modify this file directly — edit the original `.nss` file instead.\n\n" out)

    ;; Read and process input
    (let ((forms (read-all in)))
      (for-each
       (lambda (form)
         (for-each
          (lambda (out-form)
            (write out-form out)
            (newline out)
            (newline out)) ; spacing
          (process-form form)))
       forms))

    (close-input-port in)
    (close-output-port out)))

;; Main CLI entry point
(define (main)
  (let ((args (command-line)))
    (if (< (length args) 3)
        (begin
          (display "Usage: chezscheme --script nss.scm input.nss output.scm\n")
          (exit 1))
        (compile-nss (cadr args) (caddr args)))))

(main)
