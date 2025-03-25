;; nss.scm - Namespace Preprocessor for Scheme
;; Usage: chezscheme --script nss.scm input.nss output.scm

(define separator "__") ; default separator
(define namespace-stack '())

;; Utility: join strings with separator
(define (string-join lst sep)
  (if (null? lst)
      ""
      (let loop ((items (cdr lst))
                 (acc (car lst)))
        (if (null? items)
            acc
            (loop (cdr items)
                  (string-append acc sep (car items)))))))

;; Get current full namespace path
(define (current-namespace)
  (string-join (reverse namespace-stack) separator))

;; Mangle symbol with current namespace
(define (mangle sym)
  (string->symbol
   (if (null? namespace-stack)
       (symbol->string sym)
       (string-append (current-namespace) separator (symbol->string sym)))))

;; Read all forms from file
(define (read-all port)
  (let loop ((forms '()))
    (let ((form (read port)))
      (if (eof-object? form)
          (reverse forms)
          (loop (cons form forms))))))

(define inline-mode? #f)

(define (parent-namespace)
    (if (> (length namespace-stack) 1)
        (string-join (reverse (cdr namespace-stack)) separator)
        ""))

(define (alias-definition outer-sym inner-sym)
    `(define ,outer-sym ,inner-sym))

(define (process-form form)
    (cond
    ;; ns-set handling
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

    ;; regular namespace
    ((and (pair? form) (eq? (car form) 'ns))
        (let ((name (cadr form))
            (body (cddr form)))
        (set! namespace-stack (cons name namespace-stack))
        (let ((expanded (map process-form body)))
            (set! namespace-stack (cdr namespace-stack))
            (apply append expanded))))

    ;; inline namespace
    ((and (pair? form) (eq? (car form) 'ns-inline))
        (let ((name (cadr form))
            (body (cddr form)))
        (set! namespace-stack (cons name namespace-stack))
        (let ((was-inline inline-mode?))
            (set! inline-mode? #t)
            (let ((expanded (map process-form body)))
            (set! inline-mode? was-inline)
            (set! namespace-stack (cdr namespace-stack))
            (apply append expanded)))))

    ;; define with lambda
    ((and (pair? form)
            (eq? (car form) 'define)
            (pair? (cadr form)))
        (let* ((fname (caadr form))
            (mangled (mangle fname))
            (outer-ns (parent-namespace))
            (alias-sym (if (string=? outer-ns "")
                            #f
                            (string->symbol (string-append outer-ns separator (symbol->string fname))))))
        (if (and inline-mode? alias-sym)
            (list `(define (,mangled ,@(cdadr form)) ,@(cddr form))
                    (alias-definition alias-sym mangled))
            (list `(define (,mangled ,@(cdadr form)) ,@(cddr form))))))

    ;; define simple var
    ((and (pair? form)
            (eq? (car form) 'define)
            (symbol? (cadr form)))
        (let* ((sym (cadr form))
            (mangled (mangle sym))
            (outer-ns (parent-namespace))
            (alias-sym (if (string=? outer-ns "")
                            #f
                            (string->symbol (string-append outer-ns separator (symbol->string sym))))))
        (if (and inline-mode? alias-sym)
            (list `(define ,mangled ,(caddr form))
                    (alias-definition alias-sym mangled))
            (list `(define ,mangled ,(caddr form))))))

    ;; fallback
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
