;; nss.scm - Namespace Preprocessor for Scheme
;; Usage: chezscheme --script nss.scm input.nss output.scm

(define separator "__") ; default
(define namespace-stack '())

(define (ns-set key val)
  (cond
    ((and (eq? key 'separator) (string? val))
     (set! separator val))
    (else
     (error "Unknown setting"))))

(define (current-namespace)
  (string-join (reverse namespace-stack) separator))

(define (mangle sym)
  (string->symbol
   (if (null? namespace-stack)
       (symbol->string sym)
       (string-append (current-namespace) separator (symbol->string sym)))))

(define (process-form form)
  (cond
    ;; Settings
    ((and (pair? form) (eq? (car form) 'ns-set))
     (apply ns-set (cdr form))
     '())

    ;; (ns "name" ...)
    ((and (pair? form) (eq? (car form) 'ns))
     (let ((name (cadr form))
           (body (cddr form)))
       (set! namespace-stack (cons name namespace-stack))
       (let ((expanded (map process-form body)))
         (set! namespace-stack (cdr namespace-stack))
         (apply append expanded))))

    ;; (ns-inline "name" ...)
    ((and (pair? form) (eq? (car form) 'ns-inline))
     (let ((name (cadr form))
           (body (cddr form)))
       (set! namespace-stack (cons name namespace-stack))
       (let ((expanded (map process-form body)))
         (set! namespace-stack (cdr namespace-stack))
         (apply append expanded))))

    ;; Function define
    ((and (pair? form)
          (eq? (car form) 'define)
          (pair? (cadr form)))
     (let ((fname (caadr form)))
       (list `(define ,(cons (mangle fname) (cdadr form))
                ,@(cddr form)))))

    ;; Simple define
    ((and (pair? form)
          (eq? (car form) 'define)
          (symbol? (cadr form)))
     (list `(define ,(mangle (cadr form)) ,(caddr form))))

    ;; fallback (let, lambda, etc)
    (else (list form))))

(define (read-all port)
  (let loop ((forms '()))
    (let ((form (read port)))
      (if (eof-object? form)
          (reverse forms)
          (loop (cons form forms))))))

(define (compile-nss input-filename output-filename)
  (let ((in (open-input-file input-filename))
        (out (open-output-file output-filename)))
    ;; Write header
    (display ";; This file was automatically generated from a `.nss` source.\n" out)
    (display ";; Do not modify this file directly — edit the original `.nss` file instead.\n\n" out)

    (let ((forms (read-all in)))
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

;; Command-line support
(define (main)
  (let ((args (command-line)))
    (if (< (length args) 3)
        (begin
          (display "Usage: chezscheme --script nss.scm input.nss output.scm\n")
          (exit 1))
        (compile-nss (cadr args) (caddr args)))))

(main)
