;; nss.scm - Namespace Preprocessor for Scheme
;; Usage: (compile-nss "input.nss" "output.scm")

(define separator "__") ; default
(define namespace-stack '())

(define (ns-set key val)
  (cond
    ((and (eq? key 'separator) (string? val))
     (set! separator val))
    (else
     (error "Unknown setting"))))

;; Helper: current full namespace path
(define (current-namespace)
  (string-join (reverse namespace-stack) separator))

;; Helper: mangle a symbol
(define (mangle sym)
  (string->symbol
   (if (null? namespace-stack)
       (symbol->string sym)
       (string-append (current-namespace) separator (symbol->string sym)))))

;; Process forms recursively
(define (process-form form)
  (cond
    ;; (ns-set 'separator "::")
    ((and (pair? form) (eq? (car form) 'ns-set))
     (apply ns-set (cdr form))
     '()) ; no output

    ;; (ns "name" body...)
    ((and (pair? form) (eq? (car form) 'ns))
     (let ((name (cadr form))
           (body (cddr form)))
       (set! namespace-stack (cons name namespace-stack))
       (let ((expanded (map process-form body)))
         (set! namespace-stack (cdr namespace-stack))
         (apply append expanded))))

    ;; (ns-inline "name" body...)
    ((and (pair? form) (eq? (car form) 'ns-inline))
     (let ((name (cadr form))
           (body (cddr form)))
       (set! namespace-stack (cons name namespace-stack))
       ;; Inline: treat definitions as if in parent
       (let ((expanded (map process-form body)))
         (set! namespace-stack (cdr namespace-stack))
         (apply append expanded))))

    ;; (define (fname ...) ...)
    ((and (pair? form)
          (eq? (car form) 'define)
          (pair? (cadr form)))
     (let ((fname (caadr form)))
       (list `(define ,(cons (mangle fname) (cdadr form))
                ,@(cddr form)))))

    ;; Simple define: (define x val)
    ((and (pair? form)
          (eq? (car form) 'define)
          (symbol? (cadr form)))
     (list `(define ,(mangle (cadr form)) ,(caddr form))))

    ;; fallback
    (else (list form))))

;; Read full input
(define (read-all port)
  (let loop ((forms '()))
    (let ((form (read port)))
      (if (eof-object? form)
          (reverse forms)
          (loop (cons form forms))))))

(define (compile-nss input-filename output-filename)
  (let ((in (open-input-file input-filename))
        (out (open-output-file output-filename)))
    (let ((forms (read-all in)))
      (for-each
       (lambda (form)
         (for-each
          (lambda (out-form)
            (write out-form out)
            (newline out)
            (newline out)) ; spacing for readability
          (process-form form)))
       forms))
    (close-input-port in)
    (close-output-port out)))
