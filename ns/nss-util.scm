;; nss-util.scm — general utilities for ns system

(define (identifier? x)
(symbol? x))

(define (application? x)
(and (pair? x) (not (memq (car x) '(lambda define let let* letrec letrec* begin)))))

(define (lambda? x)
(and (pair? x) (eq? (car x) 'lambda)))

(define (define? x)
(and (pair? x) (eq? (car x) 'define)))

(define (let-form? x)
(and (pair? x) (memq (car x) '(let let* letrec letrec*))))

(define (let-bindings x)
(cadr x))

(define (begin-form? x)
(and (pair? x) (eq? (car x) 'begin)))

(define (safe-map f x)  ; to handle dotted pairs in rewrite
(cond
  ((null? x) '())
  ((pair? x) (cons (f (car x)) (safe-map f (cdr x))))
  (else (f x))))  ; dotted tail: apply f directly


;; --- NEW: robust extraction for any lambda form
(define (lambda-formals->bound-vars formals)
(cond
  ((symbol? formals)
   (list formals))
  ((null? formals)
   '())
  ((pair? formals)
   (let loop ((lst formals) (acc '()))
     (cond
       ((null? lst) (reverse acc))
       ((symbol? lst) (reverse (cons lst acc))) ; dotted tail
       ((pair? lst) (loop (cdr lst) (cons (car lst) acc)))
       (else (error "invalid lambda formals" formals)))))
  (else
   (error "invalid lambda formals" formals))))

