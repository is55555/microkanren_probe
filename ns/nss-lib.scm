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
;   (let loop ((frames scoped-env))
;     (cond
;       ((null? frames) sym)
;       ((assoc sym (car frames)) => cdr)
;       (else (loop (cdr frames)))))
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
            (loop (cdr forms)
                (append (reverse (process-form (car forms))) results))))))

(define (handle-define-lambda form)
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
(cond
  ;; (ns-set 'separator "...")
  ((and (pair? form)
        (eq? (car form) 'ns-set))
   (handle-ns-set form))

  ;; (ns "name" ...)
  ((and (pair? form)
        (eq? (car form) 'ns))
   (handle-ns (cadr form) (cddr form) #f))

  ;; (ns-inline "name" ...)
  ((and (pair? form)
        (eq? (car form) 'ns-inline))
   (handle-ns (cadr form) (cddr form) #t))

  ;; (define (fn args ...) ...)
  ((and (pair? form)
        (eq? (car form) 'define)
        (pair? (cadr form))
        (symbol? (car (cadr form))))
   (handle-define-lambda form))

  ;; (define var val)
  ((and (pair? form)
        (eq? (car form) 'define)
        (symbol? (cadr form)))
   (handle-define-var form))

  ;; Other forms passed through unchanged
  (else (list form))))