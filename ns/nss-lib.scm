;; nss-lib.scm - Namespace Preprocessor Lib for Scheme 
(load "nss-env-nsstack.scm")

(define inline-mode? #f)

;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

    (push-namespace! name)
    (push-scope!)
    (let ((was-inline inline-mode?))
    (when inline? (set! inline-mode? #t))
    (let loop ((forms body) (results '()))
        (if (null? forms)
            (begin
            (set! inline-mode? was-inline)
            (pop-scope!)
            (pop-namespace!)
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
