;; nss-lib.scm - Namespace Preprocessor Lib for Scheme 
(load "nss-env-nsstack.scm")

(define inline-mode? #f)

;;; Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alias-definition outer-sym inner-sym)
  `(define ,outer-sym ,inner-sym))

;; List of forms we consider special and should preserve in head position
(define special-forms
    '(if lambda define set! begin cond let let* letrec letrec* 
        quote quasiquote unquote unquote-splicing 
        ns ns-inline ns-set))
  
    (define special-forms
    '(quote lambda define set! begin cond and or case
      let let* letrec letrec* do delay
      quasiquote unquote unquote-splicing
      ns ns-inline ns-set))
  
  (define (rewrite expr)
    (cond
      ;; plain symbol — rewrite using current scope
      ((symbol? expr)
       (lookup-symbol expr))
  
      ;; lambda — bind args, rewrite body
      ((and (pair? expr)
            (eq? (car expr) 'lambda)
            (pair? (cdr expr))
            (pair? (cadr expr)))
       (let ((params (cadr expr))
             (body (cddr expr)))
         (push-scope!)
         (for-each (lambda (p) (define-symbol! p p)) params)
         (let ((new-body (map rewrite body)))
           (pop-scope!)
           `(lambda ,params ,@new-body))))
  
      ;; let / let* / letrec / letrec* 
;; Rewrite binding forms: let, let*, letrec, letrec*
((and (pair? expr) (pair? (cdr expr)))
(let ((form (car expr)))
  (cond

    ;; ---- let ----
    ((eq? form 'let)
     (let ((bindings (cadr expr))
           (body (cddr expr)))
       (push-scope!)
       ;; rewrite RHS values
       (let ((rewritten-bindings
              (map (lambda (bind)
                     (display "[rewrite-let] binding = ") (write bind) (newline)
                     (let ((name (car bind))
                           (rhs  (cadr bind)))
                       (list name (rewrite rhs))))
                   bindings)))
         ;; bind names (after RHS eval)
         (for-each (lambda (bind)
                     (define-symbol! (car bind) (car bind)))
                   bindings)
         (let ((rewritten-body (map rewrite body)))
           (pop-scope!)
           `(let ,rewritten-bindings ,@rewritten-body)))))

    ;; ---- let* ----
    ((eq? form 'let*)
     (let ((bindings (cadr expr))
           (body (cddr expr)))
       (push-scope!)
       (let ((rewritten-bindings
              (map (lambda (bind)
                     (display "[rewrite-let*] binding = ") (write bind) (newline)
                     (let ((name (car bind))
                           (rhs  (cadr bind)))
                       (let ((rval (rewrite rhs)))
                         (define-symbol! name name)
                         (list name rval))))
                   bindings)))
         (let ((rewritten-body (map rewrite body)))
           (pop-scope!)
           `(let* ,rewritten-bindings ,@rewritten-body)))))

    ;; ---- letrec / letrec* ----
    ((or (eq? form 'letrec) (eq? form 'letrec*))
     (let ((bindings (cadr expr))
           (body (cddr expr)))
       (push-scope!)
       ;; bind names first
       (for-each (lambda (bind)
                   (define-symbol! (car bind) (car bind)))
                 bindings)
       ;; rewrite RHS after all names are in scope
       (let ((rewritten-bindings
              (map (lambda (bind)
                     (display "[rewrite-letrec] binding = ") (write bind) (newline)
                     (let ((name (car bind))
                           (rhs  (cadr bind)))
                       (list name (rewrite rhs))))
                   bindings))
             (rewritten-body (map rewrite body)))
         (pop-scope!)
         `(,form ,rewritten-bindings ,@rewritten-body))))

    ;; ---- fallback ----
    (else
     (let ((head form)
           (args (cdr expr)))
       (if (symbol? head)
           (if (memq head special-forms)
               (cons head (map rewrite args))
               (cons (rewrite head) (rewrite args)))
           (cons (rewrite head) (rewrite args))))))))

          

  
      ;; other known special forms — preserve head, rewrite args
      ((and (pair? expr)
            (symbol? (car expr))
            (memq (car expr) special-forms))
       (cons (car expr) (map rewrite (cdr expr))))
  
      ;; general application — rewrite whole list
      ((pair? expr)
       (cons (rewrite (car expr)) (rewrite (cdr expr))))
  
      ;; vectors
      ((vector? expr)
       (list->vector (map rewrite (vector->list expr))))
  
      ;; constants
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
