;; nss-lib.scm --- Core namespace preprocessor logic

(load "nss-env-nsstack.scm")
(load "nss-util.scm")

(define inline-mode? #f)

(define (alias-definition outer-sym inner-sym)
  `(define ,outer-sym ,inner-sym))

(define special-forms
  '(quote lambda define set! begin cond and or case
    let let* letrec letrec* do delay
    quasiquote unquote unquote-splicing
    ns ns-inline ns-set))

(define (rewrite expr)
(cond
  ((symbol? expr)
   (lookup-symbol expr))
  ((pair? expr)
   (safe-map rewrite expr))  ; <-- NEW safe map
  (else expr)))

(define (handle-begin expr)
  `(begin ,@(map process-form (cdr expr))))

(define (handle-define expr)
  (let ((name (cadr expr))
        (value (caddr expr)))
    (if (pair? name)
        ;; (define (f x) body)
        (let ((fname (car name))
              (formals (cdr name))
              (body (cdddr expr)))
          (define-symbol! fname (mangle-symbol fname))
          `(define (,fname ,@formals)
             ,@(map rewrite body)))
        ;; (define x val)
        (begin
          (define-symbol! name (mangle-symbol name))
          `(define ,name ,(rewrite value))))))

(define (handle-lambda expr)
  (let ((formals (cadr expr))
        (body (cddr expr)))
    (push-scope!)
    (for-each (lambda (param)
                (define-symbol! param param))
              (lambda-formals->bound-vars formals))
    (let ((new-body (map rewrite body)))
      (pop-scope!)
      `(lambda ,formals ,@new-body))))

(define (handle-let expr)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (push-scope!)
    (let ((rewritten-bindings
           (map (lambda (bind)
                  (let ((name (car bind))
                        (rhs  (cadr bind)))
                    (list name (rewrite rhs))))
                bindings)))
      (for-each (lambda (bi)
            (define-symbol! (car bi)
              (or (lookup-symbol (car bi)) (mangle-symbol (car bi)))))
            bindings)
      (let ((rewritten-body (map rewrite body)))
        (pop-scope!)
        `(let ,rewritten-bindings ,@rewritten-body)))))

(define (handle-let* expr)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (push-scope!)
    (let ((rewritten-bindings
           (map (lambda (bind)
                  (let ((name (car bind))
                        (rhs  (cadr bind)))
                    (define-symbol! name (or (lookup-symbol name) (mangle-symbol name)))
                    (list name (rewrite rhs))))
                bindings)))
      (let ((rewritten-body (map rewrite body)))
        (pop-scope!)
        `(let* ,rewritten-bindings ,@rewritten-body)))))

(define (handle-letrec expr)
  (let ((bindings (cadr expr))
        (body (cddr expr)))
    (push-scope!)
    (for-each (lambda (bi)
                (define-symbol! (car bi)
                  (or (lookup-symbol (car bi)) (mangle-symbol (car bi))))
              )
              bindings)
    (let ((rewritten-bindings
           (map (lambda (bind)
                  (let ((name (car bind))
                        (rhs  (cadr bind)))
                    (list name (rewrite rhs))))
                bindings))
          (rewritten-body (map rewrite body)))
      (pop-scope!)
      `(letrec ,rewritten-bindings ,@rewritten-body))))

(define (process-form expr)
  (cond
    ((not (pair? expr)) expr)
    ((eq? (car expr) 'begin) (handle-begin expr))
    ((eq? (car expr) 'define) (handle-define expr))
    ((eq? (car expr) 'lambda) (handle-lambda expr))
    ((eq? (car expr) 'let) (handle-let expr))
    ((eq? (car expr) 'let*) (handle-let* expr))
    ((eq? (car expr) 'letrec) (handle-letrec expr))
    ((eq? (car expr) 'letrec*) (handle-letrec expr)) ; ⚠️ Differentiation between handling letrec and letrec* would only matter if we rewrote or interpreted evaluation order or semantics.

    (else (rewrite expr))))
