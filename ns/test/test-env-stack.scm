;; Simulates the interaction of namespace-stack and scoped-env

(define namespace-stack '())
(define scoped-env '())

(define (print-state label)
  (display "\n--- ") (display label) (display " ---\n")
  (display "namespace-stack = ") (write (reverse namespace-stack)) (newline)
  (display "scoped-env = ") (write scoped-env) (newline))

(define (push-scope!)
  (let ((inherited (if (null? scoped-env) '() (car scoped-env))))
    (set! scoped-env (cons (append inherited '()) scoped-env))))

(define (pop-scope!)
  (set! scoped-env (cdr scoped-env)))

(define (define-symbol! name mangled)
  (let ((top (car scoped-env)))
    (set-car! scoped-env (cons (cons name mangled) top))))

(define (mangle sym)
  (string->symbol
   (if (null? namespace-stack)
       (symbol->string sym)
       (string-append
        (apply string-append
               (map (lambda (s) (string-append s "__"))
                    (reverse namespace-stack)))
        (symbol->string sym)))))

(define (lookup-symbol sym)
  (let loop ((frames scoped-env))
    (cond
      ((null? frames) sym)
      ((assoc sym (car frames)) => cdr)
      (else (loop (cdr frames))))))

;; Begin test
(print-state "Initial state")

;; (ns "outer" ...)
(set! namespace-stack (cons "outer" namespace-stack))
(push-scope!)
(print-state "Enter ns 'outer'")

;; (define a 1)
(define-symbol! 'a (mangle 'a))
(print-state "Define 'a' in 'outer'")

;; (ns-inline "inner" ...)
(set! namespace-stack (cons "inner" namespace-stack))
(push-scope!)
(print-state "Enter ns-inline 'inner'")

;; (define b 2)
(define-symbol! 'b (mangle 'b))
(print-state "Define 'b' in 'inner'")

;; (+ a b) → rewrite manually
(let ((a* (lookup-symbol 'a))
      (b* (lookup-symbol 'b)))
  (display "\nRewritten expression (+ a b): ")
  (write (list '+ a* b*))
  (newline))

;; Exit ns-inline
(pop-scope!)
(set! namespace-stack (cdr namespace-stack))
(print-state "Exit ns-inline 'inner'")

;; Exit ns
(pop-scope!)
(set! namespace-stack (cdr namespace-stack))
(print-state "Exit ns 'outer'")
