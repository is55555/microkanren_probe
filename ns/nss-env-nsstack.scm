;; nss-env-nsstack.scm - Manages environment and namespace stacks

(define separator "__")
(define namespace-stack '())
(define scoped-env '())

;;; Namespace Stack ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (push-namespace! nsname)
  (when (or (null? nsname)
            (and (string? nsname) (string=? nsname ""))
            (and (symbol? nsname) (string=? (symbol->string nsname) "")))
    (error 'ns "Namespace name cannot be empty"))
  (set! namespace-stack (cons nsname namespace-stack)))

(define (pop-namespace!)
  (set! namespace-stack (cdr namespace-stack)))

(define (current-namespace)
  (string-join (reverse namespace-stack) separator))

(define (parent-namespace)
  (if (> (length namespace-stack) 1)
      (string-join (reverse (cdr namespace-stack)) separator)
      ""))

;;; Scoped Environment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (push-scope!)
  (let ((inherited (if (null? scoped-env) '() (car scoped-env))))
    (set! scoped-env (cons (append inherited '()) scoped-env))))

(define (pop-scope!)
  (set! scoped-env (cdr scoped-env)))

(define (define-symbol! name mangled)
  (let ((top (car scoped-env)))
    (set-car! scoped-env (cons (cons name mangled) top))))

(define (lookup-symbol sym)
  (let loop ((frames scoped-env))
    (cond
      ((null? frames) sym)
      ((assoc sym (car frames)) => cdr)
      (else (loop (cdr frames))))))

;;; Utility ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-join lst sep)
  (if (null? lst)
      ""
      (let loop ((items (cdr lst)) (acc (car lst)))
        (if (null? items)
            acc
            (loop (cdr items) (string-append acc sep (car items)))))))

(define (mangle sym)
  (string->symbol
   (if (null? namespace-stack)
       (symbol->string sym)
       (string-append (current-namespace) separator (symbol->string sym)))))
