(load "nss-env-nsstack.scm")


(define (print-state label)
    (display "\n--- ") (display label) (display " ---\n")
    (display "namespace-stack = ") (write (reverse namespace-stack)) (newline)
    (display "scoped-env = ") (write scoped-env) (newline))

(print-state "Initial state")

(push-namespace! "outer")
(push-scope!)
(print-state "Enter ns 'outer'")

(define-symbol! 'a (mangle 'a))
(print-state "Define 'a' in 'outer'")

(push-namespace! "inner")
(push-scope!)
(print-state "Enter ns-inline 'inner'")

(define-symbol! 'b (mangle 'b))
(print-state "Define 'b' in 'inner'")

(let ((a* (lookup-symbol 'a))
      (b* (lookup-symbol 'b)))
  (display "\nRewritten expression (+ a b): ")
  (write (list '+ a* b*)) (newline))

(pop-scope!)
(pop-namespace!)
(print-state "Exit ns-inline 'inner'")

(pop-scope!)
(pop-namespace!)
(print-state "Exit ns 'outer'")
