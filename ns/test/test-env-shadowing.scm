(load "nss-env-nsstack.scm")

(display "=== environment shadowing test ===\n")

(push-namespace! "demo")
(define-symbol! 'x 'demo__x)

(push-scope!)
(define-symbol! 'x 'x)
(display "Lookup inside scope (should be x): ")
(write (lookup-symbol 'x)) (newline)
(pop-scope!)

(display "Lookup after scope (should be demo__x): ")
(write (lookup-symbol 'x)) (newline)

(pop-namespace!)
