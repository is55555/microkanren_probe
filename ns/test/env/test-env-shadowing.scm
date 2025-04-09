;; test-env-shadowing.scm --- Environment shadowing test

(load "test/harness/test-harness.scm")
(load "nss-env-nsstack.scm")

(display "=== environment shadowing test ===\n")

(push-namespace! "demo")
(push-scope!)
(define-symbol! 'x 'demo__x)

(push-scope!)
(define-symbol! 'x 'x)
(run-test "lookup in inner scope" 'x (lookup-symbol 'x))
(pop-scope!)

(run-test "lookup in outer scope" 'demo__x (lookup-symbol 'x))
(pop-scope!)
(pop-namespace!)

(test-summary)
