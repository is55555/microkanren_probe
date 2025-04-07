;; test/test-env-stack.scm --- Environment stack tests

(load "test/test-harness.scm")
(load "nss-env-nsstack.scm")

(display "=== Environment stack tests ===\n")

;; Reset state
(set! namespace-stack '())
(set! scoped-env '())

(run-test "Initial namespace stack" '() namespace-stack)
(run-test "Initial scoped env" '() scoped-env)

(push-namespace! "outer")
(push-scope!)
(run-test "Enter ns 'outer'" '("outer") namespace-stack)
(run-test "Scoped env after ns" '(() ) scoped-env)

(define-symbol! 'a 'outer__a)
(run-test "Define 'a' in 'outer'" '(((a . outer__a))) scoped-env)

(push-namespace! "inner")
(push-scope!)
(run-test "Enter ns-inline 'inner' stack" '("inner" "outer") namespace-stack)
(run-test "Enter ns-inline 'inner' scoped-env" '(((a . outer__a)) ((a . outer__a))) scoped-env)

(define-symbol! 'b 'outer__inner__b)
(run-test "Define 'b' in 'inner'" '(((b . outer__inner__b) (a . outer__a)) ((a . outer__a))) scoped-env)

(run-test "Lookup 'a' in inner" 'outer__a (lookup-symbol 'a))
(run-test "Lookup 'b' in inner" 'outer__inner__b (lookup-symbol 'b))

(pop-scope!)
(pop-namespace!)
(run-test "Pop 'inner' namespace" '("outer") namespace-stack)
(run-test "Scoped env after pop" '(((a . outer__a))) scoped-env)

(pop-scope!)
(pop-namespace!)
(run-test "Final empty namespace" '() namespace-stack)
(run-test "Final empty env" '() scoped-env)

(test-summary)
