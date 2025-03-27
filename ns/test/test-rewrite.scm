(load "../nss-env-nsstack.scm")
(load "../nss-lib.scm")

(display "=== rewrite tests ===\n")

;; Setup: enter dummy namespace and scope
(push-namespace! "demo")
(push-scope!)

;; Bind symbols for testing
(define-symbol! 'x 'demo__x)
(define-symbol! 'y 'demo__y)
(define-symbol! 'add1 'demo__add1)
(define-symbol! 'my-if 'demo__my_if) ;; for testing non-special use

(define tests
  `(
    ;; special form, only arguments are rewritten
    ((if x y x) . (if demo__x demo__y demo__x))

    ;; lambda body is rewritten
    ((lambda (x) (+ x 1)) . (lambda (x) (+ demo__x 1)))

    ;; application: rewrite whole list
    ((add1 x) . (demo__add1 demo__x))

    ;; unknown form: rewritten normally
    ((my-if x y) . (demo__my_if demo__x demo__y))

    ;; unqualified symbol stays as-is if not defined
    ((unknown x) . (unknown demo__x))
  ))

(for-each
 (lambda (test)
   (let* ((input (car test))
          (expected (cdr test))
          (actual (rewrite input)))
     (display "\nInput:     ") (write input)
     (display "\nExpected:  ") (write expected)
     (display "\nRewritten: ") (write actual)
     (newline)
     (if (equal? expected actual)
         (display "✅ PASS\n")
         (display "❌ FAIL\n"))))
 tests)

(pop-scope!)
(pop-namespace!)
