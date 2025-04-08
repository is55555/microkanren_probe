(load "nss-util.scm")

(display "=== nss-util tests ===\n")

(define tests
  `(
    ;; (lambda args ...)
    ((args) .
     (lambda-formals->bound-vars 'args))

    ;; (lambda (x y))
    ((x y) .
     (lambda-formals->bound-vars '(x y)))

    ;; (lambda (x y . rest))
    ((x y rest) .
     (lambda-formals->bound-vars '(x y . rest)))

    ;; (lambda ())
    (() .
     (lambda-formals->bound-vars '()))

    ;; malformed input (uncomment to test manually)
    ;; (error . (lambda-formals->bound-vars '(x . #t)))
))

(for-each
 (lambda (test)
   (let ((expected (car test))
         (actual   (eval (cdr test))))
     (display "\nExpected: ") (write expected)
     (display "\nActual:   ") (write actual) (newline)
     (if (equal? expected actual)
         (display "✅ PASS\n")
         (display "❌ FAIL\n"))))
 tests)
