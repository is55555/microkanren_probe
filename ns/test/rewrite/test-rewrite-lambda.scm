;; test/test-rewrite-lambda.scm

(load "nss-env-nsstack.scm")
(load "nss-lib.scm")

(display "=== lambda binding form tests ===\n")

;; Setup: enter dummy namespace and scope
(push-namespace! "demo")
(push-scope!)

;; Bind only the free identifier `free`, so it gets rewritten
(define-symbol! 'free 'demo__free)

(define tests
  `(
    ;; fixed arity: bound x and y must not be rewritten
    ((lambda (x y)
       (free x y))
     .
     (lambda (x y)
       (demo__free x y)))

    ;; variadic: single argument bound
    ((lambda args
       (free args))
     .
     (lambda args
       (demo__free args)))

    ;; dotted list: x, y, rest are all bound
    ((lambda (x y . rest)
       (free x y rest))
     .
     (lambda (x y . rest)
       (demo__free x y rest)))

    ;; nested lambda: inner y is bound too
    ((lambda (x)
       (lambda (y)
         (free x y)))
     .
     (lambda (x)
       (lambda (y)
         (demo__free x y))))

    ;; lambda + internal define: define y is bound
    ((lambda (x)
       (define y 42)
       (free x y))
     .
     (lambda (x)
       (define y 42)
       (demo__free x y)))
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
