;; test-rewrite2.scm — letrec / mutual recursion tests

(load "nss-env-nsstack.scm")
(load "nss-lib.scm")

(display "=== letrec / mutual recursion tests ===\n")

;; Setup: enter dummy namespace and scope
(push-namespace! "demo")
(push-scope!)

;; Bind symbols for testing
(define-symbol! 'x 'demo__x)
(define-symbol! 'y 'demo__y)
(define-symbol! 'z 'demo__z)

(define-symbol! 'f 'demo__f)
(define-symbol! 'g 'demo__g)
(define-symbol! 'n 'demo__n)
(define-symbol! 'zero? 'demo__zero?)

(define-symbol! '+ 'demo__+)
(define-symbol! '- 'demo__-)
(define-symbol! '* 'demo__*)
(define-symbol! 'demo 'demo__demo)

(define tests
  `(

    ;; let should not allow RHS to see siblings
    (
      (let ((x 1)
        (y x))        ;; should not see x here
        (+ x y))
    .
      (let ((x 1)
        (y demo__x))  ;; rewritten because outer x isn't visible
        (demo__+ x y))
    )

    ;; let* allows RHS to see earlier bindings
    (
      (let* ((x 1)
        (y x))
        (+ x y))
    .
      (let* ((x 1)
        (y x))
        (demo__+ x y))
    )

    ;; let shadowing outer symbol
    (
      (let ((x 5))
        (let ((x 10))
          (+ x x)))
    .
      (let ((x 5))
        (let ((x 10))
          (demo__+ x x)))
    )

    ;; let* with nested shadowing
    (
      (let* ((x 1)
        (x (+ x 1)))
        x)
    .
      (let* ((x 1)
        (x (demo__+ x 1)))
        x)
    )

    ;; let with free variable in body only
    (
      (let ((x 1)) (+ y x))
    .
      (let ((x 1)) (demo__+ demo__y x))
    )     
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
