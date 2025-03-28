(load "nss-env-nsstack.scm")
(load "nss-lib.scm")

(display "=== rewrite tests ===\n")

;; Setup: enter dummy namespace and scope
(push-namespace! "demo")
(push-scope!)

;; Bind symbols for testing
(define-symbol! 'x 'demo__x)
(define-symbol! 'y 'demo__y)
(define-symbol! 'f 'demo__f)

(define-symbol! 'add1 'demo__add1)
(define-symbol! 'my-if 'demo__my_if) ;; for testing non-special use

(define tests
  `(
    ;; special form, only arguments are rewritten
    ((if x y x) . (if demo__x demo__y demo__x))

    ;; binding forms
    ((let ((x 1)) (+ x 2)) . (let ((x 1)) (+ x 2)))
    ((let ((x 1) (y x)) (+ x y)) . (let ((x 1) (y x)) (+ x y)))
    ((let ((x 1)) (+ y x)) . (let ((x 1)) (+ demo__y x)))


    ;; Top-level let binds x, so it should not be rewritten
    ((let ((x 1)) (+ x y)) . (let ((x 1)) (+ x demo__y)))

    ;; Nested let introduces a new binding for y
    ((let ((x 1))
    (let ((y x))
        (+ x y))) . (let ((x 1)) (let ((y x)) (+ x y))))

    ;; Free reference to f should be rewritten
    ((let ((x 2)) (f x)) . (let ((x 2)) (demo__f x)))

    ;; let-shadowing: inner x should not be rewritten
    ((let ((x 1))
    (let ((x 2))
        (+ x x))) . (let ((x 1)) (let ((x 2)) (+ x x))))

    ;; deeper nesting with both x and y shadowed
    ((let ((x 1) (y 2))
    (let ((x y)
            (y x))
        (+ x y))) . (let ((x 1) (y 2)) (let ((x y) (y x)) (+ x y))))



    ;; special form letrec - No symbols are rewritten because f, x are locally bound
    ((letrec ((f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))))
    (f 5))
    . (letrec ((f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))))
    (f 5)))

    ;; lambda args are not rewritten, because they are local parameters
    ((lambda (x) (+ x 1)) . (lambda (x) (+ x 1)))
    ((lambda (x) (+ y x)) . (lambda (x) (+ demo__y x)))
    ((lambda (x) (lambda (y) (+ x y))) . (lambda (x) (lambda (y) (+ x y))))

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
