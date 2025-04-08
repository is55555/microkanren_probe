;; test-rewrite2.scm — letrec / mutual recursion tests

(load "nss-env-nsstack.scm")
(load "nss-lib.scm")

(display "=== letrec / mutual recursion tests ===\n")

;; Setup: enter dummy namespace and scope
(push-namespace! "demo")
(push-scope!)

;; Bind symbols for testing
(define-symbol! 'f 'demo__f)
(define-symbol! 'g 'demo__g)
(define-symbol! 'n 'demo__n)
(define-symbol! 'zero? 'demo__zero?)
(define-symbol! '- 'demo__-)
(define-symbol! '* 'demo__*)
(define-symbol! 'demo 'demo__demo)

(define tests
  `(
    ;; letrec: mutual recursion with nested calls
    ((letrec ((f (lambda (n)
                   (if (zero? n)
                       1
                       (* n (g (- n 1))))))
              (g (lambda (n)
                   (if (zero? n)
                       1
                       (* n (f (- n 1)))))))
       (f 5))
     .
     (letrec ((f (lambda (n)
                   (if (demo__zero? n)
                       1
                       (demo__* n (g (demo__- n 1))))))
              (g (lambda (n)
                   (if (demo__zero? n)
                       1
                       (demo__* n (f (demo__- n 1)))))))
       (f 5)))

    ;; letrec*: sequential binding (no mangling needed)
    ((letrec* ((f (lambda (n) (g n)))
               (g (lambda (n) (+ n 1))))
       (f 4))
     .
     (letrec* ((f (lambda (n) (g n)))
               (g (lambda (n) (+ n 1))))
       (f 4)))

    ;; Mixed scope: free variable (demo) should be rewritten
    ((letrec ((f (lambda (n) (g n)))
              (g (lambda (n) (+ n 1))))
       (f demo))
     .
     (letrec ((f (lambda (n) (g n)))
              (g (lambda (n) (+ n 1))))
       (f demo__demo)))
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
