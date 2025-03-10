(import (scheme base) (scheme write))

;; Load microKanren implementation
(load "microkanren.scm")

;; Define a test function
(define (test name expr expected)
  (let ((result expr))
    (if (equal? result expected)
        (begin (write (list "PASS" name)) (newline))
        (begin (write (list "FAIL" name "Expected:" expected "Got:" result)) (newline)))))

;; 1️⃣ Variable Identification
(display "Testing var? and var=?...") (newline)
(display (list 'var-check (var? '(var . x)) (var? 'x) (var=? '(var . x) '(var . x)) (var=? '(var . x) '(var . y)))) (newline)

;; 2️⃣ Basic Unification: `(var . x) == 42`
(test "Unify single variable" (run 1 (== '(var . x) 42)) '((((var . x) . 42))))

;; 3️⃣ Chained Unification
(test "Unify two variables"
      (run 1 (all (== '(var . x) 42) (== '(var . y) '(var . x))))
      '((((var . x) . 42) ((var . y) . 42))))

;; 4️⃣ Logical OR (conde)
(test "Logical OR (conde)"
      (run 2 (conde (== '(var . x) 1) (== '(var . x) 2)))
      '((((var . x) . 1)) (((var . x) . 2))))

;; 5️⃣ Unification with Lists
(test "Unify with lists"
      (run 1 (== '(var . x) '(1 2 3)))
      '((((var . x) (1 2 3)))))

;; 6️⃣ Nested Unification
(test "Nested unification"
      (run 1 (all (== '(var . x) '(1 (var . y))) (== '(var . y) 2)))
      '((((var . x) (1 (var . y))) ((var . y) . 2))))

;; 7️⃣ Circular Unification
(test "Circular unification"
      (run 1 (== '(var . x) '(var . x)))
      '((((var . x) var . x))))

;; 8️⃣ Complex conde + all Combination
(test "conde and all together"
      (run 2 (conde (all (== '(var . x) 1) (== '(var . y) 2))
                    (all (== '(var . x) 3) (== '(var . y) 4))))
      '((((var . x) . 1) ((var . y) . 2))
        (((var . x) . 3) ((var . y) . 4))))

;; 9️⃣ Unification Failure Cases
(test "Unification failure" (run 1 (== 1 2)) '())

(display "All tests complete.") (newline)
