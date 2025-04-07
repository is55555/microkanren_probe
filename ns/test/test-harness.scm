;; test-harness.scm --- Basic test framework with summary counts

(define test-total 0)
(define test-passed 0)
(define test-results '())

(define (run-test desc expected actual)
  (set! test-total (+ test-total 1))
  (let ((passed? (equal? expected actual)))
    (if passed?
        (set! test-passed (+ test-passed 1)))
    (set! test-results
          (cons (list desc passed? expected actual) test-results))
    (display "[" ) (display desc) (if passed? (display "] ✅ PASS\n") (display "] ❌ FAIL\n"))
    (if (not passed?)
        (begin
          (display "  Expected: ") (write expected) (newline)
          (display "  Actual:   ") (write actual) (newline)))))

(define (test-summary)
  (let ((failed (- test-total test-passed)))
    (newline)
    (display test-total) (display " total, ")
    (display test-passed) (display " passed, ")
    (display failed) (display " failed\n")))
