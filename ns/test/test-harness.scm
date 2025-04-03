;; test-harness.scm --- Basic test framework with summary counts

(define test-total 0)
(define test-passed 0)

(define (run-test desc expected actual)
  (set! test-total (+ test-total 1))
  (if (equal? expected actual)
      (begin
        (set! test-passed (+ test-passed 1))
        (display "[" ) (display desc) (display "] ✅ PASS\n"))
      (begin
        (display "[" ) (display desc) (display "] ❌ FAIL\n")
        (display "  Expected: ") (write expected) (newline)
        (display "  Actual:   ") (write actual) (newline))))

(define (test-summary)
  (let ((failed (- test-total test-passed)))
    (newline)
    (display test-total) (display " total, ")
    (display test-passed) (display " passed, ")
    (display failed) (display " failed\n")))
