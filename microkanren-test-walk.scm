(import (scheme base) (scheme write))

;; Load microKanren implementation
(load "microkanren.scm")

;; Helper: Structured test function
(define (test-walk name var s expected)
  (let ((result (walk var s)))
    (display "Test: ") (display name) (newline)
    (display "  Input: (walk ") (display var) (display " ") (display s) (display ")") (newline)
    (display "  Expected: ") (display expected) (newline)
    (display "  Got:      ") (display result) (newline)
    (if (equal? result expected)
        (begin (display "✅ PASS") (newline))
        (begin (display "❌ FAIL") (newline)))
    (newline)))

;; ✅ 1. Simple Lookup
(define s1 '(((var . x) . 42)))  ;; x → 42
(test-walk "Simple lookup" '(var . x) s1 42)

;; ✅ 2. Chain of Substitutions
(define s2 '(((var . x) . (var . y))   ;; x → y
             ((var . y) . 42)))        ;; y → 42
(test-walk "Chained substitution x" '(var . x) s2 42)
(test-walk "Chained substitution y" '(var . y) s2 42)

;; ✅ 3. Variable Not in Substitutions
(define s3 '(((var . x) . 42)))  ;; x → 42
(test-walk "Unbound variable" '(var . z) s3 '(var . z)) ;; z isn't in s3, should return itself

;; ✅ 4. Multiple Independent Substitutions
(define s4 '(((var . x) . 1)  
             ((var . y) . 2)))
(test-walk "Independent substitution x" '(var . x) s4 1)
(test-walk "Independent substitution y" '(var . y) s4 2)

;; ✅ 5. Circular Reference (Potential Loop)
(define s5 '(((var . x) . (var . y))  
             ((var . y) . (var . x)))) ;; Circular reference
(test-walk "Circular reference" '(var . x) s5 '(var . x)) ;; Ensure no infinite loop

(display "✅ Walk test suite complete.") (newline)
