;; Load dependencies
(load "microkanren.scm")
(load "aux-well-formed.scm")

;; Test helper: Compare actual result with expected and print pass/fail
(define (test name expr expected)
  (let ((result expr))
    (display "Running test: ") (display name) (newline)
    (display "Result: ") (display result) (newline)
    (if (equal? result expected)
        (begin (write (list "PASS" name)) (newline))
        (begin (write (list "FAIL" name "Expected:" expected "Got:" result)) (newline)))))

;; ✅ Test valid cases
(test "Empty substitution is well-formed" 
  (well-formed? '()) 
  #t)

(test "Single variable binding is well-formed"
  (well-formed? '(((var . x) . 42))) 
  #t)

(test "Multiple valid bindings are well-formed"
  (well-formed? '(((var . x) . 1) ((var . y) . 2))) 
  #t)

(test "Valid nested substitution"
  (well-formed? '(((var . x) . ((var . y) (var . z)))  ;; x → (y z)
                  ((var . y) . 1)                      ;; y → 1
                  ((var . z) . 2)))                    ;; z → 2
  #t)


(test "Invalid: Right-hand side contains non-variable pair"
  (debug-well-formed? '(((var . x) . ((1 2) (var . y)))  ;; x → ((1 2) y) ❌
                  ((var . y) . 3)))               ;; y → 3
  #f)  ;; Expect failure

(test "Invalid: Right-hand side contains non-variable nested structure"
  (debug-well-formed? '(((var . x) . (1 (var . y))) ((var . y) . 2))) 
  #f)

;; ❌ Test invalid cases
(test "Invalid: Non-variable left-hand side"
  (well-formed? '(((not-a-var . x) . 42))) 
  #f)

(test "Invalid: Duplicate variable binding"
  (well-formed? '(((var . x) . 1) ((var . x) . 2))) 
  #f)

(test "Invalid: Variable bound to itself"
  (well-formed? '(((var . x) . (var . x)))) 
  #f)

(test "Invalid: Circular reference"
  (well-formed? '(((var . x) . (var . y)) ((var . y) . (var . x)))) 
  #f)

(test "Invalid: Improper pair structure (not a list)"
  (debug-well-formed? '(((var . x) 42))) 
  #f)

(test "Invalid: Extra nesting in pair"
  (well-formed? '((((var . x) . 42)))) 
  #f)

(test "Invalid: Empty list as a binding"
  (well-formed? '(())) 
  #f)

;; ✅ Test `lite-well-formed?`
(test "lite-well-formed? ignores circular references"
  (lite-well-formed? '(((var . x) . (var . y)) ((var . y) . (var . x)))) 
  #t)

(display "All well-formed substitution tests complete.") (newline)
