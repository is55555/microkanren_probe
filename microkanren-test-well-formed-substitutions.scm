(load "microkanren.scm")
(load "aux-well-formed.scm")
(load "debug.scm")

;; Test helper: Compare actual result with expected and print pass/fail
(define (test name expr expected)
  (let ((result expr))
    (begin (display "Running test: ") (display name) (newline)
    (display "Result: ") (display result) (newline))
    (if (equal? result expected)
        (begin (write (list "PASS" name)) (newline))
        (begin (write (list "FAIL" name "Expected:" expected "Got:" result)) (newline)))))


;; Valid cases
(test "Empty substitution is well-formed"
  (well-formed? '())
  #t)

(test "Single variable binding is well-formed"
  (well-formed? '(((var . x) . 42)))
  #t)

(test "Multiple valid bindings are well-formed"
  (well-formed? '(((var . x) . 1) ((var . y) . 2) ((var . z) . 3)))
  #t)

(test "Valid nested substitution"
  (well-formed? '(((var . x) . ((var . y) (var . z))) ((var . y) . 1) ((var . z) . 2)))
  #t)

;; Invalid cases
(test "Invalid: Right-hand side contains non-variable pair"
  (well-formed? '(((var . x) . (1 2)) ((var . y) . 3)))
  #f)

(test "Invalid: Right-hand side contains non-variable nested structure"
  (well-formed? '(((var . x) . ((1 2) (var . y))) ((var . y) . 3)))
  #f)

(test "Invalid: Non-variable left-hand side"
  (well-formed? '((42 . (var . y))))
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
  (well-formed? '(((var . x) . 1) . ((var . y) . 2)))
  #f)

(test "Invalid: Extra nesting in pair"
  (well-formed? '(((var . x) . ((var . y) . 1))))
  #f)

(test "Invalid: Empty list as a binding"
  (well-formed? '(((var . x) . ()))) 
  #f)

;; Check lite-well-formed? behavior
(test "lite-well-formed? ignores circular references"
  (lite-well-formed? '(((var . x) . (var . y)) ((var . y) . (var . x))))
  #t)
;(test "lite-well-formed? ignores circular references"
;  (debug-well-formed? '(((var . x) . (var . y))  ;; x → y
;                         ((var . y) . (var . z))  ;; y → z
;                         ((var . z) . (var . x))))  ;; z → x (cycle)
;  #t)  ;; Expected to pass because it *ignores* circular refs

(display "All well-formed substitution tests complete.") (newline)
