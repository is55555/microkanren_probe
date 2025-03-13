(load "mukanren_ht.scm")  ;; Load the implementation

;; --- Test Suite Management ---
(define pass-count 0)
(define fail-count 0)

(define (run-test name expr expected)
  (let ((result expr))
    (display "Running test: ") (display name) (newline)
    (display "Result: ") (display result) (newline)
    (if (equal? result expected)
        (begin
          (set! pass-count (+ pass-count 1))
          (display "PASS\n"))
        (begin
          (set! fail-count (+ fail-count 1))
          (display "FAIL - Expected: ") (display expected)
          (display " Got: ") (display result) (newline)))))

;; --- Start of Tests ---


;#|

(define s-tri (create-empty-state))
(define x-tri (new-var))
(define y-tri (new-var))

(run-test "Unify x → y (triangularity check)"
  (unify x-tri y-tri s-tri)
  s-tri)

(run-test "Check substitution x → y (triangularity enforced)"
  (walk y-tri s-tri)
  x-tri) ;; Expected: y should resolve to x


;; **Basic Unification Tests**
(define s1 (create-empty-state))
(define x (new-var))
(define y (new-var))
(define z (new-var))

(run-test "Unify x → 42"
  (unify x 42 s1)
  s1)  ;; Expect success

(run-test "Walk x (after x → 42)"
  (walk x s1)
  42)

(run-test "Unify x → y"
  (unify x y s1)
  s1)  ;; Expect success

(run-test "Walk x (after x → y)"
  (walk x s1)
  (walk y s1))  ;; Should be the same

(run-test "Unify y → 99"
  (unify y 99 s1)
  #f)  ;; y already unified to x thus 42

(run-test "Unify z → x"
  (unify z x s1)
  s1)  ;; y already unified to x thus 42

 (substitution-hash-printout s1)

(run-test "Walk x (after transitive z → x → y → 42)"
  (walk x s1)
  42)

(define s2 (create-empty-state))
(define a (new-var))
(define b (new-var))

(run-test "Unify a → 3"
  (unify a 3 s2)
  s2)


(run-test "Fail unifying a → (1 2)"
  (unify a '(1 2) s2)
  #f)  ;; Expect failure

(run-test "Unify a → b"
  (unify a b s2)
  s2)

(run-test "unifying a → 3 when already a → 3 because a → b and b → 3"
  (unify a 3 s2)
  s2)  

;; **Transitive Unification**
(define s3 (create-empty-state))

(run-test "Unify x → y over s3"
  (unify x y s3)
  s3)

(run-test "Unify y → z"
  (unify y z s3)
  s3)

(run-test "Walk x (should resolve to z)"
  (walk x s3)
  (walk z s3))

(run-test "Walk y (should resolve to z)"
  (walk y s3)
  (walk z s3))

;; **List Unifications**
(define s4 (create-empty-state))
(define p (new-var))
(define q (new-var))

(run-test "Unify p → (q 2)"
  (unify p (list q 2) s4)
  s4)

(substitution-hash-printout s4)

(run-test "Walk p should give (q 2)"
  (walk p s4)
  (list q 2))

(run-test "Fail unifying p → (3 4) when already p → (q 2)"
  (unify p '(3 4) s4)
  #f)  ;; Should fail, conflicting with (q 2)

(run-test "Unify q → 1 (affects p)"
  (unify q 1 s4)
  s4)

(run-test "Walk p should resolve to (1 2)"
  (walk p s4)
  '(1 2))

;; **Triangularity Enforcement**
(define s5 (create-empty-state))
(define v1 (new-var))
(define v2 (new-var))

(run-test "Valid binding: v1 → 10"
  (ext-s v1 10 s5)
  s5)

(run-test "Valid binding: v2 → v1 (earlier variable)"
  (ext-s v2 v1 s5)
  s5)

(run-test "Invalid binding: v1 → v2 (violates triangularity)"
  (ext-s v1 v2 s5)
  #f)  ;; Should fail

;|#

;; **Deep Nested Test with Multiple Dependencies**
(define s9 (create-empty-state))
(define p (new-var))
(define q (new-var))
(define r (new-var))
(define s (new-var))
(define u (new-var))
(define v (new-var))
(define k (new-var))

(run-test "Unify p → (q (r (s 5 u v) k 8 r))"
  (unify p (list q (list r (list s 5 u v) k 8 r)) s9)
  s9)

(substitution-hash-printout s9)

(run-test "Unify s → #t"
  (unify s #t s9)
  s9)

(run-test "Unify v → #f"
  (unify v #f s9)
  s9)

(run-test "Unify r → 'symbol"
  (unify r 'symbol s9)
  s9)

(run-test "Walk p should resolve correctly"
  (walk p s9)
  `(,q (symbol (#t 5 ,u #f) ,k 8 symbol)))  ;; Expected after substitution

(substitution-hash-printout s9)

;; **Final Test Summary**
(display "\n--- Test Summary ---\n")
(display "Total Passed: ") (display pass-count) (newline)
(display "Total Failed: ") (display fail-count) (newline)

