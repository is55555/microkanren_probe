(import (scheme base) (scheme write))

;; Load microKanren implementation
(load "microkanren.scm")

;; Display header
(display "=== Circularity Test in microKanren ===") (newline)

;; Initial empty substitution
(define s '())

(display "Step 1: Starting with an empty substitution.") (newline)
(display "s = ") (display s) (newline)

;; Step 2: Bind x → y
(define s1 (ext-s '(var . x) '(var . y) s))
(display "Step 2: Binding x → y") (newline)
(display "s1 = ") (display s1) (newline)

;; Step 3: Bind y → x
(define s2 (ext-s '(var . y) '(var . x) s1))
(display "Step 3: Binding y → x (creating apparent circularity)") (newline)
(display "s2 = ") (display s2) (newline)

;; Step 4: Walk x
(display "Step 4: Running walk on x") (newline)
(display "walk(x, s2) → ") (display (walk '(var . x) s2)) (newline)

;; Step 5: Attempt to unify x and y
(display "Step 5: Attempting unify(x, y, s2)") (newline)
(display "unify(x, y, s2) → ") (display (unify '(var . x) '(var . y) s2)) (newline)

;; Step 6: Attempt to unify x with (1 x)
(display "Step 6: Attempting unify(x, (1 x), s2)") (newline)
(display "unify(x, (1 x), s2) → ") (display (unify '(var . x) '(1 (var . x)) s2)) (newline)

(display "=== Test Complete ===") (newline)
