(define (find-first-even lst)
(call-with-current-continuation
  (lambda (exit)
    (for-each (lambda (x)
                (when (even? x) (exit x)))  ;; Immediately return first even number
              lst)
    #f)))  ;; Return #f if no even number is found

(find-first-even '(1 3 5 8 9))  ;; ⇒ 8
(find-first-even '(1 3 5 7))    ;; ⇒ #f


(define (my-or a b)
(call-with-current-continuation
  (lambda (exit)
    (let ((result-a a))
      (if result-a (exit result-a) b)))))

(my-or #f 42)   ;; ⇒ 42
(my-or 10 42)   ;; ⇒ 10


; ===== simulating exception handling

(define (safe-divide a b)
(call-with-current-continuation
  (lambda (throw)
    (if (= b 0)
        (throw 'division-by-zero)
        (/ a b)))))

(safe-divide 10 2)  ;; ⇒ 5
(safe-divide 10 0)  ;; ⇒ division-by-zero


; ===== backtracking

(define (backtrack-test-1)
(call-with-current-continuation
  (lambda (k)
    (display "A ")   ;; Always prints
    (k 'backtracked) ;; Jump out immediately
    (display "B ")))) ;; This is never reached

(backtrack-test-1)  ;; Output: "A " and returns 'backtracked
;After calling k, execution jumps back to where call/cc was originally called, skipping "B ".


; ---

(define saved-continuation #f)

(define (backtrack-test-2)
  (call-with-current-continuation
    (lambda (k)
      (set! saved-continuation k) ;; Save for later use
      (display "A ")
      'saved))) ;; Just return without jumping

(backtrack-test-2)  ;; Output: "A ", returns 'saved

(saved-continuation 'resume)  ;; Resumes after (call/cc), no new "A"
(display "B ")   ;; This now runs
; First call prints "A " and saves k but does not jump.
; When we call saved-continuation, execution resumes from where call/cc originally returned.

; ---

; === jumping back

(define saved-cont #f)

(define (test-cont)
  (call-with-current-continuation
    (lambda (k)
      (set! saved-cont k)  ;; Save the continuation
      (display "A\n")))
  (display "B\n"))

(test-cont)   ;; Output: "A" "B"
(saved-cont)  ;; Output: "B"

