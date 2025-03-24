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
(saved-cont)  ;; Output: "B"  ;; [ask]


; (restart-example)  ;; Output: "A"
; (saved-cont)       ;; Output: "A" again (restarts from `call/cc`)
; (saved-cont)       ;; Output: "A" again

; ---

;(define (exec-restart-example)

(define saved-cont #f)

(define (restart-example)
  (call-with-current-continuation
   (lambda (k)
     (set! saved-cont k)  ;; Save continuation
     (display "A\n")      ;; Always prints when resumed
     (k 'restart) ;; Jump back to this continuation
     (display "B\n")))
  (display "C\n"))      

(display "begin\n")
(restart-example)  ;; Output: "A"
(display "checkpoint 1\n")
(saved-cont)       ;; Output: "A" again (restarts from `call/cc`)
(display "checkpoint 2\n")
(saved-cont)       ;; Output: "A" again

(display "end\n")
;)

;(exec-restart-example)
; encapsulated it loops forever??
;otherwise:
; begin
; A
; C
; checkpoint 1
; C
; checkpoint 2
; C
; end

; ---


(define saved-cont #f)


(define (restart-example) ; redefining
  (let ((value
         (call-with-current-continuation
           (lambda (k)
             (set! saved-cont k)  
             (display "A\n")
             (k 'restart)
             (k 'this-never-runs)
             ))))  ;; This time, we capture the return value, but since the cont is ...
            ;; ... at the end of the lambda execution, the second k call never happens.
    (display "Got: ")
    (display value)
    (newline)))

(restart-example)
(display "---\n")
(saved-cont 'what)
(display "---\n")
(saved-cont 'again)
(display "---\n")
(saved-cont 'and-again)

; A
; Got: restart
; ---
; Got: what
; ---
; Got: again
; ---
; Got: and-again

; ===== coroutines

(define producer #f)
(define consumer #f)
(define product-count 0)

(define (produce)
  (call-with-current-continuation
    (lambda (k)
      (set! producer k)  ;; Save continuation to resume later
      (display "Producer started\n")
      ;(k 'out) this would prevent the remainder of the lambda from running, but not the rest of the function
      ))
  (set! product-count (+ product-count 1))
  (display "produced, now: ") (display product-count) (newline) 
  )


(define (consume)
  (call-with-current-continuation
    (lambda (k)
      (set! consumer k)  ;; Save consumer continuation
      (display "Consumer started\n")))
  (set! product-count (- product-count 1))
  (display "consumed, now: ") (display product-count) (newline)
  )

(produce)  ;; Starts the producer
(consume)  ;; Starts the consumer
(display " ----- \n")

(producer) (producer) (producer) (producer)
(consumer) (consumer) (consumer) (consumer)

(producer) (producer)
(consumer)
(producer) (producer)
(consumer) (consumer) (consumer) (consumer)

; Producer started
; produced, now: 1
; Consumer started
; consumed, now: 0
;  ----- 
; produced, now: 1
; produced, now: 2
; produced, now: 3
; produced, now: 4
; consumed, now: 3
; consumed, now: 2
; consumed, now: 1
; consumed, now: 0
; produced, now: 1
; produced, now: 2
; consumed, now: 1
; produced, now: 2
; produced, now: 3
; consumed, now: 2
; consumed, now: 1
; consumed, now: 0
; consumed, now: -1


; ---

(define producer #f)
(define consumer #f)
(define product-count 0)

(define (producer-maker)
  (let ((run-cc-only #t))
    (if run-cc-only
      (begin 
        (call-with-current-continuation
         (lambda (k)
           (set! producer k)  ;; Save continuation to resume later
           (display "Producer started\n")
           ;(k 'out) this would prevent the remainder of the lambda from running, but not the rest of the function
           ))
    )) ; if #t branch (no else branch, that would not work as it would be just not included in subsequent runs)

    (if (not run-cc-only) ;if2, we need a separate if for the false branch...
        ; ... because if is not a statement in scheme
        (begin
          (set! product-count (+ product-count 1))
          (display "produced, now: ") (display product-count) (newline) 
    )) ; close begin, if2
    (set! run-cc-only #f) ; here we flip the branch what will be executed in consecutive calls
    )) ; close let, define


(define (consumer-maker)
  (call-with-current-continuation
    (lambda (k)
      (set! consumer k)  ;; Save consumer continuation
      (display "Consumer started\n")))
  (set! product-count (- product-count 1))  ;; this is much simpler, but it runs the first time as well
  ;; ... so creating the consumer also consumes once. To separate consumer creator from consumer we'd have ...
  ;; ... to do something like producer-maker, I leave this unchanged for reference. The first consumption...
  ;; ... is down to the consumer-maker call itself. 
  (display "consumed, now: ") (display product-count) (newline)
  )

(producer-maker)  ;; Starts the producer
(consumer-maker)  ;; Starts the consumer (also consumes, because I left it unchanged unlike producer-maker)
(display " ----- \n")

(producer) (producer)
(consumer)
(producer) (producer)
(consumer) (consumer) (consumer) (consumer)


; ===== undo

(define undo-stack '())

(define (save-state)
  (call-with-current-continuation
    (lambda (k)
      (set! undo-stack (cons k undo-stack))  ;; Save continuation
      (display "State saved\n"))))

(define (undo)
  (if (null? undo-stack)
      (display "No more undos!\n")
      (let ((last-state (car undo-stack)))
        (set! undo-stack (cdr undo-stack))  ;; Pop stack
        (last-state 'undoing))));; Jump back

(save-state)  ;; Save first state
(display "First change\n")
(save-state)  ;; Save second state
(display "Second change\n")
(save-state)  ;; Save third state
(display "Third change\n")
(display undo-stack)(newline)
(undo)  ;; Undo third change
(display undo-stack)(newline)
(undo)  ;; Undo second change
(display undo-stack)(newline)
(undo)  ;; Undo first change
(display undo-stack)(newline)
(undo)  ;; No more undos
(display undo-stack)(newline)

; output:
; State saved
; First change
; State saved
; Second change
; State saved
; Third change
; (#<procedure> #<procedure> #<procedure>)
; undoing
; (#<procedure> #<procedure>)
; undoing
; (#<procedure>)
; undoing
; ()
; No more undos!
; ()
; >

; ---

(define saved-cont #f)

(define (debug-dump)
  (call-with-current-continuation
    (lambda (k)
      (set! saved-cont k)
      (display "Execution state saved!\n")
      (lambda (msg)
        (display "Restored state with message: ")
        (display msg)
        (newline)))))

(define restore-fn (debug-dump))  ;; Capturing the continuation

(restore-fn "First restore")  ;; Output: "Restored state with message: First restore"
(restore-fn "Second restore") ;; Output: "Restored state with message: Second restore"

; output:
; Execution state saved!
; Restored state with message: First restore
; Restored state with message: Second restore

