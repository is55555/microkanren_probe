(define saved-cont #f)


(define (show-vars vars)
  (for-each (lambda (v)
              (display (car v)) (display " = ")
              (display (cdr v)) (newline))
            vars))

(define (debug-env)
  (let ((x 10) (y "hello") (z '(1 2 3)))
    (call-with-current-continuation
      (lambda (k)
        (set! saved-cont k)
        (display "Captured Environment:\n")
        (show-vars `((x . ,x) (y . ,y) (z . ,z)))))))

(debug-env)

(saved-cont 'resumed)  ;; Call again, preserving the same variable values

; Captured Environment:
; x = 10
; y = hello
; z = (1 2 3)
; resumed

; ---

(define (debug-stack)
(call-with-current-continuation
  (lambda (k)
    (display "Captured stack trace:\n")
    (print-call-chain)
    (newline)
    (set! saved-cont k)))))

(debug-stack)

; /// print-call-chain exists in Chicken (code not tested)
;
; (define saved-cont #f)
;
; (define (debug-stack)
; (call-with-current-continuation
;   (lambda (k)
;     (display "Captured stack trace:\n")
;     (print-call-chain)
;     (newline)
;     (set! saved-cont k))))

; (debug-stack)

; ///

; ---

; also not tested, this depend on the availability of environment modules
; ///

; (define (dump-environment)
; (call-with-current-continuation
;   (lambda (k)
;     (set! saved-cont k)
;     (display "Global environment dump:\n")
;     (for-each (lambda (sym)
;                 (display sym) (display " = ")
;                 (display (eval sym (interaction-environment)))
;                 (newline))
;               (environment-bindings (interaction-environment))))))

; (dump-environment)


; ---

; same req. also needs testing

(define (list-globals)
(for-each (lambda (sym)
            (display sym) (display " = ")
            (display (eval sym (interaction-environment)))
            (newline))
          (environment-bindings (interaction-environment))))

(list-globals)  ;; Prints all user-defined variables


; ===

; undoind

(define undo-stack '())

(define (save-state)
  (call-with-current-continuation
    (lambda (k)
      (set! undo-stack (cons (list k `((counter . ,counter))) undo-stack))
      (display "State saved: counter = ")
      (display counter)
      (newline))))

(define (undo)
  (if (null? undo-stack)
      (display "No more undos!\n")
      (let ((last-state (car undo-stack)))
        (set! undo-stack (cdr undo-stack))  ;; Pop stack
        (set! counter (cdr (assq 'counter (cadr last-state))))  ;; Restore value
        ((car last-state) 'undoing))))  ;; Jump back

(define counter 0)

(define (increment)
  (save-state)
  (set! counter (+ counter 1))
  (display "Counter: ")
  (display counter)
  (newline))

(increment)  ;; Counter: 1
(increment)  ;; Counter: 2
(increment)  ;; Counter: 3

(undo)  ;; Undo → Counter: 2
(undo)  ;; Undo → Counter: 1
(undo)  ;; Undo → Counter: 0
(undo)  ;; No more undos!

; State saved: counter = 0
; Counter: 1
; State saved: counter = 1
; Counter: 2
; State saved: counter = 2
; Counter: 3
; Counter: 3
; Counter: 2
; Counter: 1
; No more undos!


