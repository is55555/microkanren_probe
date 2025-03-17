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

