(define (show-vars vars)
(for-each (lambda (v)
            (display (car v)) (display " = ")
            (display (cdr v)) (newline))
          vars))

(define-syntax track-vars
(syntax-rules ()
  ((track-vars (var ...) body ...)
   (let ((var var) ...)  ;; Capture all variables
     (show-vars `((var . ,var) ...))  ;; Print variable names and values
     body ...))))

(define (test)
(track-vars (a b c)
  (let ((a 10) (b "hello") (c '(1 2 3)))
    (display "Executing body...\n"))))

(test)


; The macro track-vars expands into a let that captures all variable names and values.
; When called, it prints all variables before executing the body.
; Limitations:

; Requires manually specifying variable names.
; Cannot introspect unknown variables created dynamically.


; ==== repl to track vars

(define user-env (make-hash-table))  ;; Store user-defined variables

(define (repl)
  (display "> ")
  (let ((input (read)))
    (cond
      ((eq? input 'exit) (display "Goodbye!\n"))
      ((and (pair? input) (eq? (car input) 'define))
       (let ((name (cadr input)) (value (eval (caddr input) (interaction-environment))))
         (hash-table-set! user-env name value)
         (display name) (display " defined.\n")))
      ((eq? input 'show-vars)
       (for-each (lambda (pair)
                   (display (car pair)) (display " = ")
                   (display (cdr pair)) (newline))
                 (hash-table->list user-env)))
      (else
       (display "Unknown command\n")
       (repl)))
    (repl)))

(repl)




; ---- static parse of vars from a file

(define (extract-vars expr scope)
(cond
  ((symbol? expr) '())  ;; Ignore symbols alone
  ((pair? expr)
   (case (car expr)
     ((define) (list (cadr expr))) ;; Capture (define var)
     ((lambda) (cadr expr)) ;; Capture lambda parameters
     ((let let*) (map car (cadr expr))) ;; Capture let bindings
     (else (apply append (map (lambda (e) (extract-vars e scope)) expr)))))
  (else '()))) ;; Ignore numbers, strings, etc.

(define (parse-file filename)
(with-input-from-file filename
  (lambda ()
    (let loop ((vars '()))
      (let ((expr (read)))
        (if (eof-object? expr)
            vars
            (loop (append vars (extract-vars expr 'global)))))))))

(display "Variables in file:\n")
(display (parse-file "example.scm"))
(newline)


; ---- var tracking macros

(define tracked-vars '())

(define-syntax track-define
  (syntax-rules ()
    ((track-define var expr)
     (begin
       (set! tracked-vars (cons 'var tracked-vars))
       (define var expr)))))

(define-syntax track-let
  (syntax-rules ()
    ((track-let bindings body ...)
     (let bindings
       (set! tracked-vars (append (map car bindings) tracked-vars))
       body ...))))

(track-define x 42)
(track-let ((y 10) (z 20))
  (+ y z))

(display "Tracked variables:\n")
(display tracked-vars)
(newline)

; -----

; To log variables properly, we need to track their names and their scopes, 
; especially when the same variable name appears in different contexts.


; Parse a Scheme file statically to detect:
; Global variables (from define).
; Function parameters (from lambda).
; Local variables (from let, let*, define inside functions).
; Log the variable name along with its scope.
; Handle shadowing (when a variable is redefined in a nested scope).

(define (extract-vars expr scope level)
(cond
  ((symbol? expr) '())  ;; Ignore single symbols
  ((pair? expr)
   (case (car expr)
     ;; Capture global definitions
     ((define) (list (list (cadr expr) scope level))) 
     
     ;; Capture function parameters
     ((lambda) (map (lambda (v) (list v "lambda-param" level)) (cadr expr)))
     
     ;; Capture let and let* bindings
     ((let let*) (append
                  (map (lambda (v) (list (car v) "let-binding" level)) (cadr expr))
                  (extract-vars (caddr expr) scope (+ level 1)))) 

     ;; Recursively process nested expressions
     (else (apply append (map (lambda (e) (extract-vars e scope level)) expr)))))
  (else '())))  ;; Ignore numbers, strings, etc.

(define (parse-file filename)
(with-input-from-file filename
  (lambda ()
    (let loop ((vars '()) (level 0))
      (let ((expr (read)))
        (if (eof-object? expr)
            vars
            (loop (append vars (extract-vars expr "global" level)) (+ level 1))))))))

;; Run parser on "example.scm"
(display "Variables in file:\n")
(for-each (lambda (var)
          (display (car var)) (display " -> ")
          (display (cadr var)) (display " (level ")
          (display (caddr var)) (display ")\n"))
        (parse-file "example.scm"))


; expecting something like this:

; Variables in file:
; x -> global (level 0)
; square -> global (level 0)
; y -> lambda-param (level 1)
; x -> let-binding (level 2)
; z -> let-binding (level 2)
; a -> let-binding (level 3)
; x -> lambda-param (level 4)

(define (group-by-scope vars)
(let ((scopes (make-hash-table)))
  (for-each (lambda (var)
              (let ((name (car var))
                    (scope (cadr var))
                    (level (caddr var)))
                (hash-table-set! scopes level
                                 (cons (list name scope) (hash-table-ref scopes level '())))))
            vars)
  scopes))

;; Display grouped variables
(let ((scopes (group-by-scope (parse-file "example.scm"))))
(for-each (lambda (level)
            (display "Scope Level ") (display level) (display ":\n")
            (for-each (lambda (var)
                        (display "  ")
                        (display (car var)) (display " -> ")
                        (display (cadr var)) (newline))
                      (hash-table-ref scopes level '())))
          (sort (hash-table-keys scopes) <)))



;         Scope Level 0:
;     x -> global
;     square -> global
  
;   Scope Level 1:
;     y -> lambda-param
  
;   Scope Level 2:
;     x -> let-binding
;     z -> let-binding
  
;   Scope Level 3:
;     a -> let-binding
  
;   Scope Level 4:
;     x -> lambda-param
  