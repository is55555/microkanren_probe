;; aux-well-formed.scm
;; Well-formedness checks for microKanren substitutions

;; Define `filter` for strict R7RS compliance
(define (filter pred lst)
  (cond
    ((null? lst) '())  ;; Base case: empty list remains empty
    ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))  ;; Keep element if it matches
    (else (filter pred (cdr lst)))))  ;; Otherwise, skip it

;; Helper: Check if all elements satisfy a predicate
(define (every? pred lst)
  (null? (filter (lambda (x) (not (pred x))) lst)))

;; Predicate: Ensure substitution is a list of valid pairs
(define (valid-structure? s)
  (and (list? s)
       (every? (lambda (entry)
                 (and (pair? entry) 
                      (pair? (car entry)) 
                      (var? (car entry))))  ;; `var?` is in `microkanren.scm`
               s)))

;; Predicate: Ensure each variable is bound only once
(define (unique-bindings? s)
  (let loop ((bindings '()) (remaining s))
    (cond
      ((null? remaining) #t)
      ((member (caar remaining) bindings) #f)  ;; Duplicate variable detected
      (else (loop (cons (caar remaining) bindings) (cdr remaining))))))

;; Predicate: Ensure no variable is bound to itself
(define (no-self-reference? s)
  (every? (lambda (entry) (not (equal? (car entry) (cdr entry)))) s))

;; Helper: Walk through the substitution to resolve a variable
(define (walk v s seen)
  (if (member v seen)
      v  ;; Stop if cycle detected
      (let ((a (assoc v s)))  ;; Find binding for v
        (if a
            (walk (cdr a) s (cons v seen))  ;; Keep following bindings
            v))))

;; Predicate: Ensure no circular references exist ;; If walk returns the same var, it's cyclic
(define (no-circular-references? s)
  (every? (lambda (entry)
            (not (equal? (walk (car entry) s '()) (car entry)))) s))


;; Predicate: Ensure substitution only contains proper pairs
(define (proper-pair-structure? s)
  (every? (lambda (entry) (and (pair? entry) (pair? (car entry)))) s))

;; Predicate to ensure the right-hand side is well-formed ; these are pretty strict rules that usually won't be needed in we control the addition of bindings to the state
(define (valid-rhs-strict? v)
  (and (not (equal? v '()))  ;; Reject empty lists explicitly
       (or (not (pair? v))   ;; Allow atomic values
           (and (list? v)    ;; Ensure it's a proper list
                (every? var? v)))))  ;; Every element must be a variable

(define (valid-rhs? v)
  (debug (begin (display "Debug: Checking RHS value: ") (display v) (newline)))
  (cond
    ((null? v) #f)  ;; ❌ Explicitly reject empty lists
    ((not (pair? v)) #t) ;; ✅ Atomic values (42, "x") are valid
    ((var? v) #t)  ;; ✅ A logic variable is valid
    ((list? v)  ;; ✅ Ensure `v` is a list before using `every?`
     (every? (lambda (x)
               (or (var? x)  ;; ✅ Variables are valid
                   (and (pair? x) (valid-rhs? x))))  ;; ✅ Ensure we only recurse on pairs
             v))
    (else #f))) ;; ❌ If none of the above are true, it's invalid



;; Predicate: Perform **all** well-formedness checks
(define (well-formed? s)
  (and (valid-structure? s)
       (unique-bindings? s)
       (no-self-reference? s)
       (no-circular-references? s)
       (proper-pair-structure? s)
       (every? (lambda (entry) (valid-rhs? (cdr entry))) s)))  ;; Ensure right-hand side is valid

;; Predicate: Perform all checks except circular references
(define (lite-well-formed? s)
  (and (valid-structure? s)
       (unique-bindings? s)
       (no-self-reference? s)
       (proper-pair-structure? s)
       (every? (lambda (entry) (valid-rhs? (cdr entry))) s)))  ;; 🔥 Ensure right-hand side is valid

;; Debugging version of well-formed?
(define (debug-well-formed? s)
  (let ((v-struct (valid-structure? s))
        (u-bind (unique-bindings? s))
        (no-self (no-self-reference? s))
        (no-circ (no-circular-references? s))
        (prop-pair (proper-pair-structure? s))
        (rhs-valid (every? (lambda (entry) (valid-rhs? (cdr entry))) s)))
    (display "Debug: well-formed? checks:") (newline)
    (display "  valid-structure? = ") (display v-struct) (newline)
    (display "  unique-bindings? = ") (display u-bind) (newline)
    (display "  no-self-reference? = ") (display no-self) (newline)
    (display "  no-circular-references? = ") (display no-circ) (newline)
    (display "  proper-pair-structure? = ") (display prop-pair) (newline)
    (display "  valid-rhs? = ") (display rhs-valid) (newline)
    (and v-struct u-bind no-self no-circ prop-pair rhs-valid)))  ;; ✅ Combine all checks
