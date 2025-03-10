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


(define (no-self-reference? s)
  (every? (lambda (entry) (not (equal? (car entry) (cdr entry)))) s))  ;; Use `equal?` for structured terms

;; Helper: Walk through the substitution to resolve a variable
(define (walk v s seen)
  (if (member v seen)
      v  ;; Stop if cycle detected
      (let ((a (assoc v s)))  ;; Find binding for v
        (if a
            (walk (cdr a) s (cons v seen))  ;; Keep following bindings
            v))))


(define (no-circular-references? s)
  (every? (lambda (entry)
            (not (equal? (walk (car entry) s '()) (car entry))))  ;; If walk returns the same var, it's cyclic
          s))


(define (no-deep-nesting-proper-pair-structure? s)
  (every? (lambda (entry) 
            (and (pair? entry) 
                 (pair? (car entry))  ;; Ensure left-hand side is a pair
                 (or (var? (cdr entry))  ;; Right-hand side must be a var...
                     (not (pair? (cdr entry))))))  ;; ...or a non-pair (prevent deep nesting)
          s))

;; Predicate: Ensure substitution contains valid pairs where LHS is a variable
(define (proper-pair-structure? s)
  (every? (lambda (entry)
            (and (pair? entry)        ;; Entry itself must be a pair
                 (pair? (car entry))  ;; Left-hand side must be a pair
                 (var? (car entry)))) ;; Left-hand side must be a variable
          s))


;; Predicate: Perform **all** well-formedness checks
(define (well-formed? s)
  (and (valid-structure? s)
       (unique-bindings? s)
       (no-self-reference? s)
       (no-circular-references? s)
       (proper-pair-structure? s)))

;; Predicate: Perform all checks except circular references
(define (lite-well-formed? s)
  (and (valid-structure? s)
       (unique-bindings? s)
       (no-self-reference? s)
       (proper-pair-structure? s)))  ;; Skips circular reference check

(define (debug-well-formed? s)
  (let ((valid-structure (valid-structure? s))
        (unique (unique-bindings? s))
        (no-self-ref (no-self-reference? s))
        (no-cycles (no-circular-references? s))
        (proper-structure (proper-pair-structure? s)))
    (display "Debug: well-formed? checks:\n")
    (display "  valid-structure? = ") (display valid-structure) (newline)
    (display "  unique-bindings? = ") (display unique) (newline)
    (display "  no-self-reference? = ") (display no-self-ref) (newline)
    (display "  no-circular-references? = ") (display no-cycles) (newline)
    (display "  proper-pair-structure? = ") (display proper-structure) (newline)
    (and valid-structure unique no-self-ref no-cycles proper-structure)))
