;; Minimal microKanren with cycle detection and proper goal handling

;; Check if a term is a logic variable
(define (var? v)
  (and (pair? v) (eq? (car v) 'var)))

;; Compare two logic variables
(define (var=? u v)
  (and (var? u) (var? v) (eq? (cdr u) (cdr v))))

;; Walk with cycle detection
(define (walk-internal-strict v s seen)
  (if (member v seen)
      v  ;; Stop if cycle detected
      (let ((a (assp (lambda (x) (var=? v x)) s)))
        (if a
            (walk-internal (cdr a) s (cons v seen))  ;; Track visited vars
            v))))
;; Helper: Walk through the substitution to resolve a variable
(define (walk-internal v s seen)
  (display "Debug: walk-internal called with v=") (display v) 
  (display " s=") (display s) 
  (display " seen=") (display seen) (newline)

  (if (member v seen)
      (begin 
        (display "⚠️ Cycle detected! Returning v=") (display v) (newline)
        v)  ;; Stop if a cycle is detected
      (let ((a (assoc v s)))  ;; Find binding for v
        (if a
            (begin
              (display "🔄 Recursing: found binding (") (display v) 
              (display " → ") (display (cdr a)) (display ")") (newline)
              (walk-internal (cdr a) s (cons v seen)))  ;; Keep following bindings
            (begin 
              (display "✅ No further bindings for v=") (display v) (newline)
              v)))))  ;; Return final resolved value

;; Wrapper for backward compatibility
(define (walk v s)
  (walk-internal v s '()))

;; Extend substitution (bind variable to value)
(define (ext-s v x s)
  (cons (cons v x) s))

;; Occurs-check: Prevent binding a variable to something that contains it
(define (occurs-check? v expr s)
  (cond
    ((eq? v expr) #t)
    ((var? expr) (occurs-check? v (walk expr s) s))
    ((pair? expr) (or (occurs-check? v (car expr) s) (occurs-check? v (cdr expr) s)))
    (else #f)))

;; Auxiliary function to prevent unify from infinite loops
(define (seen? u v seen)
  (or (member (list u v) seen) (member (list v u) seen)))

;; Unification function
(define (unify-internal u v s seen)
  (let ((u (walk u s)) (v (walk v s)))
    (if (or (eq? u v) (seen? u v seen))
        s
        (let ((new-seen (cons (list u v) seen)))
          (cond
            ((var? u)
             (if (occurs-check? u v s) #f (ext-s u v s)))
            ((var? v)
             (if (occurs-check? v u s) #f (ext-s v u s)))
            ((and (pair? u) (pair? v))
             (let ((s (unify-internal (car u) (car v) s new-seen)))
               (if s (unify-internal (cdr u) (cdr v) s new-seen) s)))
            (else #f))))))

;; Wrapper for unification
(define (unify u v s)
  (unify-internal u v s '()))

;; Goal: Equality constraint
(define (== u v)
  (lambda (s)
    (let ((s (unify u v s)))
      (if s (list s) '()))))

;; Goal: Logical OR (disj)
(define (disj . clauses)
  (lambda (s)
    (apply append
           (map (lambda (g)
                  (if (procedure? g) (g s) '()))  ;; Ensure only valid goals are called
                clauses))))

;; Goal: Logical AND (all)
(define (conj . goals)
  (lambda (s)
    (if (null? goals)
        (list s)  ;; ✅ Handle empty goal case correctly
        (foldl (lambda (g acc)
                 (if (and (procedure? g) (list? acc))  ;; Ensure `acc` is a list
                     (apply append (map g acc))
                     acc))  ;; Ignore invalid goals
               (list s)  ;; ✅ Ensure initial state is always a list
               goals))))


;; Run the logic program
(define (run n goal)
  (let ((results (goal '(()))))
    (if (list? results) (take n results) '())))

;; Take first n results
(define (take n lst)
  (if (or (zero? n) (null? lst)) '() (cons (car lst) (take (- n 1) (cdr lst)))))

