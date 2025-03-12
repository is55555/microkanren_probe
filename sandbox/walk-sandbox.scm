(import (srfi 69) (srfi 27))  ;; Hash table & random support
(load "aux-common.scm")
(load "debug.scm")

;; --- State Management ---
(define empty-state (make-hash-table))  ;; Start with an empty hash table
(define (create-empty-state) (make-hash-table))  

(define (empty-state? s)
  "Check if a substitution state s is empty."
  (zero? (hash-table-size s)))  

;; --- Variable Management ---
(define var-tag 'var)  ;; Special tag to identify logic variables

(define (var? v)
  "Check if v is a logic variable (i.e., a pair of (var . id))."
  (and (pair? v) (eq? (car v) var-tag)))

(define (var=? v1 v2)
  "Compare two logic variables based on their ID."
  (and (var? v1) (var? v2) (= (cdr v1) (cdr v2))))

(define new-var-index 0)
(define (new-var)
  "Generate a new logic variable"
  (set! new-var-index (+ new-var-index 1))
  (cons var-tag new-var-index))
(define (new-var-id id) (cons var-tag id)) ; for manual control


;; --- Substitutions ---
(define (ext-s var val s)
  "Extend substitution s with var → val, enforcing triangularity."
  (if (and (var? var)  ;; Ensure LHS is a valid variable
           (or (not (var? val))  ;; Ensure RHS is either a constant...
               (< (cdr val) (cdr var))))  ;; ...or an earlier variable
      (begin
        (hash-table-set! s var val)
        (debug (begin (display "Added: ") (display var) (display " → ") (display val) (newline)))
        s)  ;; Return the updated substitution
      (begin
        (debug (begin (display "REJECTED: ") (display var) (display " → ") (display val) (newline)))
        #f)))  ;; Invalid substitution (not triangular)

(define (safe-ext-s var val s)
  (if (ext-s var val s)
      (debug (display "Binding succeeded.\n"))
      (debug (display "Binding failed.\n"))))

;; --- Walk Function ---
(define (walk v s)
  "Look up variable v in substitution s, following chains."
  (let ((binding (hash-table-ref/default s v #f)))
    (if (and binding (var? binding))  ;; Ensure we keep following variables
        (walk binding s)  ;; Recursively follow the chain
        (or binding v))))  ;; Return found value or original variable

(define (substitution-hash-printout substitution)
  (begin (display " [ \n")
    (hash-table-for-each substitution
    (lambda (key value)
        (display key) (display " → ") (display value) (newline)))
  (display "\n ] \n")))

(define (unify u v s)
  "Unify two terms u and v within substitution s."
  (let* ((u (walk u s))  ;; Resolve u
         (v (walk v s)))  ;; Resolve v
    (cond
      ((equal? u v) s)  ;; ✅ Already equal? No change needed

      ((var? u) (ext-s u v s))  ;; ✅ u is a variable → bind it to v
      ((var? v) (ext-s v u s))  ;; ✅ v is a variable → bind it to u

      ((and (pair? u) (pair? v))  ;; ✅ Unify pair elements recursively
       (let ((s1 (unify (car u) (car v) s)))
         (if s1 (unify (cdr u) (cdr v) s1) #f)))  ;; If first fails, return #f

      (else #f))))  ;; ❌ If they're different atomic values, fail

(define (== u v)
  "Constraint goal that unifies u and v."
  (lambda (s)  ;; This is a goal function that takes a substitution
    (let ((s1 (unify u v s)))  ;; Attempt unification
      (if s1 (list s1) '()))))  ;; ✅ If success, return new state, else fail

;; --- end of general definitions ---
;; --- Run ---

;; Create substitution table 
(define s (create-empty-state))
(display "Initial state of s: ") (display s) (newline)
;; Debug: Print the entire substitution table before running tests
(begin  ; left begin to switch off by using debug instead
  (substitution-hash-printout s))

;; Generate variables
(define x (new-var))
(define w (new-var))
(define y (new-var))
(define z (new-var))
(define b (new-var))
(define c (new-var))
(define d (new-var))
(define a (new-var))

;; Populate with valid bindings using ext-s
(safe-ext-s x (random-integer 1000) s)  ;; x → random number
(safe-ext-s w 9999 s)  ;; w → 9999
(safe-ext-s y w s)   
(safe-ext-s z y s)   
(safe-ext-s a x s)   
(safe-ext-s b a s)   ; should be rejected
(safe-ext-s c b s)  
(safe-ext-s d z s)   



(display "extended s with a few vars: ") (display s) (newline)
(begin  ; left begin to switch off by using debug instead
  (substitution-hash-printout s))


;; Run tests
(display-all "walk x: " (var->string (walk x s)) " " x "\n" ) 
(display-all "walk w: " (var->string (walk w s)) " " w "\n" ) 
(display-all "walk y: " (var->string (walk y s)) " " y "\n" ) 
(display-all "walk z: " (var->string (walk z s)) " " z "\n" ) 
(display-all "walk a: " (var->string (walk a s)) " " a "\n" ) 
(display-all "walk b: " (var->string (walk b s)) " " b "\n" ) 
(display-all "walk c: " (var->string (walk c s)) " " c "\n" ) 
(display-all "walk d: " (var->string (walk d s)) " " d "\n" ) 
(define new-variable (new-var))
(display-all "walk unbound-var: " (var->string (walk new-variable s)) " " new-variable "[new variable]\n" ) 


(display "Final state of s: ") (display s) (newline)
(begin  ; left begin to switch off by using debug instead
  (substitution-hash-printout s))