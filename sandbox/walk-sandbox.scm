(import (srfi 69) (srfi 27))  ;; Hash table & random support
(load "aux-common.scm")

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


;; --- Substitutions ---
(define (ext-s var val s)
  "Extend substitution s with var → val, enforcing triangularity."
  (if (and (var? var)  ;; Ensure LHS is a valid variable
           (or (not (var? val))  ;; Ensure RHS is either a constant...
               (< (cdr val) (cdr var))))  ;; ...or an earlier variable
      (begin
        (hash-table-set! s var val)
        (display "Added: ") (display var) (display " → ") (display val) (newline)
        s)  ;; Return the updated substitution
      (begin
        (display "REJECTED: ") (display var) (display " → ") (display val) (newline)
        #f)))  ;; Invalid substitution (not triangular)

(define (safe-ext-s var val s)
  (if (ext-s var val s)
      (display "Binding succeeded.\n")
      (display "Binding failed.\n")))

;; --- Walk Function ---
(define (walk v s)
  "Look up variable v in substitution s, following chains."
  (let ((binding (hash-table-ref/default s v #f)))
    (if (and binding (var? binding))  ;; Ensure we keep following variables
        (walk binding s)  ;; Recursively follow the chain
        (or binding v))))  ;; Return found value or original variable

;; --- end definitions ---


;; Create substitution table - same thing (?)
(define s (create-empty-state))
;(define s (make-hash-table)) ;; Now we guarantee it's a fresh state
(display "Initial state of s: ") (display s) (newline)


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

;; Debug: Print the entire substitution table before running tests
(hash-table-for-each s
  (lambda (key value)
    (display key) (display " → ") (display value) (newline)))


;; Run tests
(display-all "walk x: " (walk x s) " " x "\n" )  ;; Should return a random number
(display "walk x: ") (display (walk x s)) (newline)  ;; Should return a random number
(display "walk y: ") (display (walk y s)) (newline)  ;; Expected: (var . 1)
(display "walk a: ") (display (walk a s)) (newline)  ;; Expected: (var . 4)
(display "walk w: ") (display (walk w s)) (newline)  ;; Expected: 9999
(display "walk z: ") (display (walk z s)) (newline)  ;; Expected: (var . 2)
(display "walk unbound: ") (display (walk (new-var) s)) (newline)  ;; Should return (var . 999)


(display "Final state of s: ") (display s) (newline)

