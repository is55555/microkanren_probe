(import (srfi 69) (srfi 27))  ;; Hash table & random support (chicken has these eggs)

;; --- State Management ---
(define empty-state (make-hash-table))  ;; Start with an empty hash table

(define (empty-state? s)
  "Check if a substitution state s is empty."
  (hash-table-empty? s))  ;; Directly check for an empty hash table

;; --- Variable Management ---
(define var-tag 'var)  ;; Special tag to identify logic variables

(define (var? v)
  "Check if v is a logic variable (i.e., a pair of (var . id))."
  (and (pair? v) (eq? (car v) var-tag)))

(define (var=? v1 v2)
  "Compare two logic variables based on their ID."
  (and (var? v1) (var? v2) (= (cdr v1) (cdr v2))))

(define (new-var id)
  "Generate a new logic variable with the given ID."
  (cons var-tag id))

;; --- Substitutions ---
(define (ext-s var val s)
  "Extend substitution s with var → val, enforcing triangularity."
  (if (and (var? var)  ;; Ensure LHS is a valid variable
           (or (not (var? val))  ;; Ensure RHS is either a constant...
               (< (cdr val) (cdr var))))  ;; ...or an earlier variable
      (begin (hash-table-set! s var val) s)  ;; Add to the substitution
      #f))  ;; Invalid substitution (not triangular)

;; --- Walk Function ---
(define (walk v s)
  "Look up variable v in substitution s, following chains."
  (if (var? v)
      (let ((binding (hash-table-ref/default s v #f)))
        (if binding (walk binding s) v))  ;; Follow substitution recursively
      v))  ;; Return non-variable values as-is

;; --- Testing ---
(define s (make-hash-table))  ;; Create substitution table

;; Generate variables
(define x (new-var 0))
(define y (new-var 1))
(define z (new-var 2))
(define w (new-var 3))
(define a (new-var 4))
(define b (new-var 5))
(define c (new-var 6))
(define d (new-var 7))
(define cycle1 (new-var 8))
(define cycle2 (new-var 9))
(define loop (new-var 10))

;; Populate with valid bindings using ext-s
(ext-s x (random-integer 1000) s)  ;; x → random number
(ext-s y z s)  ;; y → z
(ext-s z w s)  ;; z → w
(ext-s w 9999 s)  ;; w → 9999
(ext-s a b s)  ;; a → b
(ext-s b c s)  ;; b → c
(ext-s c d s)  ;; c → d
(ext-s d 45 s)  ;; d → 45

;; Invalid bindings (would break triangularity)
(ext-s loop loop s)  ;; Self-reference
(ext-s cycle1 cycle2 s)  ;; cycle1 → cycle2
(ext-s cycle2 cycle1 s)  ;; cycle2 → cycle1

;; Run tests
(display "walk x: ") (display (walk x s)) (newline)  ;; Should return a random number
(display "walk y: ") (display (walk y s)) (newline)  ;; Expected: 9999
(display "walk a: ") (display (walk a s)) (newline)  ;; Expected: 45
(display "walk w: ") (display (walk w s)) (newline)  ;; Expected: 9999
(display "walk z: ") (display (walk z s)) (newline)  ;; Expected: 9999
(display "walk unbound: ") (display (walk (new-var 999) s)) (newline)  ;; Should return the variable itself
