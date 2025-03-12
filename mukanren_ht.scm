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
  "Look up variable v in substitution s, following chains.
   If v is part of a list, recursively substitute its elements."
  (let ((binding (hash-table-ref/default s v #f)))
    (cond
      ((and binding (var? binding))  ;; Variable case: Keep walking
       (walk binding s))

      ((pair? binding)  ;; New case: If bound to a pair, walk each element
       (map (lambda (x) (walk x s)) binding))

      (binding binding)  ;; If there is a binding, return it
      (else v))))  ;; Otherwise return v itself

(define (substitution-hash-printout substitution)
  (begin (display " [ \n")
    (hash-table-for-each substitution
    (lambda (key value)
        (display key) (display " → ") (display value) (newline)))
  (display "\n ] \n")))


(define (unify u v s)
  "Unify two terms u and v within substitution s, ensuring triangularity."
  (let* ((u (walk u s))
         (v (walk v s)))
    (cond
      ((equal? u v) s)  ;; ✅ Already equal? No change needed

      ((var? u) (unify-variable u v s))  ;; ✅ Delegate variable unification
      ((var? v) (unify-variable v u s))  ;; ✅ Delegate variable unification

      ((and (pair? u) (pair? v))
       (unify-pair u v s))  ;; ✅ Delegate to helper

      (else #f))))  ;; ❌ Different atomic values → fail

;; --- Helper function to unify a variable with a value ---
(define (unify-variable var val s)
  "Safely unify a variable var with a value val while enforcing triangularity."
  (cond
    ((not (var? var)) #f)  ;; ❌ LHS must always be a variable

    ((not (var? val))  ;; ✅ RHS is a constant → bind normally
     (ext-s var val s))

    ((< (cdr val) (cdr var))  ;; ✅ RHS is an earlier variable → bind normally
     (ext-s var val s))

    (else  ;; 🔄 Flip order to maintain triangularity
     (ext-s val var s))))

;; --- Helper function to unify pairs recursively ---
(define (unify-pair u v s)
  "Unifies two pairs element-wise, ensuring both elements unify correctly."
  (let ((s1 (copy-substitution s)))  ;; Create a copy before modifying
         (let ((s2 (unify (car u) (car v) s1)))
           (if s2 (unify (cdr u) (cdr v) s2) #f))))  ;; Fully succeed or fail

(define (copy-substitution s)
  "Create a shallow copy of the substitution hash table."
  (let ((new-s (create-empty-state)))
    (hash-table-for-each s (lambda (k v) (hash-table-set! new-s k v)))
    new-s))


(define (== u v)
  "Constraint goal that unifies u and v."
  (lambda (s)  ;; This is a goal function that takes a substitution
    (let ((s1 (unify u v s)))  ;; Attempt unification
      (if s1 (list s1) '()))))  ;; ✅ If success, return new state, else fail

;; --- end of general definitions ---