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
(define (ext-s-no-nested-triangularity var val s)
  "Extend substitution s with var → val, enforcing triangularity."
  (debug (begin (display "Attempting to extend substitution: ") 
         (display var) (display " → ") (display val) (newline)))
         
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

;-----
(define (occurs-check? var val s)
  (cond
    ((eq? var val) #t)  ;; Direct cycle
    ((pair? val) (or (occurs-check? var (car val) s)
                     (occurs-check? var (cdr val) s)))  ;; Recursively check pairs
    ((var? val) (occurs-check? var (walk val s) s))  ;; Follow bindings
    (else #f)))  ;; Otherwise, no cycle

;-----

(define (ext-s-no-triangularity-cycle-prevention var val s)
  "Extend substitution s with var → val, ensuring triangularity inside lists."
  (if (or (not (var? var))  ;; Only variables should be assigned
          (occurs-check? var val s)  ;; Prevent circularity
          (not (valid-triangular? var val s)))  ;; Ensure proper order
      (begin
        (debug (begin (display "REJECTED: ") (display var) (display " → ") (display val) (newline)))
        #f)
      (begin
        (hash-table-set! s var val)
        (debug (begin (display "Added: ") (display var) (display " → ") (display val) (newline)))
        s)))

(define (ext-s var val s)
  "Extend substitution s with var → val, enforcing full triangularity (no occurs-check)."
  (if (or (not (var? var))  ;; Only variables should be assigned
          (not (valid-triangular? var val s)))  ;; Ensure proper order inside lists
      (begin
        (debug (begin (display "REJECTED: ") (display var) (display " → ") (display val) (newline)))
        #f)
      (begin
        (hash-table-set! s var val)
        (debug (begin (display "Added: ") (display var) (display " → ") (display val) (newline)))
        s)))


(define (safe-ext-s var val s)
  (if (ext-s var val s)
      (debug (display "Binding succeeded.\n"))
      (debug (display "Binding failed.\n"))))


(define (valid-triangular-flat? var val s)
  "Ensure val only contains earlier variables or concrete values. This is required to maintain triangularity in lists and nested structures."
  (cond
    ((not (var? var)) #f)  ;; LHS must be a variable

    ((var? val) (< (cdr val) (cdr var)))  ;; RHS is a single variable? Ensure it's earlier.

    ((pair? val)  ;; If RHS is a list, check all elements
     (and (valid-triangular? var (car val) s)
          (valid-triangular? var (cdr val) s)))

    (else #t)))  ;; If it's a constant, it's always valid.



(define (valid-triangular? var val s)
  "Ensures that var → val maintains triangularity by checking nested lists and improper lists."
  (display "Checking valid-triangular?: ") (display var) (display " → ") (display val) (newline)
  (cond
    ;; ✅ Accept constants, symbols, or atomic values
    ((not (pair? val))  
     (display "  ✅ Passed: val is a constant or non-variable\n") 
     #t)  

    ;; ✅ Ensure variables obey triangularity
    ((var? val)
     (if (< (cdr val) (cdr var))
         (begin
           (display "  ✅ Passed: val is an earlier variable\n") 
           #t)
         (begin
           (display "  ❌ Failed: val is a later variable\n") 
           #f)))

    ;; ✅ Check pairs recursively (nested lists & improper lists)
    ((pair? val)  
     (display "  🔄 Checking pair elements recursively\n")
     (and (valid-triangular? var (car val) s)
          (valid-triangular? var (cdr val) s)))  

    (else 
     (display "  ❌ Failed: Invalid triangularity\n") 
     #f)))  




;; --- Walk Function ---
(define (walk-quiet v s)
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
(define NOT-FOUND 'NOT-FOUND)  ;; Special sentinel value (consider gensym)

(define (walk v s)
  "Look up variable v in substitution s, following chains. Apply recursively for lists."
  (let ((binding (hash-table-ref/default s v NOT-FOUND)))
    (cond
      ((eq? binding NOT-FOUND) v)  ;; Return as-is if not found
      ((var? binding) (walk binding s))  ;; Keep following variable bindings
      ((pair? binding)
       (let ((resolved (walk binding s)))  ;; First resolve the variable itself
         (if (pair? resolved)
             (map (lambda (x) (walk x s)) resolved)  ;; Recursively resolve lists
             resolved)))  ;; If not a list after resolution, return as-is
      (else binding))))  ;; If found, return value

(define (substitution-hash-printout substitution)
  (begin (display " [ \n")
    (hash-table-for-each substitution
    (lambda (key value)
        (display key) (display " → ") (display value) (newline)))
  (display "\n ] \n")))


(define (unify-no-forward-bindings u v s)
  "Unify two terms u and v within substitution s, ensuring triangularity."
  (debug (begin (display "Attempting to unify ") (display u) (display " with ") (display v) (newline)))
  
  (let* ((u (walk u s))
         (v (walk v s)))
    (debug (begin (display "After walk: ") (display u) (display " with ") (display v) (newline)))

    (cond
      ((equal? u v)
       (debug (display "Already unified\n")) 
       s)  ;; ✅ Already equal? No change needed

      ((var? u)
       (let ((new-s (unify-variable u v s)))
         (debug (begin (display "Unified variable ") (display u) (display " to ") (display v) (newline)))
         new-s))

      ((var? v)
       (let ((new-s (unify-variable v u s)))
         (debug (begin (display "Unified variable ") (display v) (display " to ") (display u) (newline)))
         new-s))

      ((and (pair? u) (pair? v))
       (debug (begin (display "Recursively unifying pairs\n")))
       (let ((s1 (unify (car u) (car v) s)))
         (if s1
             (unify (cdr u) (cdr v) s1)
             (begin
               (debug (display "Pair unification failed\n"))
               #f))))

      (else
       (debug (display "Unification failed\n"))
       #f))))


;; --- Helper function to find the newest variable in a structure ---
(define (find-newest-variable val s)
  "Find the newest variable inside a structure val."
  (cond
    ((var? val) val)  ;; Base case: If val itself is a var, return it
    ((pair? val)  ;; Recursively find the max index
     (let ((left (find-newest-variable (car val) s))
           (right (find-newest-variable (cdr val) s)))
       (cond
         ((and left right) (if (> (cdr left) (cdr right)) left right))
         (left left)
         (right right)
         (else #f))))  ;; If no variable found, return #f
    (else #f)))  ;; Ignore constants


(define (unify u v s)
  "Unify two terms u and v within substitution s, ensuring triangularity."
  (let* ((u (walk u s))
         (v (walk v s)))
    (cond
      ((equal? u v) s)  ;; ✅ Already equal? No change needed

      ((var? u) (unify-variable u v s))  ;; ✅ Delegate variable unification
      ((var? v) (unify-variable v u s))  ;; ✅ Delegate variable unification

      ((and (pair? v) (var? u))  ;; ❗ New Fix: Handle cases where p → (q 2)
       (let ((max-var (find-newest-variable v s)))  ;; Find the newest var in v
         (if (and max-var (< (cdr max-var) (cdr u)))  ;; ❌ Would violate triangularity?
             (let ((z (new-var)))  ;; ✅ Introduce a fresh variable z
               (let ((s1 (ext-s z v s)))  ;; Bind z → (q 2)
                 (if s1 (unify u z s1) #f)))  ;; Unify p → z
             (unify-variable u v s))))  ;; Otherwise, proceed as usual

      ((and (pair? u) (pair? v))
       (unify-pair u v s))  ;; ✅ Recursively unify lists

      (else #f))))  ;; ❌ Different atomic values → fail


(define (unify-variable var val s)
  "Safely unify a variable `var` with a value `val`, ensuring triangularity."
  (cond
    ((not (var? var)) #f)  ;; ❌ LHS must be a variable

    ;; ✅ If RHS is not a variable, bind normally
    ((not (var? val))
     (ext-s var val s))

    ;; ✅ If RHS is a variable, bind the **newer** to the **older**
    ((< (cdr val) (cdr var))  ;; RHS is an **earlier** variable → bind normally
     (ext-s var val s))

    (else  ;; 🔄 Swap to maintain triangularity
     (ext-s val var s))))



;; --- Helper function to unify pairs recursively ---
(define (unify-pair u v s)
  "Unifies two lists element-wise while ensuring deep propagation."
  (let ((s1 (copy-substitution s)))  ;; Create a copy before modifying
    (let ((s2 (unify (walk (car u) s) (walk (car v) s) s1))) ;; 🔹 Apply `walk` first
      (if s2
          (let ((s3 (unify (walk (cdr u) s) (walk (cdr v) s) s2))) ;; 🔹 Apply `walk` first
            (if s3 
                s3  ;; ✅ Successfully unified both parts
                (begin
                  (debug (display "Failed to unify cdr\n"))
                  #f)))
          (begin
            (debug (display "Failed to unify car\n"))
            #f)))))

;; --- Hash Table Copy Function ---
(define (copy-substitution s)
  "Create a shallow copy of the substitution hash table."
  (let ((new-s (create-empty-state)))
    (hash-table-for-each s (lambda (k v) (hash-table-set! new-s k v)))
    new-s))

;; --- Goal Function ---

(define (== u v)
  "Constraint goal that unifies u and v."
  (lambda (s)  ;; This is a goal function that takes a substitution
    (let ((s1 (unify u v s)))  ;; Attempt unification
      (if s1 (list s1) '()))))  ;; ✅ If success, return new state, else fail

;; --- end of general definitions ---