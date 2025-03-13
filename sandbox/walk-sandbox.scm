(load "mukanren_ht.scm)
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