(use-modules (oop goops)
	     (djf fstruct)
	     (ice-9 vlist)
	     (srfi srfi-26))

(define-fstruct <var>)
(define (var) (make-fstruct <var>))
(define (var? v) (is-a? v <var>))
(define var=? eq?)

(define (walk u s)
  (let ((pr (vhash-assq u s)))
    (if pr (walk (cdr pr) s) u)))

(define states list)
(define (== u v)
  (lambda (state)
    (let ((uni (unify u v state)))
      (if uni (states uni) (states)))))

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond ((eqv? u v) s)
	  ((var? u) (vhash-consq u v s))
	  ((var? v) (vhash-consq v u s))
	  ((and (pair? u) (pair? v))
	   (let ((unicar (unify (car u) (car v) s)))
	     (unify (cdr u) (cdr v) s)))
	  (else #f))))

(define (call/fresh f) (f (var)))

(define (mplus s1 s2)
  (cond ((null? s1) s2)
	((procedure? s1) (lambda () (mplus s2 (s1))))
	(else (cons (car s1) (mplus (cdr s1) s2)))))

(define (bind s g)
  (cond ((null? s) (states))
	((procedure? s) (lambda () (bind (s) g)))
	(else (mplus (g (car s)) (bind (cdr s) g)))))

(define (disj g1 g2) (lambda (state) (mplus (g1 state) (g2 state))))
(define (conj g1 g2) (lambda (state) (bind (g1 state) g2)))

; microkanren ends here

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (state) (lambda () (g state))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(define (fives x)
  (disj (== x 5) (Zzz (fives x))))
  ;(disj+ (== x 5) (fives x)))

(define (take n s)
  (cond ((or (zero? n) (null? s)) '())
	((procedure? s) (take n (s)))
	(else (cons (car s) (take (1- n) s)))))

(define (take-all s)
  (cond ((null? s) '())
	((procedure? s) (take-all (s)))
	(else (cons (car s) (take-all (cdr s))))))

(define (vhash-reify s/c*)
  (map (cut vhash-fold acons '() <>) s/c*))

;(take 10 ((call/fresh fives) vlist-null))
;((call/fresh fives) vlist-null)

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

(define empty-state vlist-null)

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (vhash-reify (take n ((fresh (x ...) g0 g ...)
			   empty-state))))))

;(write (macroexpand '(fresh (x) (fives x))))
;(write (macroexpand '(call/fresh (lambda (x) (fresh () (fives x))))))
;(fresh (x) (fives x))
;(call/fresh (lambda (x) (fresh () (fives x))))
;(call/fresh (lambda (x) (conj+ (fives x))))
;(run 1 (x) (fives x))
;(vhash-reify (take 10 ((fresh (x) (fives x)) empty-state)))
;(vhash-reify (take 10 ((fresh () (call/fresh fives)) empty-state)))
;(vhash-reify (take 10 ((call/fresh (lambda (x) (conj+ (fives x)))) empty-state)))
