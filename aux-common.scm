;; aux-common.scm - Common compatibility utilities and load guards

;; Ensure DEBUG-LOADED is defined before use
(define DEBUG-LOADED #f)

;; Define compatibility utilities only if they are not already built-in
;; Unfortunately, `bound?` is not R5RS, so we manually include them here, 
;; or otherwise we comment them out

(define (filter pred lst)
  (cond
    ((null? lst) '())  ;; Base case: empty list remains empty
    ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))  ;; Keep element if it matches
    (else (filter pred (cdr lst)))))  ;; Otherwise, skip it

#| uncomment if "when" not available in your scheme
(define-syntax when
    (syntax-rules ()
    ((when test body ...)
        (if test (begin body ...))))))
|#

;(define (all pred lst)    (null? (filter (lambda (x) (not (pred x))) lst))))

;; Helper: Check if all elements satisfy a predicate
(define (every? pred lst)
  (null? (filter (lambda (x) (not (pred x))) lst)))
  ; alt ; (all pred lst)))


;; Helper: `any` function (checks if at least one element satisfies `pred`)
(define (any pred lst)
  (cond
    ((null? lst) #f)  ;; Base case: no match
    ((pred (car lst)) #t)  ;; Found a match
    (else (any pred (cdr lst)))))  ;; Recurse on rest

;; Predicate-based association list lookup
(define (assp pred alist)
  (cond
    ((null? alist) #f)
    ((pred (caar alist)) (car alist))
    (else (assp pred (cdr alist)))))

(define-syntax inc!
  (syntax-rules ()
    ((inc! var) (set! var (+ var 1)))))

