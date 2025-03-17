
(define x 10)
(define (square y) (* y y))

(let ((a 1) (b 2))
  (define c (+ a b))
  (lambda (z) (+ z c)))

