;; test/lambda-rewrite-tests-standalone.nss

(namespace (ns)

  ;; Fixed arity
  (define f1
    (lambda (x y)
      (free x y)))  ; expect (ns::free x y)

  ;; Variadic (single symbol)
  (define f2
    (lambda args
      (free args)))  ; expect (ns::free args)

  ;; Dotted (rest argument)
  (define f3
    (lambda (x y . rest)
      (free x y rest))) ; expect (ns::free x y rest)

  ;; Nested lambda
  (define f4
    (lambda (x)
      (lambda (y)
        (free x y)))) ; expect (ns::free x y)

  ;; Lambda with internal definition
  (define f5
    (lambda (x)
      (define y 42)
      (free x y))) ; expect (ns::free x y)
)
