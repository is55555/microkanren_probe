; just an example to illustrate "do"

(define (integer-sqrt n)
(do ((x n (quotient (+ x (quotient n x)) 2)))  ;; Newton's method update
    ((<= (- (* x x) n) x) x)))  ;; Stop when x^2 is close enough to n

#|

A good example of using the do loop in Scheme, which is not easily expressed with other looping 
constructs like named let or fold, is when you need multiple state variables that evolve differently 
in each iteration.

- Multiple state variables evolve naturally (x updates with each step).

- Termination condition is cleanly separated in the second argument list.

- State mutation is explicit, without requiring an auxiliary function.

- Other styles, like named let, require an explicit recursive call inside the body, 
making do syntactically cleaner here.

|#
