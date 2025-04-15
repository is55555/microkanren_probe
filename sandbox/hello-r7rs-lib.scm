(define-library (foo bar)
(import (scheme base)
        (scheme write))
(export hello)
(begin
  (define (hello)
    (display "Hello, world!\n"))))