;example-inline.nss

(ns-set 'separator "__")

(define outside-var 5)

(ns "util"
  (define a 1)

  (ns-inline "short"
    (define b 2)
    (define (c x) 
      (* 
        (+ a x b)
        outside-var))))

