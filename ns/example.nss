(ns-set 'separator "::")

(ns "geometry"

  (define pi 3.14159)

  (ns "area"
    (define (circle r)
      (* pi r r))

    (define (rectangle w h)
      (* w h)))

  (ns "volume"
    (define (sphere r)
      (* 4/3 pi r r r))

    (define (cuboid w h d)
      (* w h d))))
