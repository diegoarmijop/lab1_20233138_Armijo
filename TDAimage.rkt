#lang racket

;TDA pixbit-d
(define pixbit-d
  (lambda(x y bit depth)
    (if
     (and (number? x) 
          (number? y) 
          (number? bit)
          (number? depth)
          (>=  x 0)
          (>=  y 0)
          (and
           (>= bit 0)
           (< bit 2)
          )
          (>= depth 0)
      )
     (list x y bit depth)
     (raise "No es un pixbit-d")
    )
  )
)

;TDA pixrgb-d
(define pixrgb-d
  (lambda (x y r g b d)
    (if
     (and (number? x)
          (number? y)
          (number? r)
          (number? g)
          (number? b)
          (number? d)
          (>= x 0)
          (>= y 0)
          (and (>= r 0)
               (< r 256)
          )
          (and (>= g 0)
               (< g 256)
          )
          (and (>= b 0)
               (< b 256)
          )
          (>= d 0)
      )
     (list x y r g b d)
     (raise "No es un pixrgb")
    )
  )
)

;TDA image - constructor
