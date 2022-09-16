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

;TDA pixhex-d
(define pixhex-d
  (lambda (x y hex d)
    (if
     (and
      (number? x)
      (number? y)
      (string? hex)
      (number? d)
      (>= x 0)
      (>= y 0)
      (>= d 0)
      )
     (list x y hex d)
     (raise "No es un pixhex-d")
    )
  )
)

;TDA image - constructor

(define image
  (lambda (x y . pix)
    (if
     (and (list? pix)
          (list? (car pix)))
     (list x y pix)
     (raise "No es una imagen")
     )))

;TDAbitmap?
(define bitmap?
  (lambda(posiblebitmap)
    (if
     (and (list? posiblebitmap)
          (not (null? posiblebitmap))
          (number? (car posiblebitmap))
          (number? (cadr posiblebitmap))
          (and (list? (caddr posiblebitmap))
               (not (null? (caddr posiblebitmap)))))
     (raise "Es un bitmap")
     (raise "No es un bitmap"))
    )
  )



(define img (image 2 2 (pixbit-d 0 1 0 2) (pixbit-d 1 1 0 2)))