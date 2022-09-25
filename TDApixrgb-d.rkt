#lang racket
(provide (all-defined-out))
;TDA pixrgb
;constructor pixrgb-d
(define (pixrgb-d x y r g b d)
  (if
     (and (and(number? x)
              (integer? x)
              (>= x 0))    
          (and(number? y)
              (integer? y)
              (>= y 0))
          (and(number? r)
              (integer? r)
              (>= r 0)
              (<= r 255))
          (and(number? g)
              (integer? g)
              (>= g 0)
              (<= g 255))
          (and(number? b)
              (integer? b)
              (>= b 0)
              (<= b 255))
          (and(number? d)
              (integer? d)
              (>= d 0)
              ))
     (list (list x y) (list r g b) d "pixrgb-d" (append-rgb->hex (list r g b)))
     (raise "No es un pixrgb")
    )
  )

;selectores
(define (rgb->getRGB rgb)
  (cadr rgb))

(define (rgb->getR rgb)
  (car(cadr rgb)))

(define (rgb->getG rgb)
  (cadr(cadr rgb)))

(define (rgb->getB rgb)
  (caddr(cadr rgb)))

(define (rgb->getHex rgb)
  (cadddr(cdr rgb)))

(define (rgb->getCoord rgb)
  (car rgb))
(define (rgb->getDepth rgb)
  (caddr rgb))



(define (transform1 num)
  (define formula (quotient num 16))
  (if(and(>= formula 0)(<= formula 9))
     (number->string (inexact->exact formula))
     (if(= formula 10)
        "A"
        (if(= formula 11)
           "B"
           (if(= formula 12)
              "C"
              (if(= formula 13)
                 "D"
                 (if(= formula 14)
                    "E"
                    (if(= formula 15)
                       "F"
                       null))))))))

(define (transform2 num)
  (define formula (*(-(exact->inexact(/ num 16))(quotient num 16)) 16))
  (if(and(>= formula 0)(<= formula 9))
     (number->string (inexact->exact formula))
     (if(= formula 10)
        "A"
        (if(= formula 11)
           "B"
           (if(= formula 12)
              "C"
              (if(= formula 13)
                 "D"
                 (if(= formula 14)
                    "E"
                    (if(= formula 15)
                       "F"
                       null))))))))

(define (append-transform num)
  (string-append (transform1 num) (transform2 num)))

(define (append-rgb->hex rgb)
  (string-append "#" (append-transform (car rgb))(append-transform (cadr rgb))(append-transform (caddr rgb))))
  



(define rgb (pixrgb-d 0 0 10 20 30 10))
rgb