#lang racket
(provide (all-defined-out))

(*(-(exact->inexact(/ 16 16))(quotient 16 16)) 16)
(quotient 189 16)
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
     (list (list x y) (list r g b) d "pixrgb-d" (agrupar(list r g b)))
     (raise "No es un pixrgb")
    )
  )

(define (rgb->getR rgb)
  (car(car(cdr rgb))))

(define (rgb->getG rgb)
  (cadr(car(cdr rgb))))

(define (rgb->getB rgb)
  (caddr(car(cdr rgb))))

(define (rgb->getRGB rgb)
  (cadr rgb))

;(define (rgbR->hexR rgb)
  ;(if(=(*(-(exact->inexact(/ (rgb->getR rgb) 16))(quotient(rgb->getR rgb) 16)) 16))
     
     ;)
  


(define (reglas num)
  (define const(*(-(exact->inexact(/ num 16))(quotient num 16)) 16))
  (if(and(>= const 0)(<= const 9))
     (number->string (inexact->exact const))
     (if(= const 10)
        "A"
        (if(= const 11)
           "B"
           (if(= const 12)
              "C"
              (if(= const 13)
                 "D"
                 (if(= const 14)
                    "E"
                    (if(= const 15)
                       "F"
                       null))))))))
        
(define (reglas2 num)
  (define const (quotient num 16))
  (if(and(>= const 0)(<= const 9))
     (number->string (inexact->exact const))
     (if(= const 10)
        "A"
        (if(= const 11)
           "B"
           (if(= const 12)
              "C"
              (if(= const 13)
                 "D"
                 (if(= const 14)
                    "E"
                    (if(= const 15)
                       "F"
                       null))))))))
     
  

(define (juntar num)
  (string-append (reglas2 num) (reglas num)))


(define (agrupar rgb)
  (string-append "#" (juntar (car rgb)) (juntar (cadr rgb))  (juntar (caddr rgb))))
  

(define rgb (pixrgb-d 0 1 67 45 133 4))


                  