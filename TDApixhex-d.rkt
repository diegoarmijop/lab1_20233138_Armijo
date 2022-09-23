#lang racket
(provide (all-defined-out))
;TDA pixhex-d
(define (pixhex-d x y hex d)

  ;Funcion que me da las posiciones int de un hex. 
  (define stringHex->integerHex
    (lambda (hex)
      (list(list(string->number(substring hex 1 2))(string->number(substring hex 2 3)))
           (list(string->number(substring hex 3 4))(string->number(substring hex 4 5)))
           (list(string->number(substring hex 5 6))(string->number(substring hex 6 7))))))
  
    (if
     (and
      (and(number? x)
          (integer? x)
          (>= x 0))
      (and(number? y)
          (integer? y)
          (>= y 0))
      (and(string? hex)
          (= (string-length hex) 7)
          (equal? (substring hex 0 1) "#")
          (number?(string->number(substring hex 1))))
      (and(number? d)
          (>= d 0)))
     (list (list x y) hex d "pixhex-d" (stringHex->integerHex hex))
     (raise "No es un pixhex-d")
    )
  )

(define (hex->getPosX pixhex-d)
  (car(car pixhex-d)))


(define pix (pixhex-d 0 0 "#123345" 4))


              

