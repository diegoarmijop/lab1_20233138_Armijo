#lang racket
(provide (all-defined-out))

;constructor pixbit-d
(define (pixbit-d x y bit depth)
  (if
     (and (and(number? x)
              (integer? x)
              (>= x 0))
          (and(number? y)
              (integer? y)
              (>=  y 0))
          (and(number? bit)
              (or(= bit 0)(= bit 1)))
          (and(number? depth)
              (>= depth 0))
      )
     (if(= bit 0)
        (list (list x y) bit depth "pixbit-d" 1)
        (list (list x y) bit depth "pixbit-d" 0)
        )
     (raise "No es un pixbit-d")
  )
)

;Selectores
(define (bit->getCoord bit)
  (car bit))

(define (bit->getBit bit)
  (cadr bit))

(define (bit->getDepth bit)
  (caddr bit))

(define (bit->getType bit)
  (cadddr bit))

(define (bit->getBitOpuesto bit)
  (cadddr(cdr bit)))


(define bit (pixbit-d 0 0 1 10))



