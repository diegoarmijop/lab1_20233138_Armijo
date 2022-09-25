#lang racket

(provide (all-defined-out))

;constructor TDApixbit-d
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
        (list (list x y) bit depth "pixbit-d" 1 )
        (list (list x y) bit depth "pixbit-d" 0 )
        )
     (raise "No es un pixbit-d")
  )
)


;Funcion de pertenencia.
(define (pixbit-d? posiblePixbit-d)
  (if(and(and(number? (car(car posiblePixbit-d)))
             (integer?(car(car posiblePixbit-d))))
         (and(number? (cadr(car posiblePixbit-d)))
             (integer?(cadr(car posiblePixbit-d))))
         (and(number? (cadr posiblePixbit-d))
             (integer?(cadr posiblePixbit-d))
             (or(=(cadr posiblePixbit-d)0)(=(cadr posiblePixbit-d)1)))
         (and(number?(caddr posiblePixbit-d))
             (integer? (caddr posiblePixbit-d)))
         (and(string? (cadddr posiblePixbit-d))
             (string=? (cadddr posiblePixbit-d) "pixbit-d"))
         (if(=(cadr posiblePixbit-d)0)
            (=(cadddr(cdr posiblePixbit-d))1)
            (if(=(cadr posiblePixbit-d)1)
               (=(cadddr(cdr posiblePixbit-d))0)
               #f))       
     )
     #t
     #f
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

(define (bit->getOpuesto bit)
  (cadddr(cdr bit)))



;bit ejemplo
(define bit (pixbit-d 0 1 1 10))
bit
  



     