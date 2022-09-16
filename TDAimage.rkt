#lang racket

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

;selector posX
(define (bit->getPosX pixbit-d)
  (car (car pixbit-d)))

;selector posY
(define (bit->getPosY pixbit-d)
  (car(cadr pixbit-d)))

;selector bit
(define (bit->getBit pixbit-d)
  (cadr pixbit-d))

;selector depth
(define (bit->getDepth)
  (caddr pixbit-d))

;selector typeBit
(define (bit->getType pixbit-d)
  (cadddr pixbit-d))

;selector bit opuesto
(define (bit->getBitOpuesto pixbit-d)
  (cadddr(cdr pixbit-d)))




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

(define(hex->getType pixhex-d)
  (cadddr pixhex-d))





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
     (list (list x y) (list r g b) d "pixrgb-d")
     (raise "No es un pixrgb")
    )
  )

(define rgb->getPosX
  (lambda(pixrgb-d)
    (car(car pixrgb-d))))


(define (image x y . pix)
    (if
     (and (list? pix)
          (list? (car pix))
          (<= (length pix) (* x y))
          (not(null? pix))
          )
               
     (list x y pix)
     (raise "No es una imagen")
     ))


(define (getType image)
  (if(null? image)
     null
     (cons (image->getTypePix(image->getListPix image)) (getType(cdr image))))
  )

(define image->getListPix (lambda (image)
                            (caddr image)))


(define image->getTypePix (lambda(image)
                         (cadddr (car image))))

(define (bitmap? lista)
  (if(null? lista)
     #t
     (if (not(equal?(car lista)"pixbit-d?"))
         #f
         (bitmap?(cdr lista)))        
  )
)


(define (posX lista count1 count2)
  (if(=(length lista) 1)
     count2
     (if(= (car lista) (cadr lista ))
         (posX (cdr lista) (+ count1 1) count2)
         (if(= (+(car lista)1) (cadr lista ))
            (posX (cdr lista) count1 (+ count2 1))
            (+ count1 1)))))

(define (posY lista count1 count2)
  (if(=(length lista) 1)
     count1
     (if(=(+(car lista)1) (cadr lista))
        (posY (cdr lista) (+ count1 1) count2)
        (if(not(=(+(car lista)1) (cadr lista)))
            (posY (cdr lista) count1 (+ count2 1))
            count1))))





(define img (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 0 20)(pixbit-d 1 0 1 30)(pixbit-d 1 1 1 4)))
(define pix (pixhex-d 0 0 "#123456" 4))
(define listaPix (bit->getType (car(caddr img))))