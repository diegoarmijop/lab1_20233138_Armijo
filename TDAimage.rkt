#lang racket

(require "TDApixbit-d.rkt")
(require "TDApixrgb-d.rkt")
(require "TDApixhex-d.rkt")



;constructor image
(define (image width height . pix)
  
  (define (pix->getType pixels)
    (map cadddr pixels))
  
  (if(and (and(number? width)
              (integer? width)
              (>= width 0))
          (and(number? height)
              (integer? height)
              (>= height 0))
          (and(list? pix)
              (not(null? pix))
              (<=(length pix)(* width height)))
          )
     (list width height pix (pix->getType pix))
     (raise "No es una imagen")))


(define (image->getType img)
  (cadddr img))

(define (bitmap? image)
  (esBitmap? (image->getType image)))

(define (esBitmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixbit-d"))
         #f
         (esBitmap?  (cdr lista))
     )        
  )
)

(define (pixmap? image)
  (esPixmap? (image->getType image)))

(define (esPixmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixrgb-d"))
         #f
         (esPixmap?  (cdr lista))
     )        
  )
)

(define (hexmap? image)
  (esHexmap? (image->getType image)))

(define (esHexmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixhex-d"))
         #f
         (esHexmap?  (cdr lista))
     )        
  )
)

(define (image->getPix img)
  (caddr img))

(define (image->getWidth img)
  (car img))

(define (image->getHeight img)
  (cadr img))

(define (preFlipH L lista count aux)
  (if(null? L)
     (reverse (cons aux lista))
     (if(= count (car(car(car L))))
        (preFlipH (cdr L) lista count (cons (car L) aux))
        (preFlipH (cdr L) (cons aux lista) (+ count 1) (cons (car L)'())))))

(define (flipH img)
    (list (image->getWidth img)(image->getHeight img)(preFlipH (image->getPix img) '() 0 '()) (image->getType img)))

(define (preFlipV L lista count aux)
  (if(null? L)
     (cons aux lista)
     (if(= count (car(car(car L))))
        (preFlipV (cdr L) lista count (reverse(cons (car L) aux)))
        (preFlipV (cdr L) (cons aux lista) (+ count 1) (cons (car L)'())))))

(define (flipV img)
  (list (image->getWidth img)(image->getHeight img)(preFlipV (image->getPix img) '() 0 '()) (image->getType img)))


  
(define (preFlipVk L lista count aux)
  (if(null? L)
     (cons aux lista)
     (if(= count (car(car(car L))))
        (preFlipVk (cdr L) lista count (reverse(cons (car L) aux)))
        (preFlipVk (cdr L) (reverse(cons aux lista)) (+ count 1) (cons (car L)'())))))



(define (xddd img)
  (car(car(car img))))

(define ex '(((1 0)(1 1))((0 0)(0 1))))
(define ex2'((1 0)(1 1)(0 0)(0 1)))

;(define (lol L  aux)
  ;(if(null? L)
     ;(cons  L aux)
     ;(lol (cons(cdr(car L))(cdr L)) (cons(car(car L)) aux ))))


(define (verificar list num)
  (if(=(car(car(car list)))num)
     #t
     #f))




(define (hola ex)
  (car ex))
(define ss(map hola ex))

     


(define (lol lista aux)
  (if(null? lista)
     (cons aux lista)
     (if(null?(car lista))
        (cons(map hola lista)aux)
        (lol (cons(cdr(car lista))(list(cdr(car(cdr lista))))) (map hola lista)))))





(define img (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 1 0 0 10)(pixbit-d 1 1 0 10)))
(define img2 (image 2 2 (pixrgb-d 0 0 30 30 60 10)(pixrgb-d 0 1 30 30 60 10)(pixrgb-d 1 0 30 30 60 10)(pixrgb-d 1 1 30 30 60 10)))
(define img3 (image 2 2 (pixhex-d 0 0 "#FF0011" 10)(pixhex-d 0 1 "#FF0011" 10)(pixhex-d 1 0 "#FF0011" 10)(pixhex-d 1 1 "#FF0011" 10)))
 img
img2
img3




     
     
          