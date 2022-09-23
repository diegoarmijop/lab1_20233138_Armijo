#lang racket

(require "TDApixbit-d.rkt")
(require "TDApixrgb-d.rkt")
(require "TDApixhex-d.rkt")



;constructor image
(define (image width height . pix)
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


;Selector
(define (image->getType image)
  (cadddr image))


(define (pix->getType pixels)
    (map cadddr pixels)
)


(define (esBitmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixbit-d"))
         #f
         (esBitmap?  (cdr lista))
     )        
  )
)

(define (bitmap? image)
  (esBitmap? (image->getType image)))





(define img (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 1 10)(pixbit-d 1 0 0 10)(pixbit-d 1 1 0 10)))
 img
     
     
          