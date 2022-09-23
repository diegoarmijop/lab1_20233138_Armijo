#lang racket

(require "TDApixbit-d.rkt")

(define (image width height . pix)
    (if
     (and (and(list? pix)
          (list? (car pix))
          (<= (length pix) (* width height)))
          (and(number? width)
              (integer? width)
              (>= width 0))
          (and(number? height)
              (integer? height)
              (>= height 0))
     )
     (if(=(length pix)4)
        (list width height pix (getType pix) "compress")
        (if(<(length pix)4)
           (list width height pix (getType pix) "no-compress")
           null))
     (raise "No es una imagen")
     )
)






















;Descripcion: Funcion que recibe una lista de pixeles y retorna una lista con el nombre del tipo de cada pixel (bit, rgb, hex).
;Entrada: Lista de pixeles. 
;Salida: Lista con el nombre (string) del tipo de cada pixel. 
(define (getType listPix)
  (if(null? listPix)
     null
     (cons (image->getTypePix listPix) (getType(cdr listPix)))
  )
)

;Descripcion: Funcion que entrega 
;Entrada: pixel.
;Salida: Tipo de pixel (string).
(define image->getTypePix (lambda(pix)
                         (cadddr (car pix))
                          )
)

;Descripcion: Funcion que recibe una lista con el tipo de pixel 
;Entrada: Lista con el tipo de cada pixel.
;Salida: Boolean.
(define (esBitmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixbit-d"))
         #f
         (esBitmap?  (cdr lista))
     )        
  )
)

(define (bitmap? img)
  (esBitmap? (cadddr img)))



(define (esPixmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixrgb-d"))
         #f
         (esPixmap? (cdr lista))
     )        
  )
)


(define (pixmap? image)
  (esPixmap?(cadddr image))
)


(define (esHexmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixhex-d"))
         #f
         (esHexmap? (cdr lista))
     )        
  )
)

(define (hexmap? image)
  (esHexmap?(cadddr image))
)








;necesito encapsular
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




(define L2 '(0 0 0 1 1 1 2 2 2))
(define L3 '(0 1 2 0 1 2 0 1 2))

(define L4 '((0 2)(1 0)(1 1)(1 2)(2 0) (2 1) (2 2)))




(define (reordenar lista)
  (if(null? lista)
     null
     (if(equal? (car(car lista)) (car(car(cdr lista))))
        (append (reverse2 (list(car lista))(reordenar (cdr lista))))
        (if(not(equal? (car(car lista)) (car(car(cdr lista)))))
           (reverse2 (cdr lista))
           (reordenar(cdr lista))
        ))))


(define (sublist list start number)
  (cond ((> start 0) (sublist (cdr list) (- start 1) number))
        ((> number 0) (cons (car list)
                      (sublist (cdr list) 0 (- number 1))))
        (else '())))



(define get-n-items
    (lambda (lst num)
        (if (> num 0)
            (cons (car lst) (get-n-items (cdr lst) (- num 1)))
            '()))) ;'

(define slice
    (lambda (lst start count)
        (if (> start 1)
            (slice (cdr lst) (- start 1) count)
            (get-n-items lst count))))




(define (reverse2 lst)
  (if (null? lst)
     null
    (append (reverse2 (cdr lst))  (append(list(list(car lst)) lst)))))


     





(define img (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 0 20)(pixbit-d 1 0 1 30)(pixbit-d 1 1 1 4)))
;(define img2 (image 2 2 (pixrgb-d 0 0 40 40 40 10)(pixrgb-d 0 1 40 40 40 10)(pixrgb-d 1 0 40 40 40 10)(pixrgb-d 1 1 40 40 40 10)))
;(define pix (pixhex-d 0 0 "#123456" 4)) 
(define listaPix (bit->getType (car(caddr img))))
(define L1 (cadddr img))
(define bit (pixbit-d 0 0 1 10))



;(define (reagrupar L lista count)
  ;(if(null? L)
    ; lista
     ;(reagrupar (cortar L count) (cons (reverse(cambio L '())) lista) count)))
     
     




(define (cortar lista count)
  (if(= count 0)
     lista
     (cortar (cdr lista) (- count 1))))


;(reagrupar (cdr L) (cons( (car L) (car lista))(cdr lista)) count (+ aux 1))
(define lista1 (map bit->getCoord (caddr img)))

(define (cambio lista nueva)
  (if(=(length lista)1)
     (append nueva (list(car lista)))
  (if(= (car (car lista))(car(car(cdr lista))))
     (cambio (cdr lista) (reverse(cons (car lista) nueva)))
     (append nueva (list(car lista)))
  )))


;(append nueva (list(car lista)))
;reagrupar para flipH
(define (reagrupar L lista count aux)
  (if(null? L)
     (reverse (cons aux lista))
     (if(= count (car(car L)))
        (reagrupar (cdr L) lista count (cons (car L) aux))
        (reagrupar (cdr L) (cons aux lista) (+ count 1) (cons (car L)'())))))
        
     
     

;flipV
(define (reagrupar2 L lista count)
  (if(null? L)
     lista
     (reagrupar2 (cortar L count) (cons (slice L 0 count) lista) count)))



(define (setCoord bit coord)
  (pixbit-d coord (bit->getBit bit)))




