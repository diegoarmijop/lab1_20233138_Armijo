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
(define L1 '((0 0)(0 1)(1 0)(1 1)))



;(define (lol L  aux)
  ;(if(null? L)
     ;(cons  L aux)
     ;(lol (cons(cdr(car L))(cdr L)) (cons(car(car L)) aux ))))


(define (verificar list num)
  (if(=(car(car(car list)))num)
     #t
     #f))


(define (loll img)
  (car(list(car img))))




(define (hola ex)
  (car ex))
(define ss(map hola ex))

(define (inter L)
  (list ))     


(define (lol lista aux)
  (if(null? lista)
     (cons aux lista)
     (if(null?(car lista))
        (cons(map hola lista)aux)
        (lol (cons(cdr(car lista))(list(cdr(car(cdr lista))))) (map hola lista)))))



;(define (prerotate90 L count))



(define img (image 2 2 (pixbit-d 0 0 1 23)(pixbit-d 0 1 0 120)(pixbit-d 1 0 0 120)(pixbit-d 1 1 0 110)))
(define img2 (image 2 2 (pixrgb-d 0 0 67 30 69 10)(pixrgb-d 0 1 67 30 69 20)(pixrgb-d 1 0 15 80 55 30)(pixrgb-d 1 1 90 32 60 10)))
(define img3 (image 2 2 (pixhex-d 0 0 "#FF0011" 10)(pixhex-d 0 1 "#FF0011" 10)(pixhex-d 1 0 "#FF0011" 10)(pixhex-d 1 1 "#FF0011" 10)))
 img
img2
img3

(define (image->getCoord img)
  (car(car(caddr img))))
;(image->getCoord img2)

;(map image->getCoord img)  

(define (rotate90 img)
  (list (image->getWidth img) (image->getHeight img)(juntar(reord(pre-rotate90 (caddr img) 0 '() (image->getWidth img)))) (image->getType img)))



 

;(define mapearCoord )

(define (juntar L)
  (if(null? L)
     L
     (append (car L) (juntar (cdr L)))))

(define (reord L)
  (if(null? L)
     L
     (cons (reverse(car L)) (reord (cdr L)))))


(define (pre-rotate90 L count lista n)
  (if(= count n)
    (reverse lista)
    (pre-rotate90 (cdr L) (+ count 1)(cons (filter (lambda (e)(=(car(cdr (car e)))count)) L) lista) n)))


(define (obtenerListaPix img)
  (map loll (caddr img)))


(define (arreglar pix)
  (list (rgb->getCoord pix) (rgb->getHex pix) (rgb->getDepth pix) "pixhex-d"))

(define ouu(map arreglar (caddr img2)))



(define (imgRGB->imgHex img)
  (list (image->getWidth img) (image->getHeight img)))


(caddr img2)





(car(caddr img2))

;(filter (lambda(P1)(>= (cadr P1)0))(filter (lambda(P1 )(>=(car P1)1)) L1))

;(0 1)(1 2)


(define (crop x y z w)
(filter (lambda (P1)(and(>=(cadr P1)y)(<=(cadr P1)w)))(filter (lambda(P1)(and(>=(car P1)x)(<=(car P1)z))) L1)))

(caddr img)

;(define (histogrambit lista count aux)
  ;(if(null? lista)
     ;lista

(define (histogrambit lista count aux n)
  (if(= count n)
     (reverse aux)
     (histogrambit (cdr lista)(+ count 1) (cons(append (list count) (list(length(sort(map caddr(filter (lambda(e)(= (cadr e) count)) (caddr img))) <))))aux)n)))



;(define (historgb lista count aux n)
 ; (if(= count n)
     ;(reverse aux)
     ;(histo)

;(list
;(cons "R" (list (length (sort(map rgb->getR (caddr img2)) <)) (sort(map rgb->getR (caddr img2)) <)))
;(cons "G" (list (length (sort(map rgb->getG (caddr img2)) <)) (sort(map rgb->getR (caddr img2)) <)))
;(cons "B" (sort(map rgb->getB (caddr img2)) <)))

;(sort (map rgb->getRGB (caddr img2)) (lambda(P1 P2)(<(car P1)(car P2))))
(map rgb->getRGB (caddr img2))
(car(car (map rgb->getRGB (caddr img2))))
(car(cadr (map rgb->getRGB (caddr img2))))

(define (rgbok L count)
  (if(null? L)
     count
     (if(=(car(car L))(car(cadr L)))
        (if(=(cadr(car L))(cadr(cadr L)))
           (if(=(caddr(cadr L))(caddr(cadr L)))
              (rgbok (cdr L) (+ count 1))
              count)
           count)
        count)
     )
  )

(define xdddd '(0 6))
;(map xdddd (car(car(caddr img))))


(define (veriok L)
  (if(and(filter (lambda(e)(= (car e) (rgb->getR e)))(map rgb->getRGB (caddr img2)))
         (filter (lambda(e)(= (cadr e) 30))(map rgb->getRGB (caddr img2)))
         (filter (lambda(e)(= (caddr e) 69))(map rgb->getRGB (caddr img2))))#t
                                                                            #f))
     
;(filter (lambda(e)(= (car e) (rgb->getR )))(map rgb->getRGB (caddr img2)))

;(define (histogrambit lista count aux n)
  ;(if(= count n)
     ;(reverse aux)
     ;(histogrambit (cdr lista)(+ count 1) (cons(list (map cadr (filter (lambda(e)(= (cadr e) count)) (caddr img))) (map caddr(filter (lambda(e)(= (cadr e) count)) (caddr img))))aux) n))) 
     
 

;(list (map cadr (filter (lambda(e)(= (cadr e) 1)) (caddr img))) (map caddr(filter (lambda(e)(= (cadr e) 1)) (caddr img))))

(define (contador L)
  (filter (lambda (e)(= e 120)) L))

(define (recu L count)
  (=(length L)1)
  (+ count 1)
  (recu (cdr L)(+ count 1)))
          
(define (reu x)
(filter (lambda (P1)(=(car L1)(rgb->getR x)))(map rgb->getRGB (caddr img2))))

;(sort (map car (caddr(rotate90 img2))) (lambda (P1 P2)(<(car P1)(car P2))))


(caddr (rotate90 img))


(define (cambiarCoord L1 img)
  (list (car L1) img))


;(map cambiarCoord (caddr img))
