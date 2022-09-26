#lang racket

;Se importan todas las funciones de los otros TDAs. (pixbit-d, pixrgb-d, pixhex-d). 
(require "TDApixbit-d.rkt") 
(require "TDApixrgb-d.rkt")
(require "TDApixhex-d.rkt")

;-------------------------------- PARÁMETROS --------------------------------

;Descripción de los parámetros a recibir para la funcion constructor.
;Width -> Entero que indica el ancho de la imagen.
;Height -> ENtero que indica el alto de la imagen.
;. Pix -> Lista que contiene los pixeles, estos pueden ser de tipo bit, rgb o hex.


;-------------------------------- CONSTRUCTOR --------------------------------

;Funcion constructora de tipo variadic que recibe n-parametros de entrada. Esta se encarga
;de generar una imagen valida.
;Dominio: Width (int) x Height (int) x Pix (list).
;Recorrido: image
;Tipo de recursión: No aplica.
;Ejemplo de uso: (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 0 20)(pixbit-d 1 0 1 30)(pixbit-d 1 1 1 40)).

(define (image width height . pix)

  ;Funcion que compara dos coordenadas.
  (define verifyOrder
    (lambda (P1 P2)
      (if(=(car(car P1))(car(car P2)))
         (<(cadr(car P1))(cadr(car P2)))
         (<(car(car P1))(car(car P2))))
      )
  )
  
  (if(image? width height pix)
     (list width height (sort pix verifyOrder) (pix->getType pix))
     (raise "No es una imagen")))


;Funcion que valida si los argumentos entregados corresponden a un TDAimage.
;Dominio: width x height x pix.
;Recorrido: boolean.
;Tipo de recursion: No aplica.
(define (image? width height pix)

   (define verifyOrder
    (lambda (P1 P2)
      (if(=(car(car P1))(car(car P2)))
         (<(cadr(car P1))(cadr(car P2)))
         (<(car(car P1))(car(car P2))))
      )
  )

  (define (pix->getX pix)
    (car(car pix)))

  (define (pix->getY pix)
    (cadr(car pix)))
  
  (define (pix->getListX pix)
    (map pix->getX pix))

  (define (pix->getListY pix)
    (map pix->getY pix))

  (define (verifyX L count)
  (if(= (length L) 1)
     (+ count 1)
     (if(=(car L)(cadr L))
        (verifyX (cdr L) (+ count 1))
        (if(=(+(car L)1)(cadr L))
           (verifyX (cdr L) (+ count 1))
           #f))))

  (define (verifyY L count)
  (if(=(length L)1)
     (+ count 1)
     (if(=(+(car L)1)(cadr L))
        (verifyY (cdr L)(+ count 1))
        (if(=(cadr L)0)
        (verifyY (cdr L)(+ count 1))
        #f))))
 
  (if(and (and(number? width)
              (integer? width)
              (>= width 0))
          (and(number? height)
              (integer? height)
              (>= height 0))
          (and(list? pix)
              (not(null? pix))
              (<=(length pix)(* width height))
              (= (verifyX (pix->getListX (sort pix verifyOrder)) 0) (* width height))
              (= (verifyY (pix->getListY (sort pix verifyOrder)) 0)(* width height))
              )
                 
      )
     #t
     #f)
)

;-------------------------------- SELECTORES --------------------------------

;Funcion selectora para obtener el ancho.
;Dominio: image.
;Recorrido: integer. 
;Tipo de recursion: No aplica.
(define (image->getWidth img)
  (car img)
)

;Funcion selectora para obtener el alto.
;Dominio: image.
;Recorrido: integer. 
;Tipo de recursion: No aplica.
(define (image->getHeight img)
  (cadr img))


;Funcion selectora para obtener una lista con el type de cada pixel. ("pixbit-d", "pixrgb-d" o "pixhex-d").
;Dominio: Pix (list de list).
;Recorrido: list.
;Tipo de recursion: No aplica.
(define (pix->getType pixels)
    (map cadddr pixels)
)

;Funcion selectora para obtener la lista con los tipos de pixel.
;Dominio: image.
;Recorrido: list.
;Tipo de recursion: No aplica.
;Ejemplo de uso: (image->getType (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 0 20)(pixbit-d 1 0 1 30)(pixbit-d 1 1 1 40)))).
(define (image->getType img)
  (cadddr img)
)

;Funcion selectora para obtener la lista con los pixeles.
;Dominio: image.
;Recorrido: list de list.
;Tipo de recursion: No aplica.
;Ejemplo de uso:(image->getPix (image 2 2 (pixbit-d 0 0 1 10)(pixbit-d 0 1 0 20)(pixbit-d 1 0 1 30)(pixbit-d 1 1 1 40)))).
(define (image->getPix img)
  (caddr img)
)

(define (selectX pix)
  (car(car(car pix))))



;-------------------------------- FUNCIONALIDADES --------------------------------


;Funcion que genera coordenadas ordenas en base al width y height.
;Dominio: width (int) x height (int) x (int) x (int) x (list)
;Recorrido: lista.
;Tipo de recursion: cola.
;Ejemplo de uso: (generateCoord 2 2 0 0 '())
(define(generateCoord width height x y lista)
  (if(= height x)
     (reverse lista)
     (if(= width y)
        (generateCoord width height (+ x 1) 0 lista)
        (generateCoord width height x (+ y 1) (cons (list x y) lista)))
     )
)

;Funcion que indica si una imagen es un bitmap.
;Dominio: image.
;Recorrido: boolean.
(define (bitmap? image)
  (esBitmap? (image->getType image))
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

;Funcion que indica si una imagen es un pixmap.
;Dominio: image.
;Recorrido: boolean.
(define (pixmap? image)
  (esPixmap? (image->getType image))
)

(define (esPixmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixrgb-d"))
         #f
         (esPixmap?  (cdr lista))
     )        
  )
)

;Funcion que indica si una imagen es un hexmap.
;Dominio: image.
;Recorrido: boolean.
(define (hexmap? image)
  (esHexmap? (image->getType image))
)

(define (esHexmap? lista)
  (if(null? lista)
     #t
     (if (not(equal? (car lista) "pixhex-d"))
         #f
         (esHexmap?  (cdr lista))
     )        
  )
)

;Funcion que agrupa.
;Dominio: list.
;Recorrido: list.
;Tipo de recursion: natural.
(define (agrupar L)
  (if(null? L)
     L
     (append (car L) (agrupar (cdr L)))))


;Funcion que permite invertir una imagen horizontalmente.
;Dominio: image
;Recorrido: image
;Tipo de recursion: cola. 
(define (flipH img)
  
  (define (preFlipH L lista count aux)
  (if(null? L)
     (reverse (cons aux lista))
     (if(= count (selectX L))
        (preFlipH (cdr L) lista count (cons (car L) aux))
        (preFlipH (cdr L) (cons aux lista) (+ count 1) (cons (car L)'())))))
  

  (list (image->getWidth img)
        (image->getHeight img)
        (setCoord (sort (map car (image->getPix img)) verifyOrder) (agrupar(preFlipH (image->getPix img) '() 0 '())) '())
        (image->getType img))
)


;Funcion que permite invertir una imagen verticalmente.
;Dominio: image.
;Recorrido: image.
;Tipo de recursion: cola.  
(define (flipV img)
  
  (define (preFlipV pix lista count aux)
  (if(null? pix)
     (cons aux lista)
     (if(= count (selectX pix))
        (preFlipV (cdr pix) lista count (reverse(cons (car pix) aux)))
        (preFlipV (cdr pix) (cons aux lista) (+ count 1) (cons (car pix)'())))
     )
   )
  
  (list (image->getWidth img)
        (image->getHeight img)
        (setCoord (sort (map car (image->getPix img)) verifyOrder) (agrupar(preFlipV (image->getPix img) '() 0 '()))'())
        (image->getType img))
)


  
;(define (preFlipVk L lista count aux)
  ;(if(null? L)
     ;(cons aux lista)
     ;(if(= count (car(car(car L))))
        ;(preFlipVk (cdr L) lista count (reverse(cons (car L) aux)))
        ;(preFlipVk (cdr L) (reverse(cons aux lista)) (+ count 1) (cons (car L)'())))))



;(define (xddd img)
  ;(car(car(car img))))

(define ex '(((1 0)(1 1))((0 0)(0 1))))
;(define ex2'((1 0)(1 1)(0 0)(0 1)))
;(define L1 '((0 0)(0 1)(1 0)(1 1)(2 0)(2 1)))



;(define (lol L  aux)
  ;(if(null? L)
     ;(cons  L aux)
     ;(lol (cons(cdr(car L))(cdr L)) (cons(car(car L)) aux ))))


;(define (verificar list num)
  ;(if(=(car(car(car list)))num)
     ;#t
     ;#f))


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



(define img (image 2 3 (pixbit-d 0 0 1 10)(pixbit-d 0 1 0 10)(pixbit-d 1 0 1 30)(pixbit-d 1 1 1 50)
                   (pixbit-d 2 0 0 50)(pixbit-d 2 1 0 50))) 
(define img2 (image 3 3 (pixrgb-d 0 0 67 30 69 10)(pixrgb-d 0 1 67 30 69 20)(pixrgb-d 0 2 15 80 55 30)
                    (pixrgb-d 1 0 90 32 60 10)(pixrgb-d 1 1 90 32 60 10)(pixrgb-d 1 2 90 32 60 10)
                    (pixrgb-d 2 0 90 32 60 10)(pixrgb-d 2 1 90 32 60 10)(pixrgb-d 2 2 90 32 60 10)))
(define img3 (image 2 2 (pixhex-d 0 0 "#FF0011" 10)(pixhex-d 0 1 "#FF0011" 10)(pixhex-d 1 0 "#FF0011" 10)(pixhex-d 1 1 "#FF0011" 10)))
 img
img2
img3

(define (image->getCoord img)
  (car(car(caddr img))))
(image->getCoord img2)
(define (oi img)
  (car img))
(define verifyOrder
    (lambda (P1 P2)
      (if(=(car P1)(car P2))
         (<(cadr P1)(cadr P2))
         (<(car P1)(car P2))
      )
  ))
(sort (map oi (caddr img)) verifyOrder) 





(define (rotate90 img)
  (list (image->getHeight img)(image->getWidth img) (setCoord (generateCoord (image->getHeight img)(image->getWidth img)0 0 '()) (juntar(reord(pre-rotate90 (caddr img) 0 '() (image->getWidth img)))) '()) (image->getType img)))




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


(define (setCoord L img c)
  (if(null? img)
     (reverse c)
     (setCoord (cdr L)(cdr img) (cons(append(list(car L))(cdr(car img)))c))))


(car(caddr img2))

;(filter (lambda(P1)(>= (cadr P1)0))(filter (lambda(P1 )(>=(car P1)1)) L1))

;(0 1)(1 2)

(define (pre-crop L x y z w)
 (filter (lambda (P1)(and(>=(cadr(car P1))y)(<=(cadr (car P1))w)))(filter (lambda(P1)(and(>=(car (car P1))x)(<=(car (car P1))z))) (caddr L))))

(define (pix->getListX pix)
    (map pix->getX pix))

 (define (pix->getX pix)
    (car(car pix)))

(define (findWidth L count)
  (if(=(length L)1)
     (+ count 1)
     (if(=(car L)(cadr L))
        (findWidth (cdr L) (+ count 1))
        (+ count 1))))

(define (pix->getY pix)
    (cadr(car pix)))

  (define (pix->getListY pix)
    (map pix->getY pix))

(define (findHeight L count)
  (if(=(length L)1)
     (+ count 1)
     (if(=(+(car L)1)(cadr L))
        (findHeight (cdr L) (+ count 1))
        (+ count 1))))




(define (crop img x y z w)
  (if(or(> x z)(> y w))
     (raise "Ingrese una zona valida.")
     (if (and(= x z)(= y w))
         (list 1 1 (car(pre-crop img x y z w)) (make-list (length (pre-crop img x y z w)) (car(pix->getType(image->getPix img)))))
         (list (findWidth (pix->getListX(pre-crop img x y z w)) 0)(/(length(pix->getListY(pre-crop img x y z w)))(findHeight (pix->getListY(pre-crop img x y z w)) 0)) (setCoord  (generateCoord (findWidth (pix->getListX(pre-crop img x y z w)) 0) (/(length(pix->getListY(pre-crop img x y z w)))(findHeight (pix->getListY(pre-crop img x y z w)) 0)) 0 0 '())  (pre-crop img x y z w) '()) (make-list (length (pre-crop img x y z w)) (car(pix->getType(image->getPix img))))))))

;(define (crop L x y z w)
;(filter (lambda (P1)(and(>=(cadr P1)y)(<=(cadr P1)w)))(filter (lambda(P1)(and(>=(car P1)x)(<= (car P1)z))) L)))

(caddr img)
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
;(crop L1 1 0 2 1)
;'((1 0) (1 1) (2 0) (2 1))
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
          
;(define (reu x)
;(filter (lambda (P1)(=(car L1)(rgb->getR x)))(map rgb->getRGB (caddr img2))))

;(sort (map car (caddr(rotate90 img2))) (lambda (P1 P2)(<(car P1)(car P2))))


;(caddr (rotate90 img))


(define (cambiarCoord L1 img)
  (list (car L1) img))


;(map cambiarCoord (caddr img))

;holajean L newElement
;(append (car newElement) (cdr L))


(define L7 '((0 0)(2 1)(1 1)(0 1)))
(sort L7 (lambda(P1 P2)(<(car P1)(car P2))))
;(define L8 (caddr(rotate90 img)))


(define (setNewCoord L img)
  (list L img))

;(define (invertColorBit img)
  ;(;list (image->getWidth img) (image->getHeight img ) (image->getPix img))



(define (ff L)
  (if(null? L)
     L
     (cons(append(cons (car(reverse (car L))) (cdr(cdr(car L))))(list(car(cdr(car L))))) (ff (cdr L)))))


(define (juntarBit c L)
  (if(null? L)
     L
     (cons(cons (car c)(car L))(juntarBit (cdr c)(cdr L)))))

(cons (map car (reverse(reverse(ff (caddr img)))))(juntarBit (generateCoord 2 3 0 0 '()) (ff (caddr img))))


(define (arr L v)
  (if(null? L)
     L
     (cons(cons (car v)(cdr(cdr(reverse(car L)))))(arr (cdr L)(cdr v)))))
     
(define (lolll L)
  (if(null? L)
     L
     (cons(reverse (car L))(lolll (cdr L)))))

;(map car (reverse(car(juntarBit (lista-coordenadas 2 3 0 0 '()) (ff (caddr img))))))


;(map car (lolll (juntarBit (lista-coordenadas 2 3 0 0 '()) (ff (caddr img)))))
;(lolll(arr (juntarBit (lista-coordenadas 2 3 0 0 '()) (ff (caddr img)))(map car (lolll (juntarBit (lista-coordenadas 2 3 0 0 '()) (ff (caddr img)))))))