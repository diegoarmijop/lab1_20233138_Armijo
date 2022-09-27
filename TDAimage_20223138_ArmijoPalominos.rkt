#lang racket

;Se importan todas las funciones de los otros TDAs. (pixbit-d, pixrgb-d, pixhex-d). 
(require "TDApixbit-d_20223138_ArmijoPalominos.rkt") 
(require "TDApixrgb-d_20223138_ArmijoPalominos.rkt")
(require "TDApixhex-d_20223138_ArmijoPalominos.rkt")
(provide (all-defined-out))

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
;Tipo de recursion: cola.
(define (image? width height pix)

  ;Definicion de una condicion para ordenar una lista de coord.
  ;Dom: pix x pix
  ;Rec: No tiene.
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

(define (pix->getListX pix)
    (map pix->getX pix))

 (define (pix->getX pix)
    (car(car pix)))

(define (pix->getY pix)
    (cadr(car pix)))

  (define (pix->getListY pix)
    (map pix->getY pix))

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

;Funcion que comparar dos pixeles.
(define verifyOrder
    (lambda (P1 P2)
      (if(=(car P1)(car P2))
         (<(cadr P1)(cadr P2))
         (<(car P1)(car P2))
      )
  )
)

;Funcion que setea coordenadas.
;Tipo de recursion: cola.
(define (setCoord L img listAux)
  (if(null? img)
     (reverse listAux)
     (setCoord (cdr L)(cdr img) (cons(append(list(car L))(cdr(car img)))listAux))))


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

;Funcion que verifica si una imagen esta comprimida.
;Dominio: image.
;Recorrido: boolean.
(define (compressed? img)
  (if(<(length(image->getPix img))(*(image->getWidth img)(image->getHeight img)))
     #t
     #f)
)


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

;Funcion que recorta una imagen segun un parametro.
;Dominio: image x int x int x int x int.
;Recorrido: image.
;Tipo de recursion: Cola.
(define (crop img x y z w)

  (define (findWidth L count)
    (if(=(length L)1)
       (+ count 1)
       (if(=(car L)(cadr L))
          (findWidth (cdr L) (+ count 1))
          (+ count 1)))
  )

  (define (findHeight L count)
    (if(=(length L)1)
       (+ count 1)
       (if(=(+(car L)1)(cadr L))
          (findHeight (cdr L) (+ count 1))
          (+ count 1)))
  )
  
  (define (pre-crop L x y z w)
    (filter (lambda (P1)(and(>=(cadr(car P1))y)(<=(cadr (car P1))w)))
            (filter (lambda(P1)(and(>=(car (car P1))x)(<=(car (car P1))z))) (caddr L)))
  )
  
  (if(or(> x z)(> y w))
     (raise "Ingrese una zona valida.")
     (if (and(= x z)(= y w))
         (list 1 1 (car(pre-crop img x y z w)) (make-list (length (pre-crop img x y z w)) (car(pix->getType(image->getPix img)))))
         (list (findWidth (pix->getListX(pre-crop img x y z w)) 0)
               (/(length(pix->getListY(pre-crop img x y z w)))(findHeight (pix->getListY(pre-crop img x y z w)) 0))
               (setCoord  (generateCoord (findWidth (pix->getListX(pre-crop img x y z w)) 0)
                                         (/(length(pix->getListY(pre-crop img x y z w)))(findHeight (pix->getListY(pre-crop img x y z w)) 0))
                                         0 0 '())
                          (pre-crop img x y z w) '())
               (make-list (length (pre-crop img x y z w)) (car(pix->getType(image->getPix img))))))
     )
 )

;Funcion que transforma una imagen rgb en hexadecimal.
;Dominio: image.
;Recorrido: image.
(define (imgRGB->imgHex img)

  (define (setNewPix pix)
    (list (rgb->getCoord pix) (rgb->getHex pix) (rgb->getDepth pix) "pixhex-d")
  )
  
  (if(pixmap? img)
     (list (image->getWidth img)
           (image->getHeight img)
           (map setNewPix (image->getPix img))
           (pix->getType (map setNewPix (image->getPix img))))
     (raise "Ingrese una imagen rgb.")
     )
)

;Funcion que rota una imagen 90 grados.
;Dominio: image.
;Recorrido: image.
(define (rotate90 img)

  (define (reordenar L)
    (if(null? L)
       L
       (cons (reverse(car L)) (reordenar (cdr L))))
  )

  (define (juntar L)
    (if(null? L)
       L
       (append (car L) (juntar (cdr L))))
    )

  (define (pre-rotate90 L count lista n)
    (if(= count n)
       (reverse lista)
       (pre-rotate90 (cdr L) (+ count 1)(cons (filter (lambda (e)(=(car(cdr (car e)))count)) L) lista) n))
    )
  
  (list (image->getHeight img)
        (image->getWidth img)
        (setCoord (generateCoord (image->getHeight img)(image->getWidth img)0 0 '())
                  (juntar(reordenar(pre-rotate90 (image->getPix img) 0 '() (image->getWidth img)))) '())
        (image->getType img))
)

;Funcion que invierte el bit en una imagen.
;Dominio: image.
;Recorrido: image.
(define (invertColorBit img)

  (define (arr L v)
    (if(null? L)
       L
       (cons(cons (car v)(cdr(cdr(reverse(car L)))))(arr (cdr L)(cdr v)))))

  (define (juntarAllBit L)
    (if(null? L)
       L
       (cons(reverse (car L))(juntarAllBit (cdr L)))))

  (define (agregarBitOpuesto L)
    (if(null? L)
       L
       (cons
        (append(cons (car(reverse (car L))) (cdr(cdr(car L))))(list(car(cdr(car L)))))
        (agregarBitOpuesto (cdr L)))))

  (define (juntarBit c L)
    (if(null? L)
       L
       (cons(cons (car c)(car L))(juntarBit (cdr c)(cdr L)))))

  
  (if(bitmap? img)
     (list (image->getWidth img)
           (image->getHeight img)
           (juntarAllBit(arr
                         (juntarBit(generateCoord (image->getWidth img) (image->getHeight img) 0 0 '())
                                   (agregarBitOpuesto (image->getPix img)))
                         (map car (juntarAllBit (juntarBit (generateCoord (image->getWidth img) (image->getHeight img) 0 0 '())
                                                           (agregarBitOpuesto (image->getPix img)))
                                                )
                              )
                         )
                        )
           )
     (raise "Ingrese in bitmap.")
     )
)



;Funcion que muestra el histograma de bit.
;Dom: img.
;Rec: histogram bit.
;Recursion de cola.
(define (histogram img)
  
  (define (histogrambit lista count aux n)
  (if(= count n)
     (reverse aux)
     (histogrambit (cdr lista)(+ count 1) (cons(append (list count) (list(length(sort(map caddr(filter (lambda(e)(= (cadr e) count)) (caddr img))) <))))aux)n)))
  
  (if(bitmap? img)
     (histogrambit (image->getPix img) 0 '() 2)
     (raise "Aun no se implementa para otro tipo de pix")))
        
        
      
        



;(cons (length(hola (cddr(cdr(sort  (map rgb->getRGB (caddr img2)) ou))))) (car (cdddr(sort  (map rgb->getRGB (caddr img2)) ou))))