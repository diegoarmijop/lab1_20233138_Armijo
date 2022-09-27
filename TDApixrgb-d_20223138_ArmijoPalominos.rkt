#lang racket
(provide (all-defined-out))

;-------------------------------- PARÁMETROS --------------------------------

;Descripción de los parámetros a recibir para la funcion constructor.
;X -> Entero que indica la posicion X del pixel.
;Y -> Entero que indica la posicion Y del pixel.
;R -> Entero que indica el color R de un pixel.
;G -> Entero que indica el color G de un pixel.
;B -> Entero que indica el color B de pixel.
;D -> Entero que indica la profundidad de un pixel.



;-------------------------------- CONSTRUCTOR --------------------------------

;Funcion constructora. Esta se encarga  de generar un pixbit-d valido.
;Dominio: X (int) x Y (int) x R(C) x G(C) x B(C) x D(int).
;Recorrido: pixrgb-d.
;Tipo de recursión: No aplica.
;Ejemplo de uso: (pixrgb-d 0 0 45 49 100 10).

(define (pixrgb-d x y r g b d)
  (if(pixrgb-d? x y r g b d)
     (list (list x y) (list r g b) d "pixrgb-d" (append-rgb->hex (list r g b)))
     (raise "No es un pixrgb")
    )
  )

(define (pixrgb-d? x y r g b d)
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
     #t
     #f)
)
  

;Funcion selectora que obtiene el color rgb de un pixrgb.
;Dom: pixrgb-d
;Rec: list
(define (rgb->getRGB rgb)
  (cadr rgb))

;Funcion selectora que obtiene el color r de un pixrgb.
;Dom: pixrgb-d
;Rec: integer
(define (rgb->getR rgb)
  (car(cadr rgb)))

;Funcion selectora que obtiene el color g de un pixrgb.
;Dom: pixrgb-d
;Rec: integer
(define (rgb->getG rgb)
  (cadr(cadr rgb)))

;Funcion selectora que obtiene el color b de un pixrgb.
;Dom: pixrgb-d
;Rec: integer
(define (rgb->getB rgb)
  (caddr(cadr rgb)))

;Funcion selectora que obtiene el color hex de un pixrgb.
;Dom: pixrgb-d
;Rec: string
(define (rgb->getHex rgb)
  (cadddr(cdr rgb)))

;Funcion selectora que obtiene las coord de un pixrgb.
;Dom: pixrgb-d
;Rec: list
(define (rgb->getCoord rgb)
  (car rgb))

;Funcion selectora que obtiene el d de un pixrgb.
;Dom: pixrgb-d
;Rec: integer
(define (rgb->getDepth rgb)
  (caddr rgb))


;Funcion para trasformar un numero rgb a hex (parte 1).
;dom: integer.
;rec: string.
;Ejemplo de uso: (transform1 90)
(define (transform1 num)
  (define formula (quotient num 16))
  (if(and(>= formula 0)(<= formula 9))
     (number->string (inexact->exact formula))
     (if(= formula 10)
        "A"
        (if(= formula 11)
           "B"
           (if(= formula 12)
              "C"
              (if(= formula 13)
                 "D"
                 (if(= formula 14)
                    "E"
                    (if(= formula 15)
                       "F"
                       null))))))))

;Funcion para trasformar un numero rgb a hex (parte 2).
;dom:integer.
;rec:string.
(define (transform2 num)
  (define formula (*(-(exact->inexact(/ num 16))(quotient num 16)) 16))
  (if(and(>= formula 0)(<= formula 9))
     (number->string (inexact->exact formula))
     (if(= formula 10)
        "A"
        (if(= formula 11)
           "B"
           (if(= formula 12)
              "C"
              (if(= formula 13)
                 "D"
                 (if(= formula 14)
                    "E"
                    (if(= formula 15)
                       "F"
                       null))))))))

;Funcion que une la parte 1 y 2 de transformar un numero rgb a hex.
;Dom: integer
;Rec: string.
;Ejemplo de uso:(append-transform 90) 
(define (append-transform num)
  (string-append (transform1 num) (transform2 num)))

;Funcion que transforma un rgb a hex.
;dom: rgb.
;rec: hex.
(define (append-rgb->hex rgb)
  (string-append "#" (append-transform (car rgb))(append-transform (cadr rgb))(append-transform (caddr rgb))))
  



(define rgb (pixrgb-d 0 0 10 20 30 10))
