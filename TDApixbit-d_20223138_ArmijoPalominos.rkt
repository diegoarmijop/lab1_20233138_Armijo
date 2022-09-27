#lang racket

(provide (all-defined-out))

;-------------------------------- PARÁMETROS --------------------------------

;Descripción de los parámetros a recibir para la funcion constructor.
;X -> Entero que indica la posicion X del pixel.
;Y -> Entero que indica la posicion Y del pixel.
;Bit -> Entero que solo puede tomar valores 0 y 1.
;Depth->Entero que indica la profundiad de pixel.

;-------------------------------- CONSTRUCTOR --------------------------------

;Funcion constructora. Esta se encarga  de generar un pixbit-d valido.
;Dominio: X (int) x Y (int) x Bit (0-1) x Depth (int).
;Recorrido: pixbit-d
;Tipo de recursión: No aplica.
;Ejemplo de uso: (pixbit-d 0 0 1 10).

(define (pixbit-d x y bit depth)
  (if (pixbit-d? x y bit depth)
     (if(= bit 0)
        (list (list x y) bit depth "pixbit-d" 1 )
        (list (list x y) bit depth "pixbit-d" 0 )
        )
     (raise "No es un pixbit-d")
  )
)


;-------------------------------- PERTENENCIA --------------------------------

;Funcion que valida si los argumentos entregados corresponden a un TDApixbit.
;Dominio: X(int) x Y(int) x bit(0|1) x Depth(int).
;Recorrido: boolean.
;Tipo de recursion: No aplica.
(define (pixbit-d? x y bit depth)
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
     #t
     #f))
         

;-------------------------------- SELECTORES --------------------------------

;Funcion selectora para obtener una lista con las coordenadas.
;Dominio: pixbit-d.
;Recorrido: list 
;Tipo de recursion: No aplica.
;Ejemplo de uso: (bit->getCoord (pixbit-d 0 0 1 10))
(define (bit->getCoord bit)
  (car bit))

;Funcion selectora para obtener el valor de un bit (0|1).
;Dominio: pixbit-d.
;Recorrido: integer. 
;Tipo de recursion: No aplica.
;Ejemplo de uso: (bit->getBit (pixbit-d 0 0 1 10))
(define (bit->getBit bit)
  (cadr bit))

;Funcion selectora para obtener la profundidad de un bit (0|1).
;Dominio: pixbit-d.
;Recorrido: integer. 
;Tipo de recursion: No aplica.
;Ejemplo de uso: (bit->getDepth (pixbit-d 0 0 1 10))
(define (bit->getDepth bit)
  (caddr bit))

;Funcion selectora para obtener el type de un bit.
;Dominio: pixbit-d.
;Recorrido: string. 
;Tipo de recursion: No aplica.
;Ejemplo de uso: (bit->getType(pixbit-d 0 0 1 10))
(define (bit->getType bit)
  (cadddr bit))

;Funcion selectora para obtener el bit opuesto de un bit.
;Dominio: pixbit-d.
;Recorrido: integer. 
;Tipo de recursion: No aplica.
;Ejemplo de uso: (bit->getOpuesto (pixbit-d 0 0 1 10))
(define (bit->getOpuesto bit)
  (cadddr(cdr bit)))



;bit ejemplo
(define bit (pixbit-d 0 1 1 10))

  



     