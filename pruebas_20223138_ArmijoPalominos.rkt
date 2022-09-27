 #lang racket
(require "TDApixbit-d_20223138_ArmijoPalominos.rkt") 
(require "TDApixrgb-d_20223138_ArmijoPalominos.rkt")
(require "TDApixhex-d_20223138_ArmijoPalominos.rkt")
(require "TDAimage_20223138_ArmijoPalominos.rkt")


(define img1 (image 2 2
                  (pixrgb-d 0 0 255 0 0 10)
                  (pixrgb-d 0 1 0 255 0 20)
                  (pixrgb-d 1 0 0 0 255 10)
                  (pixrgb-d 1 1 255 255 255 1))
)

(define img2 (image 2 2
                  (pixbit-d 0 0 0 10)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 1 0 1 10)
                  (pixbit-d 1 1 0 255))
 )

(define img3 (imgRGB->imgHex img1))

(define imag1 (image 3 3
                     (pixrgb-d 0 0 180 7 0 10)
                     (pixrgb-d 0 1 0 255 0 20)
                     (pixrgb-d 0 2 0 0 255 20)
                     (pixrgb-d 1 0 20 255 255 5)
                     (pixrgb-d 1 1 0 0 0 15)
                     (pixrgb-d 1 2 0 255 180 13)
                     (pixrgb-d 2 0 10 255 255 5)
                     (pixrgb-d 2 1 0 180 255 30)
                     (pixrgb-d 2 2 30 255 255 255)
                     
               )
 )

(define imag2 (image 3 3
                  (pixbit-d 0 0 0 50)
                  (pixbit-d 0 1 1 20)
                  (pixbit-d 0 2 1 10)
                  (pixbit-d 1 0 0 0)
                  (pixbit-d 1 1 1 30)
                  (pixbit-d 1 2 0 255)
                  (pixbit-d 2 0 1 40)
                  (pixbit-d 2 1 1 1)
                  (pixbit-d 2 2 0 255))
 )

(define imag3 (imgRGB->imgHex imag1))

;imprimir una representación string de la imagen
;(display (image->string img1 pixrgb->string))

;output:
; #FF0000 #00FF00
; #0000FF #FFFFFF

;imprimir una representación string de la imagen
;(display (image->string img2 pixbit->string))

;output:
;0 1
;1 0

;El resto de los ejemplos, los puede obtener directamente desde las tablas presentadas en el enunciado. 

(bitmap? img1) ; la respuesta debería ser #f
(bitmap? img2)  ; la respuesta debería ser #t
(bitmap? img3)  ; la respuesta debería ser #f
(bitmap? imag1) ;la respuesta debería ser #f
(bitmap? imag2) ;la respuesta debería ser #t
(bitmap? imag3) ;la respuesta debería ser #f

(pixmap? img1) ; la respuesta debería ser #t
(pixmap? img2)  ; la respuesta debería ser #f
(pixmap? img3)  ; la respuesta debería ser #f
(pixmap? imag1) ;la respuesta debería ser #t
(pixmap? imag2) ;la respuesta debería ser #f
(pixmap? imag3) ;la respuesta debería ser #f

(hexmap? img1) ; la respuesta debería ser #f
(hexmap? img2)  ; la respuesta debería ser #f
(hexmap? img3)  ; la respuesta debería ser #t
(hexmap? imag1)  ; la respuesta debería ser #f
(hexmap? imag2)  ; la respuesta debería ser #f
(hexmap? imag3)  ; la respuesta debería ser #t

(compressed? img1) ; la respuesta debería ser #f
(compressed? img2) ; la respuesta debería ser #f
(compressed? img3) ; la respuesta debería ser #f
(compressed? imag1) ; la respuesta debería ser #f
(compressed? imag2) ; la respuesta debería ser #f
(compressed? imag3) ; la respuesta debería ser #f

(flipH img1)
(flipH img2)
(flipH img3)
(flipH imag1)
(flipH imag2)
(flipH imag3)

(flipV img1)
(flipV img2)
(flipV img3)
(flipV imag1)
(flipV imag2)
(flipV imag3)

(define img4 (crop img1 0 0 0 0)) ; debería retornar una imágen con un pixel
(define img5 (crop img2 0 0 0 1)) ; debería retornar una imágen con dos pixeles
(define img6 (crop img1 0 1 1 1)) ; debería retornar una imágen con dos pixeles
(define img7 (crop img2 0 0 1 1)) ; debería retornar la misma imagen
(define imag4 (crop imag1 2 1 2 1)) ; debería retornar una imágen con un pixel
(define imag5 (crop imag2 0 0 1 2)) ; debería retornar una imagen con 6 pixeles
(define imag6 (crop imag3 0 1 1 2)) ; debería retornar una imagen con 4 pixeles

;(histogram img1)
;(histogram img2)
;(histogram img3)
;(histogram img4)
;(histogram img5)
;(histogram img6)
;(histogram img7)

(rotate90 img1)
(rotate90 img2)
(rotate90 img3)
(rotate90 img5)
(rotate90 img6)
(rotate90 img7)
(rotate90 imag1)
(rotate90 imag5)
(rotate90 imag6)

;(define img8 (compress img1))
;(define img9 (compress img2))
;(define img10 (compress img3))
;(define img11 (compress img4))
;(define img12 (compress img5))
;(define img13 (compress img6))
;(define img14 (compress img7))

;(compressed? img8)  ; la respuesta debería ser #t
;(compressed? img9)  ; la respuesta debería ser #t
;(compressed? img10)  ; la respuesta debería ser #t
;(compressed? img11)  ; la respuesta debería ser #t
;(compressed? img12)  ; la respuesta debería ser #t
;(compressed? img13)  ; la respuesta debería ser #t
;(compressed? img14)  ; la respuesta debería ser #t

 (invertColorBit img2)


;se asume que las funciones de ajuste de canal están implementadas. 
;Puede cambiarlas por otras en su script de pruebas si así lo prefiere 
;(define img33 (edit (adjustChannel getR setR incCh) img1))
;(define img34 (edit (adjustChannel getG setG incCh) img1))
;(define img35 (edit (adjustChannel getB setB incCh) img1))

;imágenes no comprimidas
;(display (image->string img1 pixrgb->string))
;(display (image->string img2 pixbit->string))
;(display (image->string img3 pixhex->string))
;(display (image->string img4 pixrgb->string))
;(display (image->string img5 pixbit->string))
;(display (image->string img6 pixrgb->string))
;(display (image->string img7 pixrbit->string))

;imagenes comprimidas, podrían internamente descomprimirlas para convertir a string ;(opcional)
;(display (image->string img8 pixrgb->string))
;(display (image->string img9 pixbit->string))
;(display (image->string img10 pixhex->string)) 
;(display (image->string img11 pixrgb->string))
;(display  (image->string img12 pixbit->string))
;(display (image->string img13 pixrgb->string))
;(display (image->string img14 pixbit->string))

;imágenes no comprimidas
;(display (image->string img15 pixrgb->string))
;(display (image->string img16 pixrgb->string))
;(display (image->string img17 pixrgb->string))
;(display (image->string img18 pixrgb->string))
;(display (image->string img19 pixbit->string))
;(display (image->string img20 pixhex->string))
;(display (image->string img21 pixrgb->string))
;(display (image->string img22 pixbit->string))
;(display (image->string img23 pixrgb->string))
;(display (image->string img24 pixbit->string))

;depthLayers img1)
;(depthLayers img2)
;(depthLayers img3)
;(depthLayers img4)
;(depthLayers img5)
;(depthLayers img6)
;(depthLayers img7)

;(define img25 (decompress img8))
;(define img26 (decompress img9))
;(define img27 (decompress img10))
;(define img28 (decompress img11))
;(define img29 (decompress img12))
;(define img30 (decompress img13))
;(define img31 (decompress img14))

;las siguientes comparaciones deberían arrojar #t
;(equal? img25 img1)
;(equal? img26 img2)
;(equal? img27 img3)
;(equal? img28 img4)
;(equal? img29 img5)
;(equal? img30 img6)
;(equal? img31 img7)

;las siguientes comparaciones deberían arrojar #f
;(equal? img25 img2)
;(equal? img26 img1)
