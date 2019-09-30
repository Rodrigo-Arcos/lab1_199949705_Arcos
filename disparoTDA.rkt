#lang racket

;Se crearon solo los utilizados

;----------------------------------------REPRESENTACION--------------------------------------------------------

;3.- Disparos
;	->number identificador (2)
;	->number angulo
;	->number velocidad
;	->number x
;	->number y

;----------------------------------------CONSTRUCTOR-----------------------------------------------------------

;Funcion que entrega una lista con la informacion de un disparo
;Entrada: Id gusano, angulo, velocidad, posicion X, posicionY
;Salida: Lista con la informacion de un disparo
(define (disparo id angulo velocidad posX posY)
  (if (and (number? id)
           (number? angulo)
           (number? velocidad)
           (number? posX)
           (number? posY)
           (= id 2)
           (>= angulo -90)
           (<= angulo 90)
           (>= velocidad 0)
           (>= posX 0)
           (>= posY 0)
       )
      (list id angulo velocidad posX posY)
      null
   )
 )

;----------------------------------F. DE PERTENENCIA---------------------------------

;Funcion que retorna true si la lista dada contiene la informacion de un TDA disparo, en caso contrario, entrega false
;Entrada: Lista con informacion de un disparo
;Salida: True o False 
(define (disparo? listDisparo)
  (if (and (list? listDisparo)
           (= (length listDisparo) 5)
           (> (length (disparo (car listDisparo) (car (cdr listDisparo)) (car (cdr (cdr listDisparo))) (car (cdr (cdr (cdr listDisparo))))
                    (car (cdr (cdr (cdr (cdr listDisparo))))))) 0)
       )
      #t
      #f
   )
)

;---------------------------------------SELECTORES------------------------------------

;Funcion que retorna la posicion X de una lista disparo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Poscion X de la lista disparo o una lista vacia
(define (disparo_getPosX listDisparo)
  (if (disparo? listDisparo)
      (car (cdr (cdr (cdr listDisparo))))
      null
   )
 )

;Funcion que retorna la posicion y de una lista disparo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Poscion Y de la lista disparo o una lista vacia
(define (disparo_getPosY listDisparo)
  (if (disparo? listDisparo)
      (car (cdr (cdr (cdr (cdr listDisparo)))))
      null
   )
 )

(provide (all-defined-out))