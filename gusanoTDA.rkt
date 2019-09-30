#lang racket

;Se crearon solo los utilizados

;----------------------------------------REPRESENTACION--------------------------------------------------------

;1.- Enemigos
;	->number identificador (0)
;	->number numero del gusano
;	->number x
;	->number y

;2.- Jugador
;	->number identificador (1)
;	->number numero del gusano
;	->number x
;	->number y


;----------------------------------------CONSTRUCTOR-----------------------------------------------------------


;Funcion que entrega una lista con la informacion de un gusano (id = 0: gusanos controlados por el jugador,
;id = 1: gusanos controlados por CPU)
;Entrada: Id gusano, identificador del gusano (para saber que gusano es), posicion X, posicionY
;Salida: Lista con la informacion de un gusano
(define (gusano id idGusano posX posY)
  (if (and (number? id)
           (number? idGusano)
           (number? posX)
           (number? posY)
           (or (= id 0) (= id 1))
           (>= idGusano 0)
           (>= posX 0)
           (>= posY 0)
       )
      (list id idGusano posX posY)
      null
   )
 )

;----------------------------------F. DE PERTENENCIA---------------------------------

;Funcion que retorna true si la lista dada contiene la informacion de un TDA gusano, en caso contrario,
;entrega false
;Entrada: Lista con informacion de un gusano
;Salida: True o False 
(define (gusano? listGusano)
  (if (and (list? listGusano)
           (= (length listGusano) 4)
           (> (length (gusano (car listGusano) (car (cdr listGusano)) (car (cdr (cdr listGusano)))
                              (car (cdr (cdr (cdr listGusano)))))) 0)
       )
      #t
      #f
   )
)

;---------------------------------------SELECTORES------------------------------------

;Funcion que retorna la id de una lista gusano, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Id de la lista gusano o una lista vacia
(define (gusano_getId listGusano)
  (if (gusano? listGusano)
      (car listGusano)
      null
   )
 )

;Funcion que retorna la idGusano de una lista gusano, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: IdGusano de la lista gusano o una lista vacia
(define (gusano_getIdGusano listGusano)
  (if (gusano? listGusano)
      (car (cdr listGusano))
      null
   )
 )

;Funcion que retorna la posicion X de una lista gusano, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Posicion X de la lista gusano o una lista vacia
(define (gusano_getPosX listGusano)
  (if (gusano? listGusano)
      (car (cdr (cdr listGusano)))
      null
   )
 )

;Funcion que retorna la posicion Y de una lista gusano, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Posicion Y de la lista gusano o una lista vacia
(define (gusano_getPosY listGusano)
  (if (gusano? listGusano)
      (car (cdr (cdr (cdr listGusano))))
      null
   )
 )



(provide (all-defined-out))