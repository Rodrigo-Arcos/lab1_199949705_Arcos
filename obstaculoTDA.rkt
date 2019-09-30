#lang racket
;----------------------------------------REPRESENTACION--------------------------------------------------------

;4.- Obstaculos
;	->number identificador (3)
;	->number x
;	->number y

;----------------------------------------CONSTRUCTOR-----------------------------------------------------------

;Funcion que entrega una lista con la informacion de un obstaculo del juego
;Entrada: Posicion X, posicionY
;Salida: Lista con la informacion de un obstaculo
(define (obstaculo id posX posY)
  (if (and (number? id)
           (number? posX)
           (number? posY)
           (= id 3)
           (>= posX 0)
           (>= posY 0)
       )
      (list id posX posY)
      null
   )
 )

;----------------------------------F. DE PERTENENCIA---------------------------------

;Funcion que retorna true si la lista dada contiene la informacion de un TDA disparo, en caso contrario, entrega false
;Entrada: Lista con informacion de un obstaculo
;Salida: True o False 
(define (obstaculo? listObst)
  (if (and (list? listObst)
           (= (length listObst) 3)
           (> (length (obstaculo (car listObst) (car (cdr listObst)) (car (cdr (cdr listObst))))) 0)
       )
      #t
      #f
   )
)

;---------------------------------------SELECTORES------------------------------------


;Funcion que retorna la posicion X de una lista obstaculo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Poscion X de la lista obstaculo o una lista vacia
(define (obstaculo_getPosX listObst)
  (if (obstaculo? listObst)
      (car (cdr listObst))
      null
   )
 )

;Funcion que retorna la posicion Y de una lista obstaculo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Poscion Y de la lista obstaculo o una lista vacia
(define (obstaculo_getPosY listObst)
  (if (obstaculo? listObst)
      (car (cdr (cdr listObst)))
      null
   )
 )

(provide (all-defined-out))