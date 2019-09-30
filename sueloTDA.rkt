#lang racket
;----------------------------------------REPRESENTACION--------------------------------------------------------

;5.- Suelo
;	->number x
;	->number y

;----------------------------------F. DE PERTENENCIA---------------------------------

;Funcion que retorna true si la lista dada contiene la informacion de un TDA suelo, en caso contrario, entrega false
;Entrada: Lista con informacion de un suelo
;Salida: True o False 
(define (suelo? listSuelo)
  (if (and (list? listSuelo)
           (= (length listSuelo) 2)
           (number? (car listSuelo))
           (> (car listSuelo) 0)
           (number? (car (cdr listSuelo)))
           (> (car (cdr listSuelo)) 0))
      #t
      #f
   )
 )

;---------------------------------------SELECTORES------------------------------------

;Funcion que retorna la posicion X de una lista suelo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Poscion X de la lista suelo o una lista vacia
(define (suelo_getPosX listObst)
  (if (suelo? listObst)
      (car listObst)
      null
   )
 )

;Funcion que retorna la posicion Y de una lista suelo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Poscion Y de la lista suelo o una lista vacia
(define (suelo_getPosY listObst)
  (if (suelo? listObst)
      (car (cdr listObst))
      null
   )
 )

(provide (all-defined-out))