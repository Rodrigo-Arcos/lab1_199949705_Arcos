(define null (list ))
;----------------------------------------REPRESENTACION-----------------------------------------------------------
;Los siguientes cuatro TDAs son representados en una lista, en la cual cada posicion de esta tiene las siguientes caracteristicas
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

;3.- Disparos
;	->number identificador (2)
;	->number angulo
;	->number velocidad
;	->number x
;	->number y

;4.- Obstaculos
;	->number x
;	->number y

;Estos TDAs seran almacenados en una lista, el cual sera la representacion del escenario, los primeros elementos de esta lista seran
;las dimensiones del escenario, luego el piso o obstaculos y finalmente los gusanos y proyectiles.


;----------------------------------------CONSTRUCTOR-----------------------------------------------------------


;Funcion que entrega una lista con la informacion de un gusano (id = 0: gusanos controlados por el jugador, id = 1: gusanos controlados por CPU)
;Entrada: Id gusano, identificador del gusano (para saber que gusano es), posicion X, posicionY
;Salida: Lista con la informacion de un gusano
(define (gusano id idGusano posX posY)
  (if (and (number? id)
           (number? idGusano)
           (number? posX)
           (number? posY)
           (>= id 0)
           (>= idGusano 0)
           (>= posX 0)
           (>= posY 0)
       )
      (list id idGusano posX posY)
      null
   )
 )

;Funcion que entrega una lista con la informacion de un disparo
;Entrada: Id gusano, angulo, velocidad, posicion X, posicionY
;Salida: Lista con la informacion de un disparo
(define (disparo id angulo velocidad posX posY)
  (if (and (number? id)
           (number? angulo)
           (number? velocidad)
           (number? posX)
           (number? posY)
           (>= id 0)
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

;Funcion que entrega una lista con la informacion de un obstaculo del juego
;Entrada: Posicion X, posicionY
;Salida: Lista con la informacion de un obstaculo
(define (obstaculo posX posY)
  (if (and (number? posX)
           (number? posY)
           (>= posX 0)
           (>= posY 0)
       )
      (list posX posY)
      null
   )
 )

;----------------------------------F. DE PERTENENCIA---------------------------------

;Funcion que retorna true si la lista dada contiene la informacion de un TDA gusano, en caso contrario, entrega false
;Entrada: Lista con informacion de un gusano
;Salida: True o False 
(define (gusano? listGusano)
  (if (and (list? listGusano)
           (= (length listGusano) 4)
           (gusano (car listGusano) (car (cdr listGusano)) (car (cdr (cdr listGusano))) (car (cdr (cdr (cdr listGusano)))))
       )
      #t
      #f
   )
)

;Funcion que retorna true si la lista dada contiene la informacion de un TDA disparo, en caso contrario, entrega false
;Entrada: Lista con informacion de un disparo
;Salida: True o False 
(define (disparo? listDisparo)
  (if (and (list? listDisparo)
           (= (length listDisparo) 5)
           (disparo (car listDisparo) (car (cdr listDisparo)) (car (cdr (cdr listDisparo))) (car (cdr (cdr (cdr listDisparo))))
                    (car (cdr (cdr (cdr (cdr listDisparo))))))
       )
      #t
      #f
   )
)

;Funcion que retorna true si la lista dada contiene la informacion de un TDA disparo, en caso contrario, entrega false
;Entrada: Lista con informacion de un obstaculo
;Salida: True o False 
(define (obstaculo? listObst)
  (if (and (list? listObst)
           (= (length listObst) 2)
           (obstaculo (car listObst) (car (cdr listObst)))
       )
      #t
      #f
   )
)

;------------------------------SELECTOR------------------------------
;GUSANO
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

;DISPARO
;Funcion que retorna la id de una lista disparo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Id de la lista disparo o una lista vacia
(define (disparo_getId listDisparo)
  (if (disparo? listDisparo)
      (car listDisparo)
      null
   )
 )

;Funcion que retorna el angulo de una lista disparo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Angulo de la lista disparo o una lista vacia
(define (disparo_getAng listDisparo)
  (if (disparo? listDisparo)
      (car (cdr listDisparo))
      null
   )
 )

;Funcion que retorna la velocidad de una lista disparo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Velocidad de la lista disparo o una lista vacia
(define (disparo_getVel listDisparo)
  (if (disparo? listDisparo)
      (car (cdr (cdr listDisparo)))
      null
   )
 )

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

;OBSTACULO
;Funcion que retorna la posicion X de una lista obstaculo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Poscion X de la lista obstaculo o una lista vacia
(define (obstaculo_getPosX listObst)
  (if (obstaculo? listObst)
      (car listObst)
      null
   )
 )

;Funcion que retorna la posicion Y de una lista obstaculo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Poscion Y de la lista obstaculo o una lista vacia
(define (obstaculo_getPosY listObst)
  (if (obstaculo? listObst)
      (car (cdr listObst))
      null
   )
 )
;------------------------------MODIFICADORES------------------------------

;GUSANO
;Funcion que retorna un nueva lista gusano con la id cambiada
;Entrada: Lista, id a cambiar
;Salida: Lista gusano o lista vacia
(define (gusano_setId listGusano id)
  (if (and (gusano? listGusano)
           (number? id)
           (>= id 0)
       )
      (list id (gusano_getIdGusano listGusano) (gusano_getPosX listGusano) (gusano_getPosY listGusano))
      null
   )
 )

;Funcion que retorna un nueva lista gusano con la idGusano cambiada
;Entrada: Lista, idGusano a cambiar
;Salida: Lista gusano o lista vacia
(define (gusano_setIdGusano listGusano idGusano)
  (if (and (gusano? listGusano)
           (number? idGusano)
           (>= idGusano 0)
       )
      (list (gusano_getId listGusano) idGusano (gusano_getPosX listGusano) (gusano_getPosY listGusano))
      null
   )
 )

;Funcion que retorna un nueva lista gusano con la posicion X cambiada
;Entrada: Lista, posicion X a cambiar
;Salida: Lista gusano o lista vacia
(define (gusano_setPosX listGusano posX)
  (if (and (gusano? listGusano)
           (number? posX)
           (>= posX 0)
       )
      (list (gusano_getId listGusano) (gusano_getIdGusano listGusano) posX (gusano_getPosY listGusano))
      null
   )
 )

;Funcion que retorna un nueva lista gusano con la posicion Y cambiada
;Entrada: Lista, posicion Y a cambiar
;Salida: Lista gusano o lista vacia
(define (gusano_setPosY listGusano posY)
  (if (and (gusano? listGusano)
           (number? posY)
           (>= posY 0)
       )
      (list (gusano_getId listGusano) (gusano_getIdGusano listGusano) (gusano_getPosX listGusano) posY)
      null
   )
 )

;DISPARO
;Funcion que retorna un nueva lista disparo con la id cambiada
;Entrada: Lista, id a cambiar
;Salida: Lista disparo o lista vacia
(define (disparo_setId listDisparo id)
  (if (and (disparo? listDisparo)
           (number? id)
           (>= id 0)
       )
      (list id (disparo_getAng listDisparo) (disparo_getVel listDisparo) (disparo_getPosX listDisparo)
            (disparo_getPosY listDisparo))
      null
   )
 )

;Funcion que retorna un nueva lista disparo con el angulo cambiado
;Entrada: Lista, angulo a cambiar
;Salida: Lista disparo o lista vacia
(define (disparo_setAng listDisparo angulo)
  (if (and (disparo? listDisparo)
           (number? angulo)
           (>= angulo -90)
           (<= angulo 90)
       )
      (list (disparo_getId listDisparo) angulo (disparo_getVel listDisparo) (disparo_getPosX listDisparo)
            (disparo_getPosY listDisparo))
      null
   )
 )

;Funcion que retorna un nueva lista disparo con la velocidad cambiada
;Entrada: Lista, velocidad a cambiar
;Salida: Lista disparo o lista vacia
(define (disparo_setVelo listDisparo velo)
  (if (and (disparo? listDisparo)
           (number? velo)
           (>= velo 0)
       )
      (list (disparo_getId listDisparo) (disparo_getAng listDisparo) velo (disparo_getPosX listDisparo)
            (disparo_getPosY listDisparo))
      null
   )
 )

;Funcion que retorna un nueva lista disparo con la posicion X cambiada
;Entrada: Lista, posicion X a cambiar
;Salida: Lista disparo o lista vacia
(define (disparo_setPosX listDisparo posX)
  (if (and (disparo? listDisparo)
           (number? posX)
           (>= posX 0)
       )
      (list (disparo_getId listDisparo) (disparo_getAng listDisparo) (disparo_getVel listDisparo) posX
            (disparo_getPosY listDisparo))
      null
   )
 )

;Funcion que retorna un nueva lista disparo con la posicion Y cambiada
;Entrada: Lista, posicion Y a cambiar
;Salida: Lista disparo o lista vacia
(define (disparo_setPosY listDisparo posY)
  (if (and (disparo? listDisparo)
           (number? posY)
           (>= posY 0)
       )
      (list (disparo_getId listDisparo) (disparo_getAng listDisparo) (disparo_getVel listDisparo)
            (disparo_getPosX listDisparo) posY)
      null
   )
 )

;OBSTACULO
;Funcion que retorna un nueva lista obstaculo con la posicion X cambiada
;Entrada: Lista, posicion X a cambiar
;Salida: Lista obstaculo o lista vacia
(define (obstaculo_setPosX listObst posX)
  (if (and (obstaculo? listObst)
           (number? posX)
           (>= posX 0)
       )
      (list posX (obstaculo_getPosY listObst))
      null
   )
 )

;Funcion que retorna un nueva lista obstaculo con la posicion Y cambiada
;Entrada: Lista, posicion Y a cambiar
;Salida: Lista obstaculo o lista vacia
(define (obstaculo_setPosY listObst posY)
  (if (and (obstaculo? listObst)
           (number? posY)
           (>= posY 0)
       )
      (list (obstaculo_getPosX listObst) posY)
      null
   )
 )