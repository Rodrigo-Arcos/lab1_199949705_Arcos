#lang racket
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
;	->number identificador (3)
;	->number x
;	->number y

;5.- Suelo
;	->number x
;	->number y

;Estos TDAs seran almacenados en una lista, el cual sera la representacion del escenario, los primeros elementos de esta lista seran
;las dimensiones del escenario, luego el suelo, obstaculos y finalmente los gusanos y proyectiles.


;----------------------------------------CONSTRUCTOR-----------------------------------------------------------


;Funcion que entrega una lista con la informacion de un gusano (id = 0: gusanos controlados por el jugador, id = 1: gusanos controlados por CPU)
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

;Funcion que retorna true si la lista dada contiene la informacion de un TDA gusano, en caso contrario, entrega false
;Entrada: Lista con informacion de un gusano
;Salida: True o False 
(define (gusano? listGusano)
  (if (and (list? listGusano)
           (= (length listGusano) 4)
           (> (length (gusano (car listGusano) (car (cdr listGusano)) (car (cdr (cdr listGusano))) (car (cdr (cdr (cdr listGusano)))))) 0)
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
           (> (length (disparo (car listDisparo) (car (cdr listDisparo)) (car (cdr (cdr listDisparo))) (car (cdr (cdr (cdr listDisparo))))
                    (car (cdr (cdr (cdr (cdr listDisparo))))))) 0)
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
           (= (length listObst) 3)
           (> (length (obstaculo (car listObst) (car (cdr listObst)) (car (cdr (cdr listObst))))) 0)
       )
      #t
      #f
   )
)

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
;Funcion que retorna la id de una lista obstaculo, en caso contrario entrega una lista vacia
;Entrada: Lista
;Salida: Id de la lista obstaculo o una lista vacia
(define (obstaculo_getId listObst)
  (if (obstaculo? listObst)
      (car listObst)
      null
   )
 )

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

;SUELO
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
;------------------------------MODIFICADORES------------------------------

;GUSANO
;Funcion que retorna un nueva lista gusano con la id cambiada
;Entrada: Lista, id a cambiar
;Salida: Lista gusano o lista vacia
(define (gusano_setId listGusano id)
  (if (and (gusano? listGusano)
           (number? id)
           (or (= id 0) (= id 1))
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
           (= id 2)
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
;Funcion que retorna un nueva lista obstaculo con la id cambiada
;Entrada: Lista, id a cambiar
;Salida: Lista obstaculo o lista vacia
(define (obstaculo_setId listObst id)
  (if (and (obstaculo? listObst)
           (number? id)
           (= id 3)
       )
      (list id (obstaculo_getPosX listObst) (obstaculo_getPosY listObst))
      null
   )
 )
;Funcion que retorna un nueva lista obstaculo con la posicion X cambiada
;Entrada: Lista, posicion X a cambiar
;Salida: Lista obstaculo o lista vacia
(define (obstaculo_setPosX listObst posX)
  (if (and (obstaculo? listObst)
           (number? posX)
           (>= posX 0)
       )
      (list (obstaculo_getId listObst) posX (obstaculo_getPosY listObst))
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
      (list (obstaculo_getId listObst) (obstaculo_getPosX listObst) posY)
      null
   )
 )

;-----------------------------------------------------------------------------------------------
;---------------------------------FUNCIONES REQUERIDAS------------------------------------------
;-----------------------------------------------------------------------------------------------

;------------------------------------Funcion Random---------------------------------------------

;Estas constantes fueron sacadas de https://en.wikipedia.org/wiki/Linear_congruential_generator
(define a 1103515245)
(define c 12345)
(define m 2147483648)
;Esta función random que genera un numero aleatorio
;Entrada: Semilla (seed)
;Salida: Numero entero
(define myRandom
  (lambda
    (xn)
    (remainder (+ (* a xn) c) m)
  )
)
;Cada vez que pedimos un random, debemos pasar como argumento el random anterior.
;Acá un ejemplo que permite generar una lista de números aleatorios.
;Entrada: largo de la lista a generar, valor actual del random, se pasa en cada nivel de recursión de forma actualizada,
;         números maximo generados van desde 0 hasta maximo-1 y lista en donde se guardaran los numeros aleatorios
;Salida: Lista de numeros enteros

(define (getListaRandomR cuantos xActual maximo lista)
  (if (= 0 cuantos)
      lista
      (if (= (buscarEnLista lista (+ (remainder (myRandom xActual) maximo) 1)) 1)
          (getListaRandomR (- cuantos 1) (myRandom xActual) maximo (appendList lista (+ (remainder (myRandom xActual) maximo) 1)))
          (getListaRandomR cuantos (myRandom xActual) maximo lista)
       )
   )
)
  
;Funcion que entrega un numero random
;Entrada: Semilla (seed) y numero aleatorio maximo a generar va desde 0 hasta maximo-1
;Salida: Numero entero aleatorio
(define (getNumRandom xActual maximo)
  (let ((xNvo (myRandom xActual)))
          (remainder xNvo maximo)
    )
 )

;Se define la constante velocidad para los disparos
(define velocidad 1)
;Se define la constante tiempo para los disparos
(define tiempo 1)
;Se define la constante pi para la funcion que convierte grados a radianes
(define pi 3.141592653589793)
;Funcion que convierte de grados a radianes
;Entrada: Grados (entero)
;Salida: Radianes
(define (getRad grados)
 (/ (* grados pi) 180)
)

;Funcion que busca en una lista un numero
;Entrada: Lista y numero a buscar en la lista
;Salida: Si se encuentra el numero en la lista retorna 0, sino retorna 1
(define (buscarEnLista lista num)
  (if ( = (length lista) 0)
      1
      (if (= (car lista) num)
          0
          (buscarEnLista (cdr lista) num)
       )
   )
 )

;Funcion que agrega una lista a otra lista
;Entrada: lista y lista a agregar
;Salida: Lista que contiene un elemento mas (lista)
(define (appendList lista elemento)
  (if (list? lista)
      (append lista (list elemento))
      null
   )
 )

;--------------------------------------Funcion RF---------------------------------------------
;Función que retorna un escenario válido de tamaño NxM, si no es factible crear el escenario retorna nulo
;Entrada: Tamaño eje X, tamaño eje Y, cantidad de enemigos, dificultad del escenario, semilla para generar
;         valores pseudoaleatorios
;Salida: Lista que representa el escenario del juego
(define (createScene N M E D seed)
  (if (and (> D 0) (< D 4) (<= (+ E D) M))
      (append (crearSuelo N M) (crearObst N M D seed) (crearJugador M (crearObst N M D seed) D 1 1)
              (crearEnemigo M (crearObst N M D seed) E 1 N))
      null
   )
)

;Funcion de encapsulamiento para crear el suelo
;Entrada: Tamaño eje X y tamaño eje Y
;Salida: Lista con las dimensiones del escenario y las coordenadas del suelo
(define (crearSuelo N M)
  (if (and (> N 0) (> M 0))
      (crearSueloRL N M 0 0)
      null
   )
)

;Funcion que crea una lista con las dimensiones del escenario y las coordenadas del suelo
;Entrada: Tamaño eje X, tamaño eje Y, auxiliar para X y auxiliar para Y
;Salida: Lista con las dimensiones del escenario y las coordenadas del suelo
(define (crearSueloRL N M auxN auxM)
  (if (= auxN 0)
      (cons N (cons M (crearSueloRL N M (+ auxN 1) (+ auxM 1))))
      (if (and (> auxN 0) (< auxN N) (<= auxM (/ M 3)))
          (cons (list auxN auxM) (crearSueloRL N M (+ auxN 1) auxM))
          (if (and (= auxN N) (<= auxM M))
              (cons (list auxN auxM) (crearSueloRL N M 1 (+ auxM 1)))
              (if (and (<= auxM (/ M 3)) (<= auxM M))
                  (cons (list auxN M) (crearSueloRL N M (+ auxN 1) (+ auxM 1)))
                  '()
               )
          )
       )
   )
)

;Funcion que, dependiendo de la dificultad ingresada, crea los obstaculos del escenario
;Entrada: Tamaño eje X, tamaño eje Y, dificultad del escenario, semilla para generar
;         valores pseudoaleatorios
;Salida: Lista que contiene TDA obstaculos
(define (crearObst N M D seed)
  (if (= D 3)
      (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list)))
      (if (and (= (remainder M 3) 0) (= D 2))
          (append (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list)))
                  (crearObstMedio N M (obtenerNum (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list))
                                              (+ (getNumRandom seed N) 1) 1) (+ (/ M 3) 1) (+ (getNumRandom seed M) 1)))
          (if (= D 2)
              (append (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list)))
                      (crearObstMedio N M (obtenerNum (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list))
                                                  (+ (getNumRandom seed N) 1) 1) (+ (truncate (/ M 3)) 2)
                                                                                 (+ (getNumRandom seed M) 1)))
              (if (and (= (remainder M 3) 0) (= D 1))
                  (append (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list)))
                          (crearObstDificil N M seed (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list))
                                            (+ (/ M 3) 1) '()))
                  (append (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list)))
                          (crearObstDificil N M seed (getListaRandomR (+ (getNumRandom seed N) 1) seed N (list))
                                            (+ (truncate (/ M 3)) 2) '()))
               )
           )
       )
    )
 )

;Funcion que obtiene un numero en especifico de una lista
;Entrada: Lista de numeros, posiciond el elemento, auxiliar
;Salida: Numero entero de la lista
(define (obtenerNum lista posicion aux)
  (if (= aux posicion)
      (car lista)
      (obtenerNum (cdr lista) posicion (+ aux 1))
   )
 )

;Funcion que crea los obstaculos para la dificultad facil
;Entrada: Tamaño eje X, tamaño eje Y, semilla para generar valores pseudoaleatorios,
;         lista con la posicion X en donde van los obstaculos
;Salida: Lista que contiene TDA obstaculos
(define (crearObstFacil N M seed lista)
  (if (= (length lista) 0)
      '()
      (if (= (remainder M 3) 0)
          (cons (obstaculo 3 (car lista) (/ M 3)) (crearObstFacil N M seed (cdr lista)))
          (cons (obstaculo 3 (car lista) (+ (truncate (/ M 3)) 1)) (crearObstFacil N M seed (cdr lista)))
        )
   )
 )

;Funcion que crea los obstaculos para la dificultad medio, se elige un obstaculo creado en la funcion
;crearObstFacil y se aumenta su tamaño en el ejeY
;Entrada: Tamaño eje X, tamaño eje Y, posicion X en donde se comenzara a generar el obstaculo, posicion Y del obstaculo y
;         posicion Y hasta donde se generaran los obstaculos
;Salida: Lista que contiene TDA obstaculos
(define (crearObstMedio N M comienzo posicion final)
  (if (or (> posicion final) (= posicion M))
      '()
      (cons (obstaculo 3 comienzo posicion) (crearObstMedio N M comienzo (+ posicion 1) final))
   )
 )

;Funcion que crea los obstaculos para la dificultad dificil, se eligen todos los obstaculos creados en la funcion
;crearObstFacil y se aumenta su tamaño en el ejeY
;Entrada: Tamaño eje X, tamaño eje Y, semilla para generar valores pseudoaleatorios, lista con las posiciones X de los
;         obstaculos ya creados, posicion Y en donde se comenzaran a generar los obstaculos, lista resultante
;Salida: Lista que contiene TDA obstaculos
(define (crearObstDificil N M seed listaObst comienzo listaFinal)
  (if (= (length listaObst) 0)
      listaFinal
      (crearObstDificil N M (myRandom seed) (cdr listaObst) comienzo
                        (append listaFinal (crearObstMedio N M (car listaObst) comienzo (+ (getNumRandom seed M) 1))))
   )
 )

;Funcion que crea los gusanos que controla el usuario
;Entrada: Tamaño eje X, lista con el suelo y los obstaculos, cantidad de gusanos del usuario, auxiliar para el parametro
;         id del constructor gusano y fila en donde se comenzaran a crear
;Salida: Lista que contiene los TDA gusanos del usuario
(define (crearJugador M listaEscenario cantJugador auxCantJugador fila)
  (if (> auxCantJugador cantJugador)
      '()
      (append (crearGusano M listaEscenario auxCantJugador fila 0 1) (crearJugador M listaEscenario cantJugador
                                                                  (+ auxCantJugador 1) (+ fila 1)))
   )
 )

;Funcion que crea los gusanos que controla la CPU
;Entrada: Tamaño eje X, lista con el suelo y los obstaculos, cantidad de gusanos de la CPU, auxiliar para el parametro
;         id del constructor gusano y fila en donde se comenzaran a crear
;Salida: Lista que contiene los TDA gusanos de la CPU
(define (crearEnemigo M listaEscenario cantEnemigos auxCantEnemigos fila)
  (if (> auxCantEnemigos cantEnemigos)
      '()
      (append (crearGusano M listaEscenario auxCantEnemigos fila 0 0) (crearEnemigo M listaEscenario cantEnemigos
                                                                  (+ auxCantEnemigos 1) (- fila 1)))
   )
 )

;Funcion que crea un TDA gusano
;Entrada: Tamaño eje X, lista con el suelo y los obstaculos, cantidad de gusanos creados, fila en donde se comenzaran a crear,
;         auxiliar para saber si el gusano va encima de un obstaculo o el suelo y parametro id del constructor gusano
;Salida: Lista con un TDA gusano
(define (crearGusano M listaEscenario auxCantJugador fila mayor id)
  (if (and (= (length listaEscenario) 0) (> mayor 0))
      (cons (gusano id auxCantJugador fila (+ mayor 1)) '())
      (if (and (= (length listaEscenario) 0) (= (remainder M 3) 0))
          (cons (gusano id auxCantJugador fila (+ (/ M 3) 1)) '())
          (if (= (length listaEscenario) 0)
              (cons (gusano id auxCantJugador fila (+ (truncate (/ M 3)) 1)) '())
              (if (and (obstaculo? (car listaEscenario)) (= fila (obstaculo_getPosX (car listaEscenario)))
                       (> (obstaculo_getPosY (car listaEscenario)) mayor))
                  (crearGusano M (cdr listaEscenario) auxCantJugador fila (obstaculo_getPosY (car listaEscenario)) id)
                  (crearGusano M (cdr listaEscenario) auxCantJugador fila mayor id)
               )
           )
       )
    )
 )

;Funcion que recibe un escenario de juego y verifica si cumple con los criterios para ser considerado un escenario válido
;Entrada: Escenario de juego
;Salida: True o false
(define (checkScene scene)
  (if (and (list? scene) (> (length scene) 2)
           (number? (car scene)) (> (car scene) 0)
           (number? (car (cdr scene))) (> (car (cdr scene)) 0))
      (if (verificarTDAs (cdr (cdr scene)))
          (if (verificarPosDistintas (car (car (obtenerPosXY (cdr (cdr scene)))))
                                     (car (cdr (car (obtenerPosXY (cdr (cdr scene))))))
                                     (cdr (obtenerPosXY (cdr (cdr scene)))))
              #t
              #f
           )
          #f
       )
      #f
   )
 )

;Funcion que verifica que, a partir del tercer elemento del escenario del juego, cada elemento cumplan con los criterios
;para ser considerados alguno de los TDAs
;Entrada: Lista desde el tercer elemento del escenario
;Salida: True o False
(define (verificarTDAs listaTDAs)
  (if (= (length listaTDAs) 0)
      #t
      (if (or (gusano? (car listaTDAs)) (disparo? (car listaTDAs)) (obstaculo? (car listaTDAs)) (suelo? (car listaTDAs)))
          (verificarTDAs (cdr listaTDAs))
          #f
       )
   )
 )

;Funcion que crea una lista con las posiciones ocupadas por los TDAs
;Entrada: Lista desde el tercer elemento del escenario
;Salida: Lista de listas
(define (obtenerPosXY listaTDAs)
  (if (= (length listaTDAs) 0)
      '()
      (if (gusano? (car listaTDAs))
          (cons (list (gusano_getPosX (car listaTDAs)) (gusano_getPosY (car listaTDAs))) (obtenerPosXY (cdr listaTDAs)))
          (if (disparo? (car listaTDAs))
              (cons (list (disparo_getPosX (car listaTDAs)) (disparo_getPosY (car listaTDAs))) (obtenerPosXY (cdr listaTDAs)))
              (if (obstaculo? (car listaTDAs))
                  (cons (list (obstaculo_getPosX (car listaTDAs)) (obstaculo_getPosY (car listaTDAs))) (obtenerPosXY (cdr listaTDAs)))
                  (if (suelo? (car listaTDAs))
                      (cons (list (suelo_getPosX (car listaTDAs)) (suelo_getPosY (car listaTDAs))) (obtenerPosXY (cdr listaTDAs)))
                      '()
                   )
               )
           )
       )
    )
 )

;Funcion que verifica si algun elemento de una lista de listas de dos elementos se repite
;Entrada: Posicion X, posicion Y, lista de listas
;Salida: True o false
(define (verificarPosDistintas posX posY listaPosXY)
  (if (= (length listaPosXY) 0)
      #t
      (if (and (= posX (car (car listaPosXY))) (= posY (car (cdr (car listaPosXY)))))
          #f
          (verificarPosDistintas (car (car listaPosXY)) (car (cdr (car listaPosXY))) (cdr listaPosXY))
       )
    )
 )

;Función que permite disparar proyectil tomando en consideracion el desplazamiento del jugador, el disparo del proyectil,
;y la respuesta de la CPU
;Entrada: Escenario del juego, personaje elegido, movimiento del personaje (derecha o izquierda), funcion de trayectoria,
;         angulo del disparo, semilla para generar valores pseudoaleatorios 
;Salida: Nuevo escenario y actualizacion del juego
(define (play scene) (lambda (member) (lambda (move) (lambda (tf) (lambda (angle) (lambda (seed)
  (if (and (checkScene scene) (gusanoExiste? (cdr (cdr scene)) member 1) (= (estadoJuego scene) 2))
      (if (and (checkScene scene) (= (estadoJuego (realizarPlay scene member move tf angle seed tiempo)) 2))
          (realizarPlay scene member move tf angle seed tiempo)
          (if (and (checkScene scene) (= (estadoJuego (realizarPlay scene member move tf angle seed tiempo)) 0))
              "DEFEAT"
              (if (and (checkScene scene) (= (estadoJuego (realizarPlay scene member move tf angle seed tiempo)) 1))
                  "VICTORY"
                  "DRAW"
              )
           )
       )
      null
   )
))))))

;Funcion que verifica el estado del juego
;Entrada: Escenario del juego
;Salida: Playinig = 2, VICTORY = 1, DEFEAT = 0, y DRAW = 3
(define (estadoJuego scene)
  (if (and (gusanoIdExiste? (cdr (cdr scene)) 0) (gusanoIdExiste? (cdr (cdr scene)) 1))
      2
      (if (gusanoIdExiste? (cdr (cdr scene)) 0)
          0
          (if (gusanoIdExiste? (cdr (cdr scene)) 1)
              1
              3
           )
       )
   )
)

;Funcion que permite realizar las jugadas de la funcion play
;Entrada: Escenario del juego, personaje elegido, movimiento del personaje (derecha o izquierda), funcion de trayectoria,
;         angulo del disparo, semilla para generar valores pseudoaleatorios 
;Salida: Nuevo escenario y actualizacion del juego
(define (realizarPlay scene member move tf angle seed tiempoNormal)
  (if (and (checkScene scene) (gusanoExiste? (cdr (cdr scene)) member 1) (> (length (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)) 0))
       (if (> move 0)
          (if (> (length (tf scene member 1 angle tiempoNormal)) 0)
              (jugadaCPU (append (list 5 5) (eliminarTDA (moverJugadorDer (cdr (cdr scene)) (car scene) member move (car (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                               (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))) (tf scene member 1 angle tiempoNormal))) seed tiempoNormal)
              (jugadaCPU (append (list 5 5) (moverJugadorDer (cdr (cdr scene)) (car scene) member move (car (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                               (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)))) seed tiempoNormal)
           )
          (if (> (length (tf scene member 1 angle tiempoNormal)) 0)
              (jugadaCPU (append (list 5 5) (eliminarTDA (moverJugadorIzq (cdr (cdr scene)) (car scene) member move (car (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
               (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))) (tf scene member 1 angle tiempoNormal))) seed tiempoNormal)
              (jugadaCPU (append (list 5 5) (moverJugadorIzq (cdr (cdr scene)) (car scene) member move (car (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
               (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)))) seed tiempoNormal)
           )
       )
       null
   )
)

;Funcion que busca si existe un gusano del algun equipo
;Entrada: ;Entrada: Lista con los TDAs del escenario, id del gusano
;Salida: True o False
(define (gusanoIdExiste? listaTDAs id)
  (if (= (length listaTDAs) 0)
      #f
      (if (and (gusano? (car listaTDAs)) (= (gusano_getId (car listaTDAs)) id))
          #t
          (gusanoIdExiste? (cdr listaTDAs) id)
       )
   )
)

;Funcion que verifica si existe el gusano seleccionado
;Entrada: Lista con los TDAs del escenario, gusano a buscar, id del gusano
;Salida: True o False
(define (gusanoExiste? listaTDAs member id)
  (if (= (length listaTDAs) 0)
      #f
      (if (and (gusano? (car listaTDAs)) (= (gusano_getId (car listaTDAs)) id) (= (gusano_getIdGusano (car listaTDAs)) member))
          #t
          (gusanoExiste? (cdr listaTDAs) member id)
       )
   )
)

;Funcion que retorna una lista con la id, posicion X y posicion Y de un gusano
;Entrada: Lista de los TDAs del escenario, gusano a buscar, equipo del gusano a buscar
;Salida: Lista con id, posicion X y posicion Y de un gusano
(define (buscarGusanoIdPosXY listaTDAs member id)
  (if (= (length listaTDAs) 0)
      '()
      (if (and (gusano? (car listaTDAs)) (= (gusano_getIdGusano (car listaTDAs)) member) (= (gusano_getId (car listaTDAs)) id))
          (list (gusano_getId (car listaTDAs)) (gusano_getPosX (car listaTDAs)) (gusano_getPosY (car listaTDAs)))
          (buscarGusanoIdPosXY (cdr listaTDAs) member id)
       )
    )
 )


;Funcion que genera el movimiento a la derecha de un gusano
;Entrada: Lista de los TDAs del escenario, Tamaño eje X, gusano a buscar, cantidad de movimientos,
;         equipo del gusano a buscar, lista con la posicion X e Y del gusano a mover
;Salida: Lista de TDAs con el movimiento del jugador actualizado
(define (moverJugadorDer listaTDAs N member move id posXY)
  (if (or (> (car posXY) N) (<= (car posXY) 0))
      (eliminarGusano listaTDAs id member)
      (if (= move 0)
          (ponerGusano (eliminarGusano listaTDAs id member) posXY id member)
          (if (and (verificarPosDistintasXY (+ (car posXY) 1) (car (cdr posXY)) (obtenerPosXY listaTDAs))
                   (verificarPosDistintasXY (+ (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs)))
              (if (= (length (posXYCaidaGusano (+ (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs))) 0)
                  (eliminarGusano listaTDAs id member)
                  (moverJugadorDer listaTDAs N member (- move 1) id
                   (posXYCaidaGusano (+ (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs)))
               )
              (if (verificarPosDistintasXY (+ (car posXY) 1) (car (cdr posXY)) (obtenerPosXY listaTDAs))
                  (moverJugadorDer listaTDAs N member (- move 1) id (list (+ (car posXY) 1) (car (cdr posXY))))
                  (ponerGusano (eliminarGusano listaTDAs id member) posXY id member)
               )
           )
       )
   )                                                                                 
)

;Funcion que elimina un TDA gusano de una lista TDA
;Entrada: Lista con los TDAs del escenario, id del gusano a eliminar, gusano a buscar
;Salida: Lista de TDAs actualizada
(define (eliminarGusano listaTDAs id member)
  (if (= (length listaTDAs) 0)
      '()
      (if (and (gusano? (car listaTDAs)) (= (gusano_getId (car listaTDAs)) id) (= (gusano_getIdGusano (car listaTDAs)) member))
          (append (cdr listaTDAs) (eliminarGusano '() id member))
          (cons (car listaTDAs) (eliminarGusano (cdr listaTDAs) id member))
       )
   )
 )

;Funcion que inserta un TDA gusano al final de una lista TDA
;Entrada: Lista con los TDAs del escenario, lista con la posicion X e Y del gusano, id del gusano, gusano a insertar
;Salida: Lista de TDAs actualizada
(define (ponerGusano listaTDAs posXY id member)
   (appendList listaTDAs (gusano id member (car posXY) (car (cdr posXY))))
 )

;Funcion que verifica si una posicion XY no este en una lista de listas de dos elementos
;Entrada: Posicion X, posicion Y, lista de listas
;Salida: True o false
(define (verificarPosDistintasXY posX posY listaPosXY)
  (if (= (length listaPosXY) 0)
      #t
      (if (and (= posX (car (car listaPosXY))) (= posY (car (cdr (car listaPosXY)))))
          #f
          (verificarPosDistintasXY posX posY (cdr listaPosXY))
       )
    )
 )

;Funcion que retorna la posixion X e Y de un gusano cuando cae al moverse
;Entrada: Posicion X inicial del gusano, posicion Y inicial del gusano, lista con las posiciones X e Y de los TDAs
;Salida: Lista con la nueva posicion X e Y del gusano
(define (posXYCaidaGusano posX posY listaPosXY)
  (if (and (<= posY 0) (<= (- posY 1) 0))
      '()
      (if (verificarPosDistintasXY posX (- posY 1) listaPosXY)
          (posXYCaidaGusano posX (- posY 1) listaPosXY)
          (list posX posY)
       )
   )
 )

;Funcion que genera el movimiento a la izquierda de un gusano
;Entrada: Lista de los TDAs del escenario, Tamaño eje X, gusano a buscar, cantidad de movimientos,
;         equipo del gusano a buscar, lista con la posicion X e Y del gusano a mover
;Salida: Lista de TDAs con el movimiento del jugador actualizado
(define (moverJugadorIzq listaTDAs N member move id posXY)
  (if (or (> (car posXY) N) (<= (car posXY) 0))
      (eliminarGusano listaTDAs id member)
      (if (= move 0)
          (ponerGusano (eliminarGusano listaTDAs id member) posXY id member)
          (if (and (verificarPosDistintasXY (- (car posXY) 1) (car (cdr posXY)) (obtenerPosXY listaTDAs))
                   (verificarPosDistintasXY (- (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs)))
              (if (= (length (posXYCaidaGusano (- (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs))) 0)
                  (eliminarGusano listaTDAs id member)
                  (moverJugadorIzq listaTDAs N member (- move 1) id (posXYCaidaGusano (- (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs)))
               )
              (if (verificarPosDistintasXY (- (car posXY) 1) (car (cdr posXY)) (obtenerPosXY listaTDAs))
                  (moverJugadorIzq listaTDAs N member (- move 1) id (list (- (car posXY) 1) (car (cdr posXY))))
                  (ponerGusano (eliminarGusano listaTDAs id member posXY id member))
               )
           )
       )
   )                                                                                 
)

;Funcion que realiza un disparo en forma de movimiento rectilineo uniforme (MRU)
;Entrada: Escenario del juego, gusnao que realiza el disparo, id del gusano, angulo del disparo
;Salida: Lista vacia (en caso que el disparo no impacto) o lista con la posicion X e Y del impacto
(define (disparoMRU scene member id angle tiempo)
  (if (checkScene scene)
      (realizarDisparo (car scene) (car (cdr scene)) (cdr (cdr scene)) (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member id)) angle tiempo)
      '()
   )
 )

;Funcion que busca el impacto del disparo realizado por un gusano
;Entrada: Tamaño eje X, tamaño eje Y, lista con los TDAs del escenario, lista con la posicion X e Y del punto de partida del
;         disparo, angulo del disparo
;Salida: Lista vacia (en caso que el disparo no impacto) o lista con la posicion X e Y del impacto
(define (realizarDisparo N M listaTDAs posXY angle tiempo)
  (if (or (<= (ceiling (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo))) 0)
           (> (ceiling (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo))) N)
           (<= (ceiling (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo))) 0)
           (> (ceiling (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo))) M))
      '()
      (if (buscarPosXYOcup listaTDAs (list (ceiling (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo)))
                                           (ceiling (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo)))))
          (list (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo))
                (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo)))
          (realizarDisparo N M listaTDAs (list (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo))
                                               (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo))) angle tiempo)
       )
    )
 )

;Funcion que verifica si una posicion XY ya este utilizada por algun TDA
;Entrada: Lista con los TDAs del escenario, lista con posicion X e Y
;Salida: True o False
(define (buscarPosXYOcup listaTDAs posXY)
  (if (= (length listaTDAs) 0)
      #f
      (if (or (and (gusano? (car listaTDAs)) (= (gusano_getPosX (car listaTDAs)) (ceiling (car posXY)))
               (= (gusano_getPosY (car listaTDAs)) (ceiling (car (cdr posXY)))))
              (and (disparo? (car listaTDAs)) (= (disparo_getPosX (car listaTDAs)) (ceiling (car posXY)))
               (= (disparo_getPosY (car listaTDAs)) (ceiling (car (cdr posXY)))))
              (and (obstaculo? (car listaTDAs)) (= (obstaculo_getPosX (car listaTDAs)) (ceiling (car posXY)))
               (= (obstaculo_getPosY (car listaTDAs)) (ceiling (car (cdr posXY)))))
              (and (suelo? (car listaTDAs)) (= (suelo_getPosX (car listaTDAs)) (ceiling (car posXY)))
               (= (suelo_getPosY (car listaTDAs)) (ceiling (car (cdr posXY))))))
          #t
          (buscarPosXYOcup (cdr listaTDAs) posXY)
       )
    )
 )

;Funcion que elimina un TDA de la lista de los TDAs del escenario
;Entrada: Lista con los TDAs del escenario, lista con posicion X e Y
;Salida: Lista con los TDAs del escenario actualizada
(define (eliminarTDA listaTDAs posXY)
  (if (= (length listaTDAs) 0)
      '()
      (if (or (and (gusano? (car listaTDAs)) (= (gusano_getPosX (car listaTDAs)) (ceiling (car posXY)))
               (= (gusano_getPosY (car listaTDAs)) (ceiling (car (cdr posXY)))))
              (and (disparo? (car listaTDAs)) (= (disparo_getPosX (car listaTDAs)) (ceiling (car posXY)))
               (= (disparo_getPosY (car listaTDAs)) (ceiling (car (cdr posXY)))))
              (and (obstaculo? (car listaTDAs)) (= (obstaculo_getPosX (car listaTDAs)) (ceiling (car posXY)))
               (= (obstaculo_getPosY (car listaTDAs)) (ceiling (car (cdr posXY)))))
              (and (suelo? (car listaTDAs)) (= (suelo_getPosX (car listaTDAs)) (ceiling (car posXY)))
               (= (suelo_getPosY (car listaTDAs)) (ceiling (car (cdr posXY))))))
          (append (cdr listaTDAs) (eliminarTDA '() posXY))
          (cons (car listaTDAs) (eliminarTDA (cdr listaTDAs) posXY))
       )
   )
 )

;Funcion que realiza la jugada de la CPU
;Entrada: Escenario del juego, semilla para generar valores pseudoaleatorios
;Salida: Escenario del juego actualizado
(define (jugadaCPU scene seed tiempoNormal)
  (if (and (gusanoIdExiste? (cdr (cdr scene)) 0) (> (length (tfCPU scene seed tiempoNormal)) 0))
      (append (list (car scene) (car (cdr scene))) (eliminarTDA (cdr (cdr scene)) (tfCPU scene seed tiempoNormal)))
      scene
   )
 )

;Funcion que obtiene una lista con los gusanos del equipo CPU
;Entrada: Lista con los TDAs del escenario
;Salida: Lista con los miembros del equipo CPU
(define (obtenerListMemberCPU listaTDAs)
  (if (= (length listaTDAs) 0)
      '()
      (if (and (gusano? (car listaTDAs)) (= (gusano_getId (car listaTDAs)) 0))
          (cons (gusano_getIdGusano (car listaTDAs)) (obtenerListMemberCPU (cdr listaTDAs)))
          (obtenerListMemberCPU (cdr listaTDAs))
       )
   )
 )

;Funcion que obtiene un gusano aleatorio del equipo CPU
;Entrada: Lista con los miembros del equipo CPU, posicion a obtener de la lista
;Salida: Numero entero
(define (obtenerMemberCPU listaMember cant)
  (if (= cant 0)
      (car listaMember)
      (obtenerMemberCPU (cdr listaMember) (- cant 1))
   )
 )

;Funcion que realiza el disparo de la CPU
;Entrada: Escenario del juego, semilla para generar valores pseudoaleatorios
;Salida: Lista vacia (en caso que el disparo no impacto) o lista con la posicion X e Y del impacto
(define (tfCPU scene seed tiempoNormal)
  (disparoMRU scene (obtenerMemberCPU (obtenerListMemberCPU (cdr (cdr scene)))
                                      (getNumRandom seed (length (obtenerListMemberCPU (cdr (cdr scene)))))) 0 (+ (getNumRandom seed 359) 1) tiempoNormal)
 )

;Funcion que crea una lista infinita en donde cada elemento de esta es una actualizacion del disparo de un gusano
;Entrada: Escenario del juego, personaje elegido, movimiento del personaje (derecha o izquierda), funcion de trayectoria,
;         frecuencia de muestreo, angulo del disparo, semilla para generar valores pseudoaleatorios 
;Salida: Lista infinita
(define (playLazy scene member move tf t angle seed)
  (actualizandoDisparo scene member move tf (/ (tiempoDisparoVuelo (car scene) (car (cdr scene)) (cdr (cdr scene))
                                            (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)) angle tiempo 0) t)
                       angle seed t (/ (tiempoDisparoVuelo (car scene) (car (cdr scene)) (cdr (cdr scene))
                                                           (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)) angle
                                                           tiempo 0) t))
 )

;Funcion que actualiza el disparo de un gusano
;Entrada: Escenario del juego, personaje elegido, movimiento del personaje (derecha o izquierda), funcion de trayectoria,
;         frecuencia de muestreo, angulo del disparo, semilla para generar valores pseudoaleatorios, tiempo maximo de la
;         frecuencia de muestreo, tiempo de la frecuencia de muestreo
;Salida: Lista infinita
(define (actualizandoDisparo scene member move tf t angle seed tiempoMaximo tiempoActualizar)
  (if (>= t  tiempoMaximo)
      '()
      (if (buscarPosXYOcup (cdr (cdr scene )) (actualizarDisparo (cdr (cdr scene))
                                                                 (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                                                                 angle t))
          (cons (append (list 5 5) (eliminarTDA (cdr (cdr scene)) (actualizarDisparo (cdr (cdr scene))
                                                                   (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                                                                   angle t)))
                (lazy (actualizandoDisparo scene member move tf (+ t tiempoActualizar) angle seed tiempoMaximo
                                                                                                          tiempoActualizar)))
          (cons (appendList scene (disparo 2 angle velocidad
                                           (car (actualizarDisparo (cdr (cdr scene))
                                                                   (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                                                                   angle t))
                                           (car (cdr (actualizarDisparo (cdr (cdr scene))
                                                                        (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                                                                        angle t)))))
                (lazy (actualizandoDisparo scene member move tf (+ t tiempoActualizar) angle seed tiempoMaximo
                                                                                                          tiempoActualizar)))
      )
   )
 ) 

;Funcion que busca el tiempo de vuelo del disparo realizado por un gusano
;Entrada: Tamaño eje X, tamaño eje Y, lista con los TDAs del escenario, lista con la posicion X e Y del punto de partida del
;         disparo, angulo del disparo
;Salida: Tiempo de vuelo del gusano
(define (tiempoDisparoVuelo N M listaTDAs posXY angle tiempo cantidad)
  (if (or (<= (ceiling (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo))) 0)
           (> (ceiling (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo))) N)
           (<= (ceiling (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo))) 0)
           (> (ceiling (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo))) M))
      cantidad
      (if (buscarPosXYOcup listaTDAs (list (ceiling (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo)))
                                           (ceiling (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo)))))
          cantidad
          (tiempoDisparoVuelo N M listaTDAs (list (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo))
                                               (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo))) angle tiempo (+ cantidad 1))
       )
    )
 )

;Funcion que actualiza el disparo realizado por un gusano
;Entrada: Tamaño eje X, tamaño eje Y, lista con los TDAs del escenario, lista con la posicion X e Y del punto de partida del
;         disparo, angulo del disparo
;Salida: Lista vacia (en caso que el disparo no impacto) o lista con la posicion X e Y del impacto
(define (actualizarDisparo listaTDAs posXY angle tiempo)
  (list (+ (car posXY) (* (cos (getRad angle)) velocidad tiempo))
        (+ (car (cdr posXY)) (* (sin (getRad angle)) velocidad tiempo)))
 )

