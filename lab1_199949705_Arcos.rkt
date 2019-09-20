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
      (append (crearSuelo N M) (crearObst N M D seed) (crearJugador N (crearObst N M D seed) D 1 1)
              (crearEnemigo N (crearObst N M D seed) E 1 5))
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
      (if (and (> auxM 0) (< auxM M) (<= auxN (/ N 3)))
          (cons (list auxN auxM) (crearSueloRL N M auxN (+ auxM 1)))
          (if (and (= auxM M) (<= auxN N))
              (cons (list auxN auxM) (crearSueloRL N M (+ auxN 1) 1))
              (if (and (<= auxN (/ N 3)) (<= auxN N))
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
      (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed M) 1) seed M (list)))
      (if (and (= (remainder N 3) 0) (= D 2))
          (append (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed M) 1) seed M (list)))
                  (crearObstMedio N M (+ (/ N 3) 1)
                                  (obtenerNum (getListaRandomR (+ (getNumRandom seed M) 1) seed M (list))
                                              (+ (getNumRandom seed M) 1) 1) (+ (getNumRandom seed N) 1)))
          (if (= D 2)
              (append (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed M) 1) seed M (list)))
                      (crearObstMedio N M (+ (truncate (/ N 3)) 2)
                                      (obtenerNum (getListaRandomR (+ (getNumRandom seed M) 1) seed M (list))
                                                  (+ (getNumRandom seed M) 1) 1) (+ (getNumRandom seed N) 1)))
              (if (and (= (remainder N 3) 0) (= D 1))
                  (append (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed M) 1) seed M (list)))
                          (crearObstDificil N M seed (+ (/ N 3) 1) (getListaRandomR (+ (getNumRandom seed M) 1)
                                                                                    seed M (list)) '()))
                  (append (crearObstFacil N M (myRandom seed) (getListaRandomR (+ (getNumRandom seed M) 1) seed M (list)))
                          (crearObstDificil N M seed (+ (truncate (/ N 3)) 2)
                                            (getListaRandomR (+ (getNumRandom seed M) 1) seed M (list)) '()))
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
;Entrada: Tamaño eje X, tamaño eje Y, semilla para generar valores pseudoaleatorios
;         lista con la posicion Y en donde van los obstaculos
;Salida: Lista que contiene TDA obstaculos
(define (crearObstFacil N M seed lista)
  (if (= (length lista) 0)
      '()
      (if (= (remainder N 3) 0)
          (cons (obstaculo 3 (/ N 3) (car lista)) (crearObstFacil N M seed (cdr lista)))
          (cons (obstaculo 3 (+ (truncate (/ N 3)) 1) (car lista)) (crearObstFacil N M seed (cdr lista)))
        )
   )
 )

;Funcion que crea los obstaculos para la dificultad medio, se elige un obstaculo creado en la funcion
;crearObstFacil y se aumenta su tamaño en el ejeX
;Entrada: Tamaño eje X, tamaño eje Y, posicion X en donde se comenzara a generar el obstaculo, posicion Y del obstaculo y
;         posicion X hasta donde se puede generar el obstaculo
;Salida: Lista que contiene TDA obstaculos
(define (crearObstMedio N M comienzo posicion final)
  (if (or (> comienzo final) (= comienzo N))
      '()
      (cons (obstaculo 3 comienzo posicion) (crearObstMedio N M (+ comienzo 1) posicion final))
   )
 )

;Funcion que crea los obstaculos para la dificultad dificil, se eligen todos los obstaculos creados en la funcion
;crearObstFacil y se aumenta su tamaño en el ejeX
;Entrada: Tamaño eje X, tamaño eje Y, semilla para generar valores pseudoaleatorios, posicion X en donde se comenzaran
;         a generar los obstaculos, lista con las posiciones X de los obstaculos ya creados, lista resultante
;Salida: Lista que contiene TDA obstaculos
(define (crearObstDificil N M seed comienzo listaObst listaFinal)
  (if (= (length listaObst) 0)
      listaFinal
      (crearObstDificil N M (myRandom seed) comienzo (cdr listaObst)
                        (append listaFinal (crearObstMedio N M comienzo (car listaObst) (+ (getNumRandom seed N) 1))))
   )
 )

;Funcion que crea los gusanos que controla el usuario
;Entrada: Tamaño eje X, lista con el suelo y los obstaculos, cantidad de gusanos del usuario, auxiliar para el parametro
;         id del constructor gusano y fila en donde se comenzaran a crear
;Salida: Lista que contiene los TDA gusanos del usuario
(define (crearJugador N listaEscenario cantJugador auxCantJugador fila)
  (if (> auxCantJugador cantJugador)
      '()
      (append (crearGusano N listaEscenario auxCantJugador fila 0 1) (crearJugador N listaEscenario cantJugador
                                                                  (+ auxCantJugador 1) (+ fila 1)))
   )
 )

;Funcion que crea los gusanos que controla la CPU
;Entrada: Tamaño eje X, lista con el suelo y los obstaculos, cantidad de gusanos de la CPU, auxiliar para el parametro
;         id del constructor gusano y fila en donde se comenzaran a crear
;Salida: Lista que contiene los TDA gusanos de la CPU
(define (crearEnemigo N listaEscenario cantEnemigos auxCantEnemigos fila)
  (if (> auxCantEnemigos cantEnemigos)
      '()
      (append (crearGusano N listaEscenario auxCantEnemigos fila 0 0) (crearEnemigo N listaEscenario cantEnemigos
                                                                  (+ auxCantEnemigos 1) (- fila 1)))
   )
 )

;Funcion que crea un TDA gusano
;Entrada: Tamaño eje X, lista con el suelo y los obstaculos, cantidad de gusanos creados, fila en donde se comenzaran a crear,
;         auxiliar para saber si el gusano va encima de un obstaculo o el suelo y parametro id del constructor gusano
;Salida: Lista con un TDA gusano
(define (crearGusano N listaEscenario auxCantJugador fila mayor id)
  (if (and (= (length listaEscenario) 0) (> mayor 0))
      (cons (gusano id auxCantJugador (+ mayor 1) fila) '())
      (if (and (= (length listaEscenario) 0) (= (remainder N 3) 0))
          (cons (gusano id auxCantJugador (+ (/ N 3) 1) fila) '())
          (if (= (length listaEscenario) 0)
              (cons (gusano id auxCantJugador (+ (truncate (/ N 3)) 1) fila) '())
              (if (and (obstaculo? (car listaEscenario)) (= fila (obstaculo_getPosY (car listaEscenario)))
                       (> (obstaculo_getPosX (car listaEscenario)) mayor))
                  (crearGusano N (cdr listaEscenario) auxCantJugador fila (obstaculo_getPosX (car listaEscenario)) id)
                  (crearGusano N (cdr listaEscenario) auxCantJugador fila mayor id)
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
