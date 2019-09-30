#lang racket

;-----------------------------------------------------------------------------------------------
;---------------------------------R. Funcionales Obligatorio------------------------------------
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
;Se define la constante null que entrega una lista vacia
(define null (list ))
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
;----------------------------------------------------------------------------------------------
;--------------------------------------createScene---------------------------------------------
;----------------------------------------------------------------------------------------------
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
;Recursion: Recursion normal natural, debido a que es mas como implementarla
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
;Recursion: Recursion de cola, debido a que se necesita si o si un parametro, no dejando estados pendientes
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion de cola, debido a que se busca crear un gusano, por lo que no se dejan estados pendientes
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

;----------------------------------------------------------------------------------------------
;--------------------------------------checkScene----------------------------------------------
;----------------------------------------------------------------------------------------------
;Funcion que recibe un escenario de juego y verifica si cumple con los criterios para ser considerado un escenario válido
;Entrada: Escenario de juego
;Salida: True o false
(define (checkScene scene)
  (if (and (list? scene) (> (length scene) 2)
           (number? (car scene)) (> (car scene) 0)
           (number? (car (cdr scene))) (> (car (cdr scene)) 0))
      (if (verificarTDAs (cdr (cdr scene)) (car scene) (car (cdr scene)))
          (if (verificarPosDistintas (car (car (obtenerPosXY (cdr (cdr scene)) (list))))
                                     (car (cdr (car (obtenerPosXY (cdr (cdr scene)) (list)))))
                                     (cdr (obtenerPosXY (cdr (cdr scene)) (list))))
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
;Recursion: Recursion de cola debido a que es distinta en la utilizada en createScene
(define (verificarTDAs listaTDAs N M)
  (if (= (length listaTDAs) 0)
      #t
      (if (or (and (gusano? (car listaTDAs))
                   (<= (gusano_getPosX (car listaTDAs)) N) (> (gusano_getPosX (car listaTDAs)) 0)
                   (<= (gusano_getPosY (car listaTDAs)) M) (> (gusano_getPosX (car listaTDAs)) 0))
              (and (disparo? (car listaTDAs))
                   (<= (disparo_getPosX (car listaTDAs)) N) (> (disparo_getPosX (car listaTDAs)) 0)
                   (<= (disparo_getPosY (car listaTDAs)) M) (> (disparo_getPosX (car listaTDAs)) 0))
              (and (obstaculo? (car listaTDAs))
                   (<= (obstaculo_getPosX (car listaTDAs)) N) (> (obstaculo_getPosX (car listaTDAs)) 0)
                   (<= (obstaculo_getPosY (car listaTDAs)) M) (> (obstaculo_getPosX (car listaTDAs)) 0))
              (and (suelo? (car listaTDAs))
                   (<= (suelo_getPosX (car listaTDAs)) N) (> (suelo_getPosX (car listaTDAs)) 0)
                   (<= (suelo_getPosY (car listaTDAs)) M) (> (suelo_getPosX (car listaTDAs)) 0)))
          (verificarTDAs (cdr listaTDAs) N M)
          #f
       )
   )
 )

;Funcion que crea una lista con las posiciones ocupadas por los TDAs
;Entrada: Lista desde el tercer elemento del escenario
;Salida: Lista de listas
;Recursion: Recursion de cola debido a que es distinta en la utilizada en createScene
(define (obtenerPosXY listaTDAs posXY)
  (if (= (length listaTDAs) 0)
      posXY
      (if (gusano? (car listaTDAs))
          (obtenerPosXY (cdr listaTDAs) (appendList posXY (list (gusano_getPosX (car listaTDAs)) (gusano_getPosY (car listaTDAs)))))
          (if (disparo? (car listaTDAs))
              (obtenerPosXY (cdr listaTDAs) (appendList posXY (list (disparo_getPosX (car listaTDAs)) (disparo_getPosY (car listaTDAs)))))
              (if (obstaculo? (car listaTDAs))
                  (obtenerPosXY (cdr listaTDAs) (appendList posXY (list (obstaculo_getPosX (car listaTDAs)) (obstaculo_getPosY (car listaTDAs)))))
                  (if (suelo? (car listaTDAs))
                      (obtenerPosXY (cdr listaTDAs) (appendList posXY (list (suelo_getPosX (car listaTDAs)) (suelo_getPosY (car listaTDAs)))))
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
;Recursion: Recursion de cola debido a que es distinta en la utilizada en createScene
(define (verificarPosDistintas posX posY listaPosXY)
  (if (= (length listaPosXY) 0)
      #t
      (if (and (= posX (car (car listaPosXY))) (= posY (car (cdr (car listaPosXY)))))
          #f
          (verificarPosDistintas (car (car listaPosXY)) (car (cdr (car listaPosXY))) (cdr listaPosXY))
       )
    )
 )

;----------------------------------------------------------------------------------------------
;--------------------------------------play----------------------------------------------------
;----------------------------------------------------------------------------------------------
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
              (jugadaCPU (append (list (car scene) (car (cdr scene))) (eliminarTDA (moverJugadorDer (cdr (cdr scene)) (car scene) member move (car (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                               (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))) (tf scene member 1 angle tiempoNormal))) seed tiempoNormal)
              (jugadaCPU (append (list (car scene) (car (cdr scene))) (moverJugadorDer (cdr (cdr scene)) (car scene) member move (car (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                               (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)))) seed tiempoNormal)
           )
          (if (> (length (tf scene member 1 angle tiempoNormal)) 0)
              (jugadaCPU (append (list (car scene) (car (cdr scene))) (eliminarTDA (moverJugadorIzq (cdr (cdr scene)) (car scene) member move (car (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
               (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))) (tf scene member 1 angle tiempoNormal))) seed tiempoNormal)
              (jugadaCPU (append (list (car scene) (car (cdr scene))) (moverJugadorIzq (cdr (cdr scene)) (car scene) member move (car (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
               (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)))) seed tiempoNormal)
           )
       )
       null
   )
)

;Funcion que busca si existe un gusano del algun equipo
;Entrada: ;Entrada: Lista con los TDAs del escenario, id del gusano
;Salida: True o False
;Recursion: Recursion de cola, debido a que se quiere comprobar algo, por lo que no deja estados pendientes
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
;Recursion: Recursion de cola, debido a que se quiere comprobar algo, por lo que no deja estados pendientes
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion de cola, debido a que se quiere mover un gusano, por lo que no deja estados pendientes
(define (moverJugadorDer listaTDAs N member move id posXY)
  (if (or (> (car posXY) N) (<= (car posXY) 0))
      (eliminarGusano listaTDAs id member)
      (if (= move 0)
          (ponerGusano (eliminarGusano listaTDAs id member) posXY id member)
          (if (and (verificarPosDistintasXY (+ (car posXY) 1) (car (cdr posXY)) (obtenerPosXY listaTDAs (list)))
                   (verificarPosDistintasXY (+ (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs (list))))
              (if (= (length (posXYCaidaGusano (+ (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs (list)))) 0)
                  (eliminarGusano listaTDAs id member)
                  (if (buscarTDAGusano listaTDAs (car (posXYCaidaGusano (+ (car posXY) 1) (- (car (cdr posXY)) 1)
                                                                        (obtenerPosXY listaTDAs (list))))
                                       (- (car (cdr (posXYCaidaGusano (+ (car posXY) 1) (- (car (cdr posXY)) 1)
                                                                   (obtenerPosXY listaTDAs (list))))) 1))
                      listaTDAs
                      (moverJugadorDer listaTDAs N member (- move 1) id
                                       (posXYCaidaGusano (+ (car posXY) 1) (- (car (cdr posXY)) 1)
                                                         (obtenerPosXY listaTDAs (list))))
                   )
                  
               )
              (if (verificarPosDistintasXY (+ (car posXY) 1) (car (cdr posXY)) (obtenerPosXY listaTDAs (list)))
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion de cola, debido a que se quiere comprobar algo, por lo que no deja estados pendientes
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion de cola, debido a que se quiere mover un gusano, por lo que no deja estados pendientes
(define (moverJugadorIzq listaTDAs N member move id posXY)
  (if (or (> (car posXY) N) (<= (car posXY) 0))
      (eliminarGusano listaTDAs id member)
      (if (= move 0)
          (ponerGusano (eliminarGusano listaTDAs id member) posXY id member)
          (if (and (verificarPosDistintasXY (- (car posXY) 1) (car (cdr posXY)) (obtenerPosXY listaTDAs (list)))
                   (verificarPosDistintasXY (- (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs (list))))
              (if (= (length (posXYCaidaGusano (- (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs (list)))) 0)
                  (eliminarGusano listaTDAs id member)
                  (if (buscarTDAGusano listaTDAs (car (posXYCaidaGusano (- (car posXY) 1) (- (car (cdr posXY)) 1)
                                                                        (obtenerPosXY listaTDAs (list))))
                                       (- (car (cdr (posXYCaidaGusano (- (car posXY) 1) (- (car (cdr posXY)) 1)
                                                                   (obtenerPosXY listaTDAs (list))))) 1))
                      listaTDAs
                      (moverJugadorIzq listaTDAs N member (+ move 1) id (posXYCaidaGusano (- (car posXY) 1) (- (car (cdr posXY)) 1) (obtenerPosXY listaTDAs (list))))
                   )
               )
              (if (verificarPosDistintasXY (- (car posXY) 1) (car (cdr posXY)) (obtenerPosXY listaTDAs (list)))
                  (moverJugadorIzq listaTDAs N member (+ move 1) id (list (- (car posXY) 1) (car (cdr posXY))))
                  (ponerGusano (eliminarGusano listaTDAs id member posXY id member))
               )
           )
       )
   )                                                                                 
)

;Funcion que verifica si un gusano cae arriba de otro gusano
;Entrada: Lista de TDAs del escenario, posicion X a buscar, posicion Y a buscar
;Salida: True or False
;Recursion: Recursion de cola, debido a que se quiere verificar algo, por lo que no deja estados pendientes
(define (buscarTDAGusano listaTDAs posX posY)
  (if (and (gusano? (car listaTDAs)) (= (gusano_getPosX (car listaTDAs)) posX)
           (= (gusano_getPosY (car listaTDAs)) posY))
      #t
      (if (and (obstaculo? (car listaTDAs)) (= (obstaculo_getPosX (car listaTDAs)) posX)
               (= (obstaculo_getPosY (car listaTDAs)) posY))
          #f
          (if (and (suelo? (car listaTDAs)) (= (suelo_getPosX (car listaTDAs)) posX)
                   (= (suelo_getPosY (car listaTDAs)) posY))
              #f
              (buscarTDAGusano (cdr listaTDAs) posX posY)
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion de cola, debido a que se quiere comprobar algo, por lo que no deja estados pendientes
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion natural, debido a que es mas facil implementarla
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
;Recursion: Recursion de cola, debido a que se quiere obtener un gusano aleatorio, por lo que no deja
;           estados pendientes
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

;----------------------------------------------------------------------------------------------
;--------------------------------------playLazy------------------------------------------------
;----------------------------------------------------------------------------------------------
;Funcion que crea una lista infinita en donde cada elemento de esta es una actualizacion del disparo de un gusano
;Entrada: Escenario del juego, personaje elegido, movimiento del personaje (derecha o izquierda), funcion de trayectoria,
;         frecuencia de muestreo, angulo del disparo, semilla para generar valores pseudoaleatorios 
;Salida: Lista infinita
(define (playLazy scene member move tf t angle seed)
  (if (checkScene scene)
      (if (= (/ (tiempoDisparoVuelo (car scene) (car (cdr scene)) (cdr (cdr scene))
                                    (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)) angle tiempo 0) t) 0)
          (actualizandoDisparo scene member move tf 1 angle seed t 0)
          (actualizandoDisparo scene member move tf (/ (tiempoDisparoVuelo (car scene) (car (cdr scene)) (cdr (cdr scene))
                                                                           (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)) angle tiempo 0) t)
                               angle seed t (/ (tiempoDisparoVuelo (car scene) (car (cdr scene)) (cdr (cdr scene))
                                                                   (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1)) angle tiempo 0) t))
       )
      '()
    )
 )

;Funcion que actualiza el disparo de un gusano
;Entrada: Escenario del juego, personaje elegido, movimiento del personaje (derecha o izquierda), funcion de trayectoria,
;         frecuencia de muestreo, angulo del disparo, semilla para generar valores pseudoaleatorios, tiempo maximo de la
;         frecuencia de muestreo, tiempo de la frecuencia de muestreo
;Salida: Lista infinita
;Recursion: Recursion natural, debido a que es mas facil implementarla
(define (actualizandoDisparo scene member move tf t angle seed tiempoMaximo tiempoActualizar)
  (if (>= t tiempoMaximo)
      '()
      (if (buscarPosXYOcup (cdr (cdr scene)) (actualizarDisparo (cdr (cdr scene))
                                                                 (cdr (buscarGusanoIdPosXY (cdr (cdr scene)) member 1))
                                                                 angle t))
          (cons (append (list (car scene) (car (cdr scene))) (eliminarTDA (cdr (cdr scene)) (actualizarDisparo (cdr (cdr scene))
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
;Recursion: Recursion de cola, debido a que se quiere obtener el tiempo de vuelo, por lo que no deja
;           estados pendientes
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

;----------------------------------------------------------------------------------------------
;--------------------------------------scene->string-------------------------------------------
;----------------------------------------------------------------------------------------------
;Funcion que retorna la representacion del espacio en forma de string
;Entrada: Escenario del juego
;Salida: String que tiene que ser pasado a delay para ver de forma correcta
(define (scene->string scene)
  (if (checkScene scene)
      (matrizAString (matrizConTDAs (car scene) (car (cdr scene)) (cdr (cdr scene))
                                (crearMatriz (car scene) (car (cdr scene)))) "")
      (matrizAString (matrizConTDAs (car (car scene)) (car (cdr (car scene))) (cdr (cdr (car scene)))
                                (crearMatriz (car (car scene)) (car (cdr (car scene))))) "")
   )
 )

;Funcion que recorre la matriz con los TDAs para converirlos a string
;Entrada: Matriz con los TDAs, escenario del juego en string
;Salida: Escenario del juego en string
;Recursion: Recursion de cola, debido a que se quiere obtener el scene en string algo, por lo que no deja
;           estados pendientes
(define (matrizAString matrizScene sceneString)
  (if (= (length matrizScene) 0)
      sceneString
      (matrizAString (cdr matrizScene) (matrizAStringFila (car matrizScene) sceneString))
    )
 )

;Funcion que recorre las filas de la matriz con los TDAs para converirlos a string
;Entrada: Fila de la matriz con los TDAs, escenario del juego en string
;Salida: Escenario del juego en string
;Recursion: Recursion de cola, debido a que se quiere obtener el scene en string algo, por lo que no deja
;           estados pendientes, se van guardando en sceneString
(define (matrizAStringFila matrizSceneFila sceneString)
  (if (= (length matrizSceneFila) 0)
       (string-append sceneString "\n")
       (if (and (gusano? (car matrizSceneFila)) (= (gusano_getId (car matrizSceneFila)) 0))
           (matrizAStringFila (cdr matrizSceneFila) (string-append sceneString "E"))
           (if (and (gusano? (car matrizSceneFila)) (= (gusano_getId (car matrizSceneFila)) 1))
               (matrizAStringFila (cdr matrizSceneFila) (string-append sceneString "R"))
               (if (obstaculo? (car matrizSceneFila))
                   (matrizAStringFila (cdr matrizSceneFila) (string-append sceneString "#"))
                   (if (disparo? (car matrizSceneFila))
                       (matrizAStringFila (cdr matrizSceneFila) (string-append sceneString "o"))
                       (if (suelo? (car matrizSceneFila))
                           (matrizAStringFila (cdr matrizSceneFila) (string-append sceneString "-"))
                           (matrizAStringFila (cdr matrizSceneFila) (string-append sceneString " "))
                        )
                    )
                )
            )
        )
    )
 )

;Funcion que retorna los TDAs del juego en una matriz
;Entrada: Tamaño eje X, tamao eje Y, lista con los TDAs del escenario, matriz que representa el juego
;Salida: Matriz que representa el escenario del juego
;Recursion: Recursion de cola, debido a que se quiere crear la matriz almacenandola en la entrada matriz, por lo
;           que no deja estados pendientes
(define (matrizConTDAs N M listaTDAs matriz)
  (if (= (length listaTDAs) 0)
      matriz
      (if (gusano? (car listaTDAs))
          (matrizConTDAs N M (cdr listaTDAs) (ponerTDAenMatriz N M matriz (gusano_getPosX (car listaTDAs))
                                                               (gusano_getPosY (car listaTDAs)) (car listaTDAs)))
          (if (obstaculo? (car listaTDAs))
              (matrizConTDAs N M (cdr listaTDAs) (ponerTDAenMatriz N M matriz (obstaculo_getPosX (car listaTDAs))
                                                               (obstaculo_getPosY (car listaTDAs)) (car listaTDAs)))
              (if (disparo? (car listaTDAs))
                  (matrizConTDAs N M (cdr listaTDAs) (ponerTDAenMatriz N M matriz (disparo_getPosX (car listaTDAs))
                                                               (disparo_getPosY (car listaTDAs)) (car listaTDAs)))
                  (if (suelo? (car listaTDAs))
                      (matrizConTDAs N M (cdr listaTDAs) (ponerTDAenMatriz N M matriz (suelo_getPosX (car listaTDAs))
                                                               (suelo_getPosY (car listaTDAs)) (car listaTDAs)))
                      '()
                   )
               )
           )
       )
   )
 )

;Funcion que crea una matriz del tamaño NxM
;Entrada: Tamaño eje X, tamaño eje Y
;Salida: Lista de listas (matriz)
;Recursion: Recursion natural, debido a que es mas facil implementarla
(define (crearMatriz N M)
  (if (= M 0)
      '()
      (cons (crearMatrizFila N) (crearMatriz N (- M 1)))
   )
 )

;Funcion que crea las filas de una matriz
;Entrada: Tamaño eje X
;Salida: Lista
;Recursion: Recursion natural, debido a que es mas facil implementarla
(define (crearMatrizFila N)
  (if (= N 0)
      '()
      (cons 0 (crearMatrizFila (- N 1)))
   ) 
 )

;Funcion que posiciona un TDA en una matriz
;Entrada: Tamaño eje X, tamaño eje Y, matriz escenario del juego, posicion X del TDA, posicion Y del TDA
;         TDA a posicionar en la matriz
;Salida: Matriz actualizada
;Recursion: Recursion natural, debido a que es mas facil implementarla
(define (ponerTDAenMatriz N M matriz posX posY tda)
  (if (= M 0)
      '()
      (if (= M (ceiling posY))
          (cons (ponerTDAenMatrizFila N (car matriz) posX tda 1) (ponerTDAenMatriz N (- M 1) (cdr matriz) posX posY tda))
          (cons (car matriz) (ponerTDAenMatriz N (- M 1) (cdr matriz) posX posY tda))
       )
   )
 )

;Funcion que posiciona un TDA en una lista
;Entrada: Tamaño eje X, fila de la matriz escenario del juego, posicion X del TDA, TDA a posicionar en
;         la lista, auxiliar para recorrer la lista
;Salida: Lista actualizada
;Recursion: Recursion natural, debido a que es mas facil implementarla
(define (ponerTDAenMatrizFila N matrizFila posX tda aux)
  (if (and (= aux (ceiling posX)) (< aux N))
      (cons tda (ponerTDAenMatrizFila N (cdr matrizFila) posX tda (+ aux 1)))
      (if (and (= aux (ceiling posX)) (= aux N))
          (cons tda '())
          (if (>= aux N)
              (cons (car matrizFila) '())
              (cons (car matrizFila) (ponerTDAenMatrizFila N (cdr matrizFila) posX tda (+ aux 1)))
           )
       )
   )
 )

(require "gusanoTDA.rkt")
(require "disparoTDA.rkt")
(require "obstaculoTDA.rkt")
(require "sueloTDA.rkt")

;Ejemplos de uso
;- createScene: 1.- (createScene 5 5 2 3 5)
;               2.- (createScene 5 10 4 1 812361)
;               3.- (createScene 10 10 6 2 5498233)

;- checkScene: 1.- (checkScene (createScene 5 10 4 1 812361))
;              2.- (checkScene (createScene 10 10 6 2 5498233))
;              3.- (checkScene (list 5 5 (list 1 1) (list 2 1) (list 3 1) (list 4 1) (list 5 1) (list 3 5 2)
;                  (list 3 3 2) (list 3 1 2) (list 3 2 2) (list 3 4 2) (list 1 1 1 3) (list 1 2 2 3)
;                  (list 1 3 3 3) (list 0 1 5 3) (list 0 2 6 3)))
 
;- play: 1.- ((((((play (createScene 5 5 2 3 5)) 3) 1) disparoMRU) 0) 12345)
;        2.- ((((((play (createScene 5 10 4 1 812361)) 1) -1) disparoMRU) 45) 2546)
;        3.- ((((((play (createScene 10 10 6 2 5498233)) 2) 2) disparoMRU) 270) 1524554)

;- playLazy: 1.- (playLazy (createScene 5 5 2 3 5) 3 1 disparoMRU 5 90 12345)
;            2.- (playLazy (createScene 5 10 4 1 812361) 1 1 disparoMRU 3 0 54545)
;            3.- (playLazy (createScene 10 10 6 2 5498233) 2 2 disparoMRU 6 45 54534)

;- scene>string: 1.- (display (scene->string (createScene 5 10 4 1 812361)))
;                2.- (display (scene->string ((((((play (createScene 10 10 6 2 5498233)) 2) 2) disparoMRU) 270) 1524554)))
;                3.- (display (scene->string (playLazy (createScene 5 5 2 3 5) 3 1 disparoMRU 5 90 12345)))