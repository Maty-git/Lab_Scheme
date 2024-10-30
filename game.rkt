#lang racket

(require "board.rkt")
(require "tda_player.rkt")
(require "tda_pieces.rkt")
(provide game)
(provide game-is-draw?)
(provide game-get-current-player)
(provide game-get-board)
(provide game-set-end)
(provide game-player-set-move)


#|
Función = game

Propósito = Función que permite crear una nueva partida.

Dominio = player1 (player) X player2 (player) X board (board) X current-turn (int)

Recorrido = game

Recursión = no aplica

Tipo = constructor
|#

(define (game player1 player2 board nturno)
  (list player1 player2 board nturno))

#|
Función = game-is-draw?

Propósito = Función que verifica si el estado actual del juego es empate.

Dominio = game (game)

Recorrido = boolean (#t si es empate, #f si no)

Recursión = no aplica

Tipo = otros
|#

(define (game-history game columna)
  #t)
(define historial '())


#|
Función = game-is-draw?

Propósito = Función que verifica si el estado actual del juego es empate.

Dominio = game (game)

Recorrido = boolean (#t si es empate, #f si no)

Recursión = no aplica

Tipo = otros
|#

(define (game-is-draw? game)
  (if (board-can-play? (list-ref game 2));revisa si se puede jugar (el tablero no esta lleno)
      (if (les-quedan-fichas? (car game) (car (cdr game)))
          #f
          #t
          )
      #t
      )  
  )

(define (les-quedan-fichas? p1 p2)
  (cond
  [(and (= (last p1) 0) (= (last p2) 0)) #f]
  [else #t])
  )

#|
Función = game-get-current-player

Propósito = Función que obtiene el jugador cuyo turno está en curso.

Dominio = game (game)

Recorrido = player

Recursión = no aplica

Tipo = selector
|#


(define (game-get-current-player game)
  (if (= (modulo (last game) 2) 0)
      (list-ref game 1)
      (list-ref game 0)
      ))

#|
Función = game-get-board

Propósito = Función que entrega el estado actual del tablero en el juego.

Dominio = game

Recorrido = board

Recursión = no aplica

Tipo = selector
|#

(define (game-get-board game)
  (displayln (list-ref game 2)))

#|
Función = game-set-end

Propósito = Función finaliza el juego actualizando las estadísticas de los jugadores según el resultado.

Dominio = Game (game)

Recorrido = game

Recursión = de cola

Tipo = modificador
|#


(define (game-set-end game)
  (display game)
  (if (= (board-who-is-winner (list-ref game 2)) 0)
      (if (game-is-draw? game)
          (actualizar-ambos game)
          (no-actualiza game)
          )
      (cond
        [(= (board-who-is-winner (list-ref game 2)) 1) (actualizar-player game "red")];devuelve la actualizacion del jugador R
        [(= (board-who-is-winner (list-ref game 2)) 2) (actualizar-player game "yellow")];devuelve la actualizacion del jugador Y
        ));si no es ninguno nisiquiera se deve ejecutar porq deveria seguir el juego
  )
  

(define (actualizar-player game str)
  ;(game-get-board game)
  (if (string=? (list-ref (car game) 2) str)
      (list (player-update-stats (car game) "win")
            (player-update-stats (car (cdr game)) "loss")
            (list-ref game 2)
            (last game))
      (list (player-update-stats (car game) "loss")
            (player-update-stats (car (cdr game)) "win")
            (list-ref game 2)
            (last game))
      ))

(define (actualizar-ambos game)
  ;(game-get-board game)
  (list (player-update-stats (car game) "draw")
        (player-update-stats (car (cdr game)) "draw")
        (list-ref game 2)
        (last game)))

(define (no-actualiza game)
  ;(game-get-board game)
  (list (car game)
        (car (cdr game))
        (list-ref game 2)
        (+ (last game) 1)))

#|
Función = game-player-set-move

Propósito = Función que realiza un movimiento.

Dominio = game (game) X player (player) X column (int) 

Recorrido = game 

Recursión = cola

Tipo = modificador
|#

(define (game-player-set-move game player columna)
  (if (= (car (game-get-current-player game)) (car player)) ;verifica si el jugador es el correcto
      (if (= (last (game-get-current-player game)) 0)
          (game-set-end (list (car game)
                              (car (cdr game))
                              (list-ref game 2)
                              (last game)))
          (game-set-end (list (ver-si-juega-p1 game player)
                              (ver-si-juega-p2 game player)
                              (actualizar-game game player columna)
                              (last game)))
          ) ;si es el correcto comienza a actualizar el juego
      game                                    ;si no es el correcto da el mensaje por consola y devuelve el mismo game ya que esta función solo retorna games
      ))

(define (ver-si-juega-p1 game player )
  (if (= (car (first game)) (car player))
      (disminuye-ficha (first game))
      (first game)
      ))

(define (ver-si-juega-p2 game player )
  (if (= (car (second game)) (car player))
      (disminuye-ficha (second game))
      (second game)
      ))

(define (disminuye-ficha player)
  (cond
    [(null? (cdr player)) (list (- (first player) 1))]
    [(cons (car player) (disminuye-ficha (cdr player)))]
    ))

(define (actualizar-game game player columna)
  (if (board-can-play? (list-ref game 2)) ;verifica si se puede jugar en el board
      (board-set-play-piece (list-ref game 2) columna (elegir-pieza player)) ;si se puede va a poner una ficha
      (list-ref game 2)                                                                ;si no se puede devuelve el mismo board
      ))

(define (elegir-pieza player)
  (if (string=? (list-ref player 2) "red")
      (piece "red")
      (piece "yellow")
      ))

