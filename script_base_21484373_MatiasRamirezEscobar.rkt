#lang racket
(require "player_21484373_MatiasRamirezEscobar.rkt");importa el player
(require "piece_21484373_MatiasRamirezEscobar.rkt");importa la pieza
(require "board_21484373_MatiasRamirezEscobar.rkt");importa el board
(require "game_21484373_MatiasRamirezEscobar.rkt")


(define p1 (player 1 "Juan" "red" 0 0 0 10))
(define p2 (player 2 "Mauricio" "yellow" 0 0 0 10))

(define empty-board (board))

(define g0 (game p1 p2 empty-board 1));se crea el game donde se juega

 
(define g1 (game-player-set-move g0 p1 1))  ; Juan coloca en columna 1
(define g2 (game-player-set-move g1 p2 2))  ; Mauricio coloca en columna 2
(define g3 (game-player-set-move g2 p1 2))  ; Juan coloca en columna 2
(define g4 (game-player-set-move g3 p2 3))  ; Mauricio coloca en columna 3
(define g5 (game-player-set-move g4 p1 3))  ; Juan coloca en columna 3
(define g6 (game-player-set-move g5 p2 4))  ; Mauricio coloca en columna 4
(define g7 (game-player-set-move g6 p1 3))  ; Juan coloca en columna 3
(define g8 (game-player-set-move g7 p2 4))  ; Mauricio coloca en columna 4
(define g9 (game-player-set-move g8 p1 4))  ; Juan coloca en columna 4
(define g10 (game-player-set-move g9 p2 1)) ; Mauricio coloca en columna 1
(define g11 (game-player-set-move g10 p1 4)) ; Juan coloca en columna 4 (victoria diagonal)

; 6. Verificaciones durante el juego
(displayln "¿Se puede jugar en el tablero vacío? ")
(board-can-play? empty-board)

(displayln "¿Se puede jugar después de 11 movimientos? ")
(board-can-play? (game-get-board g11))

(displayln "Jugador actual después de 11 movimientos: ")
(game-get-current-player g11)

; 7. Verificación de victoria
(displayln "Verificación de victoria vertical: ")
(board-check-vertical-win (game-get-board g11))

(displayln "Verificación de victoria horizontal: ")
(board-check-horizontal-win (game-get-board g11))

(displayln "Verificación de victoria diagonal: ")
(board-check-diagonal-win (game-get-board g11))

(displayln "Verificación de ganador: ")
(board-who-is-winner (game-get-board g11))

; 8. Verificación de empate
(displayln "¿Es empate? ")
(game-is-draw? g11)

; 11. Mostrar historial de movimientos
(displayln "Historial de movimientos: ")
(game-history g11)

; 12. Mostrar estado final del tablero
(displayln "Estado final del tablero: ")
(game-get-board g11)
