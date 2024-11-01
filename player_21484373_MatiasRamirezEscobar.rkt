#lang racket


(provide player)
(provide player-update-stats)

#|
Función = player

Propósito = Función que permite crear un jugador.

Dominio =
id (numero que identifica al jugador)(número entero) x 
name (nombre del player)(cadena de caracteres)       x
color (color de la ficha)(cadena de caracteres)      x
wins (victorias del player)(número entero)           x
losses (derrotas del player)(número entero)          x
draws (empates del player)(número entero)            x
remainin-pieces (piezas restantes)(número entero)    

Recorrido = player (representado como lista)

Recursión = no aplica

Tipo = Constructor
|#

(define (player id name color wins losses draws remaining-pieces)
  (list id name color wins losses draws remaining-pieces)
  )

#|
Función = player-update-stats

Propósito = Función que actualiza las estadísticas del jugador, ya sea victoria, derrotas o empates.

Dominio = player x result(string: "win", "loss", o "draw")

Recorrido = player

Recursión = no aplica

Tipo = Otros
|#

(define (player-update-stats player result)
  (cond
    [(string=? result "win") (sumar1 3 player)]
    [(string=? result "loss") (sumar1 4 player)]
    [(string=? result "draw") (sumar1 5 player)]
    ))


(define (sumar1 n player);suma uno segun la posicion que se asigno segun si pierde gana o empata
  (if (= n 0)
      (cons (+ (car player) 1) (cdr player))
      (cons (car player) (sumar1 (- n 1) (cdr player)))
      ))


