#lang racket

(provide player)

#|
Función = player
Propósito = crear una lista que represente los atributos del jugador
Dominio =
id (numero que identifica al jugador)(número entero) 
name (nombre del player)(cadena de caracteres)
color (color de la ficha)(cadena de caracteres)
wins (victorias del player)(número entero)
losses (derrotas del player)(número entero)
draws (empates del player)(número entero)
remainin-pieces (piezas restantes)(número entero)
recorrido = player (representado como lista)
|#

(define (player id name color wins losses draws remaining-pieces)
  (list id name color wins losses draws remaining-pieces))





















