#lang racket
(provide board-set-play-piece)

(require "board.rkt")
(require "pieces.rkt")


(define (board-set-play-piece tablero columna pieza)
  
  (define (aux tablero n_fila columna pieza)
    (define fila (list-ref tablero n_fila))
    (cond
      [(= n_fila -1) (displayln "error no se puede jugar en esta columna")]
      [(null? (list-ref fila (- columna 1)))
       (cambiar-valor-lista tablero n_fila (cambiar-valor-lista fila (- columna 1) pieza))]
      [else (aux tablero (- n_fila 1) columna pieza)]))
  (aux tablero 5 columna pieza)
  )

(define (cambiar-valor-lista lista posicion valor)
  (cond
    [(null? lista) "error"]
    [(= posicion 0) (cons valor (cdr lista))]
    [else (cons (car lista) (cambiar-valor-lista (cdr lista) (- posicion 1) valor))]))
