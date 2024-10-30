#lang racket
(require "tda_pieces.rkt")
(provide board)
(provide board-can-play?)
(provide board-set-play-piece)
(provide board-check-vertical-win)
(provide board-check-horizontal-win)
(provide board-check-diagonal-win)
(provide board-who-is-winner)


#|
Función = board

Propósito = generar un tablero vacio, en una estructura basada en filas 

Dominio = no recibe parametros

Recorrido = board

Recursión = no aplica

Tipo = constructor
|#

(define (board)
  (list (lista-vacia) (lista-vacia) (lista-vacia)
        (lista-vacia) (lista-vacia) (lista-vacia)))

(define (lista-vacia)
  (list null null null null null null null))

#|
Función = board-can-play?

Propósito = verificar si se puede hacer una jugada

Dominio = board(board)

Recorrido = boolean (#t si se puede jugar, #f si no)

Recursión = natural 

Tipo = otros
|#

(define (board-can-play? board)
  (cond
    [(empty? board)#f]
    [(revisar-fila (car board)) #t]
    [else #f]))

(define (revisar-fila lista2)
  (cond
   [(empty? lista2)#f]
   [(null? (car lista2)) #t]
   [(not (null? (car lista2))) (revisar-fila (cdr lista2))]
   [else #f]))

#|
Función = board-set-play-piece

Propósito = poner ficha en la posicion más baja de la columna dada

Dominio = board (board) X column (int) X piece (piece)

Recorrido = board

Recursión = natural

Tipo = modificador
|#

(define (board-set-play-piece tablero columna pieza)
  
  (define (aux tablero n_fila columna pieza)
    (cond
      [(= n_fila -1) tablero]
      [(null? (list-ref (list-ref tablero n_fila) (- columna 1)))
       (cambiar-valor-lista tablero n_fila (cambiar-valor-lista (list-ref tablero n_fila) (- columna 1) pieza))]
      [else (aux tablero (- n_fila 1) columna pieza)]))
  (aux tablero 5 columna pieza)
  )

(define (cambiar-valor-lista lista posicion valor)
  (cond
    [(= posicion 0) (cons valor (cdr lista))]
    [else (cons (car lista) (cambiar-valor-lista (cdr lista) (- posicion 1) valor))]))

#|
Función = board-check-vertical-win

Propósito = Función que permite verificar el estado actual del tablero y entregar
 el posible ganador que cumple con la regla de conectar 4 fichas de forma vertical.

Dominio = board (board)

Recorrido = int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador vertical)

Recursión = natural

Tipo = otros
|#

(define (board-check-vertical-win tablero)
  (define (revisar-fila tablero fila col nfila)
    (cond
      [(= nfila 4) 0]
      [(empty? fila)
       (revisar-fila tablero (list-ref tablero (+ 1 nfila)) 0 (+ 1 nfila))]
      [(null? (car fila)) (revisar-fila tablero (cdr fila) (+ col 1) nfila)]
      [(encontrov tablero nfila col 0 "R") 1]
      [(encontrov tablero nfila col 0 "Y") 2]
      [else (revisar-fila tablero (cdr fila) (+ col 1) nfila)]))
  (revisar-fila tablero (list-ref tablero 0) 0 0))

(define (encontrov tablero n-fila n-col cont x)
  (cond
    [(= cont 4) #t]
    [(and (and (= n-fila 5) (= cont 3)) (string=? (list-ref (list-ref (list-ref tablero n-fila) n-col) 0) x))
         (encontrov tablero n-fila  n-col (+ cont 1) x)]
    [(= (+ n-fila 1) 6) #f]
    [(string=? (list-ref (list-ref (list-ref tablero n-fila) n-col) 0) x)
     (if (encontrov tablero (+ n-fila 1) n-col (+ cont 1) x) 
          #t 
          #f)]
    [else #f]))

#|
Función = board-check-horizontal-win

Propósito = Función que permite verificar el estado actual del tablero y entregar
el posible ganador que cumple con la regla de conectar 4 fichas de forma horizontal.

Dominio = board (board)

Recorrido = int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador vertical)

Recursión = natural

Tipo = otros
|#


(define (board-check-horizontal-win tablero)
  (define (revisar-fila tablero fila nfila col)
    (if (= col 4)
         (if (= (+ nfila 1) 6)
             0
             (revisar-fila tablero (list-ref tablero (+ nfila 1)) (+ nfila 1) 0)
             )
         (cond
           [(null? (car fila)) (revisar-fila tablero (cdr fila) nfila (+ col 1))]
           [(encontroh tablero nfila col 0 "R") 1]
           [(encontroh tablero nfila col 0 "Y") 2]
           [else (revisar-fila tablero (cdr fila) nfila (+ col 1))]
           )
         ))
  (revisar-fila tablero (list-ref tablero 0) 0 0))

(define (encontroh tablero fila col cont x)
  (cond
    [(= cont 4) #t]
    [(null? (list-ref (list-ref tablero fila) col)) #f]
    [(string=? (list-ref (list-ref (list-ref tablero fila) col) 0) x)
     (if (encontroh tablero fila (+ col 1) (+ cont 1) x)
         #t
         #f
         )]
    [else #f]))

#|
Función = board-check-diagonal-win

Propósito = Función que permite verificar el estado actual del tablero y entregar
el posible ganador que cumple con la regla de conectar 4 fichas de forma diagonal.

Dominio = board (board)

Recorrido = int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador vertical)

Recursión = natural

Tipo = otros
|#

(define (board-check-diagonal-win tablero)
  (define (recorre-fila tablero fila col)
    (if (= fila 4)
        0
        (if (= col 7)
        (recorre-fila tablero (+ fila 1) 0)
        (cond
          [(null? (list-ref (list-ref tablero fila) col))
           (recorre-fila tablero fila (+ col 1))]
          [(encontro-d tablero fila col "R") 1]
          [(encontro-d tablero fila col "Y") 2]
          [else (recorre-fila tablero fila (+ col 1))]
      )
    )
  )
)(recorre-fila tablero 0 0))


(define (encontro-d tablero fila col x)
  (if (string=? (list-ref (list-ref (list-ref tablero fila) col) 0) x)
      (fn-verificar tablero fila col x)
      #f
      ))


(define (fn-verificar tablero fila col x)
  (cond
    [(ver-der tablero fila col x 0) #t]
    [(ver-iz tablero fila col x 0) #t]
    [else #f]
    )
  )

(define (ver-der tablero fila col x cont)
  (if (= cont 4)
      #t
      (if (or (= fila -1) (= fila 6) (= col -1) (= col 7))
          #f
          (cond
            [(empty? (list-ref (list-ref tablero fila) col)) #f]
            [(string=? (list-ref (list-ref (list-ref tablero fila) col) 0) x)
             (ver-der tablero (+ fila 1) (+ col 1) x (+ cont 1))]
            [else #f] 
            )
          )
      )
  )

(define (ver-iz tablero fila col x cont)
  (if (= cont 4)
      #t
      (if (or (= fila -1) (= fila 6) (= col -1) (= col 7))
          #f
          (cond
            [(empty? (list-ref (list-ref tablero fila) col)) #f]
            [(string=? (list-ref (list-ref (list-ref tablero fila) col) 0) x)
             (ver-iz tablero (+ fila 1) (- col 1) x (+ cont 1))]
            [else #f] 
            )
          )
      )
  )

#|
Función = board-who-is-winner

Propósito = Función que permite verificar el estado actual del tablero y entregar
 el posible ganador que cumple con la regla de conectar 4 fichas de forma diagonal.

Dominio = board (board)

Recorrido = int (1 si gana jugador 1, 2 si gana jugador 2, 0 si no hay ganador vertical)

Recursión = no aplica

Tipo = otros
|#

(define (board-who-is-winner board)
  (cond
    [(and (= (board-check-diagonal-win board) 0)
          (= (board-check-horizontal-win board) 0)
          (= (board-check-vertical-win board) 0))
     0]
    [(or (= (board-check-diagonal-win board) 1)
          (= (board-check-horizontal-win board) 1)
          (= (board-check-vertical-win board) 1))
     1]
    [(or (= (board-check-diagonal-win board) 2)
          (= (board-check-horizontal-win board) 2)
          (= (board-check-vertical-win board) 2))
     2]
    )
  )