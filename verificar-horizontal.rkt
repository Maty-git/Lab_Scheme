#lang racket

(provide board-check-horizontal-win)


(define (board-check-horizontal-win tablero)
  (define (revisar-fila tablero fila nfila col)
    (if (= col 4)
         (if (= (+ nfila 1) 6)
             "F"
             (revisar-fila tablero (list-ref tablero (+ nfila 1)) (+ nfila 1) 0)
             )
         (cond
           [(null? (car fila)) (revisar-fila tablero (cdr fila) nfila (+ col 1))]
           [(encontro tablero nfila col 0 "R") "R"]
           [(encontro tablero nfila col 0 "Y") "Y"]
           [else "F"]
           )
         ))
  (revisar-fila tablero (list-ref tablero 0) 0 0))

(define (encontro tablero fila col cont x)
  (cond
    [(= cont 4) #t]
    [(null? (list-ref (list-ref tablero fila) col)) #f]
    [(string=? (list-ref (list-ref (list-ref tablero fila) col) 0) x)
     (if (encontro tablero fila (+ col 1) (+ cont 1) x)
         #t
         #f
         )]
    [else #f]))
