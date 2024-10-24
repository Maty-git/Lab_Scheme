#lang racket

(provide board-check-diagonal-win)



(define (board-check-diagonal-win tablero)
  (define (recorre-fila tablero fila col)
    (if (= fila 3)
        "F"
        (if (= col 7)
        (recorre-fila tablero (+ fila 1) 0)
        (cond
          [(empty? (list-ref (list-ref tablero fila) col))
           (recorre-fila tablero (+ fila 1) 0 )]
          [(null? (list-ref (list-ref (list-ref tablero fila) col) 0))
           (recorre-fila tablero fila (+ col 1))]
          [(encontro-d tablero fila col "R") "R"]
          [(encontro-d tablero fila col "Y") "Y"]
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





