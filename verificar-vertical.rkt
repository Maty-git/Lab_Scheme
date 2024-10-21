#lang racket


(provide board-check-vertical-win)


(define (board-check-vertical-win tablero)
  (define (revisar-fila tablero fila col nfila)
    (cond
      [(= nfila 4) "F"]
      [(empty? fila)
       (revisar-fila tablero (list-ref tablero (+ 1 nfila)) 0 (+ 1 nfila))]
      [(null? (car fila)) (revisar-fila tablero (cdr fila) (+ col 1) nfila)]
      [(encontro tablero nfila col 0 "R") "R"]
      [(encontro tablero nfila col 0 "Y") "Y"]
      [else "F"]))
  (revisar-fila tablero (list-ref tablero 0) 0 0))

(define (encontro tablero n-fila n-col cont x)
  (cond
    [(= cont 4) #t]
    [(and (and (= n-fila 5) (= cont 3)) (string=? (list-ref (list-ref (list-ref tablero n-fila) n-col) 0) x))
         (encontro tablero n-fila  n-col (+ cont 1) x)]
    [(= (+ n-fila 1) 6) #f]
    [(string=? (list-ref (list-ref (list-ref tablero n-fila) n-col) 0) x)
     (if (encontro tablero (+ n-fila 1) n-col (+ cont 1) x) 
          #t 
          #f)]
    [else #f]))




