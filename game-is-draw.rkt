#lang racket
(provide game-is-draw?)
(require "board-can-play.rkt")

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