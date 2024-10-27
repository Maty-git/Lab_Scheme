#lang racket

(provide game-get-current-player)


(define (game-get-current-player game)
  (if (= (modulo (last game) 2) 0)
      (list-ref game 1)
      (list-ref game 0)
      ))