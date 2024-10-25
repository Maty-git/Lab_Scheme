#lang racket

(provide board-who-is-winner)
(require "verificar-diagonal.rkt")
(require "verificar-horizontal.rkt")
(require "verificar-vertical.rkt")




(define (board-who-is-winner board)
  (cond
    [(and (string=? (board-check-diagonal-win board) "F")
          (string=? (board-check-horizontal-win board) "F")
          (string=? (board-check-vertical-win board) "F"))
     "F"]
    [(or (string=? (board-check-diagonal-win board) "R")
          (string=? (board-check-horizontal-win board) "R")
          (string=? (board-check-vertical-win board) "R"))
     "R"]
    [(or (string=? (board-check-diagonal-win board) "Y")
          (string=? (board-check-horizontal-win board) "Y")
          (string=? (board-check-vertical-win board) "Y"))
     "Y"]
    )
  )



