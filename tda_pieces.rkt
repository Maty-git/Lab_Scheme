#lang racket

(provide piece)

(define (piece x)
  (cond
    [(string=? x "red") (list "R")]
    [(string=? x "yellow") (list "Y")]
    ))



