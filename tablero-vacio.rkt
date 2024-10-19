#lang racket

(provide board-can-play?)

(require "board.rkt")

(define (board-can-play? lista1)
  (cond
    [(empty? lista1)#t]
    [(revisar-fila (car lista1)) (board-can-play? (cdr lista1))]
    [else #f]))

(define (revisar-fila lista2)
  (cond
   [(empty? lista2)#t]
   [(null? (car lista2)) (revisar-fila (cdr lista2))]
   [else #f]))


