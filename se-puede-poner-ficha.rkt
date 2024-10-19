#lang racket


(provide se-puede-jugar?)

(require "board.rkt")


(define (se-puede-jugar? lista1)
  (cond
    [(empty? lista1)#f]
    [(or(revisar-fila (car lista1)) (se-puede-jugar? (cdr lista1)))]
    [else #f]))

(define (revisar-fila lista2)
  (cond
   [(empty? lista2)#f]
   [(null? (car lista2)) #t]
   [(not (null? (car lista2))) (revisar-fila (cdr lista2))]
   [else #f]))

