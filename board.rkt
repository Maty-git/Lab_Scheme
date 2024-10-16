#lang racket

(provide board)


(define (lista-vacia)
  (list null null null null null null null))

(define (board)
  (list (lista-vacia) (lista-vacia) (lista-vacia)
        (lista-vacia) (lista-vacia) (lista-vacia)))
