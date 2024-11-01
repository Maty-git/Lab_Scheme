#lang racket

(provide piece)

#|
Función = piece

Propósito =  Función que crea una ficha de Conecta4.

Dominio = color (string)

Recorrido = piece

Recursión = no aplica

Tipo = constructor
|#

(define (piece x)
  (cond
    [(string=? x "red") (list "R")]
    [(string=? x "yellow") (list "Y")]
    [(string=? x "rojo") (list "R")]
    [(string=? x "amarillo") (list "Y")]
    ))



