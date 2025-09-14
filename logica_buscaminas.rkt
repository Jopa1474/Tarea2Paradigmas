#lang racket

;;////////////////////////////
;;Creacion del tablero
;;///////////////////////////

;; Generar tablero vacío
(define (crear_tablero filas columnas)
  (for/list ([i filas])
    (for/list ([j columnas])
      (list #f 0 #f #f ))))  ;; sin mina, 0 minas vecinas, no descubierto, no marcado

;;Para poder colocar las minas de manera aleatoria
(require racket/random)

;;Colocamos las minas de forma aleatoria en el tablero
(define (colocar_minas tablero prob)
  (for/list ([fila tablero])
    (for/list ([celda fila])
      (if (< (random) prob)
          (list #t 0 #f #f) ;; mina
          celda))))

;;Inicializacion
(define tablero (colocar_minas(crear_tablero 8 8)0.15))