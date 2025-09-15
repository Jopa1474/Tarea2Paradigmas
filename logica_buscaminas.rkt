#lang racket

;;////////////////////////////
;;Creacion del tablero
;;///////////////////////////

;; Generar tablero vacío

(define (crear_columna m)
  (if (zero? m)
      '()
      (cons '(#f 0 #f #f) (crear_columna (sub1 m))))) ;  ;; sin mina, 0 minas vecinas, no descubierto, no marcado

(define (crear_fila n m)
  (if (zero? n)
      '()
      (cons (crear_columna m) (crear_fila (sub1 n) m)))) ; n filas de columnas m

(define (crear_tablero n m)
  (crear_fila n m))
 
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
(define filas 4)
(define columnas 4)


(define tablero (colocar_minas(crear_tablero filas columnas)0.15))

;; busqueda por posicion en la matriz
(define (buscar matriz fila col)
  (list-ref (list-ref matriz fila) col))

;;Para agregar las pistas

;;Cambiar pista

;; función auxiliar para recorrer una fila
(define (recorrer-fila fila)
  (cond((null? fila) '())                ; fin de la fila
    (else
     (begin
       (display (car fila))           ; imprimir valor
       (display " ")
       (recorrer-fila (cdr fila))))))

;; función para recorrer la matriz
(define (recorrer-matriz matriz)
  (cond((null? matriz) '())               ; fin de la matriz
    (else
     (begin
       (recorrer-fila (car matriz))   ; recorrer la primera fila
       (recorrer-matriz (cdr matriz))))))

;;casilla N (arriba)

(define (N n m)
  (cond((zero? n) '())
  (else(buscar tablero (- n 1) m))))

;;casilla O (derecha)

(define (O n m columnas)
  (cond((= m columnas) '())
  (else(buscar tablero n (+ m 1)))))

;;casilla S (abajo)

(define (S n m filas)
  (cond((= n filas) '())
  (else(buscar tablero (+ n 1) m))))

;;casilla E (izquierda)

(define (E n m)
  (cond((zero? m) '())
  (else(buscar tablero n (- m 1)))))

;;casilla NO (arriba a la derecha)

(define (NO n m filas columnas)
  (cond((zero? n) '())
  ((= m columnas) '())
  (else(buscar tablero (- n 1) (+ m 1)))))

;;casilla SO (abajo a la derecha)

(define (SO n m filas columnas)
  (cond((= n filas) '())
  ((= m columnas) '())
  (else(buscar tablero (+ n 1) (+ m 1)))))

;;casilla SE (abajo a la izquierda)

(define (SE n m filas columnas)
  (cond((= n filas) '())
  ((zero? m) '())
  (else(buscar tablero (+ n 1) (- m 1)))))

;;casilla NE (arriba a la izquierda)

(define (NE n m filas columnas)
  (cond((zero? n) '())
  ((zero? m) '())
  (else(buscar tablero (- n 1) (- m 1)))))

;;Para verificar si la casilla tiene una mina
(define(tiene_mina casilla)
  (equal? (car casilla) #t))
