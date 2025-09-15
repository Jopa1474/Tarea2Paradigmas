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


;; recorrer una fila mostrando posiciones
(define (recorrer-fila fila fila-idx col-idx)
  (cond
    [(null? fila) '()] ; fin de la fila
    [else
     (begin
       (display "(")
       (display fila-idx)
       (display " , ")
       (display col-idx)
       (display ") ")
       (recorrer-fila (cdr fila) fila-idx (add1 col-idx)))]))

;; recorrer la matriz con posiciones
(define (recorrer-matriz matriz fila-idx)
  (cond
    [(null? matriz) '()] ; fin de la matriz
    [else
     (begin
       (recorrer-fila (car matriz) fila-idx 0) ; arranca en col=0
       (newline)
       (recorrer-matriz (cdr matriz) (add1 fila-idx)))]))


;;Para ver si las casillas alrededor tienen minas



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
  (cond((null? casilla) #f)
  (else(equal? (car casilla) #t))))

;;Casillas alrededor de la actual

(define (alrededor n m filas columnas)
  (list (N n m)
        (O n m columnas)
        (S n m filas)
        (E n m)
        (NO n m filas columnas)
        (SO n m filas columnas)
        (SE n m filas columnas)
        (NE n m filas columnas)))

;;Para obtener el numero de minas que hay alrededor
(define(num_minas_alrededor n m filas columnas)
  (num_minas_alrededor_aux n m filas columnas (alrededor n m filas columnas)))

(define (num_minas_alrededor_aux n m filas columnas lista_alrededor)
  (cond((null? lista_alrededor) 0)
    ((equal? (tiene_mina (car lista_alrededor)) #t) (+ 1 (num_minas_alrededor_aux n m filas columnas (cdr lista_alrededor))))
       (else(+ 0 (num_minas_alrededor_aux n m filas columnas (cdr lista_alrededor))))))

