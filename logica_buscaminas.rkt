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
(define filas 8)
(define columnas 8)


(define tablero (colocar_minas(crear_tablero filas columnas)0.15))

;; busqueda por posicion en la matriz
(define (buscar matriz fila col)
  (list-ref (list-ref matriz fila) col))

;;Para agregar las pistas a una matriz nueva

;;Cambiar pista



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

;;Para cambiar el valor de la pista de una casilla

(define (pista tablero n m filas columnas)
  (cons (car(buscar tablero n m))(cons (num_minas_alrededor n m filas columnas) (cdr(cdr(buscar tablero n m))))))


;;Creamos el tablero con las pistas ya incluidas

;;(define (crear_columna_p tablero n m fil col)
;;  (if (zero? m)
;;      '()
;;      (cons (pista tablero (- n 1) (- m 1) (- fil 1) (- col 1)) (crear_columna_p tablero n (sub1 m) fil col)))) ;  ;; sin mina, 0 minas vecinas, no descubierto, no marcado

(define (crear_columna_p tablero n m fil col)
  (cond((zero? m) '())
       ((equal? (tiene_mina(buscar tablero (- n 1)(- m 1))) #t) (cons '(#t X #f #f) (crear_columna_p tablero n (sub1 m) fil col)))
       (else(cons (pista tablero (- n 1) (- m 1) (- fil 1) (- col 1)) (crear_columna_p tablero n (sub1 m) fil col)))
       ))


;;(define (crear_columna_p tablero n m fil col)
;;  (cond((zero? m)'())
;;   ((equal? (tiene_mina(buscar tablero (- n 1) (- m 1))) #t) (cons '(#t X #f #f) (crear_columna_p tablero n (sub1 m) fil col))
;;      (else(cons (pista tablero (- n 1) (- m 1) (- fil 1) (- col 1)) (crear_columna_p tablero n (sub1 m) fil col)))))) ;  ;; sin mina, 0 minas vecinas, no descubierto, no marcado

(define (crear_fila_p tablero n m fil col)
  (if (zero? n)
      '()
      (cons (crear_columna_p tablero n m fil col) (crear_fila_p tablero (sub1 n) m fil col)))) ; n filas de columnas m

(define (crear_tablero_pistas tablero n m fil col)
  (crear_fila_p tablero n m fil col))
 

(define tablero_full (crear_tablero_pistas tablero filas columnas filas columnas))

