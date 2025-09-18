#lang racket
(require racket/random)

;; =========================================================
;; Representación de celda: '(mina? pista descubierto? marcado?)
;; mina?       : #t | #f
;; pista       : entero 0..8 (número de minas vecinas)
;; descubierto?: #t | #f
;; marcado?    : #t | #f
;; =========================================================

(provide
  ;; construcción
  crear-tablero
  colocar-minas-exacto      ; NUEVO: coloca exactamente K minas
  tablerizar-con-pistas
  generar-tablero-nivel

  ;; helpers
  filas cols
  en-rango? buscar actualizar-celda
  vecinos num-minas-alrededor

  ;; juego
  revelar
  flood-reveal
  alternar-bandera
  gano?
)

;; --------------------------
;; Construcción base
;; --------------------------
(define (crear-columna m)
  (if (zero? m)
      '()
      (cons '(#f 0 #f #f) (crear-columna (sub1 m)))))

(define (crear-fila n m)
  (if (zero? n)
      '()
      (cons (crear-columna m) (crear-fila (sub1 n) m))))

(define (crear-tablero n m)
  (crear-fila n m))

;; --------------------------
;; Utilidades de matriz
;; --------------------------
(define (filas tablero) (length tablero))
(define (cols tablero) (if (null? tablero) 0 (length (car tablero))))

(define (en-rango? tablero r c)
  (and (<= 0 r) (< r (filas tablero)) (<= 0 c) (< c (cols tablero))))

(define (buscar tablero r c)
  (list-ref (list-ref tablero r) c))

;; Actualización inmutable (sin let)
(define (actualizar-celda tablero r c nueva-celda)
  (actualizar-celda-filas tablero r c nueva-celda 0))

(define (actualizar-celda-filas filas-list r c nueva i)
  (if (null? filas-list)
      '()
      (cons (if (= i r)
                (actualizar-celda-cols (car filas-list) c nueva 0)
                (car filas-list))
            (actualizar-celda-filas (cdr filas-list) r c nueva (add1 i)))))

(define (actualizar-celda-cols fila c nueva j)
  (if (null? fila)
      '()
      (cons (if (= j c) nueva (car fila))
            (actualizar-celda-cols (cdr fila) c nueva (add1 j)))))

;; --------------------------
;; Vecindad y conteos
;; --------------------------
(define offsets
  '((-1  0) ( 1  0) ( 0 -1) ( 0  1)
    (-1 -1) (-1  1) ( 1 -1) ( 1  1)))

(define (vecinos tablero r c)
  (vecinos-ofs tablero r c offsets))

(define (vecinos-ofs tablero r c ofs)
  (if (null? ofs)
      '()
      (if (en-rango? tablero (+ r (car (car ofs))) (+ c (cadr (car ofs))))
          (cons (list (+ r (car (car ofs))) (+ c (cadr (car ofs))))
                (vecinos-ofs tablero r c (cdr ofs)))
          (vecinos-ofs tablero r c (cdr ofs)))))

(define (tiene-mina? tablero r c)
  (car (buscar tablero r c)))

(define (num-minas-alrededor tablero r c)
  (num-minas-alrededor* tablero (vecinos tablero r c)))

(define (num-minas-alrededor* tablero vecs)
  (if (null? vecs)
      0
      (+ (if (tiene-mina? tablero (car (car vecs)) (cadr (car vecs))) 1 0)
         (num-minas-alrededor* tablero (cdr vecs)))))

;; --------------------------
;; Cargar 'pista' en todas las celdas (sin let)
;; --------------------------
(define (tablerizar-con-pistas tablero)
  (tablerizar-r tablero 0))

(define (tablerizar-r tab r)
  (if (= r (filas tab))
      tab
      (tablerizar-r (tablerizar-c tab r 0) (add1 r))))

(define (tablerizar-c tab r c)
  (if (= c (cols tab))
      tab
      (tablerizar-c
       (actualizar-celda
        tab r c
        (list (car   (buscar tab r c))
              (if (car (buscar tab r c))
                  0
                  (num-minas-alrededor tab r c))
              (caddr  (buscar tab r c))
              (cadddr (buscar tab r c))))
       r (add1 c))))

;; --------------------------
;; Colocar minas: EXACTO K (no probabilístico)
;; --------------------------
(define (todas-las-coords n m)
  (todas-las-coords-r n m 0 0))

(define (todas-las-coords-r n m r c)
  (cond [(= r n) '()]
        [(= c m) (todas-las-coords-r n m (add1 r) 0)]
        [else (cons (list r c) (todas-las-coords-r n m r (add1 c)))]))

;; baraja simple (Fisher–Yates recursivo)
(define (shuffle lst)
  (if (null? lst) '()
      (let* ([len (length lst)]
             [idx (random len)]
             [x   (list-ref lst idx)]
             [rest (append (take lst idx) (drop lst (add1 idx)))])
        (cons x (shuffle rest)))))

(define (colocar-minas-exacto tablero k)
  (let* ([n (filas tablero)]
         [m (cols tablero)]
         [coords (shuffle (todas-las-coords n m))])
    (colocar-minas-en-coords tablero (take coords (min k (* n m))))))

(define (colocar-minas-en-coords tablero coords)
  (if (null? coords)
      tablero
      (let* ([r (caar coords)]
             [c (cadar coords)]
             [cel (buscar tablero r c)]
             [cel* (list #t 0 (caddr cel) (cadddr cel))])
        (colocar-minas-en-coords (actualizar-celda tablero r c cel*) (cdr coords)))))

;; --------------------------
;; Crear tablero por nivel (con % exacto)
;; --------------------------
(define (nivel->ratio nivel)
  (cond [(eq? nivel 'facil)   0.10]
        [(eq? nivel 'medio)   0.15]
        [else                 0.20]))

(define (generar-tablero-nivel n m nivel)
  (let* ([ratio (nivel->ratio nivel)]
         [total (* n m)]
         [k (max 1 (inexact->exact (round (* total ratio))))] ; exacto al %
         [vacio (crear-tablero n m)]
         [con-minas (colocar-minas-exacto vacio k)])
    (tablerizar-con-pistas con-minas)))

;; --------------------------
;; Conjuntos simples de coordenadas
;; --------------------------
(define (ig-coord? a b) (and (= (car a) (car b)) (= (cadr a) (cadr b))))
(define (en-set? p s)
  (and (not (null? s))
       (or (ig-coord? p (car s)) (en-set? p (cdr s)))))
(define (add-set p s) (if (en-set? p s) s (cons p s)))
(define (del-set p s)
  (if (null? s)
      '()
      (if (ig-coord? p (car s))
          (cdr s)
          (cons (car s) (del-set p (cdr s))))))

;; --------------------------
;; Flood reveal (sin let)
;; --------------------------
(define (flood-reveal tablero abiertas r c)
  (if (en-set? (list r c) abiertas)
      abiertas
      (if (tiene-mina? tablero r c)
          abiertas
          (if (> (cadr (buscar tablero r c)) 0)
              (add-set (list r c) abiertas)
              (flood-reveal-vecinos tablero
                                    (add-set (list r c) abiertas)
                                    (vecinos tablero r c))))))

(define (flood-reveal-vecinos tablero abiertas vecs)
  (if (null? vecs)
      abiertas
      (flood-reveal-vecinos tablero
                            (flood-reveal tablero abiertas
                                          (car (car vecs)) (cadr (car vecs)))
                            (cdr vecs))))

;; --------------------------
;; Revelar una celda (click izquierdo)
;; --------------------------
(define (revelar tablero abiertas r c)
  (if (tiene-mina? tablero r c)
      (values abiertas #t)
      (values (flood-reveal tablero abiertas r c) #f)))

;; --------------------------
;; Alternar bandera (click derecho)
;; --------------------------
(define (alternar-bandera banderas r c)
  (if (en-set? (list r c) banderas)
      (del-set (list r c) banderas)
      (add-set (list r c) banderas)))

;; --------------------------
;; ¿Ganó?
;; --------------------------
(define (gano? tablero abiertas)
  (gano?-rc tablero abiertas 0 0))

(define (gano?-rc tablero abiertas r c)
  (cond
    ((= r (filas tablero)) #t)
    ((= c (cols tablero))  (gano?-rc tablero abiertas (add1 r) 0))
    (else
     (if (or (car (buscar tablero r c)) (en-set? (list r c) abiertas))
         (gano?-rc tablero abiertas r (add1 c))
         #f))))
