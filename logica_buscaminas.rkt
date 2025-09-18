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
  crear-tablero            ; (n m) -> matriz de celdas vacías
  colocar-minas            ; (tablero prob) -> tablero con minas (prob ~ 0.10/0.15/0.20)
  tablerizar-con-pistas    ; (tablero) -> tablero con 'pista' llenas
  generar-tablero-nivel    ; (n m nivel) -> tablero final con minas+pistas

  ;; helpers
  filas cols
  en-rango? buscar actualizar-celda
  vecinos num-minas-alrededor

  ;; juego
  revelar                  ; (tablero abiertas r c) -> (values nuevas-abiertas explotó?)
  flood-reveal
  alternar-bandera         ; (banderas r c) -> nuevas-banderas
  gano?                    ; (tablero abiertas) -> #t/#f
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

;; Actualiza una celda reconstruyendo de forma inmutable
(define (actualizar-celda tablero r c nueva-celda)
  (let loop-f ((i 0) (filas-tablero tablero) (acc '()))
    (cond
      ((null? filas-tablero) (reverse acc))
      (else
       (define fila (car filas-tablero))
       (define fila-nueva
         (if (= i r)
             (let loop-c ((j 0) (cols-tab fila) (acc2 '()))
               (cond
                 ((null? cols-tab) (reverse acc2))
                 (else
                  (define cel (car cols-tab))
                  (loop-c (add1 j) (cdr cols-tab)
                          (cons (if (= j c) nueva-celda cel) acc2)))))
             fila))
       (loop-f (add1 i) (cdr filas-tablero) (cons fila-nueva acc))))))

;; --------------------------
;; Colocar minas aleatoriamente (por probabilidad 0..1)
;; --------------------------
(define (colocar-minas tablero prob)
  (map (lambda (fila)
         (map (lambda (celda)
                (if (< (random) prob)
                    '(#t 0 #f #f)
                    celda))
              fila))
       tablero))

;; --------------------------
;; Vecindad y conteos
;; --------------------------
(define offsets
  '((-1  0) ( 1  0) ( 0 -1) ( 0  1)
    (-1 -1) (-1  1) ( 1 -1) ( 1  1)))

(define (vecinos tablero r c)
  (let loop ((ofs offsets) (acc '()))
    (if (null? ofs)
        (reverse acc)
        (let* ((dr (caar ofs)) (dc (cadar ofs))
               (nr (+ r dr))  (nc (+ c dc)))
          (loop (cdr ofs)
                (if (en-rango? tablero nr nc)
                    (cons (list nr nc) acc)
                    acc))))))

(define (tiene-mina? tablero r c)
  (car (buscar tablero r c))) ; mina? es el car de la celda

(define (num-minas-alrededor tablero r c)
  (let loop ((vecs (vecinos tablero r c)) (acc 0))
    (if (null? vecs) acc
        (let* ((p (car vecs)) (rr (car p)) (cc (cadr p))
               (inc (if (tiene-mina? tablero rr cc) 1 0)))
          (loop (cdr vecs) (+ acc inc))))))

;; --------------------------
;; Cargar 'pista' (número) en todas las celdas no-mine
;; --------------------------
(define (tablerizar-con-pistas tablero)
  (let loop-f ((r 0) (tab tablero))
    (if (= r (filas tablero))
        tab
        (let loop-c ((c 0) (tab2 tab))
          (if (= c (cols tablero))
              (loop-f (add1 r) tab2)
              (let* ((cel (buscar tab2 r c))
                     (mina? (car cel))
                     (pista (if mina? 0 (num-minas-alrededor tab2 r c)))
                     (desc (caddr cel))
                     (mark (cadddr cel))
                     (nuevo (list mina? pista desc mark)))
                (loop-c (add1 c) (actualizar-celda tab2 r c nuevo))))))))

;; --------------------------
;; Crear tablero por nivel
;; nivel: 'facil (0.10) | 'medio (0.15) | 'dificil (0.20)
;; --------------------------
(define (nivel->prob nivel)
  (cond ((eq? nivel 'facil)   0.10)
        ((eq? nivel 'medio)   0.15)
        (else                 0.20)))

(define (generar-tablero-nivel n m nivel)
  (tablerizar-con-pistas (colocar-minas (crear-tablero n m) (nivel->prob nivel))))

;; --------------------------
;; Conjuntos simples de coordenadas (listas sin repeticiones)
;; --------------------------
(define (ig-coord? a b) (and (= (car a) (car b)) (= (cadr a) (cadr b))))
(define (en-set? p s)
  (and (not (null? s))
       (or (ig-coord? p (car s)) (en-set? p (cdr s)))))
(define (add-set p s) (if (en-set? p s) s (cons p s)))
(define (del-set p s)
  (cond ((null? s) '())
        ((ig-coord? p (car s)) (cdr s))
        (else (cons (car s) (del-set p (cdr s))))))

;; --------------------------
;; Flood reveal (descubrir zonas de 0 y su frontera)
;; Retorna nuevo conjunto de "abiertas"
;; --------------------------
(define (flood-reveal tablero abiertas r c)
  (if (en-set? (list r c) abiertas)
      abiertas
      (let* ((cel (buscar tablero r c))
             (mina? (car cel))
             (pista (cadr cel)))
        (if mina?
            abiertas
            (let ((ab1 (add-set (list r c) abiertas)))
              (if (> pista 0)
                  ab1
                  (let loop ((vecs (vecinos tablero r c)) (acc ab1))
                    (if (null? vecs) acc
                        (let* ((p (car vecs)) (rr (car p)) (cc (cadr p)))
                          (loop (cdr vecs) (flood-reveal tablero acc rr cc)))))))))))

;; --------------------------
;; Revelar una celda (click izquierdo)
;; Devuelve (values nuevas-abiertas explotó?)
;; --------------------------
(define (revelar tablero abiertas r c)
  (let* ((cel (buscar tablero r c))
         (mina? (car cel)))
    (if mina?
        (values abiertas #t)
        (values (flood-reveal tablero abiertas r c) #f))))

;; --------------------------
;; Alternar bandera (click derecho)
;; --------------------------
(define (alternar-bandera banderas r c)
  (let ((p (list r c)))
    (if (en-set? p banderas) (del-set p banderas) (add-set p banderas))))

;; --------------------------
;; ¿Ganó? = todas las celdas sin mina están en 'abiertas'
;; --------------------------
(define (gano? tablero abiertas)
  (let loop-f ((r 0))
    (cond
      ((= r (filas tablero)) #t)
      (else
       (let loop-c ((c 0))
         (cond
           ((= c (cols tablero)) (loop-f (add1 r)))
           (else
            (define cel (buscar tablero r c))
            (define es-mina (car cel))
            (if (or es-mina (en-set? (list r c) abiertas))
                (loop-c (add1 c))
                #f))))))))
