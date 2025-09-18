#lang racket/gui
(require "logica_buscaminas.rkt")

;; ================================
;; Parámetros
;; ================================
(define FILAS 8)
(define COLS  8)
(define NIVEL 'medio) ; 'facil | 'medio | 'dificil

;; estado = (list tablero abiertas banderas derrota? victoria?)
(define estado
  (box (list (generar-tablero-nivel FILAS COLS NIVEL) '() '() #f #f)))

(define (S-tab s) (car s))
(define (S-abr s) (cadr s))
(define (S-ban s) (caddr s))
(define (S-der s) (cadddr s))
(define (S-gana s) (car (cddddr s)))

(define (set-estado! nuevo)
  (set-box! estado nuevo)
  (send canvas refresh-now))

;; ================================
;; Ventana
;; ================================
(define cell-size 32)
(define frame (new frame% [label "Buscaminas (Racket GUI)"]))
(define _msg  (new message% [parent frame]
                    [label "Izq: descubrir  |  Der: bandera  |  R: reiniciar"]))

;; ================================
;; Dibujo
;; ================================
(define (rc->rect r c)
  (values (* c cell-size) (* r cell-size) cell-size cell-size))

(define (draw-centered dc txt x y w h)
  (define-values (tw th _1 _2) (send dc get-text-extent txt))
  (send dc draw-text txt (+ x (quotient (- w tw) 2)) (+ y (quotient (- h th) 2))))

(define (overlay dc txt canvas)
  (define w (send canvas get-width))
  (define h (send canvas get-height))
  (send dc set-brush "light gray" 'solid)
  (send dc set-pen "black" 1 'transparent)
  (send dc draw-rectangle 0 0 w h)
  (draw-centered dc txt 0 0 w h))

(define (dibujar-tablero dc s canvas)
  (define tab (S-tab s))
  (define ab  (S-abr s))
  (define ba  (S-ban s))
  (define der (S-der s))
  (define gan (S-gana s))

  (for ((r (in-range (filas tab))))
    (for ((c (in-range (cols tab))))
      (define-values (x y w h) (rc->rect r c))
      (send dc set-pen "black" 1 'solid)
      (send dc set-brush "light gray" 'solid)
      (send dc draw-rectangle x y w h)

      (define cel (buscar tab r c))
      (define mina? (car cel))
      (define pista (cadr cel))

      (define descubierto?
        (member (list r c) ab (lambda (a b) (and (= (car a) (car b))
                                                 (= (cadr a) (cadr b))))))
      (define marcado?
        (member (list r c) ba (lambda (a b) (and (= (car a) (car b))
                                                 (= (cadr a) (cadr b))))))

      (cond
        ((and descubierto? mina?) (draw-centered dc "💣" x y w h))
        (descubierto?
         (send dc set-brush "white" 'solid)
         (send dc draw-rectangle (+ x 1) (+ y 1) (- w 2) (- h 2))
         (when (> pista 0) (draw-centered dc (number->string pista) x y w h)))
        (marcado? (draw-centered dc "⚑" x y w h))
        (else (void)))))

  (when der (overlay dc "💥 BOOM — Perdiste" canvas))
  (when gan (overlay dc "🎉 ¡Ganaste!" canvas)))

;; ================================
;; Canvas personalizado (EVENTOS)
;; ================================
(define canvas #f)

(define my-canvas%
  (class canvas%
    (super-new
      [paint-callback (lambda (cnv dc) (dibujar-tablero dc (unbox estado) cnv))])

    ;; Mouse
    (define/override (on-event e)
      (define et (send e get-event-type)) ; 'left-down, 'right-down, ...
      (cond
        ((or (eq? et 'left-down) (eq? et 'right-down))
         (define s   (unbox estado))
         (define tab (S-tab s))
         (define ab  (S-abr s))
         (define ba  (S-ban s))
         (define der (S-der s))
         (define gan (S-gana s))
         (unless (or der gan)
           (define x (send e get-x))
           (define y (send e get-y))
           (define r (quotient y cell-size))
           (define c (quotient x cell-size))
           (when (en-rango? tab r c)
             (cond
               ((eq? et 'right-down) ; bandera
                (set-estado! (list tab ab (alternar-bandera ba r c) #f (gano? tab ab))))
               (else                 ; revelar
                (let-values (((ab2 boom) (revelar tab ab r c)))
                  (define win2 (and (not boom) (gano? tab ab2)))
                  (set-estado! (list tab ab2 ba boom win2))))))))
        (else (void))))

    ;; Teclado
    (define/override (on-char e)
      (define k (send e get-key-code))
      (when (equal? k #\r)
        (set-estado! (list (generar-tablero-nivel FILAS COLS NIVEL) '() '() #f #f))))
    ))

;; Instanciar canvas y mostrar
(set! canvas (new my-canvas%
                  [parent frame]
                  [min-width  (* COLS  cell-size)]
                  [min-height (* FILAS cell-size)]
                  [style '(no-autoclear)]))
(send frame show #t)
(send canvas focus)
