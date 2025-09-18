#lang racket/gui
(require "logica_buscaminas.rkt")

;; =========================================
;; Parámetros base (puedes ajustar filas/cols)
;; =========================================
(define FILAS 8)
(define COLS  8)

;; Nivel actual (box para que on-char reinicie con el mismo)
(define nivel-actual (box 'medio)) ; 'facil | 'medio | 'dificil

;; estado = (list tablero abiertas banderas derrota? victoria?)
(define estado
  (box (list (generar-tablero-nivel FILAS COLS (unbox nivel-actual))
             '() '() #f #f)))

(define (S-tab s)  (car s))
(define (S-abr s)  (cadr s))
(define (S-ban s)  (caddr s))
(define (S-der s)  (cadddr s))
(define (S-gana s) (car (cddddr s)))

(define (set-estado! nuevo)
  (set-box! estado nuevo)
  (when canvas (send canvas refresh-now)))

;; =========================================
;; Ventana y layout con “tarjetas” (menú/juego)
;; =========================================
(define cell-size 32)
(define frame (new frame% [label "Buscaminas (Racket GUI)"]))

;; Panel raíz y dos pantallas: menú y juego
(define root      (new vertical-panel% [parent frame] [stretchable-height #t] [stretchable-width #t]))
(define menu-pnl  (new vertical-panel% [parent root] [alignment '(center center)] [stretchable-height #t]))
(define game-pnl  (new vertical-panel% [parent root] [stretchable-height #t]))
(send game-pnl show #f) ; inicia oculto

;; -----------------------------------------
;; Dibujo
;; -----------------------------------------
(define (rc->rect r c)
  (values (* c cell-size) (* r cell-size) cell-size cell-size))

(define (draw-centered dc txt x y w h)
  (define-values (tw th _1 _2) (send dc get-text-extent txt))
  (send dc draw-text txt (+ x (quotient (- w tw) 2))
                        (+ y (quotient (- h th) 2))))

(define (overlay dc txt canvas)
  (define w (send canvas get-width))
  (define h (send canvas get-height))
  (send dc set-brush "light gray" 'solid) ; sin alfa por compatibilidad
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

      (define cel   (buscar tab r c))
      (define mina? (car cel))
      (define pista (cadr cel))

      (define descubierto?
        (member (list r c) ab
                (lambda (a b) (and (= (car a) (car b))
                                   (= (cadr a) (cadr b))))))
      (define marcado?
        (member (list r c) ba
                (lambda (a b) (and (= (car a) (car b))
                                   (= (cadr a) (cadr b))))))

      (cond
        ((and descubierto? mina?) (draw-centered dc "💣" x y w h))
        (descubierto?
         (send dc set-brush "white" 'solid)
         (send dc draw-rectangle (+ x 1) (+ y 1) (- w 2) (- h 2))
         (when (> pista 0) (draw-centered dc (number->string pista) x y w h)))
        (marcado? (draw-centered dc "⚑" x y w h))
        (else (void)))) )

  (when der (overlay dc "💥 BOOM — Perdiste" canvas))
  (when gan (overlay dc "🎉 ¡Ganaste!" canvas))
)


;; -----------------------------------------
;; Canvas personalizado (EVENTOS)
;; -----------------------------------------
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
        (set-estado! (list (generar-tablero-nivel FILAS COLS (unbox nivel-actual))
                           '() '() #f #f))))
    ))

;; =========================================
;; Construcción fija de la pantalla de JUEGO
;; (barra arriba + canvas abajo, orden estable)
;; =========================================

;; Barra superior fija
(define game-bar
  (new horizontal-panel% [parent game-pnl] [stretchable-height #f]))

(define lbl-msg
  (new message% [parent game-bar]
       [label (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar"
                      (symbol->string (unbox nivel-actual)))]))

(new button%  [parent game-bar] [label "Volver al menú"]
     [callback (lambda (_1 _2) (mostrar-menu!))])

;; Contenedor del canvas (debajo de la barra)
(define canvas-holder
  (new vertical-panel% [parent game-pnl] [stretchable-height #t] [stretchable-width #t]))

;; Canvas (una sola vez)
(define canvas
  (new my-canvas%
       [parent canvas-holder]
       [min-width  (* COLS cell-size)]
       [min-height (* FILAS cell-size)]
       [style '(no-autoclear)]))

;; Utilidad: actualizar texto de la barra y enfocar canvas
(define (actualizar-barra!)
  (send lbl-msg set-label
        (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar"
                (symbol->string (unbox nivel-actual))))
  (send canvas focus))

;; =========================================
;; Lógica de navegación
;; =========================================
(define (mostrar-menu!)
  (send game-pnl show #f)
  (send menu-pnl show #t)
  (send frame reflow-container))

(define (iniciar-juego! nivel)
  (set-box! nivel-actual nivel)
  (set-estado! (list (generar-tablero-nivel FILAS COLS nivel) '() '() #f #f))
  (actualizar-barra!)
  (send menu-pnl show #f)
  (send game-pnl show #t)
  (send frame reflow-container)
  (send canvas refresh-now)
  (send canvas focus))

;; =========================================
;; UI del menú
;; =========================================
(new message% [parent menu-pnl]
     [label "Elige dificultad:"] [auto-resize #t])

(define btns (new horizontal-panel% [parent menu-pnl] [alignment '(center center)]))

(new button% [parent btns] [label "Fácil (10%)"]
     [callback (lambda (_1 _2) (iniciar-juego! 'facil))])

(new button% [parent btns] [label "Medio (15%)"]
     [callback (lambda (_1 _2) (iniciar-juego! 'medio))])

(new button% [parent btns] [label "Difícil (20%)"]
     [callback (lambda (_1 _2) (iniciar-juego! 'dificil))])

;; Mostrar ventana
(send frame show #t)
