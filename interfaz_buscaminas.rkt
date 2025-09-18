#lang racket/gui
(require "logica_buscaminas.rkt")

;; =========================================
;; Parámetros base
;; =========================================
(define cell-size 32) ; tamaño de cada celda en píxeles

;; valores iniciales (se ajustan al elegir nivel)
(define FILAS 8)
(define COLS  8)

;; Nivel actual
(define nivel-actual (box 'medio)) ; 'facil | 'medio | 'dificil

;; estado = (list tablero abiertas banderas derrota? victoria? first?)
;; índices:          0       1        2        3        4         5
(define estado
  (box (list (generar-tablero-nivel FILAS COLS (unbox nivel-actual))
             '() '() #f #f #t)))

;; Accesores
(define (S-tab s)    (list-ref s 0))
(define (S-abr s)    (list-ref s 1))
(define (S-ban s)    (list-ref s 2))
(define (S-der s)    (list-ref s 3))
(define (S-gana s)   (list-ref s 4))
(define (S-first s)  (list-ref s 5))

(define canvas #f) ; lo definimos más abajo

(define (set-estado! nuevo)
  (set-box! estado nuevo)
  (when canvas (send canvas refresh-now)))

;; =========================================
;; Ventana y layout (menú/juego)
;; =========================================
(define frame (new frame% [label "Buscaminas (Racket GUI)"]))

(define root
  (new vertical-panel% [parent frame]
       [stretchable-height #t] [stretchable-width #t]))

(define menu-pnl
  (new vertical-panel% [parent root]
       [alignment '(center center)] [stretchable-height #t]))

(define game-pnl
  (new vertical-panel% [parent root]
       [alignment '(left top)] [stretchable-height #t]))
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
  ;; LIMPIEZA del lienzo (importante si usas 'no-autoclear)
  (define W (send canvas get-width))
  (define H (send canvas get-height))
  (send dc set-brush "white" 'solid)
  (send dc set-pen "white" 1 'transparent)
  (send dc draw-rectangle 0 0 W H)

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
         (when (> pista 0)
           (draw-centered dc (number->string pista) x y w h)))
        (marcado? (draw-centered dc "⚑" x y w h))
        (else (void)))) )

  (when der (overlay dc "💥 BOOM — Perdiste" canvas))
  (when gan (overlay dc "🎉 ¡Ganaste!" canvas))
)

;; -----------------------------------------
;; Primer click seguro (utilidad)
;; -----------------------------------------
;; Genera un tablero del nivel dado tal que la celda (r,c) NO tenga mina y su pista sea 0.
(define (generar-tablero-seguro filas cols nivel r c)
  (let loop ()
    (define t (generar-tablero-nivel filas cols nivel))
    (define cel (buscar t r c))
    (define mina? (car cel))
    (define pista (cadr cel))
    (if (and (not mina?) (= pista 0))
        t
        (loop))))

;; -----------------------------------------
;; Canvas personalizado (EVENTOS)
;; -----------------------------------------
(define my-canvas%
  (class canvas%
    (super-new
      [paint-callback (lambda (cnv dc)
                        (dibujar-tablero dc (unbox estado) cnv))])

    ;; Mouse
    (define/override (on-event e)
      (define et (send e get-event-type)) ; 'left-down, 'right-down, ...
      (when (or (eq? et 'left-down) (eq? et 'right-down))
        (define s      (unbox estado))
        (define tab    (S-tab s))
        (define ab     (S-abr s))
        (define ba     (S-ban s))
        (define der    (S-der s))
        (define gan    (S-gana s))
        (define first? (S-first s))
        (unless (or der gan)
          (define x (send e get-x))
          (define y (send e get-y))
          (define r (quotient y cell-size))
          (define c (quotient x cell-size))
          (when (en-rango? tab r c)
            (if (eq? et 'right-down)
                ;; bandera
                (set-estado! (list tab ab (alternar-bandera ba r c) #f (gano? tab ab) first?))
                ;; click izquierdo
                (if first?
                    (let* ((nivel (unbox nivel-actual))
                           (t2    (generar-tablero-seguro (filas tab) (cols tab) nivel r c)))
                      (let-values (((ab2 boom) (revelar t2 '() r c))) ; flood desde vacío
                        (set-estado! (list t2 ab2 ba #f (gano? t2 ab2) #f))))
                    (let-values (((ab2 boom) (revelar tab ab r c)))
                      (define win2 (and (not boom) (gano? tab ab2)))
                      (set-estado! (list tab ab2 ba boom win2 first?)))))))))

    ;; Teclado
    (define/override (on-char e)
      (define k (send e get-key-code))
      (when (equal? k #\r)
        (set-estado! (list (generar-tablero-nivel FILAS COLS (unbox nivel-actual))
                           '() '() #f #f #t)))))
) ; <-- cierra (class ...) y luego (define my-canvas%)





;; =========================================
;; Pantalla de JUEGO (barra fija + canvas directo)
;; =========================================
;; barra
(define game-bar
  (new horizontal-panel% [parent game-pnl] [stretchable-height #f]))

(define lbl-msg
  (new message% [parent game-bar]
       [label (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar"
                      (symbol->string (unbox nivel-actual)))]))

(define btn-volver
  (new button%  [parent game-bar] [label "Volver al menú"]
       [callback (lambda (_1 _2) (mostrar-menu!))]))


;; Canvas directamente debajo de la barra (no stretchable en altura)
(set! canvas
      (new my-canvas%
           [parent game-pnl]
           [min-width  (* COLS cell-size)]
           [min-height (* FILAS cell-size)]
           [stretchable-height #f]
           [style '(no-autoclear)]))

(define (actualizar-barra!)
  (send lbl-msg set-label
        (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar"
                (symbol->string (unbox nivel-actual))))
  (send canvas focus))

;; Ajusta la ventana al tamaño del tablero + barra (encoge y agranda)
(define (ajustar-ventana-a-tablero!)
  (define cw (* COLS cell-size))   ; ancho del tablero en px
  (define ch (* FILAS cell-size))  ; alto  del tablero en px

  ;; 1) asegurar que el canvas pida el tamaño correcto
  (send canvas min-width  cw)
  (send canvas min-height ch)

  ;; 2) estimar alto de la barra como el mayor min-size de sus hijos
  (define bh
    (let-values ([(w1 h1) (send lbl-msg   get-graphical-min-size)]
                 [(w2 h2) (send btn-volver get-graphical-min-size)])
      (max h1 h2)))

  ;; 3) forzar min-sizes en los contenedores (ayuda al layout)
  (send game-pnl min-width  cw)
  (send game-pnl min-height (+ ch bh))
  (send root     min-width  cw)
  (send root     min-height (+ ch bh))

  ;; 4) reflow para colocar todo antes de medir tamaños actuales
  (send frame reflow-container)

  ;; 5) calcular “chrome” del frame y redimensionar total con precisión
  (let-values ([(fw fh)   (send frame get-size)]         ; tamaño total actual
               [(fcw fch) (send frame get-client-size)]) ; área cliente actual
    (define chrome-w (- fw fcw))
    (define chrome-h (- fh fch))
    (send frame resize (+ cw chrome-w) (+ (+ ch bh) chrome-h)))

  ;; 6) foco al canvas
  (send canvas focus))



;; =========================================
;; Tamaños por nivel + ajuste del canvas
;; =========================================
(define (dims-por-nivel nivel)
  (cond [(eq? nivel 'facil)   (values 8  8)]
        [(eq? nivel 'medio)   (values 12 12)]
        [(eq? nivel 'dificil) (values 16 16)]
        [else                 (values 12 12)]))

(define (aplicar-dims-por-nivel! nivel)
  (define-values (f c) (dims-por-nivel nivel))
  (set! FILAS f)
  (set! COLS  c)
  (ajustar-ventana-a-tablero!))


;; =========================================
;; Navegación
;; =========================================
(define (mostrar-menu!)
  (send game-pnl show #f)
  (send menu-pnl show #t)
  (send frame reflow-container))

(define (iniciar-juego! nivel)
  (set-box! nivel-actual nivel)
  (aplicar-dims-por-nivel! nivel) ; ajusta y reflow + set-client-size
  ;; Reinicia con first? = #t (primer click seguro)
  (set-estado! (list (generar-tablero-nivel FILAS COLS nivel) '() '() #f #f #t))
  (actualizar-barra!)
  (send menu-pnl show #f)
  (send game-pnl show #t)
  (send frame reflow-container)
  (send canvas refresh-now)
  (send canvas focus))


;; =========================================
;; Menú
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
