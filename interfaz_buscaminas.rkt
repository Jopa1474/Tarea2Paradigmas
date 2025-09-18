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

;; Canvas del juego (se crea al abrir la ventana del juego)
(define canvas #f)

;; Setter de estado con repintado del canvas del juego
(define (set-estado! nuevo)
  (set-box! estado nuevo)
  (when canvas (send canvas refresh-now)))

;; -----------------------------------------
;; Utilidades de dibujo
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
                        (set-estado! (list t2 ab2 ba #f (gano? t2 ab2) #f)) ))
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
;; Tamaños por nivel
;; =========================================
(define (dims-por-nivel nivel)
  (cond [(eq? nivel 'facil)   (values 8  8)]
        [(eq? nivel 'medio)   (values 12 12)]
        [(eq? nivel 'dificil) (values 16 16)]
        [else                 (values 12 12)]))

(define (aplicar-dims-por-nivel! nivel)
  (define-values (f c) (dims-por-nivel nivel))
  (set! FILAS f)
  (set! COLS  c))

;; =========================================
;; VENTANA DEL MENÚ (independiente del juego)
;; =========================================
(define menu-frame (new frame% [label "Buscaminas — Menú"]))

(define menu-root
  (new vertical-panel% [parent menu-frame]
       [stretchable-width #t] [stretchable-height #t]
       [alignment '(center center)]))

(new message% [parent menu-root]
     [label "Elige dificultad:"] [auto-resize #t])

(define btns (new horizontal-panel% [parent menu-root] [alignment '(center center)]))

;; forward-declare para enlazar botones
(define iniciar-juego! #f)

(new button% [parent btns] [label "Fácil (10%)"]
     [callback (lambda (_1 _2) (iniciar-juego! 'facil))])

(new button% [parent btns] [label "Medio (15%)"]
     [callback (lambda (_1 _2) (iniciar-juego! 'medio))])

(new button% [parent btns] [label "Difícil (20%)"]
     [callback (lambda (_1 _2) (iniciar-juego! 'dificil))])

(send menu-frame show #t)

;; =========================================
;; VENTANA DEL JUEGO (se crea al iniciar)
;; =========================================
(define game-frame #f)
(define game-root  #f)
(define game-bar   #f)
(define game-pnl   #f)
(define lbl-msg    #f)
(define btn-volver #f)

;; Ajusta la ventana de juego al tamaño del tablero + barra
(define (ajustar-ventana-a-tablero!)
  (when (and game-frame canvas)
    (define cw (* COLS cell-size))   ; ancho tablero
    (define ch (* FILAS cell-size))  ; alto  tablero

    ;; 1) asegurar min-size del canvas
    (send canvas min-width  cw)
    (send canvas min-height ch)

    ;; 2) estimar alto de la barra como el mayor min-size de sus hijos
    (define bh
      (let-values ([(w1 h1) (send lbl-msg   get-graphical-min-size)]
                   [(w2 h2) (send btn-volver get-graphical-min-size)])
        (max h1 h2)))

    ;; 3) forzar min-sizes en contenedores del juego
    (send game-pnl  min-width  cw)
    (send game-pnl  min-height (+ ch bh))
    (send game-root min-width  cw)
    (send game-root min-height (+ ch bh))

    ;; 4) reflow antes de medir
    (send game-frame reflow-container)

    ;; 5) calcular “chrome” del frame y redimensionar total con precisión
    (let-values ([(fw fh)   (send game-frame get-size)]
                 [(fcw fch) (send game-frame get-client-size)])
      (define chrome-w (- fw fcw))
      (define chrome-h (- fh fch))
      (send game-frame resize (+ cw chrome-w) (+ (+ ch bh) chrome-h)))

    ;; 6) foco al canvas del juego
    (send canvas focus)))

;; Actualiza el texto de la barra del juego
(define (actualizar-barra!)
  (when lbl-msg
    (send lbl-msg set-label
          (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar"
                  (symbol->string (unbox nivel-actual)))))
  (when canvas (send canvas focus)))

;; Volver al menú (cierra/oculta la ventana de juego y muestra menú)
(define (mostrar-menu!)
  (when game-frame (send game-frame show #f))
  (send menu-frame show #t)
  (send menu-frame reflow-container))

;; Crea el frame del juego, su barra y su canvas PROPIOS (separados del menú)
(define (crear-game-frame!)
  (set! game-frame (new frame% [label "Buscaminas — Juego"]))
  (set! game-root
        (new vertical-panel% [parent game-frame]
             [stretchable-width #t] [stretchable-height #t]
             [alignment '(left top)]))

  ;; Barra fija arriba
  (set! game-bar (new horizontal-panel% [parent game-root] [stretchable-height #f]))

  (set! lbl-msg
        (new message% [parent game-bar]
             [label (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar"
                            (symbol->string (unbox nivel-actual)))]))

  (set! btn-volver
        (new button% [parent game-bar] [label "Volver al menú"]
             [callback (lambda (_1 _2) (mostrar-menu!))]))

  ;; Panel y canvas del JUEGO (no stretchable en altura; separado del menú)
  (set! game-pnl (new vertical-panel% [parent game-root]
                      [alignment '(left top)] [stretchable-height #t]))

  (set! canvas
        (new my-canvas%
             [parent game-pnl]
             [min-width  (* COLS cell-size)]
             [min-height (* FILAS cell-size)]
             [stretchable-height #f]
             [style '(no-autoclear)]))

  (send game-frame show #t)
  (send game-frame reflow-container)
  (ajustar-ventana-a-tablero!)
  (send canvas refresh-now)
  (send canvas focus))

;; =========================================
;; Iniciar juego (abre frame del juego y oculta el menú)
;; =========================================
(set! iniciar-juego!
      (lambda (nivel)
        (set-box! nivel-actual nivel)
        (aplicar-dims-por-nivel! nivel)

        ;; Reinicia estado con first? = #t (primer click seguro)
        (set-estado! (list (generar-tablero-nivel FILAS COLS nivel) '() '() #f #f #t))

        ;; Crear y mostrar frame/canvas del juego
        (crear-game-frame!)
        (actualizar-barra!)

        ;; Ocultar menú y asegurar render inicial
        (send menu-frame show #f)
        (send game-frame reflow-container)
        (send canvas refresh-now)
        (send canvas focus)))
