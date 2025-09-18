#lang racket/gui
(require "logica_buscaminas.rkt")

;; =========================================
;; Parámetros base
;; =========================================
(define cell-size 32) ; tamaño de cada celda en píxeles

;; valores iniciales (se ajustan al elegir nivel)
(define FILAS 8)
(define COLS  8)

;; Flag: ¿tamaños personalizados activos?
(define dims-personalizadas? (box #f))

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
;; Primer click seguro (misma idea)
;; -----------------------------------------
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
) ; <-- cierra (define my-canvas%)


;; =========================================
;; Tamaños por nivel (por defecto; se ignoran si el usuario personaliza)
;; =========================================
;; Ahora: siempre 8x8 para cualquier nivel
(define (dims-por-nivel _nivel)
  (values 8 8))

(define (aplicar-dims-por-nivel! nivel)
  (define-values (f c) (dims-por-nivel nivel))
  (set! FILAS f)
  (set! COLS  c))

;; =========================================
;; MENÚ (igual estilo, con botón extra “Personalizar…”)
;; =========================================

;; --- Paleta & fuentes retro ---
(define RETRO-BG       (make-object color%  20  24  28))
(define RETRO-PANEL    (make-object color%  32  36  44))
(define RETRO-EDGE     (make-object color%   0 255 153))
(define RETRO-EDGE-DIM (make-object color%   0 180 120))
(define RETRO-TEXT     (make-object color% 235 255 245))
(define RETRO-TEXT-DIM (make-object color% 180 210 200))
(define RETRO-RED      (make-object color% 255  64  64))

(define retro-title-font (make-object font% 36 'modern 'normal 'bold))
(define retro-btn-font   (make-object font% 16 'modern 'normal 'bold))

;; --- Título en canvas: “BuscaCE” (igual) ---
(define retro-title%
  (class canvas%
    (super-new [style '(no-autoclear)]
               [min-width 520] [min-height 120])
    (define/override (on-paint)
      (define dc (send this get-dc))
      (define w  (send this get-width))
      (define h  (send this get-height))
      (send dc set-brush RETRO-BG 'solid)
      (send dc set-pen   RETRO-BG 1 'transparent)
      (send dc draw-rectangle 0 0 w h)
      (send dc set-pen RETRO-EDGE 3 'solid)
      (send dc set-brush RETRO-PANEL 'solid)
      (send dc draw-rectangle 8 8 (- w 16) (- h 16))
      (send dc set-font retro-title-font)
      (send dc set-text-foreground RETRO-EDGE)
      (define title "BuscaCE")
      (define-values (tw th _1 _2) (send dc get-text-extent title))
      (define tx (quotient (- w tw) 2))
      (define ty (quotient (- h th) 2))
      (send dc set-text-foreground RETRO-EDGE-DIM)
      (send dc draw-text title (+ tx 2) (+ ty 2))
      (send dc set-text-foreground RETRO-EDGE)
      (send dc draw-text title tx ty))))

;; --- Botón retro en canvas (igual) ---
(define retro-button%
  (class canvas%
    (init-field label on-click)
    (super-new [style '(no-autoclear)]
               [min-width  260] [min-height 48]
               [stretchable-width #f] [stretchable-height #f])
    (define hover? #f)
    (define active? #f)

    (define/private (paint!)
      (define dc (send this get-dc))
      (define w  (send this get-width))
      (define h  (send this get-height))
      (send dc set-brush (if active? RETRO-BG RETRO-PANEL) 'solid)
      (send dc set-pen   (if hover? RETRO-EDGE RETRO-EDGE-DIM) 3 'solid)
      (send dc draw-rounded-rectangle 0 0 w h 6)
      (send dc set-pen (if hover? RETRO-EDGE RETRO-EDGE-DIM) 1 'solid)
      (send dc draw-rounded-rectangle 3 3 (- w 6) (- h 6) 4)
      (send dc set-font retro-btn-font)
      (send dc set-text-foreground (if hover? RETRO-TEXT RETRO-TEXT-DIM))
      (define-values (tw th _1 _2) (send dc get-text-extent label))
      (define tx (quotient (- w tw) 2))
      (define ty (quotient (- h th) 2))
      (send dc draw-text label tx ty))
    (define/override (on-paint) (paint!))
    (define/override (on-size w h) (send this refresh-now))
    (define/override (on-event e)
      (define et (send e get-event-type))
      (case et
        [(enter) (set! hover? #t) (send this refresh-now)]
        [(leave) (set! hover? #f) (set! active? #f) (send this refresh-now)]
        [(left-down) (set! active? #t) (send this refresh-now)]
        [(left-up)
         (when active?
           (set! active? #f)
           (send this refresh-now)
           (when (procedure? on-click) (on-click this e)))]
        [else (void)]))))

(define (make-retro-button parent text cb)
  (new retro-button% [parent parent]
       [label (format "▶ ~a" text)]
       [on-click (lambda (_btn _e) (cb))]))

;; -------- Frame del MENÚ --------
(define menu-frame (new frame% [label "Buscaminas — Menú"]))
(define menu-root
  (new vertical-panel% [parent menu-frame]
       [alignment '(center center)]
       [horiz-margin 16] [vert-margin 16]
       [spacing 12]
       [stretchable-width #t] [stretchable-height #t]))

(with-handlers ([exn:fail? (lambda (_e) (void))])
  (send menu-frame set-background RETRO-BG))

(new retro-title% [parent menu-root])

(define btns
  (new vertical-panel% [parent menu-root]
       [alignment '(center center)]
       [spacing 10]
       [stretchable-width #f] [stretchable-height #f]))

;; Declaración adelantada
(define iniciar-juego! #f)

(define (abrir-dialogo-personalizar!)
  (define dlg (new dialog% (label "Tamaño personalizado (8–15)")))
  (define pnl (new vertical-panel%
                   (parent dlg)
                   (alignment '(center center))
                   (spacing 6) (horiz-margin 12) (vert-margin 12)))

  (new message% (parent pnl)
       (label "Ingresa filas y columnas entre 8 y 15 (rectángulos permitidos)."))

  (define row-pnl (new horizontal-panel% (parent pnl) (spacing 6)))

  (new message% (parent row-pnl) (label "Filas:"))
  (define tf-fil
    (new text-field%
         (parent row-pnl)
         (label "")
         (init-value (number->string FILAS))
         (min-width 60)))

  (new message% (parent row-pnl) (label "Cols:"))
  (define tf-col
    (new text-field%
         (parent row-pnl)
         (label "")
         (init-value (number->string COLS))
         (min-width 60)))

  (define btns-pnl (new horizontal-panel% (parent pnl) (spacing 8)))

  ;; Botón Aceptar
  (new button% (parent btns-pnl) (label "Aceptar")
       (callback
        (lambda (_1 _2)
          (define maybe-f (string->number (send tf-fil get-value)))
          (define maybe-c (string->number (send tf-col get-value)))
          (cond
            ((or (not (integer? maybe-f)) (not (integer? maybe-c)))
             (message-box "Error" "Debes ingresar números enteros." dlg))
            ((or (< maybe-f 8) (> maybe-f 15) (< maybe-c 8) (> maybe-c 15))
             (message-box "Error" "Valores fuera de rango (8..15)." dlg))
            (else
             ;; por esto:
             (set! FILAS maybe-c)  ; alto
             (set! COLS  maybe-f)  ; ancho
             (set-box! dims-personalizadas? #t)
             (send dlg show #f)
             (message-box "Listo"
                          (format "Tamaño establecido: ~a×~a.\nElige un nivel para empezar."
                                  FILAS COLS)
                          menu-frame))))))

  ;; Botón Cancelar
  (new button% (parent btns-pnl) (label "Cancelar")
       (callback (lambda (_1 _2) (send dlg show #f))))

  (send dlg show #t))




;; Botón NUEVO
(make-retro-button btns "Cambiar tablero" abrir-dialogo-personalizar!)

;; Botones de nivel (usan tamaño por defecto, salvo que usuario haya personalizado)
(make-retro-button btns "Fácil"  (lambda () (iniciar-juego! 'facil)))
(make-retro-button btns "Medio"  (lambda () (iniciar-juego! 'medio)))
(make-retro-button btns "Difícil" (lambda () (iniciar-juego! 'dificil)))

;; Mostrar y centrar el menú
(send menu-frame show #t)
(send menu-frame center 'both)

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
    (define cw (* COLS cell-size))
    (define ch (* FILAS cell-size))
    (send canvas min-width  cw)
    (send canvas min-height ch)
    (define bh
      (let-values ([(w1 h1) (send lbl-msg   get-graphical-min-size)]
                   [(w2 h2) (send btn-volver get-graphical-min-size)])
        (max h1 h2)))
    (send game-pnl  min-width  cw)
    (send game-pnl  min-height (+ ch bh))
    (send game-root min-width  cw)
    (send game-root min-height (+ ch bh))
    (send game-frame reflow-container)
    (let-values ([(fw fh)   (send game-frame get-size)]
                 [(fcw fch) (send game-frame get-client-size)])
      (define chrome-w (- fw fcw))
      (define chrome-h (- fh fch))
      (send game-frame resize (+ cw chrome-w) (+ (+ ch bh) chrome-h)))
    (send canvas focus)))

;; Actualiza el texto de la barra del juego
(define (actualizar-barra!)
  (when lbl-msg
    (send lbl-msg set-label
          (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar   |   Tamaño: ~ax~a"
                  (symbol->string (unbox nivel-actual)) COLS FILAS)))
  (when canvas (send canvas focus)))

;; Volver al menú
(define (mostrar-menu!)
  (when game-frame (send game-frame show #f))
  (send menu-frame show #t)
  (send menu-frame center 'both)
  (send menu-frame reflow-container))

;; Crea el frame del juego
(define (crear-game-frame!)
  (set! game-frame (new frame% [label "Buscaminas — Juego"]))
  (set! game-root
        (new vertical-panel% [parent game-frame]
             [stretchable-width #t] [stretchable-height #t]
             [alignment '(left top)]))
  (set! game-bar (new horizontal-panel% [parent game-root] [stretchable-height #f]))
  (set! lbl-msg
        (new message% [parent game-bar]
             [label (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar   |   Tamaño: ~ax~a"
                            (symbol->string (unbox nivel-actual)) FILAS COLS)]))
  (set! btn-volver
        (new button% [parent game-bar] [label "Volver al menú"]
             [callback (lambda (_1 _2) (mostrar-menu!))]))
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
  (send game-frame center 'both)
  (send game-frame reflow-container)
  (ajustar-ventana-a-tablero!)
  (send canvas refresh-now)
  (send canvas focus))

;; =========================================
;; Iniciar juego (respeta tamaño personalizado)
;; =========================================
(set! iniciar-juego!
      (lambda (nivel)
        (set-box! nivel-actual nivel)
        ;; Si NO hay tamaño personalizado, usa el tamaño por defecto del nivel
        (when (not (unbox dims-personalizadas?))
          (aplicar-dims-por-nivel! nivel))
        ;; Reinicia estado con first? = #t (primer click seguro)
        (set-estado! (list (generar-tablero-nivel FILAS COLS nivel) '() '() #f #f #t))
        ;; Crear ventana de juego
        (crear-game-frame!)
        (actualizar-barra!)
        ;; Ocultar menú y asegurar render
        (send menu-frame show #f)
        (send game-frame reflow-container)
        (send canvas refresh-now)
        (send canvas focus)))
