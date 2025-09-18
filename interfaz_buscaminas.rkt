#lang racket/gui
(require racket/gui/base) ; por queue-callback (si no lo tienes ya)
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


;; ==========================
;; Animación de derrota
;; ==========================
(define minas-anim (box '())) ; lista de (list r c) ya reveladas visualmente
(define timer-anim #f)        ; timer de la animación o #f si no corre

(define (coord=? a b)
  (and (= (car a) (car b)) (= (cadr a) (cadr b))))

(define (miembro-coord? lst rc)
  (member rc (unbox minas-anim) coord=?))

;; Devuelve todas las minas del tablero, poniendo primero la del clic si se pasa
(define (todas-las-minas tab [click-rc #f])
  (define L
    (for*/list ([r (in-range (filas tab))]
                [c (in-range (cols tab))]
                #:when (car (buscar tab r c))) ; (car cel) = ¿mina?
      (list r c)))
  (if click-rc
      (append (list click-rc) (remove* (list click-rc) L coord=?))
      L))

;; Inicia la animación: revela minas de una en una
(define (start-loss-animation! tab [click-rc #f])
  ;; detener animación previa si existía
  (when timer-anim (send timer-anim stop) (set! timer-anim #f))
  (set-box! minas-anim '())
  (define cola (todas-las-minas tab click-rc)) ; cola mutable en el closure
  (set! timer-anim
        (new timer%
             [interval 60] ; ms por mina (ajústalo a gusto)
             [notify-callback
              (lambda ()
                (cond
                  [(null? cola)
                   (send timer-anim stop)
                   (set! timer-anim #f)]
                  [else
                   (set-box! minas-anim (cons (car cola) (unbox minas-anim)))
                   (set! cola (cdr cola))
                   (when canvas (send canvas refresh-now))]))])))


;; -----------------------------------------
;; Utilidades de dibujo
;; -----------------------------------------
;; ===== Centrado del tablero =====
;; Grosor del borde exterior del tablero
(define BOARD-FRAME 4)
(define BOARD-PAD 32) ; margen alrededor del tablero dentro del canvas

;; Dimensiones “reales” del tablero (sin padding)
(define (board-w) (* COLS cell-size))
(define (board-h) (* FILAS cell-size))

;; Offset para centrar el tablero dentro del canvas actual
(define (board-offset canvas)
  (define W (send canvas get-width))
  (define H (send canvas get-height))
  (define ox (max BOARD-PAD (quotient (- W (board-w)) 2)))
  (define oy (max BOARD-PAD (quotient (- H (board-h)) 2)))
  (values ox oy))

;; Rect de una celda r,c pero tomando en cuenta el offset centrado
(define (rc->rect r c)
  (define x0 (* c cell-size))
  (define y0 (* r cell-size))
  (values x0 y0 cell-size cell-size))

;; Como rc->rect ahora da coords relativas al tablero (0,0) en la esquina del tablero,
;; añadimos helpers para convertir a coords de canvas usando el offset:
(define (rc->rect+offset canvas r c)
  (define-values (x y w h) (rc->rect r c))
  (define-values (ox oy) (board-offset canvas))
  (values (+ ox x) (+ oy y) w h))

(define (draw-centered dc txt x y w h)
  (define-values (tw th _1 _2) (send dc get-text-extent txt))
  (send dc draw-text txt (+ x (quotient (- w tw) 2))
                        (+ y (quotient (- h th) 2))))

(define (overlay dc txt canvas color-ok?)
  (define W (send canvas get-width))
  (define H (send canvas get-height))

  ;; Guardar estilo actual
  (define old-font  (send dc get-font))
  (define old-color (send dc get-text-foreground))

  ;; Selección de colores según si ganó o perdió
  (define main-color   (if color-ok? RETRO-EDGE RETRO-RED))
  (define shadow-color (if color-ok? RETRO-EDGE-DIM (make-object color% 150 0 0)))

  ;; Fuente retro solo aquí
  (send dc set-font retro-message-font)

  ;; Centro del TABLERO (no del canvas)
  (define-values (ox oy) (board-offset canvas))
  (define cx (+ ox (quotient (board-w) 2)))
  (define cy (+ oy (quotient (board-h) 2)))

  ;; Medidas del texto
  (define-values (tw th _1 _2) (send dc get-text-extent txt))
  (define tx (- cx (quotient tw 2)))
  (define ty (- cy (quotient th 2)))

  ;; Sombra
  (send dc set-text-foreground shadow-color)
  (send dc draw-text txt (+ tx 2) (+ ty 2))
  ;; Texto principal
  (send dc set-text-foreground main-color)
  (send dc draw-text txt tx ty)

  ;; Restaurar estilo original
  (send dc set-font old-font)
  (send dc set-text-foreground old-color))







;; Colores clásicos de Buscaminas (1..8)
(define (numero->color n)
  (cond [(= n 1) (make-object color% "blue")]
        [(= n 2) (make-object color% "forest green")]
        [(= n 3) (make-object color% "red")]
        [(= n 4) (make-object color% "navy")]
        [(= n 5) (make-object color% "maroon")]
        [(= n 6) (make-object color% "teal")]
        [(= n 7) (make-object color% "black")]
        [(= n 8) (make-object color% "gray35")]
        [else     (make-object color% "black")]))



(define (dibujar-tablero dc s canvas)
  ;; Fondo del canvas
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

  ;; Offset del tablero centrado
  (define-values (ox oy) (board-offset canvas))

  ;; ===== Borde exterior verde (estilo retro) =====
  (define BOARD-FRAME 4) ; grosor del marco exterior (local a esta función)
  (define outer-x (- ox BOARD-FRAME))
  (define outer-y (- oy BOARD-FRAME))
  (define outer-w (+ (board-w) (* 2 BOARD-FRAME)))
  (define outer-h (+ (board-h) (* 2 BOARD-FRAME)))

  ;; Trazo principal verde brillante
  (send dc set-pen RETRO-EDGE BOARD-FRAME 'solid)
  (send dc set-brush "white" 'transparent)
  (safe-rounded-rect dc outer-x outer-y outer-w outer-h 6)

  ;; Trazo tenue para efecto "neón"
  (send dc set-pen RETRO-EDGE-DIM 1 'solid)
  (safe-rounded-rect dc (+ outer-x 2) (+ outer-y 2)
                     (- outer-w 4)  (- outer-h 4) 5)

  ;; ===== Fondo del área del tablero (gris claro) =====
  (send dc set-pen "gray50" 1 'solid)
  (send dc set-brush "light gray" 'solid)
  (send dc draw-rectangle ox oy (board-w) (board-h))

  ;; ===== Celdas =====
  (for ([r (in-range (filas tab))])
    (for ([c (in-range (cols tab))])
      (define-values (x y w h) (rc->rect+offset canvas r c))
      (send dc set-pen "black" 1 'solid)
      (send dc set-brush "light gray" 'solid)
      (send dc draw-rectangle x y w h)

      (define cel   (buscar tab r c))
      (define mina? (car  cel))
      (define pista (cadr cel))

      (define descubierto?
        (member (list r c) ab (lambda (a b) (and (= (car a) (car b))
                                                 (= (cadr a) (cadr b))))))

      (define marcado?
        (member (list r c) ba (lambda (a b) (and (= (car a) (car b))
                                                 (= (cadr a) (cadr b))))))

      (cond
        [(and der mina? (miembro-coord? (unbox minas-anim) (list r c)))
         (draw-centered dc "💣" x y w h)]

        [(and (not der) descubierto? mina?)
         (draw-centered dc "💣" x y w h)]

        [descubierto?
         (send dc set-brush "white" 'solid)
         (send dc draw-rectangle (+ x 1) (+ y 1) (- w 2) (- h 2))
         (when (> pista 0)
           (send dc set-text-foreground (numero->color pista))
           (draw-centered dc (number->string pista) x y w h)
           (send dc set-text-foreground "black"))]

        [marcado?
         (draw-centered dc "⚑" x y w h)]
        [else (void)])))

  ;; Overlay centrado respecto al tablero (puede salirse si no cabe)
  (when der (overlay dc "💥 BOOM — Perdiste" canvas #f))
  (when gan (overlay dc "🎉 ¡Ganaste!"       canvas #t)))


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
      (define et (send e get-event-type))
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
          (define-values (ox oy) (board-offset this))
          (define bx (- x ox))
          (define by (- y oy))
          (cond
            ;; Clic fuera del tablero: ignorar
            [(or (< bx 0) (< by 0)
                 (>= bx (board-w)) (>= by (board-h)))
             (void)]
            [else
             (define r (quotient by cell-size))
             (define c (quotient bx cell-size))
             (when (en-rango? tab r c)
               (if (eq? et 'right-down)
                   (set-estado! (list tab ab (alternar-bandera ba r c) #f (gano? tab ab) first?))
                   (if first?
                       (let* ((nivel (unbox nivel-actual))
                              (t2    (generar-tablero-seguro (filas tab) (cols tab) nivel r c)))
                         (let-values (((ab2 boom) (revelar t2 '() r c)))
                           (set-estado! (list t2 ab2 ba #f (gano? t2 ab2) #f))))
                       (let-values (((ab2 boom) (revelar tab ab r c)))
                         (define win2 (and (not boom) (gano? tab ab2)))
                         (set-estado! (list tab ab2 ba boom win2 first?))
                         (when boom (start-loss-animation! tab (list r c)))))))]))))


    ;; Teclado
    (define/override (on-char e)
      (define k (send e get-key-code))
      (when (equal? k #\r)
        ;; >>> NUEVO: detener/limpiar animación antes de reiniciar
        (when timer-anim (send timer-anim stop) (set! timer-anim #f))
        (set-box! minas-anim '())
        (set-estado! (list (generar-tablero-nivel FILAS COLS (unbox nivel-actual))
                           '() '() #f #f #t))))
  ))



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
(define retro-message-font   (make-object font% 30 'modern 'normal 'bold))
(define retro-status-font (make-object font% 13 'modern 'normal 'bold)) 
(define retro-btn-font-small (make-object font% 13 'modern 'normal 'bold))




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

;; --- Botón retro en canvas (configurable y compacto) ---
(define retro-button%
  (class canvas%
    (init-field label on-click)
    (init-field [btn-font   retro-btn-font]
                [min-width  260]
                [min-height 48])
    (super-new [style '(no-autoclear)]
               [min-width  min-width] [min-height min-height]
               [stretchable-width #f] [stretchable-height #f])

    (define hover? #f)
    (define active? #f)

    (define/private (paint!)
      (define dc (send this get-dc))
      (define w  (send this get-width))
      (define h  (send this get-height))
      ;; dibuja un pelín adentro para que no se corte el borde
      (send dc set-brush (if active? RETRO-BG RETRO-PANEL) 'solid)
      (send dc set-pen   (if hover? RETRO-EDGE RETRO-EDGE-DIM) 2 'solid)
      (safe-rounded-rect dc 1 1 (- w 2) (- h 2) 5)
      (send dc set-pen (if hover? RETRO-EDGE RETRO-EDGE-DIM) 1 'solid)
      (safe-rounded-rect dc 3 3 (- w 6) (- h 6) 4)

      (send dc set-font btn-font)
      (send dc set-text-foreground (if hover? RETRO-TEXT RETRO-TEXT-DIM))
      (define-values (tw th _1 _2) (send dc get-text-extent label))
      (define tx (max 4 (quotient (- w tw) 2)))
      (define ty (max 4 (quotient (- h th) 2)))
      (send dc draw-text label tx ty))

    (define/override (on-paint) (paint!))
    (define/override (on-size _w _h) (send this refresh-now))
    (define/override (on-event e)
      (case (send e get-event-type)
        [(enter)     (set! hover? #t) (send this refresh-now)]
        [(leave)     (set! hover? #f) (set! active? #f) (send this refresh-now)]
        [(left-down) (set! active? #t) (send this refresh-now)]
        [(left-up)
         (when active?
           (set! active? #f)
           (send this refresh-now)
           (when (procedure? on-click) (on-click this e)))]
        [else (void)]))))


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
         (init-value (number->string COLS))
         (min-width 60)))

  (new message% (parent row-pnl) (label "Cols:"))
  (define tf-col
    (new text-field%
         (parent row-pnl)
         (label "")
         (init-value (number->string FILAS))
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
                                  COLS FILAS)
                          menu-frame))))))

  ;; Botón Cancelar
  (new button% (parent btns-pnl) (label "Cancelar")
       (callback (lambda (_1 _2) (send dlg show #f))))

  (send dlg show #t))


;; === Helper para crear botones retro de forma cómoda ===
;; Úsalo tanto en el MENÚ como en el GAME BAR.
(define (make-retro-button parent text cb
                           #:font [font retro-btn-font]
                           #:min-width [mw 260]
                           #:min-height [mh 48])
  (new retro-button%
       [parent parent]
       [label (format "▶ ~a" text)]
       [on-click (lambda (_btn _e) (cb))]
       [btn-font font]
       [min-width mw]
       [min-height mh]))


;; Dibujo seguro de rounded-rect: evita anchos/altos negativos y radio inválido
(define (safe-rounded-rect dc x y w h r)
  (define W (max 1 (inexact->exact (ceiling w))))
  (define H (max 1 (inexact->exact (ceiling h))))
  (define R (max 0 (min (inexact->exact (ceiling r))
                        (quotient (min W H) 2))))
  (send dc draw-rounded-rectangle x y W H R))


;; Botón NUEVO
(make-retro-button btns "Cambiar tablero" abrir-dialogo-personalizar!)

;; Botones de nivel (usan tamaño por defecto, salvo que usuario haya personalizado)
(make-retro-button btns "Fácil"  (lambda () (iniciar-juego! 'facil)))
(make-retro-button btns "Medio"  (lambda () (iniciar-juego! 'medio)))
(make-retro-button btns "Difícil" (lambda () (iniciar-juego! 'dificil)))

;; --- Barra de estado retro (canvas) ---
(define (→int x) (inexact->exact (ceiling x))) ; helper por si no lo tienes ya

;; --- Barra de estado retro con AUTO-WRAP ---
(define retro-status%
  (class canvas%
    (init-field [text ""])
    (init [stretchable-width  #t]
          [stretchable-height #f]
          [min-width  420]
          [min-height 38]) ; ↑ un poco más alta

    (super-new [style '(no-autoclear)]
               [min-width  min-width]
               [min-height min-height])

    (send this stretchable-width  stretchable-width)
    (send this stretchable-height stretchable-height)

    (define status-font retro-status-font)
    (define line-gap  3)   ; ↑
    (define side-pad  12)  ; ↑
    (define vert-pad  6)   ; ↑

    (define/public (set-text! t)
      (set! text t)
      (send this refresh-now))

    ;; split por " | "
    (define (split-items s)
      (define raw (regexp-split #px"\\s*\\|\\s*" s))
      (for/list ([i (in-naturals)] [part (in-list raw)])
        (list part (if (= i (sub1 (length raw))) "" " | "))))

    (define (layout-lines dc w s)
      (send dc set-font status-font)
      (define items (split-items s))
      (define usable-w (max 0 (- w (* 2 side-pad) 8)))
      (define lines '())
      (define current '())
      (define current-w 0)
      (for ([it items])
        (define content (first it))
        (define sep     (second it))
        (define-values (ctw cth _1 _2) (send dc get-text-extent content))
        (define-values (stw _a _b _c)  (send dc get-text-extent sep))
        (define need (+ ctw stw))
        (cond
          [(zero? (length current))
           (set! current (list it))
           (set! current-w need)]
          [(<= (+ current-w need) usable-w)
           (set! current (append current (list it)))
           (set! current-w (+ current-w need))]
          [else
           (set! lines (append lines (list current)))
           (set! current (list it))
           (set! current-w need)]))
      (when (pair? current)
        (set! lines (append lines (list current))))
      lines)

    (define/override (on-paint)
      (define dc (send this get-dc))
      (define w  (send this get-width))
      (define h  (send this get-height))

      ;; fondo + panel
      (send dc set-brush RETRO-BG 'solid)
      (send dc set-pen   RETRO-BG 1 'transparent)
      (send dc draw-rectangle 0 0 w h)
      (send dc set-pen RETRO-EDGE 1 'solid)
      (send dc set-brush RETRO-PANEL 'solid)
      (safe-rounded-rect dc 4 4 (- w 8) (- h 8) 4)

      ;; layout
      (send dc set-font status-font)
      (define lines (layout-lines dc w text))
      (define-values (_tw th _1 _2) (send dc get-text-extent "Ag"))
      (define content-h (+ (* (length lines) th)
                           (* (max 0 (sub1 (length lines))) line-gap)))
      (define needed-h  (+ (* 2 vert-pad) content-h))
      (define minpanel-h (max 34 needed-h)) ; piso un poquito mayor

      (when (> minpanel-h h)
        (send this min-height (→int minpanel-h))
        (with-handlers ([exn:fail? (lambda (_e) (void))])
          (define p (send this get-parent))
          (when p (send p reflow-container))))

      ;; dibujar texto con sombra
      (define y0 (+ 4 vert-pad))
      (for/fold ([y y0]) ([ln lines])
        (define x (+ 4 side-pad))
        (for ([it ln])
          (define content (first it))
          (define sep     (second it))
          (send dc set-text-foreground RETRO-TEXT-DIM)
          (send dc draw-text content (+ x 1) (+ y 1))
          (send dc set-text-foreground RETRO-TEXT)
          (send dc draw-text content x y)
          (define-values (ctw _ _a _b) (send dc get-text-extent content))
          (set! x (+ x ctw))
          (when (not (string=? sep ""))
            (send dc set-text-foreground RETRO-TEXT-DIM)
            (send dc draw-text sep (+ x 1) (+ y 1))
            (send dc set-text-foreground RETRO-TEXT)
            (send dc draw-text sep x y)
            (define-values (stw _c _d _e) (send dc get-text-extent sep))
            (set! x (+ x stw))))
        (+ y th line-gap)))))







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
(define status-cnv #f)
(define btn-volver #f)

;; Igualar la altura del botón con la barra de estado
(define BUTTON_MIN_H 32)

;; Igualar altura del botón a la altura REAL del status (con piso)
(define (sincronizar-alturas-barra!)
  (when (and status-cnv btn-volver game-bar)
    ;; Espera al próximo ciclo de GUI para que status-cnv ya tenga su altura final
    (queue-callback
     (lambda ()
       (define status-h (send status-cnv get-height)) ; altura actual renderizada
       (define target-h (max BUTTON_MIN_H status-h))
       (send btn-volver min-height target-h)
       ;; (opcional) asegura un ancho cómodo
       (send btn-volver min-width 190)
       (send game-bar reflow-container)
       (send btn-volver refresh-now)))))

(define (ajustar-ventana-a-tablero!)
  (when (and game-frame canvas)
    (define cw (+ (board-w) (* 2 BOARD-PAD)))
    (define ch (+ (board-h) (* 2 BOARD-PAD)))
    (send canvas  min-width  cw)
    (send canvas  min-height ch)

    ;; Altura de la barra (status + botón)
    (define bh
      (let-values ([(w1 h1) (send status-cnv get-graphical-min-size)]
                   [(w2 h2) (send btn-volver  get-graphical-min-size)])
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

(define (actualizar-barra!)
  (when status-cnv
    (send status-cnv set-text!
          (format "Nivel: ~a   |   Izq: descubrir  |  Der: bandera  |  R: reiniciar   |   Tamaño: ~ax~a"
                  (symbol->string (unbox nivel-actual)) COLS FILAS))
    (sincronizar-alturas-barra!))
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
             [alignment '(left top)]
             [horiz-margin 0] [vert-margin 0] [spacing 0]))

  ;; Barra superior
  (set! game-bar (new horizontal-panel%
                      [parent game-root]
                      [alignment '(left center)]
                      [spacing 8]
                      [horiz-margin 8]
                      [vert-margin 6]
                      [stretchable-height #f]))
  (with-handlers ([exn:fail? (lambda (_e) (void))])
    (send game-bar set-background RETRO-BG))

  ;; Status (wrap + tamaño cómodo)
  (set! status-cnv
        (new retro-status%
             [parent game-bar]
             [text (format "Nivel: ~a | Izq: descubrir | Der: bandera | R: reiniciar | Tamaño: ~ax~a"
                           (symbol->string (unbox nivel-actual)) COLS FILAS)]
             [stretchable-width #t]
             [min-height 38]))

  ;; Botón “Volver al menú” (más grande para que no se vea raro)
  (set! btn-volver
        (make-retro-button game-bar "Volver al menú"
                           (lambda () (mostrar-menu!))
                           #:font       retro-btn-font-small
                           #:min-width  190
                           #:min-height 36))

  (sincronizar-alturas-barra!)


  ;; Panel del canvas de juego
  (set! game-pnl (new vertical-panel% [parent game-root]
                      [alignment '(left top)] [stretchable-height #t]
                      [horiz-margin 8] [vert-margin 0] [spacing 0]))

  (set! canvas
      (new my-canvas%
           [parent game-pnl]
           [min-width  (+ (board-w) (* 2 BOARD-PAD))]
           [min-height (+ (board-h) (* 2 BOARD-PAD))]
           [stretchable-height #f]
           [style '(no-autoclear)]))


  (send game-frame show #t)
  (send game-frame center 'both)
  (send game-frame reflow-container)
  (ajustar-ventana-a-tablero!)
  (send canvas refresh-now)
  (send canvas focus))

;; =========================================
;; Iniciar juego (tamaño personalizado)
;; =========================================
(set! iniciar-juego!
      (lambda (nivel)
        (set-box! nivel-actual nivel)
        ;; Si NO hay tamaño personalizado, usa el tamaño por defecto del nivel
        (when (not (unbox dims-personalizadas?))
          (aplicar-dims-por-nivel! nivel))
        (when timer-anim (send timer-anim stop) (set! timer-anim #f))
        (set-box! minas-anim '())
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
