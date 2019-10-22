#lang racket/gui
(require racket/draw)

(require "Logica.rkt")

;Se crea el marco en donde se realiza el juego
(define frame (new frame%
                   [label "BlaCEkJack"]
                   [width 800]
                   [height 800]
                   [style (list 'no-resize-border)]))
                   

;Se cargan todas las imagenes del deck
(define bg (read-bitmap "Fondo.png"))

(define back (read-bitmap "Back.png"))

(define Ah (read-bitmap "Ah.png"))
(define 2h (read-bitmap "2h.png"))
(define 3h (read-bitmap "3h.png"))
(define 4h (read-bitmap "4h.png"))
(define 5h (read-bitmap "5h.png"))
(define 6h (read-bitmap "6h.png"))
(define 7h (read-bitmap "7h.png"))
(define 8h (read-bitmap "8h.png"))
(define 9h (read-bitmap "9h.png"))
(define 10h (read-bitmap "10h.png"))
(define Jh (read-bitmap "Jh.png"))
(define Qh (read-bitmap "Qh.png"))
(define Kh (read-bitmap "Kh.png"))

(define Ac (read-bitmap "Ac.png"))
(define 2c (read-bitmap "2c.png"))
(define 3c (read-bitmap "3c.png"))
(define 4c (read-bitmap "4c.png"))
(define 5c (read-bitmap "5c.png"))
(define 6c (read-bitmap "6c.png"))
(define 7c (read-bitmap "7c.png"))
(define 8c (read-bitmap "8c.png"))
(define 9c (read-bitmap "9c.png"))
(define 10c (read-bitmap "10c.png"))
(define Jc (read-bitmap "Jc.png"))
(define Qc (read-bitmap "Qc.png"))
(define Kc (read-bitmap "Kc.png"))

(define As (read-bitmap "As.png"))
(define 2s (read-bitmap "2s.png"))
(define 3s (read-bitmap "3s.png"))
(define 4s (read-bitmap "4s.png"))
(define 5s (read-bitmap "5s.png"))
(define 6s (read-bitmap "6s.png"))
(define 7s (read-bitmap "7s.png"))
(define 8s (read-bitmap "8s.png"))
(define 9s (read-bitmap "9s.png"))
(define 10s (read-bitmap "10s.png"))
(define Js (read-bitmap "Js.png"))
(define Qs (read-bitmap "Qs.png"))
(define Ks (read-bitmap "Ks.png"))

(define Ad (read-bitmap "Ad.png"))
(define 2d (read-bitmap "2d.png"))
(define 3d (read-bitmap "3d.png"))
(define 4d (read-bitmap "4d.png"))
(define 5d (read-bitmap "5d.png"))
(define 6d (read-bitmap "6d.png"))
(define 7d (read-bitmap "7d.png"))
(define 8d (read-bitmap "8d.png"))
(define 9d (read-bitmap "9d.png"))
(define 10d (read-bitmap "10d.png"))
(define Jd (read-bitmap "Jd.png"))
(define Qd (read-bitmap "Qd.png"))
(define Kd (read-bitmap "Kd.png"))



;Funciones para dibujar las imagenes como bitmaps
(define bitmap-canvas%
  (class canvas%
    (init-field [bitmap #f])
    (inherit get-dc)
    (define/override (on-paint)
      (send (get-dc) draw-bitmap bitmap 0 0))
    (super-new)))

;Se define un canvas para pintar en el frame y que se muestre el fondo
(define mycanvas (new bitmap-canvas% [parent frame] [bitmap bg]))

(define msg (new message% [parent frame]
                          [label "No events so far..."]))


(new button% [parent frame]
             [label "Pedir Carta"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click 1"))])

(new button% [parent frame]
             [label "Plantarme"]
             ; Callback procedure for a button click:
             [callback (lambda (button event)
                         (send msg set-label "Button click 2"))])

(define (mostrarGUI)
  (send frame show #t))

(define t (make-table "Blackjack" 6 3))

