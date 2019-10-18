#lang racket/gui
(require net/url)

(provide mostrarGUI)

(define frame (new frame%
                   [label "BlaCEkJack"]
                   [width 800]
                   [height 800]))

(define bm (make-object bitmap% (get-pure-port
                                 (string->url "http://imagenes.4ever.eu/data/download/abstractos/fondo-verde-217279.jpg?no-logo"))))

(define (mostrarGUI)
  (send frame show #t) )

(define mycanvas%
  (class canvas%
    (super-new)
    (inherit get-dc)
    (define/override (on-paint)
      (let ([my-dc (get-dc)])
        (send my-dc draw-bitmap bm 0 0)))))

(define c (new mycanvas% [parent frame]))
