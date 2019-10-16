#lang racket/gui
(provide mostrarGUI)

(define frame (new frame%
                   [label "BlaCEkJack"]
                   [width 800]
                   [height 800]))
(new canvas% [parent frame]
             [paint-callback
              (lambda (canvas dc)
                (send dc set-scale 3 3)
                (send dc set-text-foreground "blue")
                (send dc draw-text "Aykull" 0 0))])

(define (mostrarGUI)
  (send frame show #t))
