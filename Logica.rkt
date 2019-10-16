#lang racket
(require "GUI.rkt")

(mostrarGUI)


;;;Función Principal
;(define (bCEj X)
;  (auxbCEj X (Ah 1h 2h 3h 4h 5h 6h 7h 8h 9h 10h Jh Qh Kh
;                 Ad 1d 2d 3d 4d 5d 6d 7d 8d 9d 10d Jd Qd Kd
;                 Ac 1c 2c 3c 4c 5c 6c 7c 8c 9c 10c Jc Qc Kc
;                 As 1s 2s 3s 4s 5s 6s 7s 8s 9s 10s Js Qs Ks)))
;
;;;Funcion Auxiliar Principal, la cual construye la lista
;;;de jugadores con sus respectivas cartas
;(define (auxbCEj X deck)
;  (cond ((= X -1) '())
;    (else (cons X (bCEj (- X 1))))))
;
;
;;;Función Carta Aleatoria
;(define (cartasAleatorias n)
;  (cond ((= n 0) '())
;    (else (cons (Deck (random (len Deck)))))))
;
;;;Funcion lenDeck
;(define (lenDeck deck)
;  (cond ((equal? deck '()) 0)
;  (else (+ 1 (lenDeck (cdr deck))))))
;
;             
;
;(bCEj 3)