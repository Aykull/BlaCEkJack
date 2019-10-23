#lang racket

(require games/cards racket/gui racket/class "Logica.rkt")
;(require games/cards racket/gui racket/class)

;; Se crea una mesa
(define table (make-table "Blackjack" 8 6))
(send table show #t)

;; Retorno del ancho y el largo de la mesa
(define w (send table table-width))
(define h (send table table-height))

;; Constantes que definen los límites de las regiones
(define MARGIN 10)
(define SUBMARGIN 10)
(define LABEL-H 15)

;;Contruir un Deck Gui
(define deckGUI (make-deck))

;;Construcción del deck de la GUI
(define deck (bCEj '("e" "g" "c")))        

;; Ancho y largo de las cartas
(define cwidth (send (car (make-deck)) card-width))
(define cheight (send (car (make-deck)) card-height))

;; Definición del tamaño de los botones
(define BUTTON-HEIGHT 16)
(define BUTTON-WIDTH cwidth)

;; Creacion de botones
(define (make-button title pos)
  (make-button-region (+ (/ (- w (* 4 BUTTON-WIDTH) (* 3 MARGIN)) 2)
                         (* pos (+ BUTTON-WIDTH MARGIN)))
                      (- h MARGIN BUTTON-HEIGHT)
                      BUTTON-WIDTH BUTTON-HEIGHT
                      title void))

(define hit-button (make-button "Pedir carta" 1))
(define stand-button (make-button "Plantarse" 2))

(define (solicitar_Carta)
  (auxS_C1 (recorrerDeck 1 deck "Solicitar Carta")))
(define (auxS_C1 lista)
  (cond ((equal? lista #f) "Segundo jugador")
        (else (set! deck lista)
              (set! p1 (retornarCartas 1 deck deckGUI))
              (send table stack-cards p1)
              (send table move-cards-to-region p1 player1-region)
              (send table cards-face-up p1))))

(set-region-callback! hit-button solicitar_Carta) 

;Evita que el usuario mueva las cartas 
#|(for-each (lambda (card) (send* card (user-can-move #f) (user-can-flip #f)))
          deck)|#

;; Crea regiones de juego para los jugadores
(define deck-region
  (make-region MARGIN MARGIN cwidth cheight #f #f))

(define discard-region
  (make-region (- w cwidth MARGIN) MARGIN cwidth cheight #f #f))

(define crupier-region
  (make-region (+ cwidth (* 2 MARGIN)) MARGIN
               (- w (* 2 cwidth) (* 4 MARGIN)) cheight
               #f #f))

(define player1-region
  (make-region (+ cwidth (* 2 MARGIN)) (- h (* 2 MARGIN) cheight BUTTON-HEIGHT)
               (- w (* 2 cwidth) (* 4 MARGIN)) cheight
               #f #f))

(define player2-region
  (make-region 350 215
               (+ cwidth 115) cheight
                #f #f))

(define player3-region
  (make-region  MARGIN 215
               (+ cwidth 100) cheight
               #f #f))

(send table add-cards-to-region deckGUI deck-region)

(define p1 (retornarCartas 1 deck deckGUI))
(define p2 (retornarCartas 2 deck deckGUI))
(define p3 (retornarCartas 3 deck deckGUI))
(define d (retornarCartas 4 deck deckGUI))


(define (main)
   (send table add-region hit-button)
   (send table move-cards-to-region p1 player1-region)
   (send table move-cards-to-region p2 player2-region)
   (send table move-cards-to-region p3 player3-region)
   (send table move-cards-to-region d crupier-region)
  (send table cards-face-up p1)
  (send table cards-face-up p2)
  (send table cards-face-up p3)
  (send table card-face-up (car d)))
 

(main)