#lang racket

(require games/cards racket/gui racket/class "Logica.rkt")


;; Se crea una mesa
(define table (make-table "Blackjack" 8 6))
(send table show #t)

(define winner (make-table "Congratulation" 4 3))


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

;; Creacion de botones
;;;
(define (make-button1 title esp)
  (make-button-region (+ 208 esp) 550 BUTTON-WIDTH BUTTON-HEIGHT title void))
  
(define (make-button2 title esp)
  (make-button-region (+ 400 esp) 350 BUTTON-WIDTH BUTTON-HEIGHT title void))

(define (make-button3 title esp)
  (make-button-region (+ 10 esp) 350 BUTTON-WIDTH BUTTON-HEIGHT title void))
;;;
(define (make-buttonJ1 title )
  (make-button-region 250 400 BUTTON-WIDTH BUTTON-HEIGHT title void ))

(define (make-buttonJ2 title )
  (make-button-region 450 150 BUTTON-WIDTH BUTTON-HEIGHT title void ))

(define (make-buttonJ3 title )
  (make-button-region 50 150 BUTTON-WIDTH BUTTON-HEIGHT title void ))
;;;
(define hit-button1 (make-button1 "Pedir carta" 1))
(define stand-button1 (make-button1 "Plantarse" 90))

(define hit-button2 (make-button2 "Pedir carta" 1))
(define stand-button2 (make-button2 "Plantarse" 90))

(define hit-button3 (make-button3 "Pedir carta" 1))
(define stand-button3 (make-button3 "Plantarse" 90))
;;;
(define j1 (make-buttonJ1 "Jugador 1" ))
(define j2 (make-buttonJ2 "Jugador 2" ))
(define j3 (make-buttonJ3 "Jugador 3" ))
;;;
(define  (make-buttonW title pos)
  (make-button-region 10 (+ 30 pos) 250 40 title void ))
;;;
(define (solicitar_Carta)
  (auxS_C1 (recorrerDeck 1 deck "Solicitar Carta")))
(define (auxS_C1 lista)
  (cond ((equal? lista #f) "Segundo jugador")
        (else (set! deck lista)
              (set! p1 (retornarCartas 1 deck deckGUI))
              (send table stack-cards p1)
              (send table move-cards-to-region p1 player1-region)
              (send table cards-face-up p1))))

(define (solicitar_Carta2)
  (auxS_C2 (recorrerDeck 2 deck "Solicitar Carta")))
(define (auxS_C2 lista)
  (cond ((equal? lista #f) "Segundo jugador")
        (else (set! deck lista)
              (set! p2 (retornarCartas 2 deck deckGUI))
              (send table stack-cards p2)
              (send table move-cards-to-region p2 player2-region)
              (send table cards-face-up p2))))

(define (solicitar_Carta3)
  (auxS_C3 (recorrerDeck 3 deck "Solicitar Carta")))
(define (auxS_C3 lista)
  (cond ((equal? lista #f) "Segundo jugador")
        (else (set! deck lista)
              (set! p3 (retornarCartas 3 deck deckGUI))
              (if (equal? (send table stack-cards p3) '()) '() (send table stack-cards p3))
              (send table move-cards-to-region p3 player3-region)
              (send table cards-face-up p3))))

(send table add-region hit-button1)
(send table add-region stand-button1)

(set-region-callback! hit-button1 solicitar_Carta)

(define (plantarse1)
  (send table remove-region hit-button1)
  (send table remove-region stand-button1)
  (send table add-region hit-button2)
  (send table add-region stand-button2))
  
(set-region-callback! stand-button1 plantarse1)

(set-region-callback! hit-button2 solicitar_Carta2)

(define (plantarse2)
  (send table remove-region hit-button2)
  (send table remove-region stand-button2)
  (send table add-region hit-button3)
  (send table add-region stand-button3))

(set-region-callback! stand-button2 plantarse2)

(set-region-callback! hit-button3 solicitar_Carta3)

(define (plantarse3)
  
  (send table remove-region hit-button3)
  (send table remove-region stand-button3)
  (set! deck (recorrerDeck 4 deck "Solicitar Carta"))
  (set! d (retornarCartas 4 deck deckGUI))
  (send winner show #t)
  )

;;Funcion que muestra los resultados
(define resultadosFinales (resultados deck))
;;;
(define wj1 (make-buttonW (car resultadosFinales) 1))
(define wj2 (make-buttonW (cadr resultadosFinales) 60))
(define wj3 (make-buttonW (caddr resultadosFinales) 120))
(define crupi (make-buttonW (cadddr resultadosFinales) 180))
;;;
(send winner add-region wj1)
(send winner add-region wj2)
(send winner add-region wj3)
(send winner add-region crupi)


(set-region-callback! stand-button3 plantarse3)
  
(send table add-region j1)
(send table add-region j2)
(send table add-region j3)

(define (main)
   ;(send table add-region hit-button)
   (send table move-cards-to-region p1 player1-region)
   (send table move-cards-to-region p2 player2-region)
   (send table move-cards-to-region p3 player3-region)
   (send table move-cards-to-region d crupier-region)
  (send table cards-face-up p1)
  (send table cards-face-up p2)
  (send table cards-face-up p3)
  (send table card-face-up (car d)))
 

(main)