#lang racket

(require games/cards racket/gui racket/class)
(provide (all-defined-out))


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

;;Construcción del deck de la GUI
(define deck (make-deck))

;; Ancho y largo de las cartas
(define cwidth (send (car deck) card-width))
(define cheight (send (car deck) card-height))

;; Definición del tamaño de los botones
(define BUTTON-HEIGHT 16)
(define BUTTON-WIDTH cwidth)

;;; Evita que el usuario mueva las cartas 
;(for-each (lambda (card) (send* card (user-can-move #f) (user-can-flip #f)))
;          deck)

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

;(define player1-region
;  (make-region 50 50 cwidth cheight #f #f))
;



;; Creacion de botones
(define (make-button title pos)
  (make-button-region (+ (/ (- w (* 4 BUTTON-WIDTH) (* 3 MARGIN)) 2)
                         (* pos (+ BUTTON-WIDTH MARGIN)))
                      (- h MARGIN BUTTON-HEIGHT)
                      BUTTON-WIDTH BUTTON-HEIGHT
                      title void))

(define hit-button (make-button "Hit" 1))
(define stand-button (make-button "Stand" 2))

;; Se ponen las cartas sobre la mesa

(send table add-cards-to-region deck deck-region)
;(send table move-cards-to-region (deal 2) player1-region)


;(define (play 
; PONE EL DECK EN LA MESA
;(send table move-cards-to-region deck deck-region)
;(send table stack-cards deck)


;;; Funcion principal
;(let shuffle-loop ()
;  
;  ;; AQUI VA LA FUNCION QUE REPARTE LAS CARTAS INICIALES
;  (let*
;      ([deck (shuffle-list deck 7)]
;         [discard null]
;         [deal (lambda (n)
;                 (let deal ([n n])
;                   (if (zero? n)
;                     null
;                     (let ([c (car deck)])
;                       (set! deck (cdr deck))
;                       (cons c (deal (sub1 n)))))))])
;    
;    ;; PONE EL DECK EN LA MESA
;    (send table move-cards-to-region deck deck-region)
;    (send table stack-cards deck)
;    
;    ;; LOOP PRINCIPAL QUE SE EJECUTA HASTA QUE FINALICE EL JUEGO **VER CONDICION DE CUANDO SE ACABAN LAS CARTAS DEL DECK
;    ;;se debe modificar 
;    (let loop ()
;      (let ([p (deal 2)])
;        
;        ;; Sección que mueve las cartas a la region del jugador
;        (send table move-cards-to-region p player1-region)
;        (send table cards-face-up p)
;        
;        ;; Deal to dealer
;        (let ([d (deal 2)])
;          
;          ;; Mueve las cartas del crupier a su region y muestra solo una
;          (send table move-cards-to-region d crupier-region)
;          (send table card-face-up (car d))
;          
;          (let* (
;                 [continue (make-semaphore)]
;                 
;                 ;; Funcion que establece el resultado AQUI debería implementarse la tabla
;                 [make-status
;                  (lambda (title continue)
;                    (let ([r (make-button-region
;                              (/ (- w (* 2 cwidth)) 2)
;                              (region-y hit-button)
;                              (* 2 cwidth) BUTTON-HEIGHT
;                              title #f)])
;                      (set-region-callback! r (lambda ()
;                                                (send table remove-region r)
;                                                (semaphore-post continue)))
;                      r))]
;                 
;                 ;; Esta funcion cambia los botones de la interfaz para mostrar el resultado, no necesaria para nuestra GUI
;                 [done
;                  (lambda (title continue)
;                    (send table remove-region hit-button)
;                    (send table remove-region stand-button)
;                    (send table add-region (make-status title continue))
;                    )]
;                 
;                 ;; FUNCION PARA TERMINAR : calcula los puntajes finales, verifica blackjac aquí también se podría mostrar la tabla
;                 ;;introducir aqui funcion de la lógica :v
;                 [finish
;                  (lambda (p blackjack?)
;                    (let ([pt (best-total p)]
;                          [dt (best-total d)]
;                          [continue (make-semaphore)])
;                      (cond
;                        [(or (> dt 21) (> pt dt))
;                         (done (if blackjack?
;                                 "Blackjack"
;                                 "You Win")
;                               continue)]
;                        [(> dt pt)
;                         (done (if blackjack?
;                                 "Dealer Blackjack"
;                                 "You Lose")
;                               continue)]
;                        [else (done "Push" continue)])
;                      (yield continue)))]
;
;                 ;;Termina el turno del jugador, funcion de la GUI
;                 [finish-split
;                  (lambda (p player-region player-wait-region player-border)
;                    (unless (bust? p)
;                      (send table move-cards-to-region p player-region)
;                      (send table add-region player-border)
;                      (finish p #f)
;                      (send table remove-region player-border)
;                      (send table move-cards-to-region p player-wait-region)))]
;
;                 ;; Funcion para indicar que el jugador se paso de 21
;                 [bust (lambda ()
;                         (done "Usted se ha pasado de verga" continue))]
;
;                 ;; Bust in one hand of a split
;                 [local-bust (lambda ()
;                               (let ([cont (make-semaphore)])
;                                 (done "Bust" cont)
;                                 (yield cont)))]
;
;                 ;; Callback for the hit button; the button's callback is
;                 ;; changed for different modes: normal, split part 1, or split
;                 ;; part 2
;                 [make-hit-callback
;                  (lambda (get-p set-p! player-region bust)
;                    (lambda ()
;                      (set-p! (append (deal 1) (get-p)))
;                      (send table stack-cards (get-p))
;                      (send table move-cards-to-region (get-p) player-region)
;                      (send table cards-face-up (get-p))
;                      ;; Check for bust
;                      (when (bust? (get-p)) (bust))))])
;
;
;            ;; Funcion que verifica el blackjack natural, cambiar por la de lógica
;            (if (or (= 21 (best-total p))
;                    (= 21 (best-total d)))
;              (begin
;
;                ;; Show the dealers cards...
;                (send table cards-face-up d)
;
;                ;; ... and compute the result
;                (finish p #t))
;
;              (begin
;                ;; Three basic actions are allowed:
;                (send table add-region hit-button)
;                (send table add-region stand-button)
;
;                ;; Set the callbacks for normal (unsplit) hands
;                (set-region-callback!
;                 hit-button
;                 (make-hit-callback (lambda () p)
;                                    (lambda (v) (set! p v))
;                                    player1-region
;                                    bust))
;                (set-region-callback!
;                 stand-button
;                 (lambda () (semaphore-post continue)))           
;                ;; Wait until the player is done
;                (yield continue)
;
;                ;; No more player actions; get rid of the buttons
;                (send table remove-region hit-button)
;                (send table remove-region stand-button)
;
;                ;; If all the player's hards are bust, the dealer doesn't do
;                ;; anything
;                (unless (bust? p)
;                  ;; Show the dealer's starting hand
;                  (send table card-face-up (cadr d))
;                  (let loop ()
;                    ;; Hit on 16 or lower, stand on 17 and higher
;                    (when (< (best-total d) 17)
;                      ;; Hit the dealer
;                      (set! d (append (deal 1) d))
;                      (send table stack-cards d)
;                      (send table move-cards-to-region d crupier-region)
;                      (send table cards-face-up d)
;                      (loop)))
;                  (finish p #f)
;
;                  )))
;            ;; Move all the discarded cards to the back
;            (unless (null? discard)
;              (send table card-to-back (car discard))
;              (send table stack-cards discard))
;            ;; Discard all the cards we used
;            (set! discard (append p d discard)) ;;retorna las cartas a la pila de descarte 
;            (send table cards-face-down discard) ;;pone las cartas boca abajo
;            (send table move-cards-to-region discard discard-region) ;; mueve las cartas de una zona a otra
;           
;              (loop)))))))
;
;;; Function to compute the normal or minimum value of a card
;(define (min-card-value c)
;  (let ([v (send c get-value)]) (if (> v 10) 10 v)))
;
;;; Function to compute the value of a hand, counting aces as 1 or 11
;;;  to get the highest total possible under 21
;(define (best-total l)
;  (let* ([ace? (lambda (is?) (lambda (c) (eq? is? (= (send c get-value) 1))))]
;         [aces (filter (ace? #t) l)]
;         [others (filter (ace? #f) l)]
;         [base (apply + (map min-card-value others))])
;    (let loop ([l aces][base base])
;      (cond [(null? l) base]
;            [(<= (+ base (* (length aces) 11)) 21)
;             (+ base (* (length aces) 11))]
;            [else (loop (cdr l) (add1 base))]))))
;
;;; Function to test whether a hand is a bust
;(define (bust? p)
;  (> (best-total p) 21))
;
;(define (deal n)
;                 (let deal ([n n])
;                   (if (zero? n)
;                     null
;                     (let ([c (car deck)])
;                       (set! deck (cdr deck))
;                       (cons c (deal (sub1 n)))))))